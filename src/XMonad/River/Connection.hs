{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Wayland connection management: the socket, object id allocation, request
-- batching, and the event dispatch loop.
--
-- The model mirrors @libwayland@ closely enough to be unsurprising: requests
-- are buffered and flushed together, events are read in batches and
-- dispatched to per-object listeners. It differs in that listeners are plain
-- Haskell closures stored in an 'IntMap' rather than vtables, and that a
-- decode failure is an exception rather than an abort.
--
-- Threads: requests may be queued and listeners set from any thread -- the
-- window manager's worker creates surfaces for decorations -- but reading,
-- dispatching and flushing belong to the one thread that owns the connection.
module XMonad.River.Connection
  ( -- * Connection
    Connection
  , connect
  , connectTo
  , connectSocket
  , disconnect
    -- * Requests
  , request
  , requestWithFds
  , takeFd
  , takeFdOrFail
  , newObject
  , freeObject
    -- * Listeners
  , Listener
  , setListener
  , clearListener
  , decode
    -- * Event loop
  , flush
  , connectionFd
  , dispatch
  , dispatchPending
  , roundtrip
  , syncThen
    -- * Registry
  , Global(..)
  , getRegistry
  , listenRegistry
  , bindGlobal
  , bindNamed
    -- * Errors
  , WaylandError(..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word (Word16, Word32)
import System.Environment (lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NBS

import Data.Store.Core (Peek)

import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)

import XMonad.River.Socket (recvWithFds, sendWithFds)
import XMonad.River.Wire

--------------------------------------------------------------------------------
-- Errors

data WaylandError
  = ConnectionFailed String
  | ProtocolError ObjectId Word32 ByteString
    -- ^ A @wl_display.error@ event: the offending object, an interface
    -- specific code, and a human readable message.
  | DecodeError String
  | Disconnected
  deriving (Show)

instance Exception WaylandError

--------------------------------------------------------------------------------
-- Connection

-- | A handler for events delivered to a single object. Receives the opcode and
-- the raw message body; generated code supplies the decoder.
type Listener = Word16 -> ByteString -> IO ()

-- | Decode an event body, turning a malformed message into an exception. A
-- decode failure means the server sent something this client's generated
-- bindings do not understand, which is not recoverable.
decode :: Peek a -> ByteString -> IO a
decode p body = either (throwIO . DecodeError . show) pure (decodeBody p body)

data Connection = Connection
  { connSocket    :: !N.Socket
  , connNextId    :: !(IORef Word32)
  , connFreeIds   :: !(IORef [Word32])
    -- ^ Ids reclaimed via @wl_display.delete_id@, reused before allocating new
    -- ones as the protocol requires.
  , connOut       :: !(IORef (Encoded, [Fd]))
    -- ^ Pending request bytes and their descriptors. They share one atomic
    -- queue because Wayland matches ancillary descriptors to fd arguments by
    -- position; observing one without the other corrupts the request stream.
  , connIn        :: !(IORef ByteString)
    -- ^ Bytes read but not yet forming a complete message.
  , connInFds     :: !(IORef [Fd])
    -- ^ Descriptors received but not yet claimed by a decoded event.
  , connListeners :: !(IORef (IM.IntMap Listener))
  }

-- | The @wl_display@ object always has id 1.
displayId :: ObjectId
displayId = ObjectId 1

-- | Connect using the environment, following the usual Wayland client rules:
-- @WAYLAND_SOCKET@ (an inherited, already connected fd) takes precedence, then
-- @WAYLAND_DISPLAY@ resolved against @XDG_RUNTIME_DIR@, defaulting to
-- @wayland-0@.
connect :: IO Connection
connect = do
  mSock <- lookupEnv "WAYLAND_SOCKET"
  case mSock >>= readMaybeInt of
    Just fd -> do
      sock <- N.mkSocket (fromIntegral (fd :: Int))
      newConnection sock
    Nothing -> do
      disp <- maybe "wayland-0" id <$> lookupEnv "WAYLAND_DISPLAY"
      if take 1 disp == "/"
        then connectTo disp
        else do
          mDir <- lookupEnv "XDG_RUNTIME_DIR"
          case mDir of
            Nothing -> throwIO (ConnectionFailed "XDG_RUNTIME_DIR is not set")
            Just dir -> connectTo (dir ++ "/" ++ disp)
  where
    readMaybeInt s = case reads s of
      [(n, "")] -> Just n
      _         -> Nothing

-- | Connect to an explicit socket path.
connectTo :: FilePath -> IO Connection
connectTo path = do
  sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  N.connect sock (N.SockAddrUnix path)
  newConnection sock

-- | Over a connected socket: a test's socketpair.
connectSocket :: N.Socket -> IO Connection
connectSocket = newConnection

newConnection :: N.Socket -> IO Connection
newConnection sock = do
  -- Client ids start at 2; 1 is wl_display.
  nextId    <- newIORef 2
  freeIds   <- newIORef []
  out       <- newIORef (mempty, [])
  inBuf     <- newIORef BS.empty
  inFds     <- newIORef []
  listeners <- newIORef IM.empty
  let conn = Connection sock nextId freeIds out inBuf inFds listeners
  setListener conn displayId (displayListener conn)
  pure conn

-- | The socket's descriptor, for waiting on it alongside something else.
--
-- The event loop needs this because it has two sources -- the compositor and
-- work posted by other threads -- and a blocking read on one would ignore the
-- other.  See "XMonad.River.Mailbox".
connectionFd :: Connection -> IO Fd
connectionFd conn = N.withFdSocket (connSocket conn) (pure . fromIntegral)

disconnect :: Connection -> IO ()
disconnect = N.close . connSocket

-- | @wl_display@ has two events we must handle ourselves: protocol errors, and
-- id recycling.
displayListener :: Connection -> Listener
displayListener conn opcode body = case opcode of
  -- error(object_id, code, message)
  0 -> do
    (oid, code, msg) <-
      decode ((,,) <$> getObject <*> getWord32 <*> getString) body
    throwIO (ProtocolError oid code msg)
  -- delete_id(id)
  1 -> do
    i <- decode getWord32 body
    clearListener conn (ObjectId i)
    atomicModifyIORef' (connFreeIds conn) (\is -> (i : is, ()))
  _ -> pure ()

--------------------------------------------------------------------------------
-- Objects and requests

-- | Allocate a client-side object id.
newObject :: Connection -> IO ObjectId
newObject conn = do
  reused <- atomicModifyIORef' (connFreeIds conn) $ \case
    (i:rest) -> (rest, Just i)
    []       -> ([], Nothing)
  case reused of
    Just i  -> pure (ObjectId i)
    Nothing -> ObjectId <$> atomicModifyIORef' (connNextId conn) (\i -> (i + 1, i))

-- | Drop a destroyed object's listener. The id itself is only reusable once
-- the server confirms with @delete_id@, so it is not returned to the free list
-- here.
freeObject :: Connection -> ObjectId -> IO ()
freeObject = clearListener

-- | Queue a request. Nothing is written to the socket until 'flush'.
request :: Connection -> ObjectId -> Word16 -> Encoded -> IO ()
request conn oid opcode args =
  atomicModifyIORef' (connOut conn) $ \(out, fds) ->
    ((out <> encodeMessage oid opcode args, fds), ())

-- | Queue a request that carries file descriptors.
--
-- The descriptors contribute nothing to the message body -- a Wayland @fd@
-- argument is pure ancillary data -- so the bytes are queued exactly as
-- 'request' would, and the descriptors are appended to the same atomic queue.
-- Order is the whole protocol here:
-- the server pairs each descriptor with the next fd argument it decodes.
--
-- __This takes ownership of the descriptors.__  They are closed once the
-- request has actually gone out, which is at the next 'flush' rather than
-- now.  A caller that closes its own copy immediately after calling this --
-- the obvious thing to write -- would have the connection transmit a closed
-- descriptor.
requestWithFds :: Connection -> ObjectId -> Word16 -> Encoded -> [Fd] -> IO ()
requestWithFds conn oid opcode args newFds =
  atomicModifyIORef' (connOut conn) $ \(out, fds) ->
    ((out <> encodeMessage oid opcode args, fds ++ newFds), ())

-- | Take the next descriptor delivered alongside an event.
--
-- Returns 'Nothing' if the server sent an fd argument without the descriptor,
-- which would be a protocol violation rather than something to recover from.
takeFd :: Connection -> IO (Maybe Fd)
takeFd conn = atomicModifyIORef' (connInFds conn) $ \fds -> case fds of
  (f:rest) -> (rest, Just f)
  []       -> ([], Nothing)

-- | Take a descriptor an event promised, failing loudly if it is not there.
--
-- The server sending an fd argument without the descriptor is a protocol
-- violation rather than something to paper over: everything decoded after it
-- would be misaligned, so the useful thing is to say so at the point it
-- happened.
takeFdOrFail :: Connection -> String -> IO Fd
takeFdOrFail conn argName = takeFd conn >>= \case
  Just fd -> pure fd
  Nothing -> throwIO (DecodeError ("no descriptor received for fd argument " ++ argName))

--------------------------------------------------------------------------------
-- Listeners

setListener :: Connection -> ObjectId -> Listener -> IO ()
setListener conn (ObjectId i) l =
  atomicModifyIORef' (connListeners conn) (\ls -> (IM.insert (fromIntegral i) l ls, ()))

clearListener :: Connection -> ObjectId -> IO ()
clearListener conn (ObjectId i) =
  atomicModifyIORef' (connListeners conn) (\ls -> (IM.delete (fromIntegral i) ls, ()))

--------------------------------------------------------------------------------
-- Event loop

-- | Write all buffered requests to the socket.
--
-- Every pending request is written into one buffer sized exactly to hold them
-- all, so the batch costs a single allocation and a single @write@, and no
-- byte is copied twice on its way out.
flush :: Connection -> IO ()
flush conn = do
  (pending, fds) <- atomicModifyIORef' (connOut conn) $ \p -> ((mempty, []), p)
  let bs = runEncoded pending
  unless (BS.null bs && null fds) $ sendAllWithFds conn bs fds
  -- The compositor holds its own descriptors now, so ours are dead weight --
  -- and a window manager forks constantly, so keeping them would leak a
  -- mapping into every subsequent child.
  mapM_ closeFd fds

-- | Write every byte, keeping the descriptors on the first message.
--
-- A short write is ordinary on a stream socket, and the retry must not resend
-- the descriptors: the server has already taken them, and sending them twice
-- would pair them with the wrong requests.
sendAllWithFds :: Connection -> ByteString -> [Fd] -> IO ()
sendAllWithFds conn = go
  where
    go bs fds = do
      n <- sendWithFds (connSocket conn) bs fds
      let rest = BS.drop n bs
      unless (BS.null rest) (go rest [])

-- | Flush, block for at least one batch of events, and dispatch them all.
-- This is the equivalent of @wl_display_dispatch@.
dispatch :: Connection -> IO ()
dispatch conn = do
  flush conn
  (chunk, fds) <- recvWithFds (connSocket conn) 4096
  unless (null fds) $ modifyIORef' (connInFds conn) (++ fds)
  when (BS.null chunk) $ throwIO Disconnected
  -- ByteString's append short-circuits when either side is empty, so this is
  -- free whenever the previous read ended on a message boundary -- which is
  -- the common case. Only a partial trailing message costs a copy, and then
  -- only of that fragment.
  modifyIORef' (connIn conn) (<> chunk)
  dispatchPending conn
  flush conn

-- | Dispatch any complete messages already buffered, without reading from the
-- socket.
dispatchPending :: Connection -> IO ()
dispatchPending conn = do
  buf <- readIORef (connIn conn)
  let (msgs, rest) = splitMessages buf
  writeIORef (connIn conn) rest
  mapM_ deliver msgs
  where
    deliver (h, body) = do
      ls <- readIORef (connListeners conn)
      case IM.lookup (fromIntegral (unObjectId (msgObject h))) ls of
        -- Events for objects we have already destroyed are expected and
        -- ignored: the server may not have processed the destructor yet.
        Nothing -> pure ()
        Just l  -> l (msgOpcode h) body

-- | Send @wl_display.sync@ and dispatch until the server answers, so that all
-- previously queued requests have been processed.
roundtrip :: Connection -> IO ()
roundtrip conn = do
  done <- newIORef False
  cb <- newObject conn
  setListener conn cb $ \_ _ -> writeIORef done True
  -- wl_display.sync(new_id wl_callback)
  request conn displayId 0 (argObject cb)
  let loop = do
        d <- readIORef done
        unless d (dispatch conn >> loop)
  loop
  clearListener conn cb

-- | Send @wl_display.sync@ and run the action when it is answered, on the
-- dispatching thread.  Nothing waits.
syncThen :: Connection -> IO () -> IO ()
syncThen conn act = do
  cb <- newObject conn
  -- @done@ destroys the callback; @delete_id@ returns the id.
  setListener conn cb $ \_ _ -> clearListener conn cb >> act
  request conn displayId 0 (argObject cb)

--------------------------------------------------------------------------------
-- Registry

data Global = Global
  { globalName      :: !Word32
  , globalInterface :: !ByteString
  , globalVersion   :: !Word32
  } deriving (Eq, Show)

-- | Create the registry and collect every global the server advertises. The
-- returned action performs a roundtrip, so the list is complete when it
-- returns.
getRegistry :: Connection -> IO (ObjectId, [Global])
getRegistry conn = do
  reg <- newObject conn
  acc <- newIORef []
  setListener conn reg $ \opcode body -> case opcode of
    -- global(name, interface, version)
    0 -> do
      (name, iface, ver) <-
        decode ((,,) <$> getWord32 <*> getString <*> getWord32) body
      modifyIORef' acc (Global name iface ver :)
    -- global_remove(name); nothing we bind is expected to vanish
    _ -> pure ()
  -- wl_display.get_registry(new_id wl_registry)
  request conn displayId 1 (argObject reg)
  roundtrip conn
  globals <- reverse <$> readIORef acc
  pure (reg, globals)

-- | Keep following the registry after 'getRegistry': a global that appears
-- later (an output plugged in) goes to the first handler, one that goes to
-- the second.  Replaces the listener 'getRegistry' installed.
listenRegistry :: Connection -> ObjectId -> (Global -> IO ()) -> (Word32 -> IO ()) -> IO ()
listenRegistry conn reg added removed =
  setListener conn reg $ \opcode body -> case opcode of
    0 -> do
      (name, iface, ver) <-
        decode ((,,) <$> getWord32 <*> getString <*> getWord32) body
      added (Global name iface ver)
    1 -> decode getWord32 body >>= removed
    _ -> pure ()

-- | Bind one specific global, at the lower of its version and the given one.
bindNamed :: Connection -> ObjectId -> Global -> Word32 -> IO ObjectId
bindNamed conn reg g maxVersion = do
  let ver = min maxVersion (globalVersion g)
  oid <- newObject conn
  request conn reg 0 (argUInt (globalName g) <> argNewIdAny (globalInterface g) ver oid)
  pure oid

-- | Bind a global, clamping to the version the server offers. Returns
-- 'Nothing' if the interface is absent or older than @minVersion@.
bindGlobal
  :: Connection
  -> ObjectId      -- ^ the registry
  -> [Global]
  -> ByteString    -- ^ interface name
  -> Word32        -- ^ minimum acceptable version
  -> Word32        -- ^ maximum version this client understands
  -> IO (Maybe (ObjectId, Word32))
bindGlobal conn reg globals iface minVersion maxVersion =
  case filter ((== iface) . globalInterface) globals of
    [] -> pure Nothing
    (g:_)
      | globalVersion g < minVersion -> pure Nothing
      | otherwise -> do
          let ver = min maxVersion (globalVersion g)
          oid <- newObject conn
          -- wl_registry.bind(name, new_id)
          request conn reg 0 (argUInt (globalName g) <> argNewIdAny iface ver oid)
          pure (Just (oid, ver))
