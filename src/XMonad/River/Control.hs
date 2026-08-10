{-# LANGUAGE LambdaCase #-}
-- | The channel a second @xmonad@ process uses to reach a running one.
--
-- Under X11 this came free: @xmonad --restart@ put a client message on the
-- root window and the server delivered it.  River mediates the handover
-- between one window manager and its successor but offers nothing between two
-- window manager /processes/, so the channel has to exist here.
--
-- A unix socket in @$XDG_RUNTIME_DIR@, beside the compositor's own and with
-- the same lifetime as the session.  What this replaces is a pid file plus
-- @SIGUSR1@, which had the flaw every pid file has: after pid reuse the
-- signal reaches an unrelated process, and @SIGUSR1@'s default disposition is
-- to kill what it hits.  Connecting to a socket nobody is listening on fails
-- cleanly and means exactly what it says.
--
-- Being a socket rather than a signal, the request can also be /answered/.  A
-- window manager that refuses to restart -- because the executable it would
-- come back as is gone -- can say so to the terminal that asked, with an exit
-- status, instead of only to the session log where nobody is looking.
module XMonad.River.Control
  ( Request(..)
  , Reply(..)
  , controlSocketPath
  , serveControl
  , sendRequest
  , awaitRestart
  , answerRestart
  ) where

import Control.Concurrent (ThreadId, forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, void)
import Data.IORef
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)
import System.Timeout (timeout)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B8
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB

-- | What one process may ask another to do.
--
-- One constructor today.  The point of a socket over a signal is that this
-- can grow without needing a second signal number and a second convention for
-- what it means.
data Request = Restart
  deriving (Eq, Show)

-- | The answer.  'Refused' carries something worth printing.
data Reply = Ok | Refused String
  deriving (Eq, Show)

encodeRequest :: Request -> B8.ByteString
encodeRequest Restart = B8.pack "restart\n"

decodeRequest :: B8.ByteString -> Maybe Request
decodeRequest raw = case B8.unpack (B8.takeWhile (/= '\n') raw) of
  "restart" -> Just Restart
  _         -> Nothing

encodeReply :: Reply -> B8.ByteString
encodeReply = \case
  Ok          -> B8.pack "ok\n"
  Refused msg -> B8.pack ("refused " ++ msg ++ "\n")

decodeReply :: B8.ByteString -> Reply
decodeReply raw = case break (== ' ') (B8.unpack (B8.takeWhile (/= '\n') raw)) of
  ("ok", _)            -> Ok
  ("refused", ' ':msg) -> Refused msg
  (other, _)           -> Refused ("unintelligible reply: " ++ other)

-- | Where the socket lives.
--
-- Named for the Wayland display, so that two rivers on one machine get one
-- window manager each rather than fighting over a single path.  A display
-- given as an absolute path -- which the spec allows -- contributes only its
-- final component, since the whole thing would not fit in a @sun_path@.
controlSocketPath :: IO (Either String FilePath)
controlSocketPath = do
  mDir <- lookupEnv "XDG_RUNTIME_DIR"
  disp <- maybe "wayland-0" takeFileName <$> lookupEnv "WAYLAND_DISPLAY"
  pure $ case mDir of
    Nothing  -> Left "XDG_RUNTIME_DIR is not set"
    Just dir -> Right (dir </> ("xmonad-river-" ++ disp ++ ".sock"))

-- | Listen for requests, on a thread of its own, and answer them with the
-- given handler.
--
-- Its own thread rather than an fd in the event loop's wait set, and that is
-- deliberate: the request most worth delivering is the one sent to a window
-- manager that has stopped reading.  A listener sharing the loop's fate could
-- not serve it, and @xmonad --restart@ would stop being the escape hatch it
-- is.  The handler runs here rather than on the loop for the same reason.
--
-- A socket file left behind by a window manager that was killed rather than
-- asked to stop would make this fail forever, so a path that exists but
-- refuses a connection is treated as litter and removed.  One that /accepts/
-- is left alone and reported: something is already listening, and river will
-- have said so too.
serveControl :: FilePath -> (Request -> IO Reply) -> IO (Either String ThreadId)
serveControl path handle = do
  stale <- isStale path
  case stale of
    Left err -> pure (Left err)
    Right () -> do
      r <- E.try $ do
        sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
        -- Close-on-exec, and it is load-bearing.  A restart is an @exec@ of
        -- this same process, so without it the successor inherits the
        -- listening descriptor: the path still accepts connections, which
        -- makes the successor think another window manager owns it, and the
        -- connections it accepts land in a backlog nobody is reading.  The
        -- symptom is a second @xmonad --restart@ that hangs and then reports
        -- that the window manager never answered.
        N.withFdSocket sock $ \fd ->
          setFdOption (fromIntegral fd) CloseOnExec True
        N.bind sock (N.SockAddrUnix path)
        N.listen sock 4
        pure sock
      case r of
        Left e   -> pure (Left (show (e :: E.IOException)))
        Right sk -> fmap Right $ forkIO $ forever $ do
          -- One connection at a time.  These arrive at human speed and are
          -- answered in microseconds; a queue of four is already generous.
          accepted <- E.try (N.accept sk)
          case accepted of
            Left e -> void (pure (e :: E.IOException))
            Right (conn, _) -> E.handle ignoreIO $ flip E.finally (N.close conn) $ do
              raw <- NB.recv conn 256
              case decodeRequest raw of
                Nothing  -> NB.sendAll conn (encodeReply (Refused "unknown request"))
                Just req -> NB.sendAll conn . encodeReply =<< handle req
  where
    ignoreIO :: E.IOException -> IO ()
    ignoreIO _ = pure ()

-- | Whether the path is free to bind, removing it if it is only litter.
isStale :: FilePath -> IO (Either String ())
isStale path = do
  probe <- E.try $ do
    sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
    E.finally (N.connect sock (N.SockAddrUnix path)) (N.close sock)
  case probe of
    -- Something answered: a window manager is already running here.
    Right () -> pure (Left ("another window manager is listening on " ++ path))
    -- Nothing answered, so whatever is at that path is dead.  Removing a file
    -- that was never there is not an error worth reporting.
    Left e   -> do
      void (E.try (removeFile path) :: IO (Either E.IOException ()))
      void (pure (e :: E.IOException))
      pure (Right ())

-- | Send a request to the running window manager and wait for its answer.
sendRequest :: FilePath -> Request -> IO (Either String Reply)
sendRequest path req = do
  r <- E.try $ do
    sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
    flip E.finally (N.close sock) $ do
      N.connect sock (N.SockAddrUnix path)
      NB.sendAll sock (encodeRequest req)
      -- Bounded, because a window manager that accepted the connection and
      -- then stopped answering should not hang the terminal that asked.
      timeout replyTimeoutMicros (NB.recv sock 256)
  pure $ case r of
    Left e            -> Left (show (e :: E.IOException))
    Right Nothing     -> Left "the window manager accepted the request but did not answer"
    Right (Just raw)
      | B8.null raw   -> Left "the window manager closed the connection without answering"
      | otherwise     -> Right (decodeReply raw)

replyTimeoutMicros :: Int
replyTimeoutMicros = 10 * 1000 * 1000

--------------------------------------------------------------------------------
-- Answering a restart

-- | The slot a pending restart request waits on.
--
-- A process global for the same reason 'XMonad.River.Runtime.setMainThread'
-- is one: the thing that answers is the event loop, which is reached through
-- an asynchronous exception and so cannot be handed anything.  There is one
-- window manager per process, so one slot is a faithful stand-in.
{-# NOINLINE replyRef #-}
replyRef :: IORef (Maybe (MVar Reply))
replyRef = unsafePerformIO (newIORef Nothing)

-- | Ask for a restart and wait for the loop to say what became of it.
--
-- Bounded: a loop wedged so thoroughly that even the asynchronous exception
-- does not reach it should produce a message rather than a hang.
awaitRestart :: IO () -> IO Reply
awaitRestart request = do
  slot <- newEmptyMVar
  writeIORef replyRef (Just slot)
  request
  timeout replyTimeoutMicros (takeMVar slot) >>= \case
    Just reply -> pure reply
    Nothing    -> do
      writeIORef replyRef Nothing
      pure (Refused "the window manager did not respond")

-- | Answer whatever restart request is outstanding, if any.
--
-- Called by the event loop once it knows whether it is going to restart.
-- Taking the slot rather than reading it means a second answer -- from a path
-- that runs after the first -- is silently dropped rather than blocking.
answerRestart :: Reply -> IO ()
answerRestart reply = do
  slot <- atomicModifyIORef' replyRef (\s -> (Nothing, s))
  mapM_ (\mv -> void (E.try (putMVar mv reply) :: IO (Either E.SomeException ()))) slot
