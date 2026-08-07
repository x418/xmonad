{-# LANGUAGE LambdaCase #-}

-- | A window-manager-owned window that is an ordinary Wayland client.
--
-- Prompts need something the window management protocol deliberately does not
-- provide: raw keyboard input.  @river_seat_v1@ reports which /binding/ fired
-- and never which key was pressed, which is right for a window manager and
-- useless for a text field.  A binding per keysym can express shortcuts but
-- not typing -- no dead keys, no compose sequences, no input method, no key
-- repeat -- so anyone whose language needs more than unmodified ASCII would be
-- unable to type at all.
--
-- The way out is to stop being only a window manager for a moment.  Nothing
-- prevents this process opening a /second/ connection to the same compositor
-- and behaving as an ordinary client on it: a @zwlr_layer_shell_v1@ surface
-- with keyboard interactivity gets real focus, real @wl_keyboard@ events, and
-- the keymap, which "XMonad.River.Xkb" turns into keysyms and text.
--
-- == One connection, one thread
--
-- 'Connection' buffers requests in 'IORef's and is not thread-safe.  Two
-- connections now exist -- the window manager's and this one -- and the rule
-- that keeps that safe is that each is touched by exactly one thread.
--
-- That rule is enforced by construction rather than by comment.  'startClient'
-- forks a thread that owns 'clConn' outright, and nothing in this module's
-- interface hands that connection out.  Everything a caller can do -- redraw,
-- close -- is posted to a mailbox the client thread drains, so a caller
-- physically cannot touch the wrong connection.  The window manager's loop is
-- likewise never blocked: it never waits on this thread, and results come back
-- through the callbacks the caller supplied.
module XMonad.River.Client
  ( ClientSpec(..)
  , ClientHandle(..)
  , Anchor(..)
  , startClient
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef
import Data.Word (Word32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import XMonad.River.Buffer
import XMonad.River.Connection
import XMonad.River.Mailbox (Mailbox)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.LayerShellClient
import XMonad.River.Wire (ObjectId, nullObject)
import XMonad.River.Xkb
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (closeFd)
import qualified Control.Exception as E
import System.IO.Error (isEOFError)
import System.Posix.IO.ByteString (fdRead)
import System.Posix.Types (Fd)

-- | Where on the screen the window sits.
data Anchor = AnchorTop | AnchorBottom | AnchorCentre
  deriving (Eq, Show)

data ClientSpec = ClientSpec
  { csWidth  :: !Int
  , csHeight :: !Int
  , csAnchor :: !Anchor
  , csMargin :: !(Int, Int, Int, Int)
    -- ^ Top, right, bottom, left, in pixels from the anchored edges.
    --
    -- How a surface is placed at all: a layer surface has no coordinates, only
    -- an anchor and a distance from it.  A completion list sits under a prompt
    -- by being anchored the same way with a top margin of the prompt's height.
  , csKeyboard :: !Bool
    -- ^ Whether this surface takes the keyboard.
    --
    -- Exactly one surface of a prompt may: two asking for exclusive
    -- interactivity would fight over focus, and the completion list wants none
    -- -- it is shown by the prompt, not typed into.
  , csDraw   :: Buffer -> IO ()
    -- ^ Fill the buffer.  Runs on the client thread; see the module header.
  , csOnKey  :: Word32 -> String -> IO ()
    -- ^ A key was pressed: its keysym, and the text it produces.
    --
    -- The two are separate questions.  A keysym identifies the key -- which is
    -- what a binding like @Escape@ or @Return@ matches on -- while the text is
    -- what belongs in a field, and is empty for a dead key part-way through a
    -- compose sequence.
  , csOnClose :: IO ()
    -- ^ The compositor took the window away, or 'chClose' was called.
  }

data ClientHandle = ClientHandle
  { chRedraw :: IO ()
  , chClose  :: IO ()
  }

-- | Start a client window on its own thread and connection.
--
-- Returns as soon as the thread is running; the window appears when the
-- compositor has configured it.  Nothing here blocks the caller, which matters
-- because the caller is usually the window manager's event loop.
startClient :: ClientSpec -> IO ClientHandle
startClient spec = do
  inbox <- MB.newMailbox
  void . forkIO $ clientMain spec inbox
  pure ClientHandle
    { chRedraw = MB.post inbox Redraw
    , chClose  = MB.post inbox Close
    }

data Request = Redraw | Close

data Client = Client
  { clConn    :: !Connection
  , clShm     :: !ObjectId
  , clSurface :: !ObjectId
  , clLayer   :: !ObjectId
  , clBuffer  :: !(IORef (Maybe Buffer))
  , clSize    :: !(IORef (Int, Int))
  , clXkb     :: !(IORef (Maybe XkbState))
  , clRunning :: !(IORef Bool)
  , clConfigured :: !(IORef Bool)
    -- ^ Whether the compositor has sent a @configure@ and it has been acked.
    --
    -- Attaching a buffer before that is a protocol error -- wlroots says
    -- \"layer_surface has never been configured\" and drops the connection --
    -- and it is easy to hit by accident, because the caller creates a prompt
    -- and asks it to draw in the same breath, which races the compositor's
    -- reply.  Nothing is lost by skipping such a redraw: the configure handler
    -- draws as soon as it arrives, from whatever state the prompt has by then.
  }

clientMain :: ClientSpec -> Mailbox Request -> IO ()
clientMain spec inbox = do
  conn <- connect
  (registry, globals) <- getRegistry conn

  let bind iface ver = bindGlobal conn registry globals iface 1 ver
  mComp  <- bind wlCompositorInterface wlCompositorVersion
  mShm   <- bind wlShmInterface wlShmVersion
  mSeat  <- bind wlSeatInterface wlSeatVersion
  mShell <- bind zwlrLayerShellV1Interface zwlrLayerShellV1Version

  case (mComp, mShm, mShell) of
    (Just (comp, _), Just (shm, _), Just (shell, _)) -> do
      surface <- wlCompositorCreateSurface conn comp
      -- No output named: the compositor picks, which is what puts the prompt
      -- on the monitor with focus.  "overlay" so it sits above everything --
      -- a prompt hidden behind a fullscreen window is a prompt that appears
      -- to have not opened.
      layer <- zwlrLayerShellV1GetLayerSurface conn shell surface nullObject
                 zwlrLayerShellV1LayerOverlay (BC.pack "xmonad-prompt")

      zwlrLayerSurfaceV1SetSize conn layer
        (fromIntegral (csWidth spec)) (fromIntegral (csHeight spec))
      zwlrLayerSurfaceV1SetAnchor conn layer (anchorBits (csAnchor spec))
      let (mt, mr, mb, ml) = csMargin spec
      zwlrLayerSurfaceV1SetMargin conn layer
        (fromIntegral mt) (fromIntegral mr) (fromIntegral mb) (fromIntegral ml)
      -- Exclusive rather than on-demand: a prompt wants every key while it is
      -- open, including ones the compositor would otherwise treat as its own.
      zwlrLayerSurfaceV1SetKeyboardInteractivity conn layer $
        if csKeyboard spec
          then zwlrLayerSurfaceV1KeyboardInteractivityExclusive
          else zwlrLayerSurfaceV1KeyboardInteractivityNone
      wlSurfaceCommit conn surface

      bufRef <- newIORef Nothing
      sizeRef <- newIORef (csWidth spec, csHeight spec)
      xkbRef <- newIORef Nothing
      running <- newIORef True
      configured <- newIORef False
      let cl = Client conn shm surface layer bufRef sizeRef xkbRef running
                      configured

      when (csKeyboard spec) $
        forM_ mSeat $ \(seat, _) -> setupKeyboard spec cl seat

      zwlrLayerSurfaceV1Listen conn layer $ \case
        -- The compositor decides the final size; a layer surface must ack the
        -- configure before attaching anything, or the surface is never mapped.
        ZwlrLayerSurfaceV1Configure serial w h -> do
          when (w > 0 && h > 0) $
            writeIORef sizeRef (fromIntegral w, fromIntegral h)
          zwlrLayerSurfaceV1AckConfigure conn layer serial
          writeIORef configured True
          redraw spec cl
        ZwlrLayerSurfaceV1Closed -> shutdown spec cl
        _ -> pure ()

      flush conn
      loop spec cl inbox

    _ -> do
      hPutStrLn stderr
        "xmonad-river: the compositor does not offer wl_compositor, wl_shm and \
        \zwlr_layer_shell_v1, so prompts cannot be shown"
      disconnect conn
      csOnClose spec

anchorBits :: Anchor -> Word32
anchorBits = \case
  -- Anchoring to both sides as well as an edge is what makes the width the
  -- compositor gives us the full width of the output.
  AnchorTop    -> zwlrLayerSurfaceV1AnchorTop
                  .|. zwlrLayerSurfaceV1AnchorLeft
                  .|. zwlrLayerSurfaceV1AnchorRight
  AnchorBottom -> zwlrLayerSurfaceV1AnchorBottom
                  .|. zwlrLayerSurfaceV1AnchorLeft
                  .|. zwlrLayerSurfaceV1AnchorRight
  AnchorCentre -> 0   -- no anchor: the compositor centres it

setupKeyboard :: ClientSpec -> Client -> ObjectId -> IO ()
setupKeyboard spec cl seat = do
  let conn = clConn cl
  kb <- wlSeatGetKeyboard conn seat
  wlKeyboardListen conn kb $ \case
    -- The keymap arrives as a descriptor holding the text xkb parses.  This is
    -- the one place the whole design needed fd passing to work.
    WlKeyboardKeymap _fmt fd size -> do
      text <- readFdText fd (fromIntegral size)
      closeFd fd
      newXkbState text >>= \case
        Just st -> writeIORef (clXkb cl) (Just st)
        Nothing -> hPutStrLn stderr
          "xmonad-river: the compositor's keymap did not parse; prompt input \
          \will not work"
    WlKeyboardModifiers _serial dep lat lok grp ->
      readIORef (clXkb cl) >>= mapM_ (\st -> updateModifiers st dep lat lok grp)
    -- state 1 is pressed; releases and repeats are not acted on here.
    WlKeyboardKey _serial _time code state | state == 1 ->
      readIORef (clXkb cl) >>= \case
        Nothing -> pure ()
        Just st -> do
          sym <- keycodeToKeysym st code
          txt <- keycodeToUtf8 st code
          csOnKey spec sym txt
    _ -> pure ()

-- | Read the keymap out of the descriptor the compositor sent.
--
-- The size comes from the event rather than from reading to end-of-file,
-- because the mapping is not necessarily exhausted where the text ends.
--
-- Bytes, not text, until the whole thing has arrived: a keymap is UTF-8 and a
-- chunked read can land mid-sequence, so decoding per chunk would corrupt any
-- keysym name outside ASCII.  That is what the deprecation on the text-mode
-- fdRead is warning about.
readFdText :: Fd -> Int -> IO String
readFdText fd size = BC.unpack <$> go size BS.empty
  where
    go 0 acc = pure acc
    go n acc = do
      -- fdRead throws on end-of-file rather than returning empty, so a
      -- descriptor holding fewer bytes than the event advertised -- or one
      -- whose connection has since died -- has to be caught rather than
      -- tested for.  A truncated keymap is still worth parsing; throwing here
      -- would take the prompt down.
      chunk <- E.catch (fdRead fd (fromIntegral (min n 4096)))
                       (\e -> if isEOFError e then pure BS.empty else E.throwIO e)
      if BS.null chunk
        then pure acc
        else go (n - BS.length chunk) (acc <> chunk)

redraw :: ClientSpec -> Client -> IO ()
redraw spec cl = do
  alive <- readIORef (clRunning cl)
  ready <- readIORef (clConfigured cl)
  when (alive && ready) $ do
    (w, h) <- readIORef (clSize cl)
    existing <- readIORef (clBuffer cl)
    buf <- case existing of
      Just b | bufWidth b == w && bufHeight b == h -> pure b
      Just b -> do
        destroyBuffer (clConn cl) b
        fresh <- newBuffer (clConn cl) (clShm cl) w h
        writeIORef (clBuffer cl) (Just fresh)
        pure fresh
      Nothing -> do
        fresh <- newBuffer (clConn cl) (clShm cl) w h
        writeIORef (clBuffer cl) (Just fresh)
        pure fresh
    csDraw spec buf
    wlSurfaceAttach (clConn cl) (clSurface cl) (bufObject buf) 0 0
    wlSurfaceDamageBuffer (clConn cl) (clSurface cl) 0 0
      (fromIntegral w) (fromIntegral h)
    wlSurfaceCommit (clConn cl) (clSurface cl)
    flush (clConn cl)

shutdown :: ClientSpec -> Client -> IO ()
shutdown spec cl = do
  alive <- readIORef (clRunning cl)
  when alive $ do
    writeIORef (clRunning cl) False
    readIORef (clBuffer cl) >>= mapM_ (destroyBuffer (clConn cl))
    readIORef (clXkb cl) >>= mapM_ freeXkbState
    zwlrLayerSurfaceV1Destroy (clConn cl) (clLayer cl)
    wlSurfaceDestroy (clConn cl) (clSurface cl)
    flush (clConn cl)
    disconnect (clConn cl)
    csOnClose spec

-- | The client's own event loop, on its own thread.
--
-- Waits on the compositor and on requests from the window manager together,
-- exactly as the window manager's loop does -- otherwise a redraw posted while
-- this sat in a blocking read would not be noticed until the next keystroke.
loop :: ClientSpec -> Client -> Mailbox Request -> IO ()
loop spec cl inbox = go
  where
    go = do
      alive <- readIORef (clRunning cl)
      when alive $ do
        -- Flush before waiting, for the same reason the window manager's loop
        -- does: a redraw drained from the inbox only queues its attach and
        -- commit, and waiting first means the compositor is never told, so
        -- nothing comes back to wake us and the prompt hangs unpainted.
        flush (clConn cl)
        sockFd <- connectionFd (clConn cl)
        r <- MB.waitEither sockFd (MB.mailboxFd inbox)
        case r of
          Left () -> dispatch (clConn cl)
          Right () -> do
            MB.clearWakeups inbox
            reqs <- MB.drain inbox
            forM_ reqs $ \case
              Redraw -> redraw spec cl
              Close  -> shutdown spec cl
        go

