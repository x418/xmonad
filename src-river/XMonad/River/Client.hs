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
-- == Never outliving its thread
--
-- A client with @keyboard_interactivity = exclusive@ holds the keyboard for as
-- long as its surface exists.  If the thread that owns it stops without
-- destroying it -- an exception anywhere in the loop, in a draw callback, in a
-- key callback -- the compositor keeps handing every keystroke to a surface
-- nobody is listening to, and the session has no working keyboard until the
-- window manager is restarted.  This has happened.
--
-- So teardown is not on the success path.  'clientMain' runs inside a handler
-- that destroys the surface and drops the connection whatever happens, and
-- dropping the connection is the part that actually guarantees it: a
-- compositor destroys everything a departed client owned, so even a teardown
-- that is itself broken cannot leave the keyboard grabbed.
--
-- 'closeAllClients' is the last resort, for a client that is wedged rather
-- than dead.  A config should bind it: river matches xkb bindings /before/ it
-- consults keyboard focus (see @KeyboardGroup.processKey@), so a window
-- manager binding still fires while a layer surface holds an exclusive grab,
-- and is the one thing that can still be pressed when a prompt has stopped
-- responding.
--
-- == The startup watchdog
--
-- A prompt that opens correctly and is then left alone is indistinguishable,
-- from the outside, from one that has taken the keyboard and cannot use it:
-- both are silent.  So silence is not what is watched.  What is watched is
-- whether the prompt ever became /able/ to read the keyboard, which is settled
-- within a moment of opening and needs nothing from the user:
--
-- * the compositor configured the surface, without which nothing is on screen;
-- * the seat has a keyboard at all;
-- * it granted keyboard focus, which a @keyboard_interactivity = exclusive@
--   surface is given as soon as it maps rather than when someone types;
-- * a keymap arrived and parsed, without which a keycode can never become a
--   keysym and 'csOnKey' can never fire.
--
-- Any of those missing means every keystroke is going to a surface that will
-- never do anything with it -- which is the failure this whole module is built
-- to avoid -- and no amount of waiting will change it.  'startupDeadlineMicros'
-- after the client starts, such a prompt is closed and the reason is logged.
--
-- Deliberately /not/ a timeout on keystrokes.  A prompt waiting while someone
-- reads the screen is idle for minutes and is working perfectly; closing it
-- would turn a rare failure into a routine one.
module XMonad.River.Client
  ( ClientSpec(..)
  , ClientHandle(..)
  , Anchor(..)
  , startClient
  , closeAllClients
  , readFdText
    -- ^ Exported for @tests\/river-prompt-spec.hs@, which reads a keymap twice
    -- to check that the second read still works.  That is the one property of
    -- this function worth a test and the one it got wrong; see its haddock.
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
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
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)
import XMonad.River.Xkb
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (closeFd)
import qualified Control.Exception as E
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import System.Posix.Types (COff (..), CSsize (..), Fd (Fd))

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
  void . forkIO $ withRegistration (clientMain spec inbox)
  pure ClientHandle
    { chRedraw = MB.post inbox Redraw
    , chClose  = MB.post inbox Close
    }

--------------------------------------------------------------------------------
-- The register of live clients

-- | Every client thread currently running, so that a wedged one can be killed
-- from outside.
--
-- A process-level 'IORef' for the same reason the geometry tables in
-- "XMonad.River.Runtime" are: there is one window manager per process, and the
-- caller that needs this -- a panic keybinding -- has nothing to thread it
-- through.
{-# NOINLINE liveClients #-}
liveClients :: IORef (M.Map ThreadId ())
liveClients = unsafePerformIO (newIORef M.empty)

-- | Run a client thread, registered for the duration and cleaned up however it
-- ends.
withRegistration :: IO () -> IO ()
withRegistration act = do
  tid <- myThreadId
  E.bracket_
    (modifyIORef' liveClients (M.insert tid ()))
    (modifyIORef' liveClients (M.delete tid))
    act

-- | Kill every running client, releasing any keyboard grab they hold.
--
-- Returns how many there were.  For a keybinding of last resort: a prompt that
-- has stopped reading its keyboard leaves the compositor delivering every
-- keystroke to a surface that will never answer, and no amount of typing at it
-- helps.  This is what gets the keyboard back without restarting the window
-- manager.
--
-- Killing rather than asking politely is the point.  A request posted to the
-- client's mailbox is only read if its loop is still running, which is exactly
-- what is in doubt; 'killThread' raises an asynchronous exception, which
-- interrupts the blocking wait the loop spends its life in and unwinds through
-- the handler that destroys the surface and drops the connection.
closeAllClients :: IO Int
closeAllClients = do
  tids <- M.keys <$> readIORef liveClients
  mapM_ killThread tids
  pure (length tids)

data Request = Redraw | Close

data Client = Client
  { clConn    :: !Connection
  , clShm     :: !ObjectId
  , clSurface :: !ObjectId
  , clLayer   :: !ObjectId
  , clBuffer  :: !(IORef (Maybe Buffer))
  , clSize    :: !(IORef (Int, Int))
  , clXkb     :: !(IORef (Maybe XkbState))
  , clKeyboard :: !(IORef (Maybe ObjectId))
    -- ^ The @wl_keyboard@, once the seat has said it has one.  'Nothing' means
    -- it never did, which is the difference between a prompt that is waiting
    -- and one that will wait forever.
  , clHadFocus :: !(IORef Bool)
    -- ^ Whether the compositor has ever given this surface keyboard focus.
    --
    -- Latched rather than tracking the current state: the watchdog's question
    -- is whether the grab ever worked, and a prompt that has since /lost/ focus
    -- is not eating anyone's keystrokes.
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
      kbRef <- newIORef Nothing
      focusRef <- newIORef False
      running <- newIORef True
      configured <- newIORef False
      let cl = Client conn shm surface layer bufRef sizeRef xkbRef kbRef
                      focusRef running configured

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
      -- Everything above has created a surface that may be holding the
      -- keyboard.  From here on the only acceptable outcome is that it is
      -- destroyed, so the loop runs under a handler rather than in the open.
      watchStartup spec cl inbox
      loop spec cl inbox `E.finally` shutdown spec cl

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

-- | Take the seat's keyboard, once it says it has one.
--
-- Asking unconditionally is a protocol error -- @wl_seat.get_keyboard called
-- when no keyboard capability has existed@ -- and the compositor answers it by
-- dropping the connection, so a prompt opened on a seat with no keyboard used
-- to die with a raw @ProtocolError@ traceback and no explanation.  A seat with
-- no keyboard is not a hypothetical: it is every seat before its first input
-- device is added, and every headless one.
--
-- Capabilities arrive as an event and can be sent more than once, hence the
-- listener and the guard against taking the keyboard twice.
setupKeyboard :: ClientSpec -> Client -> ObjectId -> IO ()
setupKeyboard spec cl seat =
  wlSeatListen (clConn cl) seat $ \case
    WlSeatCapabilities caps
      | caps .&. wlSeatCapabilityKeyboard /= 0 -> do
          taken <- readIORef (clKeyboard cl)
          when (isNothing taken) $ do
            kb <- wlSeatGetKeyboard (clConn cl) seat
            writeIORef (clKeyboard cl) (Just kb)
            listenKeyboard spec cl kb
    _ -> pure ()

listenKeyboard :: ClientSpec -> Client -> ObjectId -> IO ()
listenKeyboard spec cl kb = do
  let conn = clConn cl
  wlKeyboardListen conn kb $ \case
    -- Focus, which an exclusive layer surface is given when it maps rather
    -- than when anyone types.  Recorded for the watchdog and nothing else;
    -- see the module header.
    WlKeyboardEnter _serial surf _keys
      | surf == clSurface cl -> writeIORef (clHadFocus cl) True
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
          guarded "key handler" (csOnKey spec sym txt)
    _ -> pure ()

-- | How long a client has to become usable before it is assumed broken.
--
-- Generous on purpose.  Everything it checks is settled within a round trip of
-- the surface being created, so ten seconds is not a guess at how long the
-- compositor might take -- it is the margin by which the answer is already in.
startupDeadlineMicros :: Int
startupDeadlineMicros = 10 * 1000 * 1000

-- | How long to wait for a client to close itself before killing its thread.
closeGraceMicros :: Int
closeGraceMicros = 1 * 1000 * 1000

-- | Close a client that never became able to read the keyboard.
--
-- See the module header for why this watches capability rather than silence.
-- The escalation matters as much as the check: 'Close' is posted first, which
-- is the orderly route and the one that works when the loop is healthy and only
-- the keyboard is not.  A loop that does not answer within 'closeGraceMicros'
-- is wedged as well as useless, and gets what 'closeAllClients' gives -- the
-- asynchronous exception that unwinds through the handler around 'loop'.
--
-- Note what this thread does /not/ do: touch 'clConn'.  One thread owns that
-- connection, and it is not this one.
watchStartup :: ClientSpec -> Client -> Mailbox Request -> IO ()
watchStartup spec cl inbox = do
  tid <- myThreadId
  void . forkIO $ do
    threadDelay startupDeadlineMicros
    alive <- readIORef (clRunning cl)
    when alive $ do
      faults <- startupFaults spec cl
      unless (null faults) $ do
        hPutStrLn stderr $
          "xmonad-river: closing a prompt that never became usable: "
            ++ intercalate "; " faults
            ++ ".  Every keystroke was going to it and it could not have "
            ++ "answered any of them."
        MB.post inbox Close
        threadDelay closeGraceMicros
        stuck <- readIORef (clRunning cl)
        when stuck $ do
          hPutStrLn stderr
            "xmonad-river: that prompt did not answer its own close request \
            \either, so killing its thread"
          killThread tid

-- | What is wrong with a client that has had 'startupDeadlineMicros' to start.
--
-- Empty means nothing is: it drew, it has focus, and it can turn a keycode
-- into a keysym.  Whether anyone has typed at it is not asked, and must not
-- be -- an idle prompt and a wedged one look the same from here, and only one
-- of them should be closed.
startupFaults :: ClientSpec -> Client -> IO [String]
startupFaults spec cl = do
  configured <- readIORef (clConfigured cl)
  keyboard   <- readIORef (clKeyboard cl)
  focused    <- readIORef (clHadFocus cl)
  xkb        <- readIORef (clXkb cl)
  pure $ concat
    [ [ "the compositor never configured its surface" | not configured ]
    , [ "the seat has no keyboard"
      | csKeyboard spec, isNothing keyboard ]
    , [ "it was never given keyboard focus"
      | csKeyboard spec, isJust keyboard, not focused ]
    , [ "the compositor's keymap never arrived, or did not parse"
      | csKeyboard spec, isJust keyboard, isNothing xkb ]
    ]

-- | Run one of the caller's callbacks without letting it take the client down.
--
-- These are the one place arbitrary code runs on this thread: a prompt's key
-- handler and its draw function come from xmonad-contrib and, through the
-- completion functions and 'XMonad.Prompt.XPrompt' instances, from the
-- config.  An exception from any of them used to unwind the whole loop, and
-- the surface holding the keyboard went with it -- except that it did not go,
-- it stayed, with nothing left running to service it.
--
-- Swallowing is the right call here even though it hides a bug.  A prompt that
-- misdraws one frame or ignores one keystroke is a nuisance; a session with no
-- keyboard is not, and there is no third option available at this point.  The
-- diagnostic names the callback so the nuisance is at least traceable.
guarded :: String -> IO () -> IO ()
guarded what act = act `E.catch` \e -> hPutStrLn stderr
  ("xmonad-river: a prompt's " ++ what ++ " raised: "
     ++ show (e :: E.SomeException))

-- | Read the keymap out of the descriptor the compositor sent.
--
-- __Positioned reads, never sequential ones.__  This is not a style
-- preference; a plain @read@ here breaks every prompt after the first, and did.
--
-- wlroots keeps one read-only descriptor for the keymap and hands that same
-- descriptor to every client -- see @seat_client_send_keymap@, which sends
-- @keyboard->keymap_fd@ itself rather than a fresh open.  Passing a descriptor
-- over a Wayland socket gives the receiver a new number for the /same open file
-- description/, which means the same file offset.  So a client that reads
-- sequentially leaves that shared offset at end-of-file, and the next client to
-- ask for the keymap -- the next prompt of the session -- reads zero bytes, gets
-- an empty keymap, fails to parse it and cannot turn a single keystroke into a
-- keysym.  It never recovers, because nothing ever rewinds the offset.
--
-- Worse than it first looks: the offset belongs to the compositor's one open
-- file description, so it is shared by every client for the whole session.
-- Whichever client drains it first breaks the keymap for all of them, until
-- river builds a new one -- which is why the symptom reads as "the first
-- prompt after a keymap change works and no other one does".
--
-- The protocol's own answer is @mmap@ with @MAP_PRIVATE@, for exactly this
-- reason.  @pread@ is the same guarantee -- an explicit offset, no shared state
-- touched -- without the unmapping bookkeeping, and libc has it.
-- @tests\/river-prompt-spec.hs --keymap-probe-sequential@ demonstrates the old
-- behaviour against a live compositor, if it ever needs demonstrating again.
--
-- Bytes, not text, until the whole thing has arrived: a keymap is UTF-8 and a
-- chunked read can land mid-sequence, so decoding per chunk would corrupt any
-- keysym name outside ASCII.
readFdText :: Fd -> Int -> IO String
readFdText (Fd fd) size
  | size <= 0 = pure ""
  | otherwise = allocaBytes size $ \buf -> do
      got <- go buf 0
      -- The advertised size counts the terminator, and an interior NUL in a
      -- Haskell String is a trap waiting for whoever marshals it back out.
      BC.unpack . BS.takeWhile (/= 0) <$> BS.packCStringLen (buf, got)
  where
    go buf got
      | got >= size = pure got
      | otherwise = do
          n <- c_pread fd (buf `plusPtr` got)
                          (fromIntegral (size - got)) (fromIntegral got)
          -- Short reads are legal and a zero is end-of-file; a negative is an
          -- error.  All three end the loop with what has arrived, because a
          -- truncated keymap is still worth trying to parse and throwing here
          -- would take the prompt down with it.
          if n <= 0 then pure got else go buf (got + fromIntegral n)

-- | @pread(2)@: read at an explicit offset, leaving the descriptor's own
-- offset alone.  The @unix@ package does not wrap it.
foreign import ccall unsafe "pread"
  c_pread :: CInt -> Ptr CChar -> CSize -> COff -> IO CSsize

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
    guarded "draw" (csDraw spec buf)
    wlSurfaceAttach (clConn cl) (clSurface cl) (bufObject buf) 0 0
    wlSurfaceDamageBuffer (clConn cl) (clSurface cl) 0 0
      (fromIntegral w) (fromIntegral h)
    wlSurfaceCommit (clConn cl) (clSurface cl)
    flush (clConn cl)

-- | Take the surface down and drop the connection.
--
-- Idempotent, because it is reached both from the loop -- a @Close@ request or
-- the compositor closing the surface -- and from the handler that runs when
-- the thread ends for any other reason.
--
-- Every step is individually guarded.  The steps are ordered cheapest-to-lose
-- first and 'disconnect' last, because 'disconnect' is the one that cannot
-- fail to work: whatever this process neglects to destroy, the compositor
-- destroys when the connection goes -- including the keyboard grab, which is
-- the thing that must not survive.  Letting an earlier step's exception skip
-- it would defeat the entire point of calling this from a handler.
shutdown :: ClientSpec -> Client -> IO ()
shutdown spec cl = do
  alive <- readIORef (clRunning cl)
  when alive $ do
    writeIORef (clRunning cl) False
    let step what act = act `E.catch` \e -> hPutStrLn stderr
          ("xmonad-river: while closing a prompt (" ++ what ++ "): "
             ++ show (e :: E.SomeException))
    step "buffer"  (readIORef (clBuffer cl) >>= mapM_ (destroyBuffer (clConn cl)))
    step "keymap"  (readIORef (clXkb cl) >>= mapM_ freeXkbState)
    step "surface" $ do
      zwlrLayerSurfaceV1Destroy (clConn cl) (clLayer cl)
      wlSurfaceDestroy (clConn cl) (clSurface cl)
      flush (clConn cl)
    step "disconnect" (disconnect (clConn cl))
    -- Last, and outside the connection teardown: this is the caller's code,
    -- and the keyboard is already released by the time it runs.
    step "onClose" (csOnClose spec)

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

