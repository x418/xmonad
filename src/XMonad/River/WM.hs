{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The river window management state machine, driving an xmonad 'WindowSet'.
--
-- river splits state in two.  /Window management state/ -- dimensions,
-- keyboard focus, bindings -- may only change during a manage sequence,
-- between @manage_start@ and @manage_finish@.  /Rendering state/ -- position,
-- stacking, borders, hide\/show -- may change during either sequence and is
-- applied at @render_finish@.  So the layout runs in the manage sequence and
-- its result, a 'Plan', is transmitted from there and again from the render
-- sequence.
--
-- Two threads (see @DESIGN.md@):
--
-- * the __loop__ (the main thread) owns the 'Connection' and runs no user code;
-- * the __worker__ owns 'XState' and runs every 'X' action -- bindings, hooks,
--   layouts -- serialised, in the order they were asked for.
--
-- The loop hands the worker a manage sequence and waits a bounded time for the
-- worker to publish a 'Plan'; if the worker is late the loop answers river with
-- the plan it already has, and transmits the late one as soon as it lands.
-- Bindings fire outside any sequence; their actions are queued and run at the
-- start of the next one, which river is asked for with @manage_dirty@.
module XMonad.River.WM (riverMain) where

import Control.Concurrent (forkIO, killThread, newChan, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay, writeChan)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, fromException, handle, throwIO)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.Int (Int32)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (All(..), appEndo)
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import System.Timeout (timeout)

import XMonad.Core
import XMonad.Operations (StateFile (..), broadcastMessage, floatLocation, focus, isFixedSizeOrTransient, readStateFile, scaleRationalRect, writeStateToFile)
import XMonad.River.Client (closeAllClients)
import XMonad.River.Connection
import qualified XMonad.River.Control as Ctl
import XMonad.River.Keyboard (riverModifiers)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Plan
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.LayerShell
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Runtime (RestartRequested(..), emitOp, exitLoopWith, forgetBorderOverride, lookupBorderOverride, nowOpsPending, publishGeometry, publishSizeHints, sendRestart, setMainThread, setModifierWatcher, takeModifierWatcher, takeNowOps, takeOps)
import XMonad.River.State (InputCapture(..), RiverState(..))
import XMonad.River.Types
import XMonad.River.Wire (ObjectId, isNullObject)
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Runtime

-- | What the worker publishes and the loop waits on.  Worker-written only.
data Shared = Shared
  { shPlan    :: !(TVar Plan)
    -- ^ The last plan the layout produced.  'planSerial' is monotonic.
  , shSeqDone :: !(TVar Int)
    -- ^ The highest manage-sequence number the worker has finished.
  }

-- | The loop's state.  Each field has one writing thread; the comment says
-- which when it is not the loop.
data Runtime = Runtime
  { -- constants
    rtConn           :: !Connection
  , rtManager        :: !ObjectId
  , rtBindingsGlobal :: !ObjectId
  , rtXkbVersion     :: !Word32
    -- ^ Negotiated @river_xkb_bindings_v1@ version; @get_seat@ and
    -- @ensure_next_key_eaten@ arrived in 2.
  , rtLayerShell     :: !(Maybe ObjectId)
  , rtFollowsMouse   :: !Bool
  , rtKeyActions     :: !(M.Map (KeyMask, KeySym) (X ()))
  , rtButtonActions  :: !(M.Map (KeyMask, Button) (Window -> X ()))
  , rtSubmit         :: !(X () -> IO ())
    -- ^ Hand an action to the worker.
    -- shared with the X monad
  , rtState          :: !(RiverState X)
  , rtShared         :: !Shared
    -- loop only
  , rtPending        :: !(IORef [X ()])
    -- ^ Binding actions awaiting the next manage sequence, newest first.
  , rtJobs           :: !(MB.Mailbox (IO ()))
    -- ^ Work other threads want done on the loop.
  , rtSeqNo          :: !(IORef Int)
  , rtSent           :: !(IORef Int)
    -- ^ 'planSerial' of the last plan transmitted in a manage sequence.
  , rtAsked          :: !(IORef Int)
    -- ^ 'planSerial' a @manage_dirty@ has already been sent for.
  , rtBindings       :: !(IORef (M.Map ObjectId (X ())))
    -- ^ The config's key bindings, by binding object.
  , rtPointerBind    :: !(IORef (M.Map ObjectId (Window -> X ())))
  , rtBoundSeats     :: !(IORef (S.Set ObjectId))
  , rtGrabbed        :: !(IORef [ObjectId])
    -- ^ Bindings 'XMonad.River.grabKeys' asked for.
  , rtArmed          :: !(IORef [ObjectId])
    -- ^ Bindings an open capture installed.
  , rtDisarm         :: !(IORef Bool)
    -- ^ A capture ended and its bindings are still installed; torn down in
    -- the next manage sequence, the only place @enable@ is legal.
  , rtHovered        :: !(IORef (Maybe Window))
  , rtLayerDefault   :: !(IORef (Maybe ObjectId))
    -- ^ The output last nominated for layer surfaces that name none.
  , rtStartupSent    :: !(IORef Bool)
    -- worker only
  , rtAdopted        :: !(IORef (S.Set Window))
    -- ^ Windows the manage hook has run for.
  , rtRestored       :: !(IORef Bool)
  , rtLayoutMoved    :: !(IORef Bool)
    -- ^ The last layout pass moved a window.  Worker writes, loop takes; the
    -- next @pointer_enter@ is then the layout's doing, not the pointer's.
  }

--------------------------------------------------------------------------------
-- Entry point

-- | Connect to river and run the window manager.  Does not return.
riverMain :: XConfig Layout -> Directories -> IO ()
riverMain userConfig dirs = do
  conn <- connect
  (registry, globals) <- getRegistry conn
  mManager <- bindGlobal conn registry globals
                riverWindowManagerV1Interface 4 riverWindowManagerV1Version
  mBindings <- bindGlobal conn registry globals
                 riverXkbBindingsV1Interface 1 riverXkbBindingsV1Version
  -- Binding layer shell is what tells river this window manager supports it;
  -- without it river closes every layer surface (bars, launchers, lockers).
  mLayerShell <- bindGlobal conn registry globals
                   riverLayerShellV1Interface 1 riverLayerShellV1Version
  mCompositor <- bindGlobal conn registry globals
                   wlCompositorInterface 4 wlCompositorVersion
  mShm <- bindGlobal conn registry globals wlShmInterface 1 wlShmVersion

  case (mManager, mBindings) of
    (Just (manager, _), Just (bindings, bindingsVer)) -> do
      when (isNothing mLayerShell) $ hPutStrLn stderr
        "xmonad-river: river_layer_shell_v1 is unavailable; layer surfaces \
        \(launchers, notifications, wallpaper, bars) will not be shown"
      when (isNothing mCompositor || isNothing mShm) $ hPutStrLn stderr
        "xmonad-river: wl_compositor or wl_shm is unavailable; anything the \
        \window manager draws itself (prompts, decorations) will not appear"
      when (bindingsVer < 2) $ hPutStrLn stderr
        "xmonad-river: river_xkb_bindings_v1 is version 1; submaps cannot \
        \detect an unbound key and will wait for one of their own"
      run conn manager bindings bindingsVer (fmap fst mLayerShell)
          (fmap fst mCompositor) (fmap fst mShm) userConfig dirs
    _ -> do
      hPutStrLn stderr
        "xmonad-river: river_window_manager_v1 (>= 4) or \
        \river_xkb_bindings_v1 not supported by the compositor"
      exitFailure

run :: Connection -> ObjectId -> ObjectId -> Word32 -> Maybe ObjectId
    -> Maybe ObjectId -> Maybe ObjectId -> XConfig Layout
    -> Directories -> IO ()
run conn manager bindings bindingsVer layerShell compositor shm userConfig dirs = do
  rs <- do
    windowsRef  <- newIORef M.empty
    outputsRef  <- newIORef M.empty
    seatsRef    <- newIORef M.empty
    dirtyVar    <- newTVarIO False
    manageRef   <- newIORef False
    restartRef  <- newIORef Nothing
    mailbox     <- MB.newMailbox
    placeRef    <- newIORef []
    extraKeys   <- newIORef []
    restackRef  <- newIORef []
    overlayRef  <- newIORef []
    overlayPos  <- newIORef M.empty
    captureRef  <- newIORef Nothing
    dragOrigin  <- newIORef (0, 0)
    afterLayout <- newIORef []
    pure RiverState
      { riverManager     = manager
      , riverBindings    = bindings
      , riverCompositor  = compositor
      , riverShm         = shm
      , riverWindows     = windowsRef
      , riverOutputs     = outputsRef
      , riverSeats       = seatsRef
      , riverDirty       = dirtyVar
      , inManageSeq      = manageRef
      , riverRestart     = restartRef
      , riverMailbox     = mailbox
      , riverPlacements  = placeRef
      , riverExtraKeys   = extraKeys
      , riverRestack     = restackRef
      , riverOverlays    = overlayRef
      , riverOverlayPos  = overlayPos
      , riverCapture     = captureRef
      , riverDragOrigin  = dragOrigin
      , riverAfterLayout = afterLayout
      }

  when (null (workspaces userConfig)) $ hPutStrLn stderr
    "xmonad-river: the config has no workspaces; using a single one named \"1\""

  -- 'XMonad.xmonad' has already wrapped the layout in the existential, so
  -- every workspace can hold a different one.  One placeholder screen: the
  -- real ones are reconciled against river's outputs in the first manage
  -- sequence, before anything is laid out.
  let layout = layoutHook userConfig
      initialWorkspaces = case workspaces userConfig of
        []    -> [ W.Workspace "1" layout Nothing ]
        names -> [ W.Workspace i layout Nothing | i <- names ]
      (firstWorkspace, otherWorkspaces) = case initialWorkspaces of
        (w:ws) -> (w, ws)
        []     -> error "unreachable: initialWorkspaces is never empty"
      placeholder = W.Screen firstWorkspace 0 (SD (Rectangle 0 0 0 0))
      initialSet = W.StackSet placeholder [] otherWorkspaces M.empty

      xconf = XConf
        { config = userConfig
        , display = conn
        , riverState = rs
        , normalBorder = parseColor (normalBorderColor userConfig)
        , focusedBorder = parseColor (focusedBorderColor userConfig)
        , keyActions = keys userConfig userConfig
        , buttonActions = mouseBindings userConfig userConfig
        , mouseFocused = False
        , mousePosition = Nothing
        , currentEvent = Nothing
        , directories = dirs
        }
      xstate = XState
        { windowset = initialSet
        , mapped = S.empty
        , waitingUnmap = M.empty
        , dragging = Nothing
        , extensibleState = M.empty
        , numberlockMask = 0
        }

  stateRef <- newIORef xstate
  let runX' :: X a -> IO a
      runX' act = do
        st <- readIORef stateRef
        (a, st') <- runX xconf st act
        writeIORef stateRef st'
        pure a

  -- The worker.  One handler per action, so that an action which throws costs
  -- its own effect and not the window manager.  A config ending the session
  -- (@io exitSuccess@ from an exit prompt) throws 'ExitCode', which only the
  -- loop can act on: it owns the connection and tells river before going.
  work <- newChan
  let submit :: X () -> IO ()
      submit = writeChan work
  workerTid <- forkIO $ forever $ do
    act <- readChan work
    runX' act `catch` \e -> case fromException e of
      Just code -> exitLoopWith code
      Nothing -> hPutStrLn stderr
        ("xmonad-river: worker: " ++ show (e :: SomeException))

  sh <- Shared <$> newTVarIO emptyPlan <*> newTVarIO 0
  rt <- do
    pending    <- newIORef []
    jobs       <- MB.newMailbox
    seqNo      <- newIORef 0
    sent       <- newIORef 0
    asked      <- newIORef 0
    bindRef    <- newIORef M.empty
    pointerRef <- newIORef M.empty
    bound      <- newIORef S.empty
    grabbed    <- newIORef []
    armed      <- newIORef []
    disarm     <- newIORef False
    hovered    <- newIORef Nothing
    layerDef   <- newIORef Nothing
    startup    <- newIORef False
    adopted    <- newIORef S.empty
    restored   <- newIORef False
    moved      <- newIORef False
    pure Runtime
      { rtConn = conn
      , rtManager = manager
      , rtBindingsGlobal = bindings
      , rtXkbVersion = bindingsVer
      , rtLayerShell = layerShell
      , rtFollowsMouse = focusFollowsMouse userConfig
      , rtKeyActions = keyActions xconf
      , rtButtonActions = buttonActions xconf
      , rtSubmit = submit
      , rtState = rs
      , rtShared = sh
      , rtPending = pending
      , rtJobs = jobs
      , rtSeqNo = seqNo
      , rtSent = sent
      , rtAsked = asked
      , rtBindings = bindRef
      , rtPointerBind = pointerRef
      , rtBoundSeats = bound
      , rtGrabbed = grabbed
      , rtArmed = armed
      , rtDisarm = disarm
      , rtHovered = hovered
      , rtLayerDefault = layerDef
      , rtStartupSent = startup
      , rtAdopted = adopted
      , rtRestored = restored
      , rtLayoutMoved = moved
      }

  riverWindowManagerV1Listen conn manager (onManagerEvent rt)
  riverWindowManagerV1ManageDirty conn manager

  setMainThread
  -- @xmonad --restart@ arrives over a unix socket, on a thread of its own so
  -- that it can still interrupt a wedged loop.  See "XMonad.River.Control".
  ctlPath <- Ctl.controlSocketPath
  case ctlPath of
    Left err -> hPutStrLn stderr
      ("xmonad-river: no control socket (" ++ err ++ "); \
       \`xmonad --restart` will not work")
    Right path -> do
      served <- Ctl.serveControl path $ \case
        Ctl.Restart -> Ctl.awaitRestart sendRestart
      case served of
        Left err -> hPutStrLn stderr
          ("xmonad-river: could not listen on " ++ path ++ ": " ++ err)
        Right _  -> pure ()

  let mailbox = riverMailbox rs
      -- What can wake the loop besides the compositor: actions posted from
      -- other threads, loop jobs, a dirty flag or a now-op from the worker, and
      -- a plan that landed after its sequence was answered.
      signals sent asked =
                MB.awaitMail mailbox
        `orElse` MB.awaitMail (rtJobs rt)
        `orElse` (readTVar (riverDirty rs) >>= check)
        `orElse` nowOpsPending
        `orElse` (readTVar (shPlan sh) >>= \p -> check (planSerial p > max sent asked))
      loop = do
        MB.drain mailbox >>= queueActions rt
        MB.drain (rtJobs rt) >>= sequence_
        dirty <- atomically (swapTVar (riverDirty rs) False)
        sent <- readIORef (rtSent rt)
        asked <- readIORef (rtAsked rt)
        serial <- planSerial <$> readTVarIO (shPlan sh)
        -- A plan published after its sequence was answered is transmitted in
        -- a sequence of its own; one request per serial.
        let stranded = serial > max sent asked
        when (dirty || stranded) $ riverWindowManagerV1ManageDirty conn manager
        when stranded $ writeIORef (rtAsked rt) serial
        takeNowOps >>= mapM_ (sendNow rt)
        -- Flush before waiting, or a request queued on this pass never
        -- reaches the compositor and nothing comes back to wake us.
        flush conn
        sockFd <- connectionFd conn
        ready <- MB.waitSocketOr sockFd (signals sent asked)
        case ready of
          Left ()  -> dispatch conn
          Right () -> pure ()
        loop
  -- A restart request is an async exception into this thread.  Ask river to
  -- release us and keep dispatching: the @finished@ event does the exec.
  let restartGraceMicros = 2 * 1000 * 1000
  -- Tell river before exiting: @stop@ is what releases this window manager,
  -- and river exits with it.
  let onExit :: ExitCode -> IO ()
      onExit code = do
        riverWindowManagerV1Stop conn manager
        flush conn
        throwIO code
  handle onExit $ loop `catch` \RestartRequested -> do
    mExe <- restartTarget
    case mExe of
      -- Once river has released this window manager there is no way back, so
      -- a session with an out-of-date window manager beats one with none.
      Nothing -> do
        let msg = "the executable is gone; still running the old one"
        Ctl.answerRestart (Ctl.Refused msg)
        hPutStrLn stderr ("xmonad-river: refusing to restart, " ++ msg)
        loop
      Just exe -> do
        args <- getArgs
        -- What 'XMonad.Operations.restart' does before asking river to stop,
        -- on the worker because that owns 'XState'.  Bounded: if the stuck
        -- action is the one being escaped, waiting for it would make the
        -- escape hatch as stuck as the thing it is escaping.
        done <- newEmptyMVar
        submit (broadcastMessage ReleaseResources >> writeStateToFile
                  >> io (putMVar done ()))
        yielded <- timeout restartGraceMicros (takeMVar done)
        when (isNothing yielded) $ do
          hPutStrLn stderr
            "xmonad-river: the worker did not yield; restarting from the last \
            \committed state"
          killThread workerTid
          runX' writeStateToFile
        atomicWriteIORef (riverRestart rs) (Just (exe, args))
        -- Answered before the stop: after it there is an exec and no thread
        -- left to answer with.
        Ctl.answerRestart Ctl.Ok
        riverWindowManagerV1Stop conn manager
        loop

-- | What to exec on restart, or 'Nothing' if there is nothing to exec.
--
-- Linux appends @\" (deleted)\"@ to @\/proc\/self\/exe@ once the binary has
-- been replaced, which is exactly what installing a new build does.  Stripped,
-- and checked for existence, because the point is not to tear down a working
-- session for a successor that is not there.
restartTarget :: IO (Maybe FilePath)
restartTarget = do
  raw <- getExecutablePath
  let suffix = " (deleted)"
      path | suffix `isSuffixOf` raw = take (length raw - length suffix) raw
           | otherwise = raw
  ok <- doesFileExist path
  pure (if ok then Just path else Nothing)

-- | Requests that need no sequence and must not wait for one.
sendNow :: Runtime -> Op -> IO ()
sendNow rt = \case
  OpExitSession -> riverWindowManagerV1ExitSession conn (rtManager rt)
  OpStop -> riverWindowManagerV1Stop conn (rtManager rt)
  OpSetXcursorTheme s name size -> do
    seats <- readIORef (riverSeats (rtState rt))
    when (M.member s seats) $ riverSeatV1SetXcursorTheme conn s name size
  _ -> pure ()
  where conn = rtConn rt

--------------------------------------------------------------------------------
-- Manager events

onManagerEvent :: Runtime -> RiverWindowManagerV1Event -> IO ()
onManagerEvent rt = \case
  RiverWindowManagerV1Unavailable -> do
    hPutStrLn stderr "xmonad-river: another window manager is already running"
    exitFailure
  -- river has released this window manager; a successor may connect.  Exec'd
  -- in place, with no shell in between, which is what makes M-q a restart.
  RiverWindowManagerV1Finished -> readIORef (riverRestart rs) >>= \case
    Nothing  -> exitSuccess
    Just (prog, as) -> executeFile prog True as Nothing
  RiverWindowManagerV1ManageStart -> do
    n <- atomicModifyIORef' (rtSeqNo rt) (\k -> (k + 1, k + 1))
    acts <- atomicModifyIORef' (rtPending rt) (\as -> ([], reverse as))
    rtSubmit rt (manageSequence rt n acts)
    -- Bounded: an action that finishes in time has its result in this
    -- sequence; one that overruns leaves the sequence to be answered with the
    -- plan in hand, and its own plan is transmitted when it lands (see the
    -- loop).  What is never allowed is waiting for user code.
    landed <- awaitPlan (rtShared rt) n planGraceMicros
    -- Only once this sequence's 'reapClosed' has seen the closed entries, or a
    -- window would be dropped from the map while the WindowSet still holds it.
    when landed (reapObjects rt)
    plan <- readTVarIO (shPlan (rtShared rt))
    transmitManage rt plan
    writeIORef (rtSent rt) (planSerial plan)
    riverWindowManagerV1ManageFinish conn (rtManager rt)
    -- Delivered before anything slow: the startup hook spawns enough to trip
    -- river's watchdog otherwise.
    flush conn
    first <- atomicModifyIORef' (rtStartupSent rt) (\d -> (True, not d))
    when first $ rtSubmit rt runStartupHook
  RiverWindowManagerV1RenderStart -> do
    transmitRender rt
    riverWindowManagerV1RenderFinish conn (rtManager rt)
  -- Bookkeeping on the loop; only the config's hook goes to the worker.
  RiverWindowManagerV1Window win -> addWindow rt win
  RiverWindowManagerV1Output out -> do
    addOutput rt out
    rtSubmit rt (void (broadcastEvent (OutputAdded out)))
  RiverWindowManagerV1Seat seat  -> do
    addSeat rt seat
    rtSubmit rt (void (broadcastEvent (SeatAdded seat)))
  RiverWindowManagerV1SessionLocked   -> rtSubmit rt (void (broadcastEvent SessionLocked))
  RiverWindowManagerV1SessionUnlocked -> rtSubmit rt (void (broadcastEvent SessionUnlocked))
  _ -> pure ()
  where
    conn = rtConn rt
    rs = rtState rt

-- | Wait, briefly, for the worker to finish the given manage sequence.
--
-- The bound is what makes the thread split safe: river holds every input
-- event for the seat until the sequence is answered.
awaitPlan :: Shared -> Int -> Int -> IO Bool
awaitPlan sh wanted micros = do
  expired <- registerDelay micros
  atomically $
        (True  <$ (readTVar (shSeqDone sh) >>= check . (>= wanted)))
    `orElse` (False <$ (readTVar expired >>= check))

-- | How long the loop waits for a sequence before answering with the plan it
-- has.  Long enough that anything not doing I/O lands in its own sequence;
-- river's own watchdog is far longer.
planGraceMicros :: Int
planGraceMicros = 50 * 1000

broadcastEvent :: Event -> X All
broadcastEvent ev = do
  hook <- asks (handleEventHook . config)
  userCodeDef (All True) (hook ev)

--------------------------------------------------------------------------------
-- Object tracking (loop)

-- | Take note of a window river has just told us about.
--
-- Ignored on purpose: @decoration_hint@ (SSD is requested regardless, and a
-- CSD-only client ignores that), @maximize_requested@, @minimize_requested@,
-- @show_window_menu_requested@, @presentation_hint@ -- a tiling layout has no
-- meaning for any of them.
addWindow :: Runtime -> ObjectId -> IO ()
addWindow rt win = do
  node <- riverWindowV1GetNode conn win
  let ref = riverWindows (rtState rt)
  modifyIORef' ref $ M.insert win RiverWindow
    { rwObject = win, rwNode = node
    , rwAppId = Nothing, rwTitle = Nothing, rwPid = Nothing
    , rwIdentifier = Nothing, rwParent = Nothing
    , rwDimensions = (0, 0)
    , rwSizeHints = noSizeHints
    , rwClosed = False, rwFullscreen = False, rwHidden = False
    }
  riverWindowV1Listen conn win $ \case
    RiverWindowV1Closed        -> adjust ref win $ \w -> w { rwClosed = True }
    RiverWindowV1AppId a       -> adjust ref win $ \w -> w { rwAppId = a }
    RiverWindowV1Title t       -> adjust ref win $ \w -> w { rwTitle = t }
    RiverWindowV1UnreliablePid p -> adjust ref win $ \w -> w { rwPid = Just p }
    RiverWindowV1Identifier i  -> adjust ref win $ \w -> w { rwIdentifier = Just i }
    RiverWindowV1Parent p      -> adjust ref win $ \w ->
      w { rwParent = if isNullObject p then Nothing else Just p }
    RiverWindowV1Dimensions width height ->
      adjust ref win $ \w -> w { rwDimensions = (width, height) }
    -- A bound of zero or less means the window did not state one.
    RiverWindowV1DimensionsHint minW minH maxW maxH ->
      adjust ref win $ \w -> w { rwSizeHints = SizeHints
        { sh_min_size   = sizeBound minW minH
        , sh_max_size   = sizeBound maxW maxH
        , sh_resize_inc = Nothing
        , sh_aspect     = Nothing
        , sh_base_size  = Nothing
        } }
    -- The flag is set before the event is queued, so a hook reading
    -- 'XMonad.Hooks.ManageHelpers.isFullscreen' sees the answer it is told.
    RiverWindowV1FullscreenRequested _ -> do
      adjust ref win $ \w -> w { rwFullscreen = True }
      queueAction rt $ void $ broadcastEvent (WindowFullscreenChanged win True)
    RiverWindowV1ExitFullscreenRequested -> do
      adjust ref win $ \w -> w { rwFullscreen = False }
      queueAction rt $ void $ broadcastEvent (WindowFullscreenChanged win False)
    _ -> pure ()
  where conn = rtConn rt

adjust :: IORef (M.Map ObjectId a) -> ObjectId -> (a -> a) -> IO ()
adjust ref k f = modifyIORef' ref (M.adjust f k)

addOutput :: Runtime -> ObjectId -> IO ()
addOutput rt out = do
  let ref = riverOutputs (rtState rt)
  mLayer <- forM (rtLayerShell rt) $ \shell -> do
    lo <- riverLayerShellV1GetOutput conn shell out
    riverLayerShellOutputV1Listen conn lo $ \case
      RiverLayerShellOutputV1NonExclusiveArea x y width height ->
        adjust ref out $ \o -> o { roLayerArea = Just (Rectangle x y
          (fromIntegral width) (fromIntegral height)) }
      _ -> pure ()
    pure lo
  modifyIORef' ref $ M.insert out RiverOutput
    { roObject = out, roPosition = (0, 0), roSize = (0, 0), roRemoved = False
    , roLayerObject = mLayer, roLayerArea = Nothing }
  riverOutputV1Listen conn out $ \case
    RiverOutputV1Removed -> adjust ref out $ \o -> o { roRemoved = True }
    RiverOutputV1Position x y -> adjust ref out $ \o -> o { roPosition = (x, y) }
    RiverOutputV1Dimensions width height ->
      adjust ref out $ \o -> o { roSize = (width, height) }
    _ -> pure ()
  where conn = rtConn rt

addSeat :: Runtime -> ObjectId -> IO ()
addSeat rt seat = do
  let ref = riverSeats rs
  mLayer <- forM (rtLayerShell rt) $ \shell -> do
    ls <- riverLayerShellV1GetSeat conn shell seat
    riverLayerShellSeatV1Listen conn ls $ \ev -> do
      let set f = adjust ref seat $ \s -> s { rsLayerFocus = f }
      case ev of
        RiverLayerShellSeatV1FocusExclusive    -> set LayerFocusExclusive
        RiverLayerShellSeatV1FocusNonExclusive -> set LayerFocusNonExclusive
        RiverLayerShellSeatV1FocusNone         -> set LayerFocusNone
        _ -> pure ()
    pure ls

  -- The object @ensure_next_key_eaten@ is requested on.  Once per seat (twice
  -- is a protocol error), and only from version 2, where it exists.
  mXkbSeat <- if rtXkbVersion rt < 2 then pure Nothing else do
    xs <- riverXkbBindingsV1GetSeat conn (rtBindingsGlobal rt) seat
    riverXkbBindingsSeatV1Listen conn xs $ \case
      -- A key the open capture did not ask for ends it.  Without this a
      -- submap would stay armed with every binding disabled.
      RiverXkbBindingsSeatV1AteUnboundKey -> do
        taken <- atomicModifyIORef' (riverCapture rs) (\s -> (Nothing, s))
        forM_ taken $ \cap -> do
          writeIORef (rtDisarm rt) True
          queueAction rt (icOnEnd cap)
      -- Only sent for modifiers something asked to watch.
      RiverXkbBindingsSeatV1ModifiersUpdate old new ->
        takeModifierWatcher >>= mapM_ (\f -> f old new)
      _ -> pure ()
    pure (Just xs)

  modifyIORef' ref $ M.insert seat RiverSeat
    { rsObject = seat, rsRemoved = False
    , rsLayerObject = mLayer, rsLayerFocus = LayerFocusNone
    , rsPointer = (0, 0), rsXkbSeat = mXkbSeat }

  riverSeatV1Listen conn seat $ \case
    RiverSeatV1Removed -> adjust ref seat $ \s -> s { rsRemoved = True }
    -- X11's EnterNotify.  X11 marked the crossings a window's own movement
    -- synthesised; river's carries a window and nothing else, so a crossing
    -- that follows a layout pass which moved something is taken as that
    -- layout's doing -- Magnifier enlarging the focused window would
    -- otherwise refocus its displaced neighbour, forever.
    RiverSeatV1PointerEnter win -> do
      writeIORef (rtHovered rt) (Just win)
      byLayout <- atomicModifyIORef' (rtLayoutMoved rt) (\m -> (False, m))
      when (rtFollowsMouse rt && not byLayout) $ queueAction rt $ do
        -- Runs a sequence later: the pointer may have moved on, a drag owns
        -- the focus, and a window on a hidden workspace cannot really be
        -- entered (a restart shows every window until the successor's first
        -- sequence hides them).
        stillThere <- io ((== Just win) <$> readIORef (rtHovered rt))
        drag <- gets dragging
        onScreen <- gets (elem win . concatMap (W.integrate' . W.stack . W.workspace)
                            . screensOf . windowset)
        when (stillThere && isNothing drag && onScreen) (focus win)
    RiverSeatV1PointerLeave -> writeIORef (rtHovered rt) Nothing
    RiverSeatV1PointerPosition x y ->
      adjust ref seat $ \s -> s { rsPointer = (x, y) }
    -- A surface this window manager drew was pressed: X11's ButtonPress on a
    -- decoration.  The position is the one river sent for this sequence.
    RiverSeatV1ShellSurfaceInteraction surf -> do
      seats <- readIORef ref
      let (px, py) = maybe (0, 0) rsPointer (M.lookup seat seats)
      queueAction rt $ void $ broadcastEvent (SurfaceClicked surf px py)
    -- An interactive operation reports the total offset since it began;
    -- 'XMonad.Operations.mouseDrag' promised its caller an absolute position.
    RiverSeatV1OpDelta dx dy -> queueAction rt $ do
      drag <- gets dragging
      whenJust drag $ \(motion, _) -> do
        (ox, oy) <- io . readIORef =<< asks (riverDragOrigin . riverState)
        motion (ox + dx) (oy + dy)
    RiverSeatV1OpRelease -> queueAction rt $ do
      emitOp (OpPointerOpEnd seat)
      drag <- gets dragging
      whenJust drag snd
    _ -> pure ()
  where
    conn = rtConn rt
    rs = rtState rt

-- | Destroy the protocol objects for everything river has closed.
--
-- On the loop, after the worker's 'reapClosed' has seen the same entries and
-- before a plan is transmitted, so that transmitting filters them out.
reapObjects :: Runtime -> IO ()
reapObjects rt = do
  let winRef = riverWindows rs
  ws <- readIORef winRef
  forM_ [ w | w <- M.elems ws, rwClosed w ] $ \w -> do
    riverNodeV1Destroy conn (rwNode w)
    riverWindowV1Destroy conn (rwObject w)
    modifyIORef' winRef (M.delete (rwObject w))
    modifyIORef' (rtHovered rt) (\h -> if h == Just (rwObject w) then Nothing else h)

  let outRef = riverOutputs rs
  outs <- readIORef outRef
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o -> do
    forM_ (roLayerObject o) (riverLayerShellOutputV1Destroy conn)
    riverOutputV1Destroy conn (roObject o)
    modifyIORef' outRef (M.delete (roObject o))

  let seatRef = riverSeats rs
  seats <- readIORef seatRef
  forM_ [ s | s <- M.elems seats, rsRemoved s ] $ \s -> do
    forM_ (rsLayerObject s) (riverLayerShellSeatV1Destroy conn)
    forM_ (rsXkbSeat s) (riverXkbBindingsSeatV1Destroy conn)
    riverSeatV1Destroy conn (rsObject s)
    modifyIORef' seatRef (M.delete (rsObject s))
    modifyIORef' (rtBoundSeats rt) (S.delete (rsObject s))
  where
    conn = rtConn rt
    rs = rtState rt

-- | Queue an action for the next manage sequence, and ask river for one.
-- Loop thread only.
queueAction :: Runtime -> X () -> IO ()
queueAction rt act = queueActions rt [act]

queueActions :: Runtime -> [X ()] -> IO ()
queueActions _ [] = pure ()
queueActions rt acts = do
  modifyIORef' (rtPending rt) (reverse acts ++)
  riverWindowManagerV1ManageDirty (rtConn rt) (rtManager rt)

--------------------------------------------------------------------------------
-- The manage sequence (worker)

manageSequence :: Runtime -> Int -> [X ()] -> X ()
manageSequence rt n acts = do
  let inSeq = inManageSeq (rtState rt)
  io (writeIORef inSeq True)
  body `catchX` pure ()
  io (writeIORef inSeq False)
  io (atomically (modifyTVar' (shSeqDone (rtShared rt)) (max n)))
  where
    body = do
      before <- gets windowset
      restoreState rt
      reapClosed rt
      syncScreens
      nominateLayerOutput rt
      adoptNewWindows rt
      mapM_ userCode acts
      applyLayout rt
      -- 'windows' runs the log hook; a window opening or closing on its own,
      -- a restore and a rescreen do not go through it.
      after <- gets windowset
      unless (sameWindows before after) $
        asks (logHook . config) >>= userCodeDef ()

-- | Whether two 'WindowSet's would produce the same status line.  Not 'Eq':
-- that would compare layouts, which may rewrite their state on every pass.
sameWindows :: WindowSet -> WindowSet -> Bool
sameWindows a b =
     map wsOf (W.workspaces a) == map wsOf (W.workspaces b)
  && W.currentTag a == W.currentTag b
  && map (W.tag . W.workspace) (W.visible a) == map (W.tag . W.workspace) (W.visible b)
  where
    wsOf w = (W.tag w, W.integrate' (W.stack w), fmap W.focus (W.stack w))

-- | Pick up where the previous window manager left off, if it left a state
-- file.  First in the first sequence and never again: river sends every
-- existing window, identifier included, before the first @manage_start@, so
-- that is the earliest the file's identifiers can be resolved.
restoreState :: Runtime -> X ()
restoreState rt = do
  done <- io (readIORef (rtRestored rt))
  unless done $ do
    io (writeIORef (rtRestored rt) True)
    path <- asks (stateFileName . directories)
    exists <- io (doesFileExist path)
    when exists $ do
      xmc <- asks config
      wanted <- io (windowsInStateFile path)
      mst <- readStateFile xmc
      whenJust mst $ \st -> do
        modify $ \s -> s { windowset = windowset st
                         , extensibleState = extensibleState st }
        let restored = W.allWindows (windowset st)
        -- The two numbers differing means the identifiers did not match what
        -- river now reports; the symptom is windows back on the wrong
        -- workspace, otherwise indistinguishable from a missing file.
        io $ hPutStrLn stderr $ "xmonad-river: note: restored "
          <> show (length restored) <> " of " <> show wanted
          <> " windows from " <> path

-- | How many windows the state file claims, from the raw text.
windowsInStateFile :: FilePath -> IO Int
windowsInStateFile path = handle (\(_ :: SomeException) -> pure 0) $ do
  raw <- readFile path
  case reads raw of
    [(sf, _)] -> pure (length (W.allWindows (sfWins sf)))
    _         -> pure 0

-- | Drop what river has closed from the 'WindowSet', and tell the config.
-- The objects are destroyed by 'reapObjects' on the loop, afterwards.
reapClosed :: Runtime -> X ()
reapClosed rt = do
  ws <- io (readIORef (riverWindows rs))
  forM_ [ w | w <- M.elems ws, rwClosed w ] $ \w -> do
    modify $ \st -> st { windowset = W.delete (rwObject w) (windowset st) }
    io $ modifyIORef' (rtAdopted rt) (S.delete (rwObject w))
    -- river recycles ids; an override left behind would land on an
    -- unrelated window.
    io $ forgetBorderOverride (rwObject w)

  outs <- io (readIORef (riverOutputs rs))
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o ->
    void (broadcastEvent (OutputRemoved (roObject o)))

  seats <- io (readIORef (riverSeats rs))
  let gone = [ s | s <- M.elems seats, rsRemoved s ]
  forM_ gone $ \s -> void (broadcastEvent (SeatRemoved (rsObject s)))
  -- A drag's @op_release@ never arrives once its seat is gone; end it here
  -- or every later drag is ignored.
  unless (null gone) $ gets dragging >>= mapM_ snd
  where rs = rtState rt

-- | Nominate the output layer surfaces that name none go to: the one under
-- the current screen, so a launcher opens where the work is.  Matched by
-- position, the only link between a 'W.Screen' and its output.
nominateLayerOutput :: Runtime -> X ()
nominateLayerOutput rt = forM_ (rtLayerShell rt) $ \_ -> do
  outs <- io (readIORef (riverOutputs (rtState rt)))
  ws <- gets windowset
  let SD current = W.screenDetail (W.current ws)
      live = filter (not . roRemoved) (M.elems outs)
      onScreen o = let (x, y) = roPosition o
                   in x == rect_x current && y == rect_y current
      chosen = case filter onScreen live of
        (o:_) -> Just o
        []    -> case sortOn roPosition live of
          (o:_) -> Just o
          []    -> Nothing
  io $ atomically $ modifyTVar' (shPlan (rtShared rt)) $ \p ->
    p { planLayerDefault = (\o -> (roObject o, roLayerObject o)) <$> chosen }

-- | Reconcile the 'WindowSet'\'s screens with river's outputs: xmonad's
-- @rescreen@, driven by the output list.  Outputs are ordered by position so
-- that screen ids are stable across reconnects, which
-- "XMonad.Actions.PhysicalScreens" relies on.
syncScreens :: X ()
syncScreens = do
  outs <- io . readIORef =<< asks (riverOutputs . riverState)
  let rects =
        [ rect
        | o <- sortOn roPosition (filter (not . roRemoved) (M.elems outs))
        , let (x, y) = roPosition o
        , let (width, height) = roSize o
        , width > 0 && height > 0
          -- The area layer shell reports, so a bar's exclusive zone shrinks
          -- the tiling area rather than being tiled over.
        , let rect = case roLayerArea o of
                Just a | rect_width a > 0 && rect_height a > 0 -> a
                _ -> Rectangle x y (fromIntegral width) (fromIntegral height)
        ]
  unless (null rects) $ do
    -- Compared in screen-id order: current-first order changes on every view.
    before <- gets (map (screenRect . W.screenDetail)
                    . sortOn W.screen . screensOf . windowset)
    -- Only when the outputs changed.  'rescreen' re-seats the current
    -- workspace as screen 0; run on every sequence it pinned focus, and
    -- everything that follows W.current, to the leftmost output.
    when (before /= rects) $ do
      modify $ \st -> st { windowset = rescreen rects (windowset st) }
      void (broadcastEvent ScreenLayoutChanged)

-- | The screens a 'WindowSet' currently has, current first.
screensOf :: WindowSet -> [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
screensOf ws = W.current ws : W.visible ws

-- | Lay the given screen rectangles over the current workspaces, keeping
-- each workspace on its screen where possible.  A screen with no workspace of
-- its own takes a distinct one from the hidden pool.
rescreen :: [Rectangle] -> WindowSet -> WindowSet
rescreen rects ws = ws
    { W.current = (W.current ws) { W.screen = 0, W.screenDetail = SD firstRect }
    , W.visible = zipWith reseat [1 ..] restRects
    , W.hidden = newHidden
    }
  where
    (firstRect, restRects) = case rects of
      (r:rs) -> (r, rs)
      []     -> (Rectangle 0 0 0 0, [])
    oldVisible = W.visible ws
    extras = drop (length oldVisible) restRects
    reseat i r = case drop (i - 1) oldVisible of
      (s:_) -> s { W.screen = fromIntegral i, W.screenDetail = SD r }
      [] -> case drop (i - 1 - length oldVisible) pool of
        (h:_) -> W.Screen h (fromIntegral i) (SD r)
        []    -> W.Screen (W.workspace (W.current ws)) (fromIntegral i) (SD r)
    surplus = drop (length restRects) oldVisible
    pool = map W.workspace surplus ++ W.hidden ws
    newHidden = drop (length extras) pool

-- | Run the manage hook for windows river has told us about since the last
-- sequence, and insert them.  Before the window has been rendered, which is
-- the ordering guarantee xmonad's manage hook has always had.
adoptNewWindows :: Runtime -> X ()
adoptNewWindows rt = do
  ws <- io (readIORef (riverWindows (rtState rt)))
  adopted <- io (readIORef (rtAdopted rt))
  let fresh = [ w | w <- M.elems ws, not (rwClosed w)
                  , not (S.member (rwObject w) adopted) ]
  forM_ fresh $ \w -> do
    io $ modifyIORef' (rtAdopted rt) (S.insert (rwObject w))
    -- river's default is CSD.  Asked before the manage hook, so a hook can
    -- override it; a CSD-only client ignores it, which river documents.
    emitOp (OpUseDecorations (rwObject w) True)
    -- A window restored from the state file is already managed -- by the
    -- process that wrote the file -- and gets the setup above only.
    managed <- gets (W.allWindows . windowset)
    unless (rwObject w `elem` managed) $ do
      mh <- asks (manageHook . config)
      g <- userCodeDef mempty (runQuery mh (rwObject w))
      -- As upstream's 'manage': a fixed-size or transient window floats.
      shouldFloat <- withDisplay $ \d -> isFixedSizeOrTransient d (rwObject w)
      rr <- snd <$> floatLocation (rwObject w)
      ws' <- gets windowset
      let clamp (W.RationalRect x y wid h)
            | x + wid > 1 || y + h > 1 || x < 0 || y < 0 =
                W.RationalRect (0.5 - wid / 2) (0.5 - h / 2) wid h
          clamp r = r
          inserted = W.insertUp (rwObject w) ws'
          placed
            | shouldFloat = W.float (rwObject w) (clamp rr) inserted
            | otherwise = inserted
      modify $ \st -> st { windowset = appEndo g placed }
      void (broadcastEvent (WindowAdded (rwObject w)))

-- | The user's startup hook, once, after the first manage sequence has been
-- answered.  @XMONAD_RIVER_NO_STARTUP_HOOK@ skips it, so a real config can run
-- under a test compositor without reaching into the desktop it belongs to.
runStartupHook :: X ()
runStartupHook = do
  skip <- io (lookupEnv "XMONAD_RIVER_NO_STARTUP_HOOK")
  case skip of
    Just _ -> io $ hPutStrLn stderr
      "xmonad-river: note: startup hook skipped \
      \(XMONAD_RIVER_NO_STARTUP_HOOK is set)"
    Nothing -> asks (startupHook . config) >>= void . userCode

--------------------------------------------------------------------------------
-- Bindings (loop)

-- | Create river bindings for the config's keys and buttons on one seat.
-- Inside a manage sequence, where @enable@ is legal.
bindSeat :: Runtime -> ObjectId -> IO ()
bindSeat rt seat = do
  bindPanic rt seat
  forM_ (M.toList (rtKeyActions rt)) $ \((mask, keysym), action) -> do
    b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt) seat keysym
           (riverModifiers mask)
    modifyIORef' (rtBindings rt) (M.insert b action)
    riverXkbBindingV1Listen conn b $ \case
      RiverXkbBindingV1Pressed -> do
        acts <- readIORef (rtBindings rt)
        forM_ (M.lookup b acts) (queueAction rt)
      _ -> pure ()
    riverXkbBindingV1Enable conn b
  forM_ (M.toList (rtButtonActions rt)) $ \((mask, button), action) -> do
    b <- riverSeatV1GetPointerBinding conn seat (linuxButton button)
           (riverModifiers mask)
    modifyIORef' (rtPointerBind rt) (M.insert b action)
    riverPointerBindingV1Listen conn b $ \case
      RiverPointerBindingV1Pressed -> do
        acts <- readIORef (rtPointerBind rt)
        mHover <- readIORef (rtHovered rt)
        forM_ ((,) <$> M.lookup b acts <*> mHover) $ \(a, win) ->
          queueAction rt (a win)
      _ -> pure ()
    riverPointerBindingV1Enable conn b
  where conn = rtConn rt

-- | The chord that always works: @Ctrl-Alt-Shift-Escape@ closes every prompt
-- and re-enables every binding.  Not in 'rtBindings', so no capture can
-- disable it, and not in the config, so nothing can rebind it.  It fires
-- through an exclusive layer-shell grab because river matches xkb bindings
-- before it consults keyboard focus.
bindPanic :: Runtime -> ObjectId -> IO ()
bindPanic rt seat = do
  b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt) seat
         xK_Escape (riverModifiers (controlMask .|. mod1Mask .|. shiftMask))
  riverXkbBindingV1Listen conn b $ \case
    RiverXkbBindingV1Pressed -> do
      n <- closeAllClients
      globals <- readIORef (rtBindings rt)
      forM_ (M.keys globals) (riverXkbBindingV1Enable conn)
      atomicWriteIORef (riverCapture (rtState rt)) Nothing
      writeIORef (rtArmed rt) []
      writeIORef (rtDisarm rt) False
      hPutStrLn stderr $ "xmonad-river: panic: closed " <> show n
        <> " prompt(s) and re-enabled " <> show (M.size globals) <> " binding(s)"
      riverWindowManagerV1ManageDirty conn (rtManager rt)
    _ -> pure ()
  riverXkbBindingV1Enable conn b
  where conn = rtConn rt

-- | X11 button numbers to Linux input event codes.
linuxButton :: Button -> Word32
linuxButton = \case
  1 -> 0x110  -- BTN_LEFT
  2 -> 0x112  -- BTN_MIDDLE
  3 -> 0x111  -- BTN_RIGHT
  4 -> 0x113  -- BTN_SIDE
  5 -> 0x114  -- BTN_EXTRA
  n -> 0x110 + fromIntegral n

--------------------------------------------------------------------------------
-- Layout (worker)

-- | Run the layout for every visible screen and publish the result as a
-- 'Plan'.  Floats are withheld from the layout and placed from the rectangles
-- 'XMonad.Operations.float' recorded, first, which is upstream's order.
applyLayout :: Runtime -> X ()
applyLayout rt = do
  ws <- gets windowset
  perScreen <- forM (screensOf ws) $ \scr -> do
    let wsp = W.workspace scr
        SD rect = W.screenDetail scr
        floats = W.floating ws
        onWs = W.integrate' (W.stack wsp)
        tiled = W.stack wsp >>= W.filter (`M.notMember` floats)
        flt = [ (fw, scaleRationalRect rect rr)
              | fw <- onWs, Just rr <- [M.lookup fw floats] ]
    (rs, mLayout) <- userCodeDef ([], Nothing) (runLayout wsp { W.stack = tiled } rect)
    forM_ mLayout $ \l' -> modify $ \st ->
      st { windowset = updateLayout (W.tag wsp) l' (windowset st) }
    pure (flt ++ rs, map fst flt)

  let placements = concatMap fst perScreen
      floating = S.fromList (concatMap snd perScreen)
      placed = S.fromList (map fst placements)
      mFocus = W.peek ws

  -- Borders are decided here.  The focused window takes the focused colour
  -- whatever an override says: X11 repainted it on every 'windows', and a
  -- colour WindowNavigation had set would otherwise hide the focus forever.
  bw <- asks (borderWidth . config)
  focusedCol <- asks focusedBorder
  normalCol <- asks normalBorder
  borders <- fmap M.fromList $ forM placements $ \(win, _) -> do
    (mWidth, mColor) <- io (lookupBorderOverride win)
    let rgba = case mColor of
          Just c | Just win /= mFocus -> c
          _ -> pixelColor (if Just win == mFocus then focusedCol else normalCol)
    pure (win, (fromMaybe bw mWidth, rgba))

  restackRef <- asks (riverRestack . riverState)
  raised <- io (readIORef restackRef)
  let stillUp = filter (`S.member` placed) raised
  io (atomicWriteIORef restackRef stillUp)

  -- X11 kept 'mapped' from map and unmap events; here it is what this pass
  -- placed, which is what contrib (EasyMotion, for one) asks.
  modify $ \st -> st { mapped = placed }

  placeRef <- asks (riverPlacements . riverState)
  old <- io (readIORef placeRef)
  io (atomicWriteIORef (rtLayoutMoved rt) (old /= placements))
  io (writeIORef placeRef placements)

  io $ atomically $ modifyTVar' (shPlan (rtShared rt)) $ \p -> p
    { planSerial     = planSerial p + 1
    , planPlacements = placements
    , planFloating   = floating
    , planBorders    = borders
    , planVisible    = placed
    , planRaised     = stillUp
    , planFocus      = maybe ClearFocus FocusWindow mFocus
    }

  -- What 'getWindowAttributes' and 'getWMNormalHints' answer with.  Every
  -- known window, as X11 did: one off screen is unmapped at the origin.
  allKnown <- io (readIORef (riverWindows (rtState rt)))
  let placedMap = M.fromList placements
      attrs w rw = case M.lookup w placedMap of
        Just r -> WindowAttributes
          { wa_x = rect_x r, wa_y = rect_y r
          , wa_width = rect_width r, wa_height = rect_height r
          , wa_border_width = bw, wa_map_state = waIsViewable
          , wa_override_redirect = False }
        Nothing -> let (dw, dh) = rwDimensions rw in WindowAttributes
          { wa_x = 0, wa_y = 0
          , wa_width = fromIntegral dw, wa_height = fromIntegral dh
          , wa_border_width = bw, wa_map_state = waIsUnmapped
          , wa_override_redirect = False }
  io $ publishGeometry (M.mapWithKey attrs allKnown)
  io $ publishSizeHints (M.map rwSizeHints allKnown)

  -- Last, so 'XMonad.River.afterLayout' sees everything above.  Drained once:
  -- an action these queue waits for the next layout.
  ref <- asks (riverAfterLayout . riverState)
  queued <- io (atomicModifyIORef' ref (\as -> ([], reverse as)))
  mapM_ userCode queued

updateLayout :: WorkspaceId -> Layout Window -> WindowSet -> WindowSet
updateLayout i l = W.mapWorkspace $ \wsp ->
  if W.tag wsp == i then wsp { W.layout = l } else wsp

--------------------------------------------------------------------------------
-- Transmission (loop)

-- | Send the window management half of a plan.
--
-- Every reference is checked against the windows river currently has: the
-- plan may be older than the maps, and naming a destroyed window is a
-- protocol error that disconnects the window manager.
transmitManage :: Runtime -> Plan -> IO ()
transmitManage rt plan = do
  known <- readIORef (riverWindows rs)
  seats <- readIORef (riverSeats rs)

  -- Seats that appeared since the last sequence get the config's bindings.
  bound <- readIORef (rtBoundSeats rt)
  forM_ [ s | (s, rsx) <- M.toList seats, not (rsRemoved rsx), not (S.member s bound) ] $ \seat -> do
    bindSeat rt seat
    modifyIORef' (rtBoundSeats rt) (S.insert seat)

  -- Restore the config's bindings after a capture ended.  First, so a capture
  -- opened by the action the last one ran arms on a clean set.
  disarm <- readIORef (rtDisarm rt)
  when disarm $ do
    temps <- atomicModifyIORef' (rtArmed rt) (\ts -> ([], ts))
    forM_ temps $ \b -> do
      riverXkbBindingV1Disable conn b
      riverXkbBindingV1Destroy conn b
    globals <- readIORef (rtBindings rt)
    forM_ (M.keys globals) (riverXkbBindingV1Enable conn)
    writeIORef (rtDisarm rt) False

  -- river remembers the layer-surface default; reissued only when it moves.
  forM_ (planLayerDefault plan) $ \(out, mLayerObj) -> do
    prev <- readIORef (rtLayerDefault rt)
    forM_ mLayerObj $ \lo -> unless (prev == Just out) $ do
      riverLayerShellOutputV1SetDefault conn lo
      writeIORef (rtLayerDefault rt) (Just out)

  -- One-shot requests, drained: each is an effect river performs once.
  ops <- takeOps
  forM_ ops $ \case
    OpClose w -> when (M.member w known) $ riverWindowV1Close conn w
    OpWarpPointer s x y -> when (M.member s seats) $ riverSeatV1PointerWarp conn s x y
    OpPointerOpStart s -> when (M.member s seats) $ riverSeatV1OpStartPointer conn s
    OpPointerOpEnd s -> when (M.member s seats) $ riverSeatV1OpEnd conn s
    OpUseDecorations w ssd -> when (M.member w known) $
      (if ssd then riverWindowV1UseSsd else riverWindowV1UseCsd) conn w
    OpSetPosition w x y -> forM_ (M.lookup w known) $ \rw ->
      riverNodeV1SetPosition conn (rwNode rw) x y
    OpProposeDimensions w dw dh -> when (M.member w known) $
      riverWindowV1ProposeDimensions conn w (fromIntegral dw) (fromIntegral dh)
    OpCaptureInput ks mods oneShot gen -> armCapture rt seats ks mods oneShot gen
    OpUngrabKeys -> do
      old <- atomicModifyIORef' (rtGrabbed rt) (\bs -> ([], bs))
      forM_ old $ \b -> do
        riverXkbBindingV1Disable conn b
        riverXkbBindingV1Destroy conn b
    OpGrabKeys ks -> do
      bs <- fmap concat $ forM (M.elems seats) $ \seat ->
        forM (zip [0 :: Int ..] ks) $ \(i, (mask, keysym)) -> do
          b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt)
                 (rsObject seat) keysym (riverModifiers mask)
          -- By index: the actions stay with the config, the loop holds only
          -- the objects.
          let fire pick = do
                acts <- readIORef (riverExtraKeys rs)
                forM_ (take 1 (drop i acts)) (queueAction rt . pick)
          riverXkbBindingV1Listen conn b $ \case
            RiverXkbBindingV1Pressed  -> fire fst
            RiverXkbBindingV1Released -> fire snd
            _ -> pure ()
          riverXkbBindingV1Enable conn b
          pure b
      writeIORef (rtGrabbed rt) bs
    -- Sent by the loop's own pass; see 'sendNow'.
    OpExitSession -> pure ()
    OpStop -> pure ()
    OpSetXcursorTheme{} -> pure ()

  -- Dimensions are window management state.  A window not told it is tiled
  -- draws itself as floating: its own decorations, shadows outside its size.
  forM_ (planPlacements plan) $ \(win, r) -> when (M.member win known) $ do
    riverWindowV1ProposeDimensions conn win
      (fromIntegral (rect_width r)) (fromIntegral (rect_height r))
    riverWindowV1SetTiled conn win
      (if S.member win (planFloating plan) then 0 else allEdges)

  -- Keyboard focus.  A seat whose keyboard has gone to a layer surface is left
  -- alone: river discards the request under an exclusive grab and, under a
  -- non-exclusive one, would silently steal the keyboard back.
  forM_ (M.elems seats) $ \s ->
    unless (layerHasFocus (rsLayerFocus s)) $
      case planFocus plan of
        FocusWindow win | M.member win known ->
          riverSeatV1FocusWindow conn (rsObject s) win
        _ -> riverSeatV1ClearFocus conn (rsObject s)
  where
    conn = rtConn rt
    rs = rtState rt

-- | Take the keyboard for a submap or a hold-to-cycle, disabling the config's
-- own bindings meanwhile (river leaves it to policy which of two bindings for
-- one key fires).  Inside the sequence that carries the key press which asked
-- for it, so arming is atomic with the press.
armCapture
  :: Runtime -> M.Map ObjectId RiverSeat
  -> [(KeyMask, KeySym)] -> KeyMask -> Bool -> Int -> IO ()
armCapture rt seats ks mods oneShot gen = do
  globals <- readIORef (rtBindings rt)
  forM_ (M.keys globals) (riverXkbBindingV1Disable conn)

  -- Whoever takes the slot owns the teardown: a key, an unbound key, a
  -- modifier release or the deadline, exactly one of them.
  let claim = atomicModifyIORef' (riverCapture rs) $ \case
        Just cap | icGeneration cap == gen -> (Nothing, Just cap)
        other -> (other, Nothing)

  temps <- fmap concat $ forM (M.elems seats) $ \seat ->
    forM (zip [0 :: Int ..] ks) $ \(i, (mask, keysym)) -> do
      b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt)
             (rsObject seat) keysym (riverModifiers mask)
      riverXkbBindingV1Listen conn b $ \case
        RiverXkbBindingV1Pressed
          | oneShot -> claim >>= \taken -> forM_ taken $ \cap -> do
              writeIORef (rtDisarm rt) True
              queueAction rt (icOnKey cap True i)
          | otherwise -> do
              held <- readIORef (riverCapture rs)
              forM_ held $ \cap -> queueAction rt (icOnKey cap True i)
        RiverXkbBindingV1Released | not oneShot -> do
          held <- readIORef (riverCapture rs)
          forM_ held $ \cap -> queueAction rt (icOnKey cap False i)
        _ -> pure ()
      riverXkbBindingV1Enable conn b
      pure b
  writeIORef (rtArmed rt) temps

  -- A hold-to-cycle ends when the modifier goes up.
  when (mods /= 0) $ do
    setModifierWatcher $ Just $ \old new ->
      when (old .&. mods /= 0 && new .&. mods /= old .&. mods) $ do
        taken <- claim
        forM_ taken $ \cap -> do
          writeIORef (rtDisarm rt) True
          queueAction rt (icOnEnd cap)
    forM_ (M.elems seats) $ \s ->
      forM_ (rsXkbSeat s) $ \x -> riverXkbBindingsSeatV1ModifiersWatch conn x mods

  -- Be told about a key this did not want, so the capture can be abandoned.
  when oneShot $ forM_ (M.elems seats) $ \s ->
    forM_ (rsXkbSeat s) (riverXkbBindingsSeatV1EnsureNextKeyEaten conn)

  -- A deadline, or an abandoned capture is a session with no bindings.  The
  -- work is posted to the loop; this thread touches no connection.
  void $ forkIO $ do
    threadDelay captureDeadlineMicros
    MB.post (rtJobs rt) $ do
      taken <- claim
      forM_ taken $ \cap -> do
        hPutStrLn stderr
          "xmonad-river: keyboard capture abandoned after 60s; restoring bindings"
        writeIORef (rtDisarm rt) True
        queueAction rt (icOnEnd cap)
  where
    conn = rtConn rt
    rs = rtState rt

captureDeadlineMicros :: Int
captureDeadlineMicros = 60 * 1000 * 1000

-- | Send the rendering half of a plan: positions, borders, visibility and
-- stacking, then the window manager's own surfaces above the windows.
transmitRender :: Runtime -> IO ()
transmitRender rt = do
  plan <- readTVarIO (shPlan (rtShared rt))
  let winRef = riverWindows rs
  known <- readIORef winRef

  forM_ (planPlacements plan) $ \(win, r) -> forM_ (M.lookup win known) $ \w -> do
    riverNodeV1SetPosition conn (rwNode w) (rect_x r) (rect_y r)
    -- Width 0 is how NoBorders removes a border; river reads it as none.
    let (width, (red, green, blue, alpha)) =
          M.findWithDefault (0, (0, 0, 0, 0)) win (planBorders plan)
    riverWindowV1SetBorders conn win allEdges (fromIntegral width)
      red green blue alpha
    when (rwHidden w) $ do
      riverWindowV1Show conn win
      adjust winRef win $ \x -> x { rwHidden = False }

  -- What the layout did not place is on a workspace that is off screen.
  -- river has no workspaces; this is what implements them.
  forM_ (M.elems known) $ \w ->
    unless (S.member (rwObject w) (planVisible plan) || rwHidden w) $ do
      riverWindowV1Hide conn (rwObject w)
      adjust winRef (rwObject w) $ \x -> x { rwHidden = True }

  -- Stacking, bottom to top.  The placement list is topmost-first (upstream's
  -- convention, and what 'windowUnderPointer' relies on), hence the reverse;
  -- Magnifier is the layout that can tell.
  forM_ (reverse (planPlacements plan)) $ \(win, _) ->
    forM_ (M.lookup win known) $ \w -> riverNodeV1PlaceTop conn (rwNode w)
  forM_ (planRaised plan) $ \win ->
    forM_ (M.lookup win known) $ \w -> riverNodeV1PlaceTop conn (rwNode w)

  -- Surfaces this window manager draws (decorations, overlays) are not
  -- windows and not in the plan; contrib records them and their positions.
  overlays <- readIORef (riverOverlays rs)
  positions <- readIORef (riverOverlayPos rs)
  forM_ overlays $ \n -> do
    forM_ (M.lookup n positions) $ \(x, y) -> riverNodeV1SetPosition conn n x y
    riverNodeV1PlaceTop conn n
  where
    conn = rtConn rt
    rs = rtState rt

-- | A dimension bound river reports as zero or less was not stated.
sizeBound :: Int32 -> Int32 -> Maybe (Dimension, Dimension)
sizeBound w h
  | w > 0 && h > 0 = Just (fromIntegral w, fromIntegral h)
  | otherwise      = Nothing

allEdges :: Word32
allEdges = riverWindowV1EdgesTop + riverWindowV1EdgesBottom
         + riverWindowV1EdgesLeft + riverWindowV1EdgesRight
