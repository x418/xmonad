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
module XMonad.River.WM
  ( riverMain
  ) where

import Control.Concurrent (forkIO, killThread, newChan, newEmptyMVar, putMVar, readChan, takeMVar, writeChan)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, fromException, handle, throwIO)
import Control.Monad (forever, unless, void, when)
import Data.IORef
import Data.List (isSuffixOf)
import Data.Maybe (isNothing)
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import System.Timeout (timeout)

import XMonad.Core
import XMonad.Operations (broadcastMessage, writeStateToFile)
import XMonad.River.Connection
import qualified XMonad.River.Control as Ctl
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Plan
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.LayerShell
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Runtime (RestartRequested(..), exitLoopWith, sendRestart, setMainThread)
import XMonad.River.State (Display'(..), RiverState(..), nowOpsPending, takeNowOps)
import XMonad.River.Types
import XMonad.River.WM.Events
import XMonad.River.WM.Runtime
import XMonad.River.WM.Sequence (manageSequence, runStartupHook)
import XMonad.River.WM.Transmit (transmitManage, transmitRender)
import XMonad.River.Wire (ObjectId)
import qualified XMonad.StackSet as W

-- | Connect to river and run the window manager.  Does not return.
riverMain :: XConfig Layout -> Directories -> IO ()
riverMain userConfig dirs = do
  conn <- connect
  (registry, globals) <- getRegistry conn
  let named = M.fromList [ (globalName g, g) | g <- globals ]
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
      run conn registry named manager bindings bindingsVer (fmap fst mLayerShell)
          (fmap fst mCompositor) (fmap fst mShm) userConfig dirs
    _ -> do
      hPutStrLn stderr
        "xmonad-river: river_window_manager_v1 (>= 4) or \
        \river_xkb_bindings_v1 not supported by the compositor"
      exitFailure

run :: Connection -> ObjectId -> M.Map Word32 Global -> ObjectId -> ObjectId -> Word32
    -> Maybe ObjectId -> Maybe ObjectId -> Maybe ObjectId -> XConfig Layout
    -> Directories -> IO ()
run conn registry named manager bindings bindingsVer layerShell compositor shm userConfig dirs = do
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
    geometry    <- newIORef M.empty
    sizeHints   <- newIORef M.empty
    borders     <- newIORef M.empty
    submapGen   <- newIORef 0
    ops         <- newIORef []
    nowOps      <- newTVarIO []
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
      , riverGeometry    = geometry
      , riverSizeHints   = sizeHints
      , riverBorders     = borders
      , riverSubmapGen   = submapGen
      , riverOps         = ops
      , riverNowOps      = nowOps
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
        , display = Display' conn rs
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
    modWatcher <- newIORef Nothing
    globalsRef <- newIORef named
    lastManage <- newIORef M.empty
    lastRender <- newIORef M.empty
    lastStack  <- newIORef []
    lastOvPos  <- newIORef M.empty
    adopted    <- newIORef S.empty
    restored   <- newIORef False
    moved      <- newIORef False
    pure Runtime
      { rtConn = conn
      , rtRegistry = registry
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
      , rtModWatcher = modWatcher
      , rtGlobals = globalsRef
      , rtLastManage = lastManage
      , rtLastRender = lastRender
      , rtLastStack = lastStack
      , rtLastOverlayPos = lastOvPos
      , rtAdopted = adopted
      , rtRestored = restored
      , rtLayoutMoved = moved
      }

  riverWindowManagerV1Listen conn manager (onManagerEvent rt)
  listenRegistry conn registry
    (\g -> modifyIORef' (rtGlobals rt) (M.insert (globalName g) g))
    (\n -> modifyIORef' (rtGlobals rt) (M.delete n))
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
        `orElse` nowOpsPending rs
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
        takeNowOps rs >>= mapM_ (sendNow rt)
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
