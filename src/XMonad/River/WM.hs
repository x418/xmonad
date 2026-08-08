{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | The river window management state machine, driving an xmonad 'WindowSet'.
--
-- river splits state into two disjoint categories, and the split shapes this
-- module:
--
-- * __Window management state__ — dimensions, keyboard focus, keyboard
--   bindings — may only be modified during a /manage sequence/, between the
--   @manage_start@ event and the @manage_finish@ request.
-- * __Rendering state__ — position, stacking order, borders, hide\/show — may
--   be modified during either sequence, but is only applied at
--   @render_finish@.
--
-- So layout runs during the manage sequence (it must, to propose dimensions),
-- its results are stashed, and positions are applied during the following
-- render sequence.
--
-- Bindings fire outside any sequence. Their actions are queued and run at the
-- start of the next manage sequence, which river is asked to schedule with
-- @manage_dirty@. This is the same deferral river's own reference window
-- manager uses.
module XMonad.River.WM
  ( riverMain
  , applyLayout
  , queueAction
  ) where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (All(..), appEndo)
import Data.Int (Int32)
import Data.Word (Word32)
import Control.Exception (SomeException, catch, handle)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.Posix.Process (executeFile, getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR1)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.Core
import XMonad.Operations (StateFile (..), broadcastMessage, focus, readStateFile, scaleRationalRect, writeStateToFile)
import XMonad.River.Runtime (RestartRequested(..), forgetBorderOverride, takeModifierWatcher, lookupBorderOverride, pidFilePath, publishGeometry, publishSizeHints, sendRestart, setMainThread, warnUnimplemented)
import XMonad.River.Client (closeAllClients)
import XMonad.River.Connection
import XMonad.River.Keyboard (riverModifiers)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.LayerShell
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Wire (ObjectId, isNullObject)
import XMonad.River.Types
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Mutable plumbing shared with the event callbacks

-- | State that the Wayland event callbacks write and the manage sequence
-- reads. Kept outside 'XState' because callbacks run in 'IO', below the 'X'
-- monad.
data Runtime = Runtime
  { rtPending     :: !(IORef [X ()])
    -- ^ Binding actions awaiting the next manage sequence, newest first.
  , rtBindings    :: !(IORef (M.Map ObjectId (X ())))
    -- ^ The same 'IORef' as 'riverKeyBindings' in the 'XConf'.  A submap
    -- disables every one of these while it is open, so it needs to reach them
    -- from the 'X' monad; the callbacks here reach them from 'IO'.
  , rtSubmap      :: !(IORef (Maybe (X ())))
    -- ^ Likewise 'riverSubmap': set from 'X' when a submap opens, read from
    -- the @ate_unbound_key@ callback.
  , rtXkbVersion  :: !Word32
    -- ^ Negotiated @river_xkb_bindings_v1@ version.  @get_seat@ and
    -- @ensure_next_key_eaten@ arrived in 2.
  , rtPointerBind :: !(IORef (M.Map ObjectId (Window -> X ())))
  , rtPlacements  :: !(IORef [(Window, Rectangle)])
    -- ^ Layout output from the last manage sequence, applied during render.
  , rtVisible     :: !(IORef (S.Set Window))
  , rtBoundSeats  :: !(IORef (S.Set ObjectId))
  , rtHovered     :: !(IORef (Maybe Window))
  , rtManager     :: !ObjectId
    -- ^ Needed by binding callbacks, which run outside the 'X' monad and must
    -- request a manage sequence with @manage_dirty@.
  , rtConn        :: !Connection
    -- ^ Likewise: 'queueAction' has to ask for the sequence it queued into.
  , rtFollowsMouse :: !Bool
    -- ^ The config's 'focusFollowsMouse', copied here because @pointer_enter@
    -- is handled in 'IO' with no 'XConf' to consult -- and consulting it from
    -- a queued action would be too late: queueing at all asks river for a
    -- manage sequence, so a config with this off would pay for one on every
    -- pointer crossing.  Safe to snapshot: it cannot change without a restart.
  , rtStartupDone :: !(IORef Bool)
  , rtRestored    :: !(IORef Bool)
    -- ^ Whether the state file left by a previous window manager has been read
    -- back yet.  Only the first manage sequence may do it; see 'restoreState'.
  , rtLayerShell  :: !(Maybe ObjectId)
    -- ^ The @river_layer_shell_v1@ global, when the compositor offers it.
    -- Binding it is what tells river to let clients map layer surfaces at all;
    -- without it river closes every one on sight.
  , rtLayerDefault :: !(IORef (Maybe ObjectId))
    -- ^ The output most recently nominated as the default for layer surfaces
    -- that do not name one. Held so the request is only reissued when the
    -- choice actually changes.
  }

--------------------------------------------------------------------------------
-- Entry point

-- | Connect to river and run the window manager. Does not return.
riverMain :: XConfig Layout -> Directories -> IO ()
riverMain userConfig dirs = do
  conn <- connect
  (registry, globals) <- getRegistry conn
  mManager <- bindGlobal conn registry globals
                riverWindowManagerV1Interface 4 riverWindowManagerV1Version
  mBindings <- bindGlobal conn registry globals
                 riverXkbBindingsV1Interface 1 riverXkbBindingsV1Version
  -- Optional, and river only advertises it alongside the window manager
  -- global. Binding it is not merely how we learn about exclusive zones: it is
  -- the signal that this window manager supports layer shell, without which
  -- river refuses to map layer surfaces. Everything that draws outside the
  -- window layout depends on it — fuzzel prompts, notification daemons,
  -- wallpaper setters, bars and lock screens.
  mLayerShell <- bindGlobal conn registry globals
                   riverLayerShellV1Interface 1 riverLayerShellV1Version
  -- Surfaces the window manager draws itself -- prompts, decorations -- need
  -- these two.  Both are core Wayland rather than river extensions, so a
  -- compositor without them would be broken in a way worth reporting rather
  -- than working around.
  mCompositor <- bindGlobal conn registry globals
                   wlCompositorInterface 4 wlCompositorVersion
  mShm <- bindGlobal conn registry globals wlShmInterface 1 wlShmVersion

  case (mManager, mBindings) of
    (Just (manager, _), Just (bindings, bindingsVer)) -> do
      when (mLayerShell == Nothing) $ hPutStrLn stderr
        "xmonad-river: river_layer_shell_v1 is unavailable; layer surfaces \
        \(fuzzel prompts, notifications, wallpaper, bars) will not be shown"
      when (mCompositor == Nothing || mShm == Nothing) $ hPutStrLn stderr
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
  windowsRef <- newIORef M.empty
  outputsRef <- newIORef M.empty
  seatsRef   <- newIORef M.empty
  dirtyRef   <- newIORef False
  manageRef  <- newIORef False
  restartRef <- newIORef Nothing
  dragOrigin <- newIORef (0, 0)
  mailbox    <- MB.newMailbox
  -- One IORef each, shared between the XConf and the Runtime: the event loop's
  -- callbacks reach them from IO, and a submap reaches them from X.
  bindingsRef <- newIORef M.empty
  submapRef   <- newIORef Nothing
  placeRef    <- newIORef []
  restackRef  <- newIORef []
  extraKeysRef <- newIORef M.empty
  rt <- Runtime
          <$> newIORef []
          <*> pure bindingsRef
          <*> pure submapRef
          <*> pure bindingsVer
          <*> newIORef M.empty
          <*> pure placeRef
          <*> newIORef S.empty
          <*> newIORef S.empty
          <*> newIORef Nothing
          <*> pure manager
          <*> pure conn
          <*> pure (focusFollowsMouse userConfig)
          <*> newIORef False
          <*> newIORef False
          <*> pure layerShell
          <*> newIORef Nothing

  when (null (workspaces userConfig)) $ hPutStrLn stderr
    "xmonad-river: the config has no workspaces; using a single one named \"1\""

  -- Already existentially wrapped: XMonad.xmonad does that before calling
  -- here, so that every workspace can hold a different layout.
  let layout = layoutHook userConfig
      -- One workspace per name, and a single screen to begin with. Screens are
      -- reconciled against river's outputs at the start of every manage
      -- sequence, so the placeholder here is replaced before anything is laid
      -- out.
      -- A StackSet must have a current screen, so there must be at least one
      -- workspace.  A config with `workspaces = []` is a config error rather
      -- than something to crash on later with a head of an empty list, so it
      -- gets one unnamed workspace and a diagnostic.
      initialWorkspaces = case workspaces userConfig of
        []    -> [ W.Workspace "1" layout Nothing ]
        names -> [ W.Workspace i layout Nothing | i <- names ]
      (firstWorkspace, otherWorkspaces) = case initialWorkspaces of
        (w:ws) -> (w, ws)
        []     -> error "unreachable: initialWorkspaces is never empty"
      placeholder = W.Screen firstWorkspace 0 (SD (Rectangle 0 0 0 0))
      -- The fourth field is the floating window map, empty at startup.
      initialSet = W.StackSet placeholder [] otherWorkspaces M.empty

      xconf = XConf
        { config = userConfig
        , display = conn
        , riverManager = manager
        , riverBindings = bindings
        , riverCompositor = compositor
        , riverShm = shm
        , riverWindows = windowsRef
        , riverOutputs = outputsRef
        , riverSeats = seatsRef
        , riverDirty = dirtyRef
        , inManageSeq = manageRef
        , riverRestart = restartRef
        , riverMailbox = mailbox
        , riverKeyBindings = bindingsRef
        , riverPlacements = placeRef
        , riverExtraKeys = extraKeysRef
        , riverRestack = restackRef
        , riverSubmap = submapRef
        , riverDragOrigin = dragOrigin
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

  riverWindowManagerV1Listen conn manager $
    onManagerEvent conn manager restartRef rt runX'

  riverWindowManagerV1ManageDirty conn manager

  setMainThread
  -- Two ways in for a restart request that does not come from a keybinding.
  -- The signal is what @xmonad --restart@ sends, and is also the only handle a
  -- script -- or a test -- has on a running window manager; the pid file is
  -- how the sender finds this process, since river offers nothing between the
  -- two.  Both are set up after 'setMainThread', because that is what the
  -- handler needs to reach the event loop.
  _ <- installHandler sigUSR1 (Catch sendRestart) Nothing
  writeFile (pidFilePath (dataDir dirs)) . show =<< getProcessID
  -- The startup hook is deliberately *not* run here. river holds a watchdog
  -- over the manage sequence, and this config's startup hook spawns upwards of
  -- a dozen processes; doing that before the event loop starts means river
  -- waits on a manage_finish that cannot come, and logs
  -- "timeout occurred, some imperfect frames may be shown". It runs at the end
  -- of the first manage sequence instead, once manage_finish is already on its
  -- way. See 'runStartupHook'.
  -- The loop waits on the compositor socket *and* the mailbox, so an action
  -- posted from a timer thread is noticed now rather than whenever the next
  -- unrelated Wayland event happens along -- which on an idle desktop could be
  -- minutes.  Anything drained is queued for the next manage sequence and one
  -- is requested, because river permits window management state to change
  -- nowhere else.
  let loop = do
        -- Flush before waiting, or a request queued but not yet written --
        -- the manage_dirty just above, on the very first pass -- never
        -- reaches the compositor, and the wait blocks for a reply to
        -- something that was never sent.  The loop this replaced could not
        -- get this wrong: dispatch flushes before it reads.
        flush conn
        sockFd <- connectionFd conn
        ready <- MB.waitEither sockFd (MB.mailboxFd mailbox)
        case ready of
          Left () -> dispatch conn
          Right () -> do
            MB.clearWakeups mailbox
            acts <- MB.drain mailbox
            mapM_ (queueAction rt) acts
            flush conn
        loop
  -- A restart request arrives as an async exception thrown into this thread,
  -- which interrupts the blocking read. Ask river to release us, then keep
  -- dispatching: the 'finished' event does the exec.
  loop `catch` \RestartRequested -> do
    mExe <- restartTarget
    case mExe of
      -- Nothing to come back as.  Say so and carry on rather than stopping:
      -- once river has released this window manager there is no way back, and
      -- a session with an out-of-date window manager beats a session with
      -- none.
      Nothing -> do
        hPutStrLn stderr
          "xmonad-river: refusing to restart, the executable is gone; \
          \still running the old one"
        loop
      Just exe -> do
        args <- getArgs
        -- The same two things 'XMonad.Operations.restart' does before asking
        -- river to stop.  This path exists because 'sendRestart' is callable
        -- from any thread and so cannot run 'X' code itself; it must not
        -- therefore be a restart that quietly loses state.
        runX' (broadcastMessage ReleaseResources >> writeStateToFile)
        writeIORef restartRef (Just (exe, args))
        riverWindowManagerV1Stop conn manager
        loop

-- | What to exec on restart, or 'Nothing' if there is nothing to exec.
--
-- 'getExecutablePath' reads @\/proc\/self\/exe@, and Linux appends
-- @\" (deleted)\"@ to that link once the binary has been replaced -- which is
-- exactly what installing a new build does, since it links a fresh inode over
-- the old name.  Passing the literal string through means @sh@ looks for a
-- file whose name ends in @\" (deleted)\"@, does not find it, and the restart
-- dies /after/ river has already released the window manager: the session is
-- left with no window manager at all, no keybindings, and no way to ask for
-- one.
--
-- Stripping the suffix gives the path the new build was installed to, which is
-- what should be exec'd.  Checked for existence first, because the whole point
-- is not to tear down a working session for a successor that is not there.
restartTarget :: IO (Maybe FilePath)
restartTarget = do
  raw <- getExecutablePath
  let suffix = " (deleted)"
      path | suffix `isSuffixOf` raw = take (length raw - length suffix) raw
           | otherwise = raw
  ok <- doesFileExist path
  pure (if ok then Just path else Nothing)

--------------------------------------------------------------------------------
-- Manager events

onManagerEvent
  :: Connection -> ObjectId -> IORef (Maybe (FilePath, [String])) -> Runtime
  -> (forall a. X a -> IO a)
  -> RiverWindowManagerV1Event -> IO ()
onManagerEvent conn manager restartRef rt runX' = \case
  RiverWindowManagerV1Unavailable -> do
    hPutStrLn stderr "xmonad-river: another window manager is already running"
    exitFailure
  -- river has confirmed this window manager is no longer active, so a
  -- successor may now connect. Exec it in place, which is what makes M-q a
  -- restart rather than a logout.
  RiverWindowManagerV1Finished -> readIORef restartRef >>= \case
    Nothing  -> exitSuccess
    -- Straight to the successor, with no shell in between.  @sh -c@ execs
    -- only in the narrowest cases and otherwise forks and waits, so it does
    -- not disappear the way an exec'd process does -- and since each restart
    -- would inherit the last one's shell, they nested: four M-q presses, four
    -- idle dash processes, one inside the next.
    Just (prog, as) -> executeFile prog True as Nothing
  RiverWindowManagerV1ManageStart -> do
    runX' (manageSequence rt)
    riverWindowManagerV1ManageFinish conn manager
    -- Deliver manage_finish before running anything slow. Requests are only
    -- buffered until the event loop flushes, and the startup hook spawns
    -- enough processes to trip river's watchdog in the meantime.
    flush conn
    runX' (runStartupHook rt)
  RiverWindowManagerV1RenderStart -> do
    runX' (renderSequence rt)
    riverWindowManagerV1RenderFinish conn manager
  RiverWindowManagerV1Window win -> runX' (addWindow win)
  RiverWindowManagerV1Output out -> runX' (addOutput rt out)
  RiverWindowManagerV1Seat seat  -> runX' (addSeat rt seat)
  RiverWindowManagerV1SessionLocked   -> void (runX' (broadcastEvent SessionLocked))
  RiverWindowManagerV1SessionUnlocked -> void (runX' (broadcastEvent SessionUnlocked))
  _ -> pure ()

broadcastEvent :: Event -> X All
broadcastEvent ev = do
  hook <- asks (handleEventHook . config)
  userCodeDef (All True) (hook ev)

--------------------------------------------------------------------------------
-- Object tracking

addWindow :: ObjectId -> X ()
addWindow win = do
  conn <- asks display
  node <- io (riverWindowV1GetNode conn win)
  ref <- asks riverWindows
  io $ modifyIORef' ref $ M.insert win RiverWindow
    { rwObject = win, rwNode = node
    , rwAppId = Nothing, rwTitle = Nothing, rwPid = Nothing
    , rwIdentifier = Nothing, rwParent = Nothing
    , rwDimensions = (0, 0)
    , rwSizeHints = noSizeHints
    , rwNew = True, rwClosed = False, rwFullscreen = False, rwHidden = False
    }
  io $ riverWindowV1Listen conn win $ \case
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
    -- Both are followed by a manage_start, so a manage hook asking
    -- 'XMonad.Hooks.ManageHelpers.isFullscreen' sees the up-to-date answer.
    RiverWindowV1FullscreenRequested _ ->
      adjust ref win $ \w -> w { rwFullscreen = True }
    RiverWindowV1ExitFullscreenRequested ->
      adjust ref win $ \w -> w { rwFullscreen = False }
    RiverWindowV1PointerMoveRequested _ -> pure ()
    _ -> pure ()

adjust :: IORef (M.Map ObjectId a) -> ObjectId -> (a -> a) -> IO ()
adjust ref k f = modifyIORef' ref (M.adjust f k)

addOutput :: Runtime -> ObjectId -> X ()
addOutput rt out = do
  conn <- asks display
  ref <- asks riverOutputs

  mLayer <- forM (rtLayerShell rt) $ \shell -> io $ do
    lo <- riverLayerShellV1GetOutput conn shell out
    riverLayerShellOutputV1Listen conn lo $ \case
      RiverLayerShellOutputV1NonExclusiveArea x y width height ->
        adjust ref out $ \o -> o { roLayerArea = Just (Rectangle x y
          (fromIntegral width) (fromIntegral height)) }
      _ -> pure ()
    pure lo

  io $ modifyIORef' ref $ M.insert out RiverOutput
    { roObject = out, roPosition = (0, 0), roSize = (0, 0), roRemoved = False
    , roLayerObject = mLayer, roLayerArea = Nothing }
  void (broadcastEvent (OutputAdded out))

  io $ riverOutputV1Listen conn out $ \case
    RiverOutputV1Removed -> adjust ref out $ \o -> o { roRemoved = True }
    RiverOutputV1Position x y -> adjust ref out $ \o -> o { roPosition = (x, y) }
    RiverOutputV1Dimensions width height ->
      adjust ref out $ \o -> o { roSize = (width, height) }
    _ -> pure ()

addSeat :: Runtime -> ObjectId -> X ()
addSeat rt seat = do
  conn <- asks display
  ref <- asks riverSeats

  mLayer <- forM (rtLayerShell rt) $ \shell -> io $ do
    ls <- riverLayerShellV1GetSeat conn shell seat
    riverLayerShellSeatV1Listen conn ls $ \ev -> do
      let set f = adjust ref seat $ \s -> s { rsLayerFocus = f }
      case ev of
        RiverLayerShellSeatV1FocusExclusive    -> set LayerFocusExclusive
        RiverLayerShellSeatV1FocusNonExclusive -> set LayerFocusNonExclusive
        RiverLayerShellSeatV1FocusNone         -> set LayerFocusNone
        _ -> pure ()
    pure ls

  -- The object a submap requests @ensure_next_key_eaten@ on.  Created once per
  -- seat, because doing it twice is a protocol error, and only when river
  -- offers version 2 or better -- the request does not exist before that.
  bindingsGlobal <- asks riverBindings
  mXkbSeat <- if rtXkbVersion rt < 2 then pure Nothing else io $ do
    xs <- riverXkbBindingsV1GetSeat conn bindingsGlobal seat
    riverXkbBindingsSeatV1Listen conn xs $ \case
      -- A key the open submap did not ask for.  Under X11 this arrived on the
      -- keyboard grab and the submap simply did not match it; here the only
      -- way to hear about it at all is to have asked for the key to be eaten.
      -- Acting on it matters more than it sounds: a submap disables every one
      -- of the window manager's bindings while it is open, so a submap that
      -- could not notice an unknown key would stay armed and leave the session
      -- with no working shortcuts.
      RiverXkbBindingsSeatV1AteUnboundKey -> do
        pending <- atomicModifyIORef' (rtSubmap rt) (\s -> (Nothing, s))
        forM_ pending (queueAction rt)
      -- What ends an Alt-Tab.  Only sent for modifiers something asked to
      -- watch, so this is silent unless 'XMonad.River.whileModifiersHeld' has
      -- an interaction open.
      RiverXkbBindingsSeatV1ModifiersUpdate old new ->
        takeModifierWatcher >>= mapM_ (\f -> f old new)
      _ -> pure ()
    pure (Just xs)

  io $ modifyIORef' ref $ M.insert seat RiverSeat
    { rsObject = seat, rsRemoved = False
    , rsLayerObject = mLayer, rsLayerFocus = LayerFocusNone
    , rsPointer = (0, 0), rsXkbSeat = mXkbSeat }
  void (broadcastEvent (SeatAdded seat))

  io $ riverSeatV1Listen conn seat $ \case
    RiverSeatV1Removed -> adjust ref seat $ \s -> s { rsRemoved = True }
    -- @pointer_enter@ is river's equivalent of X11's @EnterNotify@, and is
    -- what makes 'focusFollowsMouse' work.  X11 checked @ev_mode ==
    -- notifyNormal@ to ignore the crossings a grab synthesises; river sends
    -- this only for genuine pointer movement, so there is nothing to filter.
    RiverSeatV1PointerEnter win -> do
      writeIORef (rtHovered rt) (Just win)
      when (rtFollowsMouse rt) $ queueAction rt $ do
        -- Both conditions are about the delay.  The action runs at the start
        -- of the next manage sequence rather than now, so the pointer may
        -- have moved on -- refocusing to where it used to be would fight the
        -- crossing that has already been queued behind this one.  And a drag
        -- in progress owns the focus outright: X11 held a pointer grab for
        -- the duration, which suppressed crossing events entirely, and there
        -- is no grab here to do it.
        stillThere <- io ((== Just win) <$> readIORef (rtHovered rt))
        drag <- gets dragging
        when (stillThere && isNothing drag) (focus win)
    RiverSeatV1PointerLeave -> writeIORef (rtHovered rt) Nothing
    RiverSeatV1PointerPosition x y ->
      adjust ref seat $ \s -> s { rsPointer = (x, y) }
    -- An interactive operation reports the total offset since it began.
    -- XMonad.Operations.mouseDrag stashed where the pointer was at the time,
    -- so the absolute position its caller expects is origin + delta.
    RiverSeatV1OpDelta dx dy -> queueAction rt $ do
      drag <- gets dragging
      whenJust drag $ \(motion, _) -> do
        (ox, oy) <- io . readIORef =<< asks riverDragOrigin
        motion (ox + dx) (oy + dy)
    RiverSeatV1OpRelease -> do
      riverSeatV1OpEnd conn seat
      queueAction rt $ do
        drag <- gets dragging
        whenJust drag $ \(_, cleanup) -> cleanup
    _ -> pure ()

--------------------------------------------------------------------------------
-- The manage sequence

manageSequence :: Runtime -> X ()
manageSequence rt = do
  asks inManageSeq >>= \r -> io (writeIORef r True)
  restoreState rt
  reapClosed
  syncScreens
  nominateLayerOutput rt
  createBindings rt
  adoptNewWindows
  runPending rt
  applyLayout rt
  asks inManageSeq >>= \r -> io (writeIORef r False)

-- | Pick up where the previous window manager left off, if it left a state
-- file.
--
-- This is what makes @M-q@ keep windows on the workspaces they were on.  It
-- runs first in the first manage sequence and never again, and both halves of
-- that matter:
--
-- * /First in the sequence/, because everything after it -- reconciling
--   screens, adopting windows, laying out -- has to see the restored
--   'WindowSet' rather than the empty one the process started with.
--
-- * /The first sequence/, because that is the earliest moment the identifiers
--   in the file can be resolved, and also the last moment they can be resolved
--   without a visible flicker.  river sends a @window@ event, and the
--   @identifier@ with it, for every window it already has /before/ it sends
--   @manage_start@ -- @WindowManager.manageStart@ iterates the windows and
--   then sends the event -- so by the time this runs the map is complete.
--
-- A restored window has already been through a manage hook, in the process
-- that wrote the file, so it is marked as no longer new and 'adoptNewWindows'
-- leaves it alone.  Anything the file did not account for -- opened while the
-- successor was starting, or belonging to a client that connected since --
-- stays new and is managed normally.
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
        -- Worth saying out loud, and not only for the test that reads it: the
        -- two numbers differing is the one interesting failure here.  It means
        -- identifiers written by the previous window manager did not match the
        -- ones river is now reporting, and the symptom a user sees is windows
        -- silently back on the wrong workspace -- with nothing else to
        -- distinguish it from a state file that was never written.
        io $ hPutStrLn stderr $ "xmonad-river: note: restored "
          <> show (length restored) <> " of " <> show wanted
          <> " windows from " <> path

-- | How many windows the state file claims, read before it is consumed.
--
-- Counted from the raw text rather than from a parse, because the point is to
-- compare against what the parse produced.  Returns 0 for a file that cannot
-- be read, which is also what 'readStateFile' will make of it.
windowsInStateFile :: FilePath -> IO Int
windowsInStateFile path = handle (\(_ :: SomeException) -> pure 0) $ do
  raw <- readFile path
  case reads raw of
    [(sf, _)] -> pure (length (W.allWindows (sfWins sf)))
    _         -> pure 0

-- | Drop windows river has told us are gone, and destroy the protocol objects.
reapClosed :: X ()
reapClosed = do
  conn <- asks display
  ref <- asks riverWindows
  ws <- io (readIORef ref)
  let closed = [ w | w <- M.elems ws, rwClosed w ]
  forM_ closed $ \w -> do
    modify $ \st -> st { windowset = W.delete (rwObject w) (windowset st) }
    io $ do
      forgetBorderOverride (rwObject w)
      riverNodeV1Destroy conn (rwNode w)
      riverWindowV1Destroy conn (rwObject w)
      modifyIORef' ref (M.delete (rwObject w))

  outRef <- asks riverOutputs
  outs <- io (readIORef outRef)
  -- The layer shell objects are inert once removed is sent, but destroying
  -- them is still what completes destruction of the output.
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o -> do
    void (broadcastEvent (OutputRemoved (roObject o)))
    io $ do
      forM_ (roLayerObject o) (riverLayerShellOutputV1Destroy conn)
      riverOutputV1Destroy conn (roObject o)
      modifyIORef' outRef (M.delete (roObject o))

  seatRef <- asks riverSeats
  seats <- io (readIORef seatRef)
  forM_ [ s | s <- M.elems seats, rsRemoved s ] $ \s -> do
    void (broadcastEvent (SeatRemoved (rsObject s)))
    io $ do
      forM_ (rsLayerObject s) (riverLayerShellSeatV1Destroy conn)
      forM_ (rsXkbSeat s) (riverXkbBindingsSeatV1Destroy conn)
      riverSeatV1Destroy conn (rsObject s)
      modifyIORef' seatRef (M.delete (rsObject s))

-- | Nominate an output for layer surfaces that do not pick one themselves.
--
-- Until this is done the default output is undefined, and a client like fuzzel
-- that names no output has nowhere to be placed. @set_default@ modifies window
-- management state, so it belongs in the manage sequence.
--
-- The choice follows the current screen, so that a prompt opens on the output
-- being worked on. It is reissued whenever that changes, which also covers the
-- case of the previous default being unplugged.
nominateLayerOutput :: Runtime -> X ()
nominateLayerOutput rt = forM_ (rtLayerShell rt) $ \_ -> do
  outs <- io . readIORef =<< asks riverOutputs
  ws <- gets windowset
  let SD current = W.screenDetail (W.current ws)
      live = filter (not . roRemoved) (M.elems outs)
      -- Match by position: that is the only thing tying a StackSet screen back
      -- to the output it was built from in 'syncScreens'.
      onScreen o = let (x, y) = roPosition o
                   in x == rect_x current && y == rect_y current
      chosen = case filter onScreen live of
        (o:_) -> Just o
        []    -> case sortOn roPosition live of
          (o:_) -> Just o
          []    -> Nothing
  forM_ chosen $ \o -> forM_ (roLayerObject o) $ \lo -> do
    prev <- io (readIORef (rtLayerDefault rt))
    unless (prev == Just (roObject o)) $ do
      conn <- asks display
      io (riverLayerShellOutputV1SetDefault conn lo)
      io (writeIORef (rtLayerDefault rt) (Just (roObject o)))

-- | Reconcile the 'WindowSet'\'s screens with river's outputs.
--
-- This is xmonad's @rescreen@, driven by the output list rather than xinerama.
-- Outputs are ordered by position so that screen ids are stable across
-- reconnects, which is what @XMonad.Actions.PhysicalScreens@ relies on.
syncScreens :: X ()
syncScreens = do
  outs <- io . readIORef =<< asks riverOutputs
  let rects =
        [ rect
        | o <- sortOn roPosition (filter (not . roRemoved) (M.elems outs))
        , let (x, y) = roPosition o
        , let (width, height) = roSize o
        , width > 0 && height > 0
          -- Prefer the area layer shell reports, so a bar or dock that claims
          -- an exclusive zone shrinks the tiling area instead of being tiled
          -- over. It is only a hint, but honouring it is what users expect and
          -- it costs nothing to do so.
        , let rect = case roLayerArea o of
                Just a | rect_width a > 0 && rect_height a > 0 -> a
                _ -> Rectangle x y (fromIntegral width) (fromIntegral height)
        ]
  unless (null rects) $ do
    before <- gets (map (screenRect . W.screenDetail) . screensOf . windowset)
    modify $ \st -> st { windowset = rescreen rects (windowset st) }
    -- Only when it actually changed.  A manage sequence runs for all sorts of
    -- reasons and most of them leave the outputs alone; a config restarting
    -- its status bars on every one of them would be unusable.
    when (before /= rects) $ void (broadcastEvent ScreenLayoutChanged)

-- | The screens a 'WindowSet' currently has, current first.
screensOf :: WindowSet -> [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
screensOf ws = W.current ws : W.visible ws

-- | Lay the given screen rectangles over the current workspaces, preserving
-- which workspace is on which screen where possible.
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
    -- Workspaces that were on now-absent screens fall back to hidden.
    oldVisible = W.visible ws
    reseat i r = case drop (i - 1) oldVisible of
      (s:_) -> s { W.screen = fromIntegral i, W.screenDetail = SD r }
      [] -> case newHidden of
        (h:_) -> W.Screen h (fromIntegral i) (SD r)
        []    -> W.Screen (W.workspace (W.current ws)) (fromIntegral i) (SD r)
    surplus = drop (length restRects) oldVisible
    newHidden = map W.workspace surplus ++ W.hidden ws

-- | Run the manage hook for windows river has just told us about, and insert
-- them into the 'WindowSet'.
--
-- This happens during a manage sequence, before the window has been rendered,
-- which is the same ordering guarantee xmonad's manage hook has — and the one
-- sway's IPC cannot provide.
adoptNewWindows :: X ()
adoptNewWindows = do
  ref <- asks riverWindows
  ws <- io (readIORef ref)
  let fresh = [ w | w <- M.elems ws, rwNew w, not (rwClosed w) ]
  conn <- asks display
  forM_ fresh $ \w -> do
    io $ adjust ref (rwObject w) $ \x -> x { rwNew = False }
    -- Ask for server-side decoration before the manage hook runs, so a config
    -- that wants otherwise can override it there.
    --
    -- This has to be said out loud: river's default, if a window manager makes
    -- neither request, is use_csd -- the client draws its own title bar and
    -- close button.  That is right for a floating desktop and wrong for a
    -- tiling one, where the window manager owns the frame and a client-drawn
    -- title bar is decoration nobody asked for on every single window.
    --
    -- "Server side" here does not mean river draws a title bar either: it
    -- draws exactly what this window manager asks for, which is the border
    -- from 'borderWidth' and nothing else.  A client that only supports CSD
    -- ignores the request, which river documents and which is why this is not
    -- conditional on the decoration_hint.
    io (riverWindowV1UseSsd conn (rwObject w))
    -- Everything above is setup this connection owes river for any window it
    -- has not spoken to before, including one carried over from the previous
    -- window manager: those requests were made on a connection that no longer
    -- exists, and river's defaults are back.  Everything below is /managing/ a
    -- window, which a restored one has already had done to it -- by the
    -- process that wrote the state file.  Running the manage hook again would
    -- re-shift it to whatever the hook says and undo the restore, and
    -- 'W.insertUp' would put a second copy of it in the 'WindowSet'.
    --
    -- Membership in the 'WindowSet' is the test rather than a flag, because it
    -- is the actual question being asked: is this already a managed window.
    managed <- gets (W.allWindows . windowset)
    unless (rwObject w `elem` managed) $ do
      mh <- asks (manageHook . config)
      g <- userCodeDef (mempty) (runQuery mh (rwObject w))
      ws' <- gets windowset
      let placed = W.insertUp (rwObject w) ws'
      modify $ \st -> st { windowset = appEndo g placed }
      void (broadcastEvent (WindowAdded (rwObject w)))

-- | Run the user's startup hook exactly once, after the first manage sequence
-- has been finished.
--
-- Anything it does that changes window management state goes through
-- 'XMonad.Operations.windows', which requests another manage sequence, so
-- deferring costs nothing but keeps a slow hook from tripping river's
-- watchdog.
-- Setting @XMONAD_RIVER_NO_STARTUP_HOOK@ skips it, which is what makes it safe
-- to run a real config under a test compositor.  A startup hook is the one
-- part of a config that reaches outside the session it belongs to: this one
-- spawns a dozen processes and kills tmux sessions by name, and doing that
-- from a throwaway headless river would interfere with the desktop the user is
-- actually sitting in front of.  Everything worth testing -- manage sequences,
-- layout, bindings -- happens without it.
runStartupHook :: Runtime -> X ()
runStartupHook rt = do
  done <- io (readIORef (rtStartupDone rt))
  unless done $ do
    io (writeIORef (rtStartupDone rt) True)
    skip <- io (lookupEnv "XMONAD_RIVER_NO_STARTUP_HOOK")
    case skip of
      Just _ -> io $ hPutStrLn stderr
        "xmonad-river: note: startup hook skipped \
        \(XMONAD_RIVER_NO_STARTUP_HOOK is set)"
      Nothing -> do
        hook <- asks (startupHook . config)
        _ <- userCode hook
        pure ()

--------------------------------------------------------------------------------
-- Bindings

-- | Create river bindings for any seat that does not have them yet.
createBindings :: Runtime -> X ()
createBindings rt = do
  seats <- io . readIORef =<< asks riverSeats
  bound <- io (readIORef (rtBoundSeats rt))
  let new = [ s | s <- M.keys seats, not (S.member s bound) ]
  forM_ new $ \seat -> do
    bindSeat rt seat
    io $ modifyIORef' (rtBoundSeats rt) (S.insert seat)

bindSeat :: Runtime -> ObjectId -> X ()
bindSeat rt seat = do
  conn <- asks display
  bindingsGlobal <- asks riverBindings
  ks <- asks keyActions
  bs <- asks buttonActions

  bindPanic rt seat

  forM_ (M.toList ks) $ \((mask, keysym), action) -> do
    b <- io (riverXkbBindingsV1GetXkbBinding conn bindingsGlobal seat keysym
               (riverModifiers mask))
    io $ modifyIORef' (rtBindings rt) (M.insert b action)
    io $ riverXkbBindingV1Listen conn b $ \case
      RiverXkbBindingV1Pressed -> do
        acts <- readIORef (rtBindings rt)
        forM_ (M.lookup b acts) $ \a -> do
          modifyIORef' (rtPending rt) (a :)
          riverWindowManagerV1ManageDirty conn (rtManager rt)
      _ -> pure ()
    io (riverXkbBindingV1Enable conn b)

  forM_ (M.toList bs) $ \((mask, button), action) -> do
    b <- io (riverSeatV1GetPointerBinding conn seat (linuxButton button)
               (riverModifiers mask))
    io $ modifyIORef' (rtPointerBind rt) (M.insert b action)
    io $ riverPointerBindingV1Listen conn b $ \case
      RiverPointerBindingV1Pressed -> do
        acts <- readIORef (rtPointerBind rt)
        forM_ (M.lookup b acts) $ \a -> do
          mHover <- readIORef (rtHovered rt)
          forM_ mHover $ \win -> do
            modifyIORef' (rtPending rt) (a win :)
            riverWindowManagerV1ManageDirty conn (rtManager rt)
      _ -> pure ()
    io (riverPointerBindingV1Enable conn b)

-- | The chord that always works.
--
-- Two things in this window manager can make a session unusable by taking
-- something and not giving it back, and both have done so:
--
-- * a prompt is a layer surface holding an exclusive keyboard grab, so if its
--   thread stops answering, every keystroke goes somewhere nothing is reading;
-- * a submap disables every one of the config's bindings until it closes, so
--   one that never closes leaves a session with no shortcuts at all.
--
-- Each now has its own guard -- guaranteed teardown, and a deadline -- and
-- this is what remains when a guard is the thing that failed.  It is deliberately
-- not part of @keyActions@: it is registered separately, never recorded in
-- 'rtBindings', and therefore not among the bindings a submap disables.  A
-- config cannot rebind or remove it, which is the point; a break-glass key
-- that the broken thing can switch off is not one.
--
-- The chord is @Ctrl-Alt-Shift-Escape@, chosen to be one nothing else wants.
--
-- This works at all because of how river dispatches: @KeyboardGroup.processKey@
-- matches xkb bindings /before/ it consults keyboard focus, so a binding fires
-- even while a layer surface holds an exclusive grab.  Without that ordering
-- there would be no key that could rescue a wedged prompt, and the only way
-- out would be a TTY.
bindPanic :: Runtime -> ObjectId -> X ()
bindPanic rt seat = do
  conn <- asks display
  bindingsGlobal <- asks riverBindings
  b <- io (riverXkbBindingsV1GetXkbBinding conn bindingsGlobal seat
             xK_Escape (riverModifiers (controlMask .|. mod1Mask .|. shiftMask)))
  io $ riverXkbBindingV1Listen conn b $ \case
    RiverXkbBindingV1Pressed -> do
      n <- closeAllClients
      -- Re-enable everything the config asked for, and forget any submap.
      -- Whatever state a half-finished submap left behind, the bindings the
      -- user knows about are the ones that should be live afterwards.
      globals <- readIORef (rtBindings rt)
      forM_ (M.keys globals) (riverXkbBindingV1Enable conn)
      writeIORef (rtSubmap rt) Nothing
      hPutStrLn stderr $ "xmonad-river: panic: closed " <> show n
        <> " prompt(s) and re-enabled " <> show (M.size globals) <> " binding(s)"
      riverWindowManagerV1ManageDirty conn (rtManager rt)
    _ -> pure ()
  io (riverXkbBindingV1Enable conn b)

-- | X11 button numbers to Linux input event codes, which is what river's
-- pointer bindings take.
linuxButton :: Button -> Word32
linuxButton = \case
  1 -> 0x110  -- BTN_LEFT
  2 -> 0x112  -- BTN_MIDDLE
  3 -> 0x111  -- BTN_RIGHT
  4 -> 0x113  -- BTN_SIDE
  5 -> 0x114  -- BTN_EXTRA
  n -> 0x110 + fromIntegral n

-- | Queue an action to run at the start of the next manage sequence.
-- | Queue an action to run at the start of the next manage sequence, and ask
-- river for one.
--
-- Everything that fires outside a sequence -- bindings, and the deltas of an
-- interactive drag -- has to go through here, because river only permits
-- window management state to change during a sequence.
queueAction :: Runtime -> X () -> IO ()
queueAction rt act = do
  modifyIORef' (rtPending rt) (act :)
  riverWindowManagerV1ManageDirty (rtConn rt) (rtManager rt)

runPending :: Runtime -> X ()
runPending rt = do
  acts <- io (atomicModifyIORef' (rtPending rt) (\as -> ([], reverse as)))
  mapM_ (userCode) acts

--------------------------------------------------------------------------------
-- Layout

-- | Run the layout for every visible screen, propose the resulting dimensions,
-- set keyboard focus, and stash the rectangles for the render sequence.
applyLayout :: Runtime -> X ()
applyLayout rt = do
  ws <- gets windowset
  let screens = W.current ws : W.visible ws
  placements <- fmap concat $ forM screens $ \scr -> do
    let wsp = W.workspace scr
        SD rect = W.screenDetail scr
        -- The floating layer, as upstream's 'windows' handles it.  Floats are
        -- withheld from the layout -- it would tile them -- and placed from the
        -- rectangles 'XMonad.Operations.float' recorded, scaled to the screen.
        --
        -- Leaving this out is not a small omission: it makes 'float' inert, so
        -- every drag, keyboard move and doFloat hook is undone by the next
        -- manage sequence, which looks like the mouse bindings not working at
        -- all.
        floats = W.floating ws
        onWs = W.integrate' (W.stack wsp)
        tiled = W.stack wsp >>= W.filter (`M.notMember` floats)
        flt = [ (fw, scaleRationalRect rect rr)
              | fw <- onWs, Just rr <- [M.lookup fw floats] ]
    (rs, mLayout) <- userCodeDef ([], Nothing) (runLayout wsp { W.stack = tiled } rect)
    forM_ mLayout $ \l' -> modify $ \st ->
      st { windowset = updateLayout (W.tag wsp) l' (windowset st) }
    -- Floats last, because the render sequence place_tops this list in order,
    -- so later is higher.  Upstream expresses the same thing the other way
    -- round and then restacks.
    pure (rs ++ flt)

  io $ writeIORef (rtPlacements rt) placements
  io $ writeIORef (rtVisible rt) (S.fromList (map fst placements))

  -- Publish what the layout decided, so that the IO-shaped queries in
  -- "XMonad.Core" -- getWindowAttributes, getGeometry -- have something to
  -- answer with.  Every window river has told us about is included, not just
  -- the placed ones: X11 answered for any window that existed, and a window on
  -- a workspace that is not on screen still exists.  It is simply unmapped and
  -- at the origin, which is what X11 would have said of an unmapped window
  -- too.
  bw <- asks (borderWidth . config)
  allKnown <- io . readIORef =<< asks riverWindows
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

  conn <- asks display
  winRef <- asks riverWindows
  known <- io (readIORef winRef)

  -- Dimensions are window management state, so they go here rather than in
  -- the render sequence.
  forM_ placements $ \(win, r) -> when (M.member win known) $
    io $ riverWindowV1ProposeDimensions conn win
           (fromIntegral (rect_width r)) (fromIntegral (rect_height r))

  -- Keyboard focus, likewise. A seat whose keyboard has gone to a layer
  -- surface is left alone: river discards focus requests outright while focus
  -- is exclusive, and in the non-exclusive case setting focus in this same
  -- manage sequence would silently steal the keyboard back — which is the
  -- difference between a fuzzel prompt you can type into and one you cannot.
  seats <- io . readIORef =<< asks riverSeats
  forM_ (M.elems seats) $ \s ->
    unless (layerHasFocus (rsLayerFocus s)) $
      case W.peek ws of
        Just win | M.member win known ->
          io (riverSeatV1FocusWindow conn (rsObject s) win)
        _ -> io (riverSeatV1ClearFocus conn (rsObject s))

updateLayout :: WorkspaceId -> Layout Window -> WindowSet -> WindowSet
updateLayout i l = W.mapWorkspace $ \wsp ->
  if W.tag wsp == i then wsp { W.layout = l } else wsp

--------------------------------------------------------------------------------
-- The render sequence

renderSequence :: Runtime -> X ()
renderSequence rt = do
  conn <- asks display
  placements <- io (readIORef (rtPlacements rt))
  visible <- io (readIORef (rtVisible rt))
  winRef <- asks riverWindows
  known <- io (readIORef winRef)
  bw <- asks (borderWidth . config)
  focusedCol <- asks focusedBorder
  normalCol <- asks normalBorder
  mFocus <- W.peek <$> gets windowset

  forM_ placements $ \(win, r) -> forM_ (M.lookup win known) $ \w -> do
    io $ riverNodeV1SetPosition conn (rwNode w) (rect_x r) (rect_y r)
    -- Borders are rendering state, so river keeps no memory of them: every
    -- render sequence states them again from scratch.  That is why a
    -- per-window override has to be stored rather than issued directly -- see
    -- 'XMonad.Core.setWindowBorderWidth'.
    (mWidth, mColor) <- io (lookupBorderOverride win)
    let width = fromMaybe bw mWidth
        -- Widened here rather than stored widened: the colours a config and
        -- contrib deal in are Pixels, as they were under X11, and the RGBA
        -- quadruple is the wire format rather than the vocabulary.
        (red, green, blue, alpha) = case mColor of
          Just c  -> c
          Nothing -> pixelColor (if Just win == mFocus then focusedCol else normalCol)
    -- Unconditional, where this used to skip a zero width entirely: an
    -- override *to* zero is how NoBorders removes a border, and skipping the
    -- request would leave the previous one standing.  river reads width 0 as
    -- "no borders", so the two cases need no distinguishing here.
    io $ riverWindowV1SetBorders conn win allEdges (fromIntegral width)
           red green blue alpha
    when (rwHidden w) $ do
      io (riverWindowV1Show conn win)
      io $ adjust winRef win $ \x -> x { rwHidden = False }

  -- Anything not placed by the layout belongs to a workspace that is not on
  -- screen. river has no concept of workspaces, so this is what implements
  -- them.
  forM_ (M.elems known) $ \w ->
    unless (S.member (rwObject w) visible || rwHidden w) $ do
      io (riverWindowV1Hide conn (rwObject w))
      io $ adjust winRef (rwObject w) $ \x -> x { rwHidden = True }

  -- Stacking order: the layout list is in the desired bottom-to-top order.
  forM_ placements $ \(win, _) -> forM_ (M.lookup win known) $ \w ->
    io (riverNodeV1PlaceTop conn (rwNode w))

  -- Then anything asked to sit above that, re-applied every frame because this
  -- loop would otherwise have just undone it.  Filtered to what the layout
  -- placed, so a raised window that has since gone away or moved to another
  -- workspace stops being raised rather than lingering as a stale request.
  raised <- io . readIORef =<< asks riverRestack
  let placed = S.fromList (map fst placements)
      stillUp = filter (`S.member` placed) raised
  io . flip writeIORef stillUp =<< asks riverRestack
  forM_ stillUp $ \win -> forM_ (M.lookup win known) $ \w ->
    io (riverNodeV1PlaceTop conn (rwNode w))

-- | A dimension bound river reports as zero or less was not stated.
sizeBound :: Int32 -> Int32 -> Maybe (Dimension, Dimension)
sizeBound w h
  | w > 0 && h > 0 = Just (fromIntegral w, fromIntegral h)
  | otherwise      = Nothing

allEdges :: Word32
allEdges = riverWindowV1EdgesTop + riverWindowV1EdgesBottom
         + riverWindowV1EdgesLeft + riverWindowV1EdgesRight

