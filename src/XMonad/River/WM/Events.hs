{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | What the compositor tells us about its objects, kept on the loop.
--
-- Everything here runs in 'IO' on the event loop and touches no 'XState':
-- it records what river said and queues the config's hooks for the worker.
module XMonad.River.WM.Events
  ( addWindow
  , addOutput
  , addSeat
  , reapObjects
  ) where

import Control.Concurrent.STM (atomically, writeTVar)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Data.IORef
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.Core
import XMonad.Operations (focus, mouseMoveWindow)
import XMonad.River (mouseResizeWindowEdges)
import XMonad.River.Connection
import XMonad.River.Ops (emitOp)
import XMonad.River.Plan (FocusTarget(..), Op(..))
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.LayerShell
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.State (InputCapture(..), Shared(..), riverDragOrigin)
import XMonad.River.Types
import XMonad.River.WM.Input (outputsChanged)
import XMonad.River.WM.Runtime
import XMonad.River.Wire (ObjectId, isNullObject)
import qualified XMonad.StackSet as W

-- | Take note of a window river has just told us about.
--
-- Ignored on purpose: @decoration_hint@ (SSD is requested regardless, and a
-- CSD-only client ignores that), @maximize_requested@, @minimize_requested@,
-- @show_window_menu_requested@, @presentation_hint@ -- a tiling layout has no
-- meaning for any of them.
addWindow :: Runtime -> ObjectId -> IO ()
addWindow rt win = do
  node <- riverWindowV1GetNode conn win
  let ref = shWindows (rtShared rt)
  modifyIORef' (rtWindowsGen rt) (+ 1)
  modifyIORef' ref $ M.insert win RiverWindow
    { rwObject = win, rwNode = node
    , rwAppId = Nothing, rwTitle = Nothing, rwPid = Nothing
    , rwIdentifier = Nothing, rwParent = Nothing
    , rwDimensions = (0, 0)
    , rwSizeHints = noSizeHints
    , rwClosed = False, rwFullscreen = False, rwHidden = False
    , rwCaptureSessions = 0
    }
  riverWindowV1Listen conn win $ \case
    RiverWindowV1Closed        -> adjust ref win $ \w -> w { rwClosed = True }
    RiverWindowV1AppId a       -> adjust ref win $ \w -> w { rwAppId = a }
    RiverWindowV1Title t       -> adjust ref win $ \w -> w { rwTitle = t }
    RiverWindowV1UnreliablePid p -> adjust ref win $ \w -> w { rwPid = Just p }
    RiverWindowV1Identifier i  -> adjust ref win $ \w -> w { rwIdentifier = Just i }
    RiverWindowV1CaptureSessions n -> adjust ref win $ \w -> w { rwCaptureSessions = n }
    RiverWindowV1Parent p      -> adjust ref win $ \w ->
      w { rwParent = if isNullObject p then Nothing else Just p }
    -- The first of these is the map (river sends none before it).  Keyboard
    -- focus was held back until now, see 'transmitManage'; ask for the
    -- sequence that sends it.
    RiverWindowV1Dimensions width height -> do
      first <- (== Just (0, 0)) . fmap rwDimensions . M.lookup win <$> readIORef ref
      adjust ref win $ \w -> w { rwDimensions = (width, height) }
      when first $ atomically (writeTVar (shDirty (rtShared rt)) True)
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
    -- A client-side title bar being dragged: the same interactive operation
    -- the mod-drag bindings start, from the edges the client named.
    RiverWindowV1PointerMoveRequested _ -> queueAction rt (mouseMoveWindow win)
    RiverWindowV1PointerResizeRequested _ edges -> queueAction rt (mouseResizeWindowEdges edges win)
    _ -> pure ()
  where conn = rtConn rt

addOutput :: Runtime -> ObjectId -> IO ()
addOutput rt out = do
  let ref = shOutputs (rtShared rt)
  mLayer <- forM (shLayerShell (rtShared rt)) $ \shell -> do
    lo <- riverLayerShellV1GetOutput conn shell out
    riverLayerShellOutputV1Listen conn lo $ \case
      RiverLayerShellOutputV1NonExclusiveArea x y width height -> do
        adjust ref out $ \o -> o { roLayerArea = Just (Rectangle x y
          (fromIntegral width) (fromIntegral height)) }
        -- river follows this with a manage_start; asked for anyway.
        atomically (writeTVar (shDirty (rtShared rt)) True)
      _ -> pure ()
    pure lo
  modifyIORef' ref $ M.insert out RiverOutput
    { roObject = out, roPosition = (0, 0), roSize = (0, 0), roRemoved = False
    , roLayerObject = mLayer, roLayerArea = Nothing
    , roWlOutput = Nothing, roName = Nothing, roCaptureSessions = 0 }
  riverOutputV1Listen conn out $ \case
    RiverOutputV1Removed -> adjust ref out $ \o -> o { roRemoved = True }
    RiverOutputV1Position x y -> adjust ref out $ \o -> o { roPosition = (x, y) }
    RiverOutputV1Dimensions width height ->
      adjust ref out $ \o -> o { roSize = (width, height) }
    RiverOutputV1CaptureSessions n -> adjust ref out $ \o -> o { roCaptureSessions = n }
    -- The wl_output behind this output, for its connector name.  Bound by
    -- registry name; the name event needs version 4.
    RiverOutputV1WlOutput name -> do
      globals <- readIORef (rtGlobals rt)
      forM_ (M.lookup name globals) $ \g -> when (globalVersion g >= wlOutputNameSince) $ do
        wl <- bindNamed conn (rtRegistry rt) g wlOutputVersion
        adjust ref out $ \o -> o { roWlOutput = Just wl }
        wlOutputListen conn wl $ \case
          WlOutputName n -> do
            adjust ref out $ \o -> o { roName = Just n }
            outputsChanged (rtInput rt)
          _ -> pure ()
    _ -> pure ()
  where conn = rtConn rt

addSeat :: Runtime -> ObjectId -> IO ()
addSeat rt seat = do
  let ref = shSeats sh
  mLayer <- forM (shLayerShell (rtShared rt)) $ \shell -> do
    ls <- riverLayerShellV1GetSeat conn shell seat
    riverLayerShellSeatV1Listen conn ls $ \ev -> do
      let set f = adjust ref seat $ \s -> s { rsLayerFocus = f }
      case ev of
        RiverLayerShellSeatV1FocusExclusive    -> set LayerFocusExclusive
        RiverLayerShellSeatV1FocusNonExclusive -> set LayerFocusNonExclusive
        -- The keyboard is back from the layer surface with whatever focus
        -- river left it; what was last sent no longer describes it.
        RiverLayerShellSeatV1FocusNone         -> do
          set LayerFocusNone
          modifyIORef' (rtLastFocus rt) (M.delete seat)
        RiverLayerShellSeatV1Unknown{} -> pure ()
    pure ls

  -- The object @ensure_next_key_eaten@ is requested on.  Once per seat (twice
  -- is a protocol error), and only from version 2, where it exists.
  mXkbSeat <- if rtXkbVersion rt < riverXkbBindingsV1GetSeatSince then pure Nothing else do
    xs <- riverXkbBindingsV1GetSeat conn (rtBindingsGlobal rt) seat
    riverXkbBindingsSeatV1Listen conn xs $ \case
      -- A key the open capture did not ask for ends it.  Without this a
      -- submap would stay armed with every binding disabled.  Only the
      -- capture whose bindings are armed: the key may have been eaten for a
      -- capture that has since ended, with another armed in its place.
      RiverXkbBindingsSeatV1AteUnboundKey -> do
        generation <- atomicModifyIORef' (rtEatGenerations rt) $ \pending ->
          (M.delete xs pending, M.lookup xs pending)
        forM_ generation $ \gen -> do
          taken <- claimCapture rt gen
          forM_ taken $ \cap -> do
            writeIORef (rtDisarm rt) True
            queueAction rt (icOnEnd cap)
      -- Only sent for modifiers something asked to watch.
      RiverXkbBindingsSeatV1ModifiersUpdate old new ->
        readIORef (rtModWatcher rt) >>= mapM_ (\f -> do
          finished <- f old new
          when finished (writeIORef (rtModWatcher rt) Nothing))
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
      atomicWriteIORef (shHovered sh) (Just win)
      byLayout <- atomicModifyIORef' (shLayoutMoved sh) (\m -> (False, m))
      when (rtFollowsMouse rt && not byLayout) $ queueAction rt $ do
        -- Runs a sequence later: the pointer may have moved on, a drag owns
        -- the focus, and a window on a hidden workspace cannot really be
        -- entered (a restart shows every window until the successor's first
        -- sequence hides them).
        stillThere <- io ((== Just win) <$> readIORef (shHovered sh))
        drag <- gets dragging
        onScreen <- gets (elem win . concatMap (W.integrate' . W.stack . W.workspace)
                            . screensOf . windowset)
        when (stillThere && isNothing drag && onScreen) (focus win)
    RiverSeatV1PointerLeave -> atomicWriteIORef (shHovered sh) Nothing
    -- A click, or a touch or tablet tool, on a window: X11's ButtonPress on an
    -- unfocused client.  Focus follows it whatever 'focusFollowsMouse' says.
    RiverSeatV1WindowInteraction win -> queueAction rt (focus win)
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
    sh = rtShared rt

-- | Destroy the protocol objects for everything river has closed.
--
-- On the loop, after the worker's 'reapClosed' has seen the same entries and
-- before a plan is transmitted, so that transmitting filters them out.
reapObjects :: Runtime -> IO ()
reapObjects rt = do
  let winRef = shWindows sh
  ws <- readIORef winRef
  forM_ [ w | w <- M.elems ws, rwClosed w ] $ \w -> do
    riverNodeV1Destroy conn (rwNode w)
    riverWindowV1Destroy conn (rwObject w)
    modifyIORef' winRef (M.delete (rwObject w))
    -- river recycles ids: what was last sent for this slot must not
    -- suppress the first proposal, position or stacking of its successor.
    modifyIORef' (rtLastManage rt) (M.delete (rwObject w))
    modifyIORef' (rtLastRender rt) (M.delete (rwObject w))
    modifyIORef' (rtLastStack rt) (filter (/= rwNode w))
    modifyIORef' (rtLastFocus rt) (M.filter (/= FocusWindow (rwObject w)))
    atomicModifyIORef' (shHovered sh) (\h -> (if h == Just (rwObject w) then Nothing else h, ()))

  let outRef = shOutputs sh
  outs <- readIORef outRef
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o -> do
    forM_ (roLayerObject o) (riverLayerShellOutputV1Destroy conn)
    forM_ (roWlOutput o) (wlOutputRelease conn)
    riverOutputV1Destroy conn (roObject o)
    modifyIORef' outRef (M.delete (roObject o))
  unless (null [ () | o <- M.elems outs, roRemoved o ]) $ outputsChanged (rtInput rt)

  let seatRef = shSeats sh
  seats <- readIORef seatRef
  forM_ [ s | s <- M.elems seats, rsRemoved s ] $ \s -> do
    forM_ (rsLayerObject s) (riverLayerShellSeatV1Destroy conn)
    forM_ (rsXkbSeat s) (riverXkbBindingsSeatV1Destroy conn)
    riverSeatV1Destroy conn (rsObject s)
    modifyIORef' seatRef (M.delete (rsObject s))
    modifyIORef' (rtBoundSeats rt) (S.delete (rsObject s))
    modifyIORef' (rtLastFocus rt) (M.delete (rsObject s))
  where
    conn = rtConn rt
    sh = rtShared rt
