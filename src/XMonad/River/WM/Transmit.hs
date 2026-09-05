{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Sending a 'Plan' to the compositor, on the loop.
--
-- 'transmitManage' sends the window management half inside a manage
-- sequence -- bindings for new seats, the one-shot ops, dimensions, focus --
-- and 'transmitRender' the rendering half inside a render sequence.  Both
-- send only what changed since the last transmission, and both check every
-- reference against the objects river still has.  The bindings themselves
-- -- the config's, a capture's, a grab's -- are "XMonad.River.WM.Bindings".
module XMonad.River.WM.Transmit
  ( transmitManage
  , transmitRender
  ) where

import Control.Concurrent.STM (readTVarIO, registerDelay)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO (hPutStrLn, stderr)

import XMonad.Core
import XMonad.River.Client (closeAllClients)
import XMonad.River.Connection
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Plan
import XMonad.River.Protocol.LayerShell
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.State (InputCapture(..), Shared(..))
import XMonad.River.Types
import XMonad.River.WM.Bindings (armCapture, bindGrabbedSeat, bindSeat, isCapture, reconcileAddedSeat)
import XMonad.River.WM.Input (installInputConfig, setKeyboardLayout, setKeymap)
import XMonad.River.WM.Runtime
import XMonad.River.Wire (ObjectId)

-- | Send the window management half of a plan.
--
-- Every reference is checked against the windows river currently has: the
-- plan may be older than the maps, and naming a destroyed window is a
-- protocol error that disconnects the window manager.
transmitManage :: Runtime -> Plan -> IO ()
transmitManage rt plan = do
  known <- readIORef (shWindows sh)
  seats <- readIORef (shSeats sh)

  -- Seats that appeared since the last sequence get the config's bindings.
  bound <- readIORef (rtBoundSeats rt)
  forM_ [ rsx | (s, rsx) <- M.toList seats, not (rsRemoved rsx), not (S.member s bound) ] $ \seat -> do
    bindSeat rt (rsObject seat)
    modifyIORef' (rtBoundSeats rt) (S.insert (rsObject seat))
    reconcileAddedSeat rt seat

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
    -- The watch is withdrawn with the capture that asked for it.  Left in
    -- force, river would report every modifier change -- and start a manage
    -- sequence for each -- for the rest of the session, to a handler that
    -- has already been taken.
    watched <- atomicModifyIORef' (rtModWatched rt) (\w -> (False, w))
    when watched $ do
      writeIORef (rtModWatcher rt) Nothing
      forM_ (liveSeats seats) $ \s -> forM_ (rsXkbSeat s) $ \x ->
        riverXkbBindingsSeatV1ModifiersWatch conn x 0
    when (rtXkbVersion rt >= riverXkbBindingsSeatV1CancelEnsureNextKeyEatenSince) $
      forM_ (liveSeats seats) $ \s -> forM_ (rsXkbSeat s) $
        riverXkbBindingsSeatV1CancelEnsureNextKeyEaten conn
    writeIORef (rtEatGenerations rt) M.empty
    writeIORef (rtDisarm rt) False

  -- river remembers the layer-surface default; reissued only when it moves.
  forM_ (planLayerDefault plan) $ \(out, mLayerObj) -> do
    prev <- readIORef (rtLayerDefault rt)
    forM_ mLayerObj $ \lo -> unless (prev == Just out) $ do
      riverLayerShellOutputV1SetDefault conn lo
      writeIORef (rtLayerDefault rt) (Just out)

  -- One-shot requests, drained: each is an effect river performs once.
  ops <- atomicModifyIORef' (shOps sh) (\os -> ([], reverse os))
  forM_ ops $ \case
    OpClose w -> when (M.member w known) $ riverWindowV1Close conn w
    OpWarpPointer s x y -> when (M.member s seats) $ riverSeatV1PointerWarp conn s x y
    OpPointerOpStart s -> when (M.member s seats) $ riverSeatV1OpStartPointer conn s
    OpPointerOpEnd s -> when (M.member s seats) $ riverSeatV1OpEnd conn s
    OpUseDecorations w ssd -> when (M.member w known) $
      (if ssd then riverWindowV1UseSsd else riverWindowV1UseCsd) conn w
    OpSetCapabilities w caps -> when (M.member w known) $
      riverWindowV1SetCapabilities conn w caps
    OpInformFullscreen w full -> when (M.member w known) $
      (if full then riverWindowV1InformFullscreen else riverWindowV1InformNotFullscreen) conn w
    OpInformResize w start -> when (M.member w known) $
      (if start then riverWindowV1InformResizeStart else riverWindowV1InformResizeEnd) conn w
    OpSetPosition w x y -> forM_ (M.lookup w known) $ \rw ->
      riverNodeV1SetPosition conn (rwNode rw) x y
    OpProposeDimensions w dw dh -> when (M.member w known) $
      riverWindowV1ProposeDimensions conn w (fromIntegral dw) (fromIntegral dh)
    OpCaptureInput ks mods oneShot gen -> armCapture rt seats ks mods oneShot gen
    OpUngrabKeys -> do
      writeIORef (rtGrabbedKeys rt) Nothing
      old <- atomicModifyIORef' (rtGrabbed rt) (\bs -> ([], bs))
      forM_ old $ \b -> do
        riverXkbBindingV1Disable conn b
        riverXkbBindingV1Destroy conn b
    OpGrabKeys gen ks -> do
      writeIORef (rtGrabbedKeys rt) (Just (gen, ks))
      bs <- fmap concat $ forM (liveSeats seats) $ \seat ->
        bindGrabbedSeat rt (rsObject seat) gen ks
      writeIORef (rtGrabbed rt) bs
    -- Legal anywhere; queued here only if something used emitOp for it.
    OpInstallInputConfig cfg -> installInputConfig (rtInput rt) cfg
    OpKeyboardLayout req -> setKeyboardLayout (rtInput rt) req
    OpSetKeymap text -> setKeymap (rtInput rt) text
    OpDeclareSubmapPrefixes ks -> writeIORef (rtPrefixKeys rt) (S.fromList ks)
    -- Sent by the loop's own pass; see 'sendNow'.
    OpExitSession -> pure ()
    OpStop -> pure ()
    OpSetXcursorTheme{} -> pure ()

  -- Dimensions are window management state.  A window not told it is tiled
  -- draws itself as floating: its own decorations, shadows outside its size.
  -- Proposed when the answer changed since last time, and again when the
  -- client's size has moved since the last proposal to something other than
  -- what was asked -- re-proposing is how a tiled window that resizes itself
  -- is put back, as X11's ConfigureRequest refusal did.  A client that
  -- settles on a size of its own, a terminal rounding to its cell, is left
  -- there rather than re-proposed every sequence forever, which would cost it
  -- a configure per sequence and river a render sequence for each.
  --
  -- A float is only ever proposed a change of the ask, never put back: it
  -- owns its size, as X11's granted ConfigureRequest let it.  A configure
  -- the client did not ask for, while its own resize is pending, is one JBR
  -- ignores without committing, and river then waits out its transaction
  -- timeout showing stale buffers for the whole output.  One proposed 0x0
  -- ('planUnsized') decides for itself; the worker settles it at what it
  -- decided.
  -- The plan that ran a held prefix's action releases the hold: a capture
  -- among its ops keeps the bindings disabled on its own terms, and none
  -- means the action opened no submap.
  held <- readIORef (rtPrefixHeld rt)
  lastSent <- readIORef (rtSent rt)
  when (held && planSerial plan > lastSent) $ do
    writeIORef (rtPrefixHeld rt) False
    unless (any isCapture ops) $
      readIORef (rtBindings rt) >>= mapM_ (riverXkbBindingV1Enable conn) . M.keys

  -- No plan yet -- a sequence before the usable areas were known -- places
  -- nothing: river keeps what the windows have.
  when (planSerial plan > 0) $ do
    lastM <- readIORef (rtLastManage rt)
    current <- fmap M.fromList $ forM
      [ (win, pl, rw) | win <- planOrder plan, Just pl <- [M.lookup win (planPlaced plan)]
                      , Just rw <- [M.lookup win known] ] $
      \(win, pl, rw) -> do
        let tiled = not (plFloating pl)
            r = plRect pl
            (pw, ph) | plUnsized pl = (0, 0)
                     | otherwise = (rect_width r, rect_height r)
            want = (pw, ph, tiled)
            asked = (fromIntegral pw, fromIntegral ph)
            dims = rwDimensions rw
            resend = case M.lookup win lastM of
              Just sent ->
                msWant sent /= want || (tiled && dims /= asked && dims /= msSeen sent)
              Nothing -> True
        when resend $ do
          riverWindowV1ProposeDimensions conn win (fromIntegral pw) (fromIntegral ph)
          riverWindowV1SetTiled conn win (if tiled then allEdges else 0)
        pure (win, ManageSent want dims)
    atomicWriteIORef (rtLastManage rt) $! current

    -- Keyboard focus.  A seat whose keyboard has gone to a layer surface is left
    -- alone: river discards the request under an exclusive grab and, under a
    -- non-exclusive one, would silently steal the keyboard back.
    --
    -- A window without dimensions has not mapped.  river would give it the
    -- keyboard at once, and a client that gets @wl_keyboard.enter@ before its
    -- first buffer may have nothing to attach it to: JBR drops it, and IDEA
    -- then dispatches no shortcut until focus leaves and returns.  Every other
    -- compositor focuses on map, so that is the only order clients have met.
    -- The keyboard stays where it is; the first dimensions event asks for the
    -- sequence that sends this ('XMonad.River.WM.Events.addWindow').
    --
    -- Sent when it differs from what this seat was last told: river keeps
    -- the focus between sequences, and restating it every sequence for
    -- every seat was the one request that never diffed.
    lastFocus <- readIORef (rtLastFocus rt)
    forM_ (liveSeats seats) $ \s ->
      unless (layerHasFocus (rsLayerFocus s)) $ do
        let target = case planFocus plan of
              FocusWindow win | Just rw <- M.lookup win known, rwDimensions rw /= (0, 0) ->
                FocusWindow win
              _ -> ClearFocus
        unless (M.lookup (rsObject s) lastFocus == Just target) $ do
          case target of
            FocusWindow win -> riverSeatV1FocusWindow conn (rsObject s) win
            ClearFocus -> riverSeatV1ClearFocus conn (rsObject s)
          modifyIORef' (rtLastFocus rt) (M.insert (rsObject s) target)
  where
    conn = rtConn rt
    sh = rtShared rt

-- | Send the rendering half of a plan: positions, borders, visibility and
-- stacking, then the window manager's own surfaces above the windows.
transmitRender :: Runtime -> IO ()
transmitRender rt = do
  plan <- readTVarIO (shPlan sh)
  let winRef = shWindows sh
  known <- readIORef winRef
  overlays <- readIORef (shOverlays sh)
  positions <- readIORef (shOverlayPos sh)
  windowsGen <- readIORef (rtWindowsGen rt)

  -- river starts a render sequence of its own whenever a client changes its
  -- size, with nothing new for this side to say.  Given the same plan, the
  -- same windows and the same overlays as last time, there is nothing to
  -- diff and nothing is sent.
  let given = RenderInput (planSerial plan) windowsGen overlays
                (M.restrictKeys positions (S.fromList overlays))
  lastGiven <- readIORef (rtLastRendered rt)
  when (planSerial plan > 0) $ unless (given == lastGiven) $ do
    atomicWriteIORef (rtLastRendered rt) $! given
    renderPlan rt plan known overlays positions
  where
    sh = rtShared rt

-- | The rendering half of a plan, against the windows river has.
renderPlan :: Runtime -> Plan -> M.Map ObjectId RiverWindow -> [ObjectId]
           -> M.Map ObjectId (Position, Position) -> IO ()
renderPlan rt plan known overlays positions = do
  -- Rendering state persists between frames, so only what changed is sent:
  -- a window that was hidden, or is new, gets everything.
  lastR <- readIORef (rtLastRender rt)
  shown <- fmap M.fromList $ forM
    [ (win, pl, w) | win <- planOrder plan, Just pl <- [M.lookup win (planPlaced plan)]
                   , Just w <- [M.lookup win known] ] $
    \(win, pl, w) -> do
      let r = plRect pl
          Border width (red, green, blue, alpha) = plBorder pl
          entry = RenderSent r (plBorder pl)
      when (rwHidden w) $ do
        riverWindowV1Show conn win
        adjust winRef win $ \x -> x { rwHidden = False }
      unless (M.lookup win lastR == Just entry) $ do
        riverNodeV1SetPosition conn (rwNode w) (rect_x r) (rect_y r)
        riverWindowV1SetBorders conn win allEdges (fromIntegral width)
          red green blue alpha
      pure (win, entry)
  atomicWriteIORef (rtLastRender rt) $! shown

  -- What the layout did not place is on a workspace that is off screen.
  -- river has no workspaces; this is what implements them.
  let toHide = [ w | w <- M.elems known
                   , not (planVisible plan (rwObject w)), not (rwHidden w) ]
  forM_ toHide $ \w -> riverWindowV1Hide conn (rwObject w)
  unless (null toHide) $ modifyIORef' winRef $ \m ->
    foldr (\w -> M.adjust (\x -> x { rwHidden = True }) (rwObject w)) m toHide

  -- Stacking, bottom to top, restated whenever the order differs from the
  -- last one sent -- a new node's position in the render list is undefined.
  -- The placement list is topmost-first (upstream's convention, and what
  -- 'windowUnderPointer' relies on), hence the reverse; Magnifier is the
  -- layout that can tell.  The window manager's own surfaces (decorations,
  -- overlays), which contrib records, go above the windows.
  let nodeOf win = rwNode <$> M.lookup win known
      order = concat
        [ [ n | win <- reverse (planOrder plan), Just n <- [nodeOf win] ]
        , [ n | win <- planRaised plan, Just n <- [nodeOf win] ]
        , overlays ]
  lastOrder <- readIORef (rtLastStack rt)
  lastPos <- readIORef (rtLastOverlayPos rt)
  forM_ overlays $ \n -> forM_ (M.lookup n positions) $ \p ->
    unless (M.lookup n lastPos == Just p) $ uncurry (riverNodeV1SetPosition conn n) p
  unless (order == lastOrder) $ mapM_ (riverNodeV1PlaceTop conn) order
  length order `seq` atomicWriteIORef (rtLastStack rt) order
  atomicWriteIORef (rtLastOverlayPos rt) $! M.restrictKeys positions (S.fromList overlays)
  where
    conn = rtConn rt
    winRef = shWindows (rtShared rt)
