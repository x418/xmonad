{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Sending a 'Plan' to the compositor, on the loop.
--
-- 'transmitManage' sends the window management half inside a manage
-- sequence -- bindings for new seats, the one-shot ops, dimensions, focus --
-- and 'transmitRender' the rendering half inside a render sequence.  Both
-- send only what changed since the last transmission, and both check every
-- reference against the objects river still has.
module XMonad.River.WM.Transmit
  ( transmitManage
  , transmitRender
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.IORef
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
import XMonad.River.State (InputCapture(..), RiverState(..), takeOps)
import XMonad.River.Types
import XMonad.River.WM.Runtime
import XMonad.River.Wire (ObjectId)

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
      -- The capture is dropped here; its bindings are destroyed and the
      -- config's re-enabled by the disarm in 'transmitManage', as for every
      -- other way a capture ends.  river sends this press in the batch that
      -- precedes a @manage_start@, so that is this sequence.  Tearing down
      -- here instead would leave the capture's own bindings alive and its
      -- keys eaten from every client for the rest of the session.
      atomicWriteIORef (riverCapture (rtState rt)) Nothing
      writeIORef (rtDisarm rt) True
      hPutStrLn stderr $ "xmonad-river: panic: closed " <> show n
        <> " prompt(s); the config's bindings return with this sequence"
    _ -> pure ()
  riverXkbBindingV1Enable conn b
  where conn = rtConn rt

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
    -- The watch is withdrawn with the capture that asked for it.  Left in
    -- force, river would report every modifier change -- and start a manage
    -- sequence for each -- for the rest of the session, to a handler that
    -- has already been taken.
    watched <- atomicModifyIORef' (rtModWatched rt) (\w -> (False, w))
    when watched $ do
      writeIORef (rtModWatcher rt) Nothing
      forM_ (liveSeats seats) $ \s -> forM_ (rsXkbSeat s) $ \x ->
        riverXkbBindingsSeatV1ModifiersWatch conn x 0
    writeIORef (rtDisarm rt) False

  -- river remembers the layer-surface default; reissued only when it moves.
  forM_ (planLayerDefault plan) $ \(out, mLayerObj) -> do
    prev <- readIORef (rtLayerDefault rt)
    forM_ mLayerObj $ \lo -> unless (prev == Just out) $ do
      riverLayerShellOutputV1SetDefault conn lo
      writeIORef (rtLayerDefault rt) (Just out)

  -- One-shot requests, drained: each is an effect river performs once.
  ops <- takeOps rs
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
      old <- atomicModifyIORef' (rtGrabbed rt) (\bs -> ([], bs))
      forM_ old $ \b -> do
        riverXkbBindingV1Disable conn b
        riverXkbBindingV1Destroy conn b
    OpGrabKeys gen ks -> do
      bs <- fmap concat $ forM (liveSeats seats) $ \seat ->
        forM (zip [0 :: Int ..] ks) $ \(i, (mask, keysym)) -> do
          b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt)
                 (rsObject seat) keysym (riverModifiers mask)
          -- By index, into the table of the generation these were made for:
          -- the actions stay with the config, the loop holds only the
          -- objects.  The previous grab's bindings live until this sequence
          -- destroys them and must not index a table they were not built
          -- against.
          let fire pick = do
                (tableGen, acts) <- readIORef (riverExtraKeys rs)
                when (tableGen == gen) $
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
  -- Sent when the answer changed since last time, or when the client has not
  -- taken the size it was given -- re-proposing is how a tiled window is kept
  -- from resizing itself, as X11's ConfigureRequest refusal did.
  lastM <- readIORef (rtLastManage rt)
  current <- fmap M.fromList $ forM
    [ (win, r, rw) | (win, r) <- planPlacements plan, Just rw <- [M.lookup win known] ] $
    \(win, r, rw) -> do
      let tiled = not (S.member win (planFloating plan))
          want = (rect_width r, rect_height r, tiled)
          taken = rwDimensions rw == (fromIntegral (rect_width r), fromIntegral (rect_height r))
      unless (M.lookup win lastM == Just want && taken) $ do
        riverWindowV1ProposeDimensions conn win
          (fromIntegral (rect_width r)) (fromIntegral (rect_height r))
        riverWindowV1SetTiled conn win (if tiled then allEdges else 0)
      pure (win, want)
  writeIORef (rtLastManage rt) current

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
  writeIORef (rtArmedGen rt) gen

  -- Whoever takes the slot owns the teardown: a key, an unbound key, a
  -- modifier release or the deadline, exactly one of them, and only for
  -- this generation.
  let claim = claimCapture rt gen
      live = liveSeats seats

  temps <- fmap concat $ forM live $ \seat ->
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

  -- A hold-to-cycle ends when the modifier goes up.  @modifiers_watch@ is a
  -- version-3 request; "XMonad.River.whileModifiersHeld" does not arm on an
  -- older server, and this is the check that keeps it a protocol error only
  -- there.
  when (mods /= 0 && rtXkbVersion rt >= 3) $ do
    writeIORef (rtModWatcher rt) $ Just $ \old new ->
      when (old .&. mods /= 0 && new .&. mods /= old .&. mods) $ do
        taken <- claim
        forM_ taken $ \cap -> do
          writeIORef (rtDisarm rt) True
          queueAction rt (icOnEnd cap)
    writeIORef (rtModWatched rt) True
    forM_ live $ \s ->
      forM_ (rsXkbSeat s) $ \x -> riverXkbBindingsSeatV1ModifiersWatch conn x mods

  -- Be told about a key this did not want, so the capture can be abandoned.
  when oneShot $ forM_ live $ \s ->
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

  -- Rendering state persists between frames, so only what changed is sent:
  -- a window that was hidden, or is new, gets everything.
  lastR <- readIORef (rtLastRender rt)
  shown <- fmap M.fromList $ forM
    [ (win, r, w) | (win, r) <- planPlacements plan, Just w <- [M.lookup win known] ] $
    \(win, r, w) -> do
      -- Width 0 is how NoBorders removes a border; river reads it as none.
      let border@(width, (red, green, blue, alpha)) =
            M.findWithDefault (0, (0, 0, 0, 0)) win (planBorders plan)
          entry = (r, border)
      when (rwHidden w) $ do
        riverWindowV1Show conn win
        adjust winRef win $ \x -> x { rwHidden = False }
      unless (M.lookup win lastR == Just entry) $ do
        riverNodeV1SetPosition conn (rwNode w) (rect_x r) (rect_y r)
        riverWindowV1SetBorders conn win allEdges (fromIntegral width)
          red green blue alpha
      pure (win, entry)
  writeIORef (rtLastRender rt) shown

  -- What the layout did not place is on a workspace that is off screen.
  -- river has no workspaces; this is what implements them.
  forM_ (M.elems known) $ \w ->
    unless (S.member (rwObject w) (planVisible plan) || rwHidden w) $ do
      riverWindowV1Hide conn (rwObject w)
      adjust winRef (rwObject w) $ \x -> x { rwHidden = True }

  -- Stacking, bottom to top, restated whenever the order differs from the
  -- last one sent -- a new node's position in the render list is undefined.
  -- The placement list is topmost-first (upstream's convention, and what
  -- 'windowUnderPointer' relies on), hence the reverse; Magnifier is the
  -- layout that can tell.  The window manager's own surfaces (decorations,
  -- overlays), which contrib records, go above the windows.
  overlays <- readIORef (riverOverlays rs)
  positions <- readIORef (riverOverlayPos rs)
  let nodeOf win = rwNode <$> M.lookup win known
      order = concat
        [ [ n | (win, _) <- reverse (planPlacements plan), Just n <- [nodeOf win] ]
        , [ n | win <- planRaised plan, Just n <- [nodeOf win] ]
        , overlays ]
  lastOrder <- readIORef (rtLastStack rt)
  lastPos <- readIORef (rtLastOverlayPos rt)
  forM_ overlays $ \n -> forM_ (M.lookup n positions) $ \p ->
    unless (M.lookup n lastPos == Just p) $ uncurry (riverNodeV1SetPosition conn n) p
  unless (order == lastOrder) $ mapM_ (riverNodeV1PlaceTop conn) order
  writeIORef (rtLastStack rt) order
  writeIORef (rtLastOverlayPos rt) (M.restrictKeys positions (S.fromList overlays))
  where
    conn = rtConn rt
    rs = rtState rt
