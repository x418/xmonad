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
import XMonad.River.WM.Input (installInputConfig, setKeyboardLayout, setKeymap)
import XMonad.River.WM.Runtime
import XMonad.River.Wire (ObjectId)

-- | Create river bindings for the config's keys and buttons on one seat.
-- Inside a manage sequence, where @enable@ and the layout override are legal.
bindSeat :: Runtime -> ObjectId -> IO ()
bindSeat rt seat = do
  bindPanic rt seat
  forM_ (M.toList (rtKeyActions rt)) $ \((mask, keysym), action) -> do
    b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt) seat keysym
           (riverModifiers mask)
    riverXkbBindingV1SetLayoutOverride conn b firstLayout
    modifyIORef' (rtBindings rt) (M.insert b action)
    modifyIORef' (rtBindingKeys rt) (M.insert b (mask, keysym))
    riverXkbBindingV1Listen conn b $ \case
      RiverXkbBindingV1Pressed -> do
        acts <- readIORef (rtBindings rt)
        forM_ (M.lookup b acts) (queueAction rt)
        prefixes <- readIORef (rtPrefixKeys rt)
        when (S.member (mask, keysym) prefixes) $ writeIORef (rtPrefixPressed rt) True
      -- A binding's action runs once per press; nothing repeats it, so there
      -- is nothing for @stop_repeat@ to stop.
      RiverXkbBindingV1Released -> pure ()
      RiverXkbBindingV1StopRepeat -> pure ()
      RiverXkbBindingV1Unknown{} -> pure ()
    riverXkbBindingV1Enable conn b
  forM_ (M.toList (rtButtonActions rt)) $ \((mask, button), action) -> do
    b <- riverSeatV1GetPointerBinding conn seat (linuxButton button)
           (riverModifiers mask)
    modifyIORef' (rtPointerBind rt) (M.insert b action)
    riverPointerBindingV1Listen conn b $ \case
      RiverPointerBindingV1Pressed -> do
        acts <- readIORef (rtPointerBind rt)
        mHover <- readIORef (shHovered (rtShared rt))
        forM_ ((,) <$> M.lookup b acts <*> mHover) $ \(a, win) ->
          queueAction rt (a win)
      RiverPointerBindingV1Released -> pure ()
      RiverPointerBindingV1Unknown{} -> pure ()
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
  riverXkbBindingV1SetLayoutOverride conn b firstLayout
  riverXkbBindingV1Listen conn b $ \case
    RiverXkbBindingV1Pressed -> do
      n <- closeAllClients
      -- The capture is dropped here; its bindings are destroyed and the
      -- config's re-enabled by the disarm in 'transmitManage', as for every
      -- other way a capture ends.  river sends this press in the batch that
      -- precedes a @manage_start@, so that is this sequence.  Tearing down
      -- here instead would leave the capture's own bindings alive and its
      -- keys eaten from every client for the rest of the session.  Claimed
      -- by generation like every other end: a capture the config armed
      -- since the bindings were installed is not this one's to drop.
      gen <- readIORef (rtArmedGen rt)
      taken <- claimCapture rt gen
      forM_ taken $ \cap -> queueAction rt (icOnEnd cap)
      writeIORef (rtDisarm rt) True
      hPutStrLn stderr $ "xmonad-river: panic: closed " <> show n
        <> " prompt(s); the config's bindings return with this sequence"
    RiverXkbBindingV1Released -> pure ()
    RiverXkbBindingV1StopRepeat -> pure ()
    RiverXkbBindingV1Unknown{} -> pure ()
  riverXkbBindingV1Enable conn b
  where conn = rtConn rt

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
    when (rtXkbVersion rt >= 2) $
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
    forM_ (M.elems seats) $ \s ->
      unless (layerHasFocus (rsLayerFocus s)) $
        case planFocus plan of
          FocusWindow win | Just rw <- M.lookup win known ->
            when (rwDimensions rw /= (0, 0)) $
              riverSeatV1FocusWindow conn (rsObject s) win
          _ -> riverSeatV1ClearFocus conn (rsObject s)
  where
    conn = rtConn rt
    sh = rtShared rt

-- | Take the keyboard for a submap or a hold-to-cycle, disabling the config's
-- own bindings meanwhile (river leaves it to policy which of two bindings for
-- one key fires).  Inside the sequence that carries the key press which asked
-- for it, so arming is atomic with the press.
armCapture
  :: Runtime -> M.Map ObjectId RiverSeat
  -> [(KeyMask, KeySym)] -> KeyMask -> Bool -> Int -> IO ()
armCapture rt seats ks mods oneShot gen = do
  globals <- readIORef (rtBindings rt)
  -- More than one capture can be requested by actions drained in one manage
  -- sequence. Supersede the previous protocol state before installing the
  -- newer generation, otherwise its bindings become unreachable.
  previous <- atomicModifyIORef' (rtArmed rt) (\bindings -> ([], bindings))
  forM_ previous $ \binding -> do
    riverXkbBindingV1Disable conn binding
    riverXkbBindingV1Destroy conn binding
  watched <- atomicModifyIORef' (rtModWatched rt) (\active -> (False, active))
  when watched $ forM_ (liveSeats seats) $ \seat ->
    forM_ (rsXkbSeat seat) $ \x ->
      riverXkbBindingsSeatV1ModifiersWatch conn x 0
  when (rtXkbVersion rt >= 2) $ forM_ (liveSeats seats) $ \seat ->
    forM_ (rsXkbSeat seat) (riverXkbBindingsSeatV1CancelEnsureNextKeyEaten conn)
  writeIORef (rtEatGenerations rt) M.empty
  writeIORef (rtModWatcher rt) Nothing
  forM_ (M.keys globals) (riverXkbBindingV1Disable conn)
  writeIORef (rtArmedGen rt) gen

  -- Whoever takes the slot owns the teardown: a key, an unbound key, a
  -- modifier release or the deadline, exactly one of them, and only for
  -- this generation.
  let claim = claimCapture rt gen
      live = liveSeats seats

  temps <- fmap concat $ forM live $ \seat ->
    bindCaptureSeat rt (rsObject seat) ks oneShot gen
  writeIORef (rtArmed rt) temps

  -- A hold-to-cycle ends when the modifier goes up.  @modifiers_watch@ is a
  -- version-3 request; "XMonad.River.whileModifiersHeld" does not arm on an
  -- older server, and this is the check that keeps it a protocol error only
  -- there.
  when (mods /= 0 && rtXkbVersion rt >= 3) $ do
    writeIORef (rtModWatcher rt) $ Just $ \old new ->
      if old .&. mods /= 0 && new .&. mods /= old .&. mods
      then do
        taken <- claim
        forM_ taken $ \cap -> do
          writeIORef (rtDisarm rt) True
          queueAction rt (icOnEnd cap)
        pure True
      else pure False
    writeIORef (rtModWatched rt) True
    forM_ live $ \s ->
      forM_ (rsXkbSeat s) $ \x -> riverXkbBindingsSeatV1ModifiersWatch conn x mods

  -- Be told about a key this did not want, so the capture can be abandoned.
  when oneShot $ forM_ live $ \s ->
    forM_ (rsXkbSeat s) $ \x -> do
      riverXkbBindingsSeatV1EnsureNextKeyEaten conn x
      modifyIORef' (rtEatGenerations rt) (M.insert x gen)

  -- A deadline, or an abandoned capture is a session with no bindings.  The
  -- work is posted to the loop; this thread touches no connection.
  void $ forkIO $ do
    threadDelay captureDeadlineMicros
    MB.post (shLoopJobs (rtShared rt)) $ do
      taken <- claim
      forM_ taken $ \cap -> do
        hPutStrLn stderr
          "xmonad-river: keyboard capture abandoned after 60s; restoring bindings"
        writeIORef (rtDisarm rt) True
        queueAction rt (icOnEnd cap)
  where
    conn = rtConn rt

isCapture :: Op -> Bool
isCapture OpCaptureInput{} = True
isCapture _ = False

-- | Every binding matches keysyms in the keymap's first layout, whichever is
-- active: X11 grabbed keycodes resolved in the first group, and a binding on
-- @z@ stays on that key under a layout that swaps it.
firstLayout :: Word32
firstLayout = 0

bindGrabbedSeat :: Runtime -> ObjectId -> Int -> [(KeyMask, KeySym)] -> IO [ObjectId]
bindGrabbedSeat rt seat gen ks =
  forM (zip [0 :: Int ..] ks) $ \(i, (mask, keysym)) -> do
    b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt)
           seat keysym (riverModifiers mask)
    riverXkbBindingV1SetLayoutOverride conn b firstLayout
    let fire pick = do
          (tableGen, acts) <- readIORef (shExtraKeys sh)
          when (tableGen == gen) $
            forM_ (take 1 (drop i acts)) (queueAction rt . pick)
    -- @stop_repeat@ -- another key went down while this one is held -- is
    -- not a release and is not surfaced: the config's pair is press and
    -- release, and a hold-to-cycle ends on the release alone.
    riverXkbBindingV1Listen conn b $ \case
      RiverXkbBindingV1Pressed  -> fire fst
      RiverXkbBindingV1Released -> fire snd
      RiverXkbBindingV1StopRepeat -> pure ()
      RiverXkbBindingV1Unknown{} -> pure ()
    riverXkbBindingV1Enable conn b
    pure b
  where
    conn = rtConn rt
    sh = rtShared rt

bindCaptureSeat
  :: Runtime -> ObjectId -> [(KeyMask, KeySym)] -> Bool -> Int -> IO [ObjectId]
bindCaptureSeat rt seat ks oneShot gen =
  forM (zip [0 :: Int ..] ks) $ \(i, (mask, keysym)) -> do
    b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt)
           seat keysym (riverModifiers mask)
    riverXkbBindingV1SetLayoutOverride conn b firstLayout
    riverXkbBindingV1Listen conn b $ \case
      RiverXkbBindingV1Pressed
        | oneShot -> claim >>= \taken -> forM_ taken $ \cap -> do
            writeIORef (rtDisarm rt) True
            queueAction rt (icOnKey cap True i)
        | otherwise -> do
            held <- readIORef (shCapture sh)
            forM_ held $ \cap -> queueAction rt (icOnKey cap True i)
      RiverXkbBindingV1Released | not oneShot -> do
        held <- readIORef (shCapture sh)
        forM_ held $ \cap -> queueAction rt (icOnKey cap False i)
      RiverXkbBindingV1Released -> pure ()
      RiverXkbBindingV1StopRepeat -> pure ()
      RiverXkbBindingV1Unknown{} -> pure ()
    riverXkbBindingV1Enable conn b
    pure b
  where
    conn = rtConn rt
    sh = rtShared rt
    claim = claimCapture rt gen

reconcileAddedSeat :: Runtime -> RiverSeat -> IO ()
reconcileAddedSeat rt seat = do
  readIORef (rtGrabbedKeys rt) >>= mapM_ (\(gen, ks) ->
    bindGrabbedSeat rt (rsObject seat) gen ks >>= modifyIORef' (rtGrabbed rt) . (++))
  active <- readIORef (shCapture sh)
  forM_ active $ \cap -> do
    globals <- readIORef (rtBindings rt)
    forM_ (M.keys globals) (riverXkbBindingV1Disable conn)
    bindings <- bindCaptureSeat rt (rsObject seat) (icKeys cap)
                  (icOneShot cap) (icGeneration cap)
    modifyIORef' (rtArmed rt) (++ bindings)
    writeIORef (rtArmedGen rt) (icGeneration cap)
    forM_ (rsXkbSeat seat) $ \x -> do
      when (icMods cap /= 0 && rtXkbVersion rt >= 3) $ do
        riverXkbBindingsSeatV1ModifiersWatch conn x (icMods cap)
        writeIORef (rtModWatched rt) True
      when (icOneShot cap && rtXkbVersion rt >= 2) $ do
        riverXkbBindingsSeatV1EnsureNextKeyEaten conn x
        modifyIORef' (rtEatGenerations rt) (M.insert x (icGeneration cap))
  where
    conn = rtConn rt
    sh = rtShared rt

captureDeadlineMicros :: Int
captureDeadlineMicros = 60 * 1000 * 1000

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
