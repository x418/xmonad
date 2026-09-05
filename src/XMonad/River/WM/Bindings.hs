{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Key and pointer bindings on river's seats, on the loop.
--
-- The config's bindings are created per seat and live for the session; a
-- capture (a submap, a hold-to-cycle) installs its own and disables the
-- config's until it ends; a grab ('XMonad.River.grabKeys') installs a
-- standing set beside the config's.  All three are @river_xkb_binding_v1@
-- objects, requested inside a manage sequence where @enable@ is legal.
module XMonad.River.WM.Bindings
  ( bindSeat
  , bindGrabbedSeat
  , reconcileAddedSeat
  , armCapture
  , isCapture
  , captureDeadlineMicros
  ) where

import Control.Concurrent.STM (registerDelay)
import Control.Monad (forM, forM_, unless, when)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO (hPutStrLn, stderr)

import XMonad.Core
import XMonad.River.Client (closeAllClients)
import XMonad.River.Plan
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.State (InputCapture(..), Shared(..))
import XMonad.River.Types
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
  when (rtXkbVersion rt >= riverXkbBindingsSeatV1CancelEnsureNextKeyEatenSince) $
    forM_ (liveSeats seats) $ \seat ->
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
  when (mods /= 0 && rtXkbVersion rt >= riverXkbBindingsSeatV1ModifiersWatchSince) $ do
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

  -- A deadline, or an abandoned capture is a session with no bindings.  A
  -- 'registerDelay' flag the loop waits on ('expireCapture'), not a thread
  -- per capture sleeping for a minute: a dozen submaps used to be a dozen
  -- threads, each waking the loop at its expiry.
  expired <- registerDelay captureDeadlineMicros
  writeIORef (rtCaptureDeadline rt) (Just (gen, expired))
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
      when (icMods cap /= 0 && rtXkbVersion rt >= riverXkbBindingsSeatV1ModifiersWatchSince) $ do
        riverXkbBindingsSeatV1ModifiersWatch conn x (icMods cap)
        writeIORef (rtModWatched rt) True
      when (icOneShot cap && rtXkbVersion rt >= riverXkbBindingsSeatV1EnsureNextKeyEatenSince) $ do
        riverXkbBindingsSeatV1EnsureNextKeyEaten conn x
        modifyIORef' (rtEatGenerations rt) (M.insert x (icGeneration cap))
  where
    conn = rtConn rt
    sh = rtShared rt

captureDeadlineMicros :: Int
captureDeadlineMicros = 60 * 1000 * 1000

