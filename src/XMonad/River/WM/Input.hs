{-# LANGUAGE LambdaCase #-}
-- | The loop's half of input-device configuration: the objects river
-- announces, what each advertised, and reconciling that with the installed
-- rules; and the xkb keyboards, for switching layouts.  Loop thread only.
-- See @LIBINPUT.md@.
module XMonad.River.WM.Input
  ( InputRuntime
  , bindInput
  , newInputRuntime
  , installInputConfig
  , setKeyboardLayout
  , setKeymap
  , outputsChanged
  , inputDeviceCount
  , inputPendingCount
  ) where

import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Maybe (isNothing)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO (hPutStrLn, stderr)

import XMonad.River.Buffer (sealedMemfd)
import XMonad.River.Connection
import XMonad.River.Input
import XMonad.River.Plan (KeyboardLayoutRequest(..))
import XMonad.River.Protocol.InputManagement
import XMonad.River.Protocol.LibinputConfig
import XMonad.River.Protocol.XkbConfig
import XMonad.River.Wire (ObjectId, bytesDouble, bytesFloats, decodeUtf8, doubleBytes, floatsBytes, nullObject)
import qualified Data.ByteString as BS

-- | One @river_input_device_v1@ and, once linked, its libinput object.
data Device = Device
  { dvType         :: !(Maybe InputType)
  , dvName         :: !ByteString
  , dvSynced       :: !Bool
    -- ^ The sync after @done@ answered; still unlinked means libinput-less.
  , dvLib          :: !(Maybe ObjectId)
  , dvSnapshot     :: !Snapshot
  , dvSnapshotDone :: !Bool
    -- ^ The libinput object's first @done@ has arrived.
  , dvInflight     :: !(S.Set Field)
    -- ^ Fields with a result outstanding.
  , dvGeneration   :: !Int
    -- ^ The config generation last reconciled against.
  , dvRefused      :: !(S.Set Field)
    -- ^ Refused at 'dvRefusedGen': diagnosed once, not sent again.
  , dvRefusedGen   :: !Int
  }

newDevice :: Device
newDevice = Device Nothing BC.empty False Nothing emptySnapshot False S.empty 0 S.empty 0

-- | A setter whose result has not arrived.
data Pending = Pending
  { pdDevice     :: !ObjectId
  , pdField      :: !Field
  , pdValue      :: !Value
  , pdGeneration :: !Int
  }

-- | One @river_xkb_keyboard_v1@.
data XkbKeyboard = XkbKeyboard
  { kbIndex   :: !Word32
  , kbName    :: !(Maybe ByteString)
  , kbDone    :: !Bool
  , kbWrapAt  :: !(Maybe Word32)
    -- ^ A next-layout request sent from this index; if the sync finds it
    -- unchanged, the index was out of range and layout 0 is set instead.
  }

data InputRuntime = InputRuntime
  { irConn       :: !Connection
  , irManager    :: !(Maybe ObjectId)
  , irLibinput   :: !(Maybe ObjectId)
    -- ^ @river_libinput_config_v1@ at version 2, with the input manager;
    -- 'Nothing' and devices are left alone.
  , irXkb        :: !(Maybe ObjectId)
    -- ^ @river_xkb_config_v1@ at version 2; 'Nothing' and layouts cannot
    -- be switched.
  , irOnLayout   :: !(Maybe (Int, String) -> IO ())
    -- ^ Told the active layout whenever a keyboard reports one.
  , irOutput     :: !(ByteString -> IO (Maybe ObjectId))
    -- ^ The @wl_output@ bound for a connector name, if river has it.
  , irKeyboards  :: !(IORef (M.Map ObjectId XkbKeyboard))
  , irSeats      :: !(IORef (S.Set ByteString))
    -- ^ Seats this client created, destroyed once no rule names them.
  , irKeymap     :: !(IORef (Maybe ObjectId))
    -- ^ The @river_xkb_keymap_v1@ in force, set on every keyboard to come.
  , irDevices    :: !(IORef (M.Map ObjectId Device))
  , irLinks      :: !(IORef (M.Map ObjectId ObjectId))
    -- ^ libinput object to input device object.
  , irPending    :: !(IORef (M.Map ObjectId Pending))
    -- ^ By result object.
  , irConfig     :: !(IORef (Maybe (Int, InputConfig)))
  , irGeneration :: !(IORef Int)
  , irWarn       :: !(String -> IO ())
  , irWarnedOff  :: !(IORef Bool)
  }

-- | Bind the globals, the input manager first: a libinput device or xkb
-- keyboard names its input device once, at creation, only if that object
-- already exists.  Each of the other two is optional on its own.
bindInput
  :: Connection -> ObjectId -> [Global]
  -> (Maybe (Int, String) -> IO ())  -- ^ told the active layout
  -> (ByteString -> IO (Maybe ObjectId))  -- ^ a connector's @wl_output@
  -> IO InputRuntime
bindInput conn registry globals onLayout output = do
  mManager <- bindGlobal conn registry globals
                riverInputManagerV1Interface 2 riverInputManagerV1Version
  case mManager of
    Nothing -> do
      warn "river_input_manager_v1 is not offered at version 2; input devices \
           \are left as they are and layouts cannot be switched"
      newInputRuntime conn warn onLayout output Nothing Nothing Nothing
    Just (im, _) -> do
      mConfig <- bindGlobal conn registry globals
                   riverLibinputConfigV1Interface 2 riverLibinputConfigV1Version
      mXkb <- bindGlobal conn registry globals
                riverXkbConfigV1Interface 2 riverXkbConfigV1Version
      when (isNothing mConfig) $
        warn "river_libinput_config_v1 is not offered at version 2; input \
             \devices are left as they are"
      when (isNothing mXkb) $
        warn "river_xkb_config_v1 is not offered at version 2; layouts cannot \
             \be switched"
      newInputRuntime conn warn onLayout output (Just im) (fst <$> mConfig) (fst <$> mXkb)
  where
    warn = hPutStrLn stderr . ("xmonad-river: input: " ++)

-- | The runtime over bound globals; separate from 'bindInput' for the tests.
newInputRuntime
  :: Connection
  -> (String -> IO ())              -- ^ where diagnostics go
  -> (Maybe (Int, String) -> IO ())  -- ^ told the active layout
  -> (ByteString -> IO (Maybe ObjectId))  -- ^ a connector's @wl_output@
  -> Maybe ObjectId                 -- ^ input manager
  -> Maybe ObjectId                 -- ^ libinput config
  -> Maybe ObjectId                 -- ^ xkb config
  -> IO InputRuntime
newInputRuntime conn warn onLayout output mManager mConfig mXkb = do
  rt <- InputRuntime conn mManager mConfig mXkb onLayout output
          <$> newIORef M.empty <*> newIORef S.empty <*> newIORef Nothing
          <*> newIORef M.empty <*> newIORef M.empty <*> newIORef M.empty
          <*> newIORef Nothing <*> newIORef 0 <*> pure warn <*> newIORef False
  forM_ mManager $ \im ->
    riverInputManagerV1Listen conn im $ \case
      RiverInputManagerV1InputDevice dev -> addDevice rt dev
      _ -> pure ()
  forM_ mConfig $ \lc ->
    riverLibinputConfigV1Listen conn lc $ \case
      RiverLibinputConfigV1LibinputDevice lib -> addLibinputDevice rt lib
      _ -> pure ()
  forM_ mXkb $ \xk ->
    riverXkbConfigV1Listen conn xk $ \case
      RiverXkbConfigV1XkbKeyboard kb -> addKeyboard rt kb
      _ -> pure ()
  pure rt

inputDeviceCount :: InputRuntime -> IO Int
inputDeviceCount rt = M.size <$> readIORef (irDevices rt)

inputPendingCount :: InputRuntime -> IO Int
inputPendingCount rt = M.size <$> readIORef (irPending rt)

-- | A new generation of rules, reconciled against every device.
installInputConfig :: InputRuntime -> InputConfig -> IO ()
installInputConfig rt cfg = case irLibinput rt of
  Nothing -> do
    warned <- atomicModifyIORef' (irWarnedOff rt) (\w -> (True, w))
    unless warned $ irWarn rt "input rules ignored: the protocols are unavailable"
  Just _ -> do
    gen <- atomicModifyIORef' (irGeneration rt) (\n -> (n + 1, n + 1))
    writeIORef (irConfig rt) (Just (gen, cfg))
    -- Seats before devices, so an assignment finds its seat; a seat no rule
    -- names any more goes, its devices back to the default.
    forM_ (irManager rt) $ \im -> do
      have <- readIORef (irSeats rt)
      let want = S.fromList (inputSeats cfg)
      forM_ (S.toList (S.difference want have)) (riverInputManagerV1CreateSeat (irConn rt) im)
      forM_ (S.toList (S.difference have want)) (riverInputManagerV1DestroySeat (irConn rt) im)
      writeIORef (irSeats rt) want
    devs <- readIORef (irDevices rt)
    forM_ (M.keys devs) (reconcileDevice rt)

--------------------------------------------------------------------------------
-- Devices

adjustDevice :: InputRuntime -> ObjectId -> (Device -> Device) -> IO ()
adjustDevice rt dev f = modifyIORef' (irDevices rt) (M.adjust f dev)

addDevice :: InputRuntime -> ObjectId -> IO ()
addDevice rt dev = do
  modifyIORef' (irDevices rt) (M.insert dev newDevice)
  riverInputDeviceV1Listen conn dev $ \case
    RiverInputDeviceV1Type t -> adjustDevice rt dev $ \d -> d { dvType = decodeType t }
    RiverInputDeviceV1Name n -> adjustDevice rt dev $ \d -> d { dvName = n }
    RiverInputDeviceV1Done -> do
      -- The defaults river does not report, by device type.
      adjustDevice rt dev $ \d -> d { dvSnapshot = (dvSnapshot d)
        { snDefaults = M.union (M.fromList (deviceDefaults (dvType d))) (snDefaults (dvSnapshot d)) } }
      -- The libinput object, if any, is in this batch: settled by the sync.
      syncThen conn $ do
        adjustDevice rt dev $ \d -> d { dvSynced = True }
        reconcileDevice rt dev
    RiverInputDeviceV1Removed -> removeDevice rt dev
    _ -> pure ()
  where
    conn = irConn rt
    decodeType t
      | t == riverInputDeviceV1TypeKeyboard = Just Keyboard
      | t == riverInputDeviceV1TypePointer = Just Pointer
      | t == riverInputDeviceV1TypeTouch = Just Touch
      | t == riverInputDeviceV1TypeTablet = Just Tablet
      | otherwise = Nothing

-- | What @river_input_device_v1@'s own settings default to, unreported.
deviceDefaults :: Maybe InputType -> [(Field, Value)]
deviceDefaults = \case
  Just Pointer -> (FScrollFactor, VDouble 1) : mapped
  Just Touch -> mapped
  Just Tablet -> mapped
  Just Keyboard -> (FRepeat, VPair 25 600) : seated
  Nothing -> []
  where
    seated = [(FSeat, VText (BC.pack "default"))]
    mapped = (FMapToOutput, VText BC.empty) : (FMapToRectangle, VRect 0 0 0 0) : seated

-- | The input-device fields a device can take, by type.
deviceFields :: Maybe InputType -> S.Set Field
deviceFields = S.fromList . map fst . deviceDefaults

-- | An output came or went: a mapping may now be sendable, or is gone with
-- its object and has to be sent again if it comes back.
outputsChanged :: InputRuntime -> IO ()
outputsChanged rt = do
  modifyIORef' (irDevices rt) $ M.map $ \d -> d
    { dvSnapshot = (dvSnapshot d) { snCurrents = M.delete FMapToOutput (snCurrents (dvSnapshot d)) } }
  readIORef (irDevices rt) >>= mapM_ (reconcileDevice rt) . M.keys

-- | A setter that raced the removal gets no result and no @delete_id@: its
-- entry is dropped and its id stays retired.
removeDevice :: InputRuntime -> ObjectId -> IO ()
removeDevice rt dev = do
  riverInputDeviceV1Destroy (irConn rt) dev
  modifyIORef' (irDevices rt) (M.delete dev)
  orphaned <- atomicModifyIORef' (irPending rt) $ \m ->
    let (mine, rest) = M.partition ((== dev) . pdDevice) m in (rest, M.keys mine)
  forM_ orphaned (freeObject (irConn rt))

addLibinputDevice :: InputRuntime -> ObjectId -> IO ()
addLibinputDevice rt lib = riverLibinputDeviceV1Listen conn lib $ \case
  RiverLibinputDeviceV1InputDevice dev -> do
    modifyIORef' (irLinks rt) (M.insert lib dev)
    adjustDevice rt dev $ \d -> d { dvLib = Just lib }
  RiverLibinputDeviceV1Removed -> do
    riverLibinputDeviceV1Destroy conn lib
    mDev <- atomicModifyIORef' (irLinks rt) (\m -> (M.delete lib m, M.lookup lib m))
    forM_ mDev $ \dev -> adjustDevice rt dev $ \d -> d { dvLib = Nothing }
  -- The first ends the snapshot; later ones follow any client's setter.
  RiverLibinputDeviceV1Done -> withLinked $ \dev -> do
    adjustDevice rt dev $ \d -> d { dvSnapshotDone = True }
    reconcileDevice rt dev
  ev -> withLinked $ \dev ->
    adjustDevice rt dev $ \d -> d { dvSnapshot = noteEvent ev (dvSnapshot d) }
  where
    conn = irConn rt
    withLinked k = readIORef (irLinks rt) >>= mapM_ k . M.lookup lib

-- | The boolean @*_support@ events add nothing to the defaults; the masks
-- and the finger count do.
noteEvent :: RiverLibinputDeviceV1Event -> Snapshot -> Snapshot
noteEvent ev sn = case ev of
  RiverLibinputDeviceV1SendEventsSupport m -> sn { snSendEventsModes = m }
  RiverLibinputDeviceV1SendEventsDefault v -> dflt FSendEvents (VUInt v)
  RiverLibinputDeviceV1SendEventsCurrent v -> cur FSendEvents (VUInt v)
  RiverLibinputDeviceV1TapSupport n -> sn { snTapFingers = n }
  RiverLibinputDeviceV1TapDefault v -> dflt FTap (VUInt v)
  RiverLibinputDeviceV1TapCurrent v -> cur FTap (VUInt v)
  RiverLibinputDeviceV1TapButtonMapDefault v -> dflt FTapButtonMap (VUInt v)
  RiverLibinputDeviceV1TapButtonMapCurrent v -> cur FTapButtonMap (VUInt v)
  RiverLibinputDeviceV1DragDefault v -> dflt FDrag (VUInt v)
  RiverLibinputDeviceV1DragCurrent v -> cur FDrag (VUInt v)
  RiverLibinputDeviceV1DragLockDefault v -> dflt FDragLock (VUInt v)
  RiverLibinputDeviceV1DragLockCurrent v -> cur FDragLock (VUInt v)
  RiverLibinputDeviceV1ThreeFingerDragDefault v -> dflt FThreeFingerDrag (VUInt v)
  RiverLibinputDeviceV1ThreeFingerDragCurrent v -> cur FThreeFingerDrag (VUInt v)
  RiverLibinputDeviceV1AccelProfilesSupport m -> sn { snAccelProfiles = m }
  RiverLibinputDeviceV1AccelProfileDefault v -> dflt FAccelProfile (VUInt v)
  RiverLibinputDeviceV1AccelProfileCurrent v -> cur FAccelProfile (VUInt v)
  RiverLibinputDeviceV1AccelSpeedDefault bs -> maybe sn (dflt FAccelSpeed . VDouble) (bytesDouble bs)
  RiverLibinputDeviceV1AccelSpeedCurrent bs -> maybe sn (cur FAccelSpeed . VDouble) (bytesDouble bs)
  RiverLibinputDeviceV1NaturalScrollDefault v -> dflt FNaturalScroll (VUInt v)
  RiverLibinputDeviceV1NaturalScrollCurrent v -> cur FNaturalScroll (VUInt v)
  RiverLibinputDeviceV1LeftHandedDefault v -> dflt FLeftHanded (VUInt v)
  RiverLibinputDeviceV1LeftHandedCurrent v -> cur FLeftHanded (VUInt v)
  RiverLibinputDeviceV1ClickMethodSupport m -> sn { snClickMethods = m }
  RiverLibinputDeviceV1ClickMethodDefault v -> dflt FClickMethod (VUInt v)
  RiverLibinputDeviceV1ClickMethodCurrent v -> cur FClickMethod (VUInt v)
  RiverLibinputDeviceV1ClickfingerButtonMapDefault v -> dflt FClickfingerButtonMap (VUInt v)
  RiverLibinputDeviceV1ClickfingerButtonMapCurrent v -> cur FClickfingerButtonMap (VUInt v)
  RiverLibinputDeviceV1MiddleEmulationDefault v -> dflt FMiddleEmulation (VUInt v)
  RiverLibinputDeviceV1MiddleEmulationCurrent v -> cur FMiddleEmulation (VUInt v)
  RiverLibinputDeviceV1ScrollMethodSupport m -> sn { snScrollMethods = m }
  RiverLibinputDeviceV1ScrollMethodDefault v -> dflt FScrollMethod (VUInt v)
  RiverLibinputDeviceV1ScrollMethodCurrent v -> cur FScrollMethod (VUInt v)
  RiverLibinputDeviceV1ScrollButtonDefault v -> dflt FScrollButton (VUInt v)
  RiverLibinputDeviceV1ScrollButtonCurrent v -> cur FScrollButton (VUInt v)
  RiverLibinputDeviceV1ScrollButtonLockDefault v -> dflt FScrollButtonLock (VUInt v)
  RiverLibinputDeviceV1ScrollButtonLockCurrent v -> cur FScrollButtonLock (VUInt v)
  RiverLibinputDeviceV1DwtDefault v -> dflt FDwt (VUInt v)
  RiverLibinputDeviceV1DwtCurrent v -> cur FDwt (VUInt v)
  RiverLibinputDeviceV1DwtpDefault v -> dflt FDwtp (VUInt v)
  RiverLibinputDeviceV1DwtpCurrent v -> cur FDwtp (VUInt v)
  RiverLibinputDeviceV1RotationDefault v -> dflt FRotation (VUInt v)
  RiverLibinputDeviceV1RotationCurrent v -> cur FRotation (VUInt v)
  RiverLibinputDeviceV1CalibrationMatrixDefault bs -> maybe sn (dflt FCalibration . VFloats) (bytesFloats bs)
  RiverLibinputDeviceV1CalibrationMatrixCurrent bs -> maybe sn (cur FCalibration . VFloats) (bytesFloats bs)
  -- Boolean support flags say only that the *_default that follows is
  -- meaningful; the rest are lifecycle, handled by the listener.
  RiverLibinputDeviceV1ThreeFingerDragSupport{} -> sn
  RiverLibinputDeviceV1CalibrationMatrixSupport{} -> sn
  RiverLibinputDeviceV1NaturalScrollSupport{} -> sn
  RiverLibinputDeviceV1LeftHandedSupport{} -> sn
  RiverLibinputDeviceV1MiddleEmulationSupport{} -> sn
  RiverLibinputDeviceV1DwtSupport{} -> sn
  RiverLibinputDeviceV1DwtpSupport{} -> sn
  RiverLibinputDeviceV1RotationSupport{} -> sn
  RiverLibinputDeviceV1InputDevice{} -> sn
  RiverLibinputDeviceV1Removed -> sn
  RiverLibinputDeviceV1Done -> sn
  RiverLibinputDeviceV1Unknown{} -> sn
  where
    dflt f v = sn { snDefaults = M.insert f v (snDefaults sn) }
    cur f v = sn { snCurrents = M.insert f v (snCurrents sn) }

--------------------------------------------------------------------------------
-- Keyboards

addKeyboard :: InputRuntime -> ObjectId -> IO ()
addKeyboard rt kb = do
  modifyIORef' (irKeyboards rt) (M.insert kb (XkbKeyboard 0 Nothing False Nothing))
  riverXkbKeyboardV1Listen conn kb $ \case
    RiverXkbKeyboardV1Layout i n -> do
      modifyIORef' (irKeyboards rt) (M.adjust (\k -> k { kbIndex = i, kbName = n }) kb)
      report rt kb
    RiverXkbKeyboardV1Done -> do
      first <- not . maybe False kbDone . M.lookup kb <$> readIORef (irKeyboards rt)
      modifyIORef' (irKeyboards rt) (M.adjust (\k -> k { kbDone = True }) kb)
      when first $ readIORef (irKeymap rt) >>= mapM_ (applyKeymap rt kb)
    RiverXkbKeyboardV1Removed -> do
      riverXkbKeyboardV1Destroy conn kb
      modifyIORef' (irKeyboards rt) (M.delete kb)
    _ -> pure ()
  where conn = irConn rt

-- | Tell the worker a keyboard's active layout.
report :: InputRuntime -> ObjectId -> IO ()
report rt kb = readIORef (irKeyboards rt) >>= mapM_ tell . M.lookup kb
  where
    tell k = irOnLayout rt (Just (fromIntegral (kbIndex k), maybe "" decodeUtf8 (kbName k)))

-- | Compile-free: the text is the worker's.  A sealed memfd goes to
-- @create_keymap@; on success the keymap is set on every keyboard, present
-- and to come, and the previous one destroyed.
setKeymap :: InputRuntime -> ByteString -> IO ()
setKeymap rt text = case irXkb rt of
  Nothing -> irWarn rt "keymap ignored: river_xkb_config_v1 is unavailable"
  Just xk -> do
    fd <- sealedMemfd "xmonad-keymap" text
    km <- riverXkbConfigV1CreateKeymap conn xk fd riverXkbConfigV1KeymapFormatTextV1
    riverXkbKeymapV1Listen conn km $ \case
      RiverXkbKeymapV1Success -> do
        old <- atomicModifyIORef' (irKeymap rt) (\o -> (Just km, o))
        kbs <- readIORef (irKeyboards rt)
        forM_ (M.keys (M.filter kbDone kbs)) $ \kb -> applyKeymap rt kb km
        forM_ old (riverXkbKeymapV1Destroy conn)
      RiverXkbKeymapV1Failure msg -> do
        irWarn rt ("keymap rejected: " ++ decodeUtf8 msg)
        riverXkbKeymapV1Destroy conn km
      _ -> pure ()
  where conn = irConn rt

-- | @set_keymap@ resets the layout; the index the keyboard reported is put
-- back, so a restart's re-send keeps the layout the user was on.
applyKeymap :: InputRuntime -> ObjectId -> ObjectId -> IO ()
applyKeymap rt kb km = do
  kbs <- readIORef (irKeyboards rt)
  forM_ (M.lookup kb kbs) $ \k -> do
    riverXkbKeyboardV1SetKeymap conn kb km
    when (kbIndex k /= 0) $
      riverXkbKeyboardV1SetLayoutByIndex conn kb (fromIntegral (kbIndex k))
  where conn = irConn rt

-- | Make a layout active on every keyboard.  Legal outside a sequence.
setKeyboardLayout :: InputRuntime -> KeyboardLayoutRequest -> IO ()
setKeyboardLayout rt req = case irXkb rt of
  Nothing -> irWarn rt "layout request ignored: river_xkb_config_v1 is unavailable"
  Just _ -> readIORef (irKeyboards rt) >>= mapM_ one . M.toList
  where
    conn = irConn rt
    one (kb, k) = case req of
      KeyboardLayoutIndex i -> riverXkbKeyboardV1SetLayoutByIndex conn kb i
      KeyboardLayoutName n -> riverXkbKeyboardV1SetLayoutByName conn kb n
      -- Out of range has no effect and no event: a sync that finds the
      -- index unchanged wraps to 0.
      KeyboardLayoutNext -> do
        let from = kbIndex k
        riverXkbKeyboardV1SetLayoutByIndex conn kb (fromIntegral from + 1)
        modifyIORef' (irKeyboards rt) (M.adjust (\x -> x { kbWrapAt = Just from }) kb)
        syncThen conn $ do
          kbs <- readIORef (irKeyboards rt)
          forM_ (M.lookup kb kbs) $ \x -> when (kbWrapAt x == Just from) $ do
            modifyIORef' (irKeyboards rt) (M.adjust (\y -> y { kbWrapAt = Nothing }) kb)
            when (kbIndex x == from && from /= 0) $
              riverXkbKeyboardV1SetLayoutByIndex conn kb (0 :: Int32)

--------------------------------------------------------------------------------
-- Reconciliation

-- | Sends what the installed generation wants of one device; idempotent.
reconcileDevice :: InputRuntime -> ObjectId -> IO ()
reconcileDevice rt dev = readIORef (irConfig rt) >>= \case
  Nothing -> pure ()
  Just (gen, cfg) -> readIORef (irDevices rt) >>= mapM_ (go gen cfg) . M.lookup dev
  where
    go gen cfg d = do
      let sn = dvSnapshot d
          facts = DeviceFacts (dvType d) (dvName d) (isTouchpad (dvType d) sn)
          libReady = dvSnapshotDone d
          -- Unlinked after the sync means libinput-less.
          ownReady = libReady || (dvSynced d && isNothing (dvLib d))
          ready = (if libReady then libinputFields else S.empty)
                  `S.union` (if ownReady then deviceFields (dvType d) else S.empty)
          refused = if dvRefusedGen d == gen then dvRefused d else S.empty
          Outcome sends unsup = reconcile ready (S.union (dvInflight d) refused) sn
                                  (desiredValues cfg facts)
      forM_ unsup $ \f -> refuse rt dev gen f "the device does not support it"
      forM_ sends $ \(f, v) -> send rt dev d gen f v
      adjustDevice rt dev $ \d' -> d' { dvGeneration = gen }

-- | A field refused at this generation, said once.
refuse :: InputRuntime -> ObjectId -> Int -> Field -> String -> IO ()
refuse rt dev gen f why = do
  devs <- readIORef (irDevices rt)
  forM_ (M.lookup dev devs) $ \d -> do
    let known = if dvRefusedGen d == gen then dvRefused d else S.empty
    unless (S.member f known) $ do
      irWarn rt (BC.unpack (dvName d) ++ " (" ++ show dev ++ "): "
                 ++ fieldName f ++ " not applied, " ++ why)
      adjustDevice rt dev $ \d' -> d'
        { dvRefused = S.insert f known, dvRefusedGen = gen }

send :: InputRuntime -> ObjectId -> Device -> Int -> Field -> Value -> IO ()
send rt dev d gen f v = case (f, v) of
  -- No result and nothing reported back: what was sent is what is known.
  (FScrollFactor, VDouble x) -> riverInputDeviceV1SetScrollFactor conn dev x >> sent
  (FRepeat, VPair r dl) -> riverInputDeviceV1SetRepeatInfo conn dev (fromIntegral r) (fromIntegral dl) >> sent
  (FMapToOutput, VText name)
    | BC.null name -> riverInputDeviceV1MapToOutput conn dev nullObject >> sent
    -- Left unrecorded until the output is there; 'outputsChanged' retries.
    | otherwise -> irOutput rt name >>= mapM_ (\o -> riverInputDeviceV1MapToOutput conn dev o >> sent)
  (FMapToRectangle, VRect x y w h) -> riverInputDeviceV1MapToRectangle conn dev x y w h >> sent
  (FSeat, VText name) -> riverInputDeviceV1AssignToSeat conn dev name >> sent
  (FScrollFactor, _) -> pure ()
  (FRepeat, _) -> pure ()
  (FMapToOutput, _) -> pure ()
  (FMapToRectangle, _) -> pure ()
  (FSeat, _) -> pure ()
  -- A config object per apply: points, apply, destroy; the apply's result
  -- is the field's.  libinput copies the config, so the object is free.
  (FAccelCustom, VCurves cs) -> forM_ ((,) <$> dvLib d <*> irLibinput rt) $ \(lib, lc) -> do
    cfg <- riverLibinputConfigV1CreateAccelConfig conn lc riverLibinputDeviceV1AccelProfileCustom
    forM_ cs $ \c -> do
      r <- riverLibinputAccelConfigV1SetPoints conn cfg (accelType (curveType c))
             (doubleBytes (curveStep c)) (BS.concat (map doubleBytes (curvePoints c)))
      riverLibinputResultV1Listen conn r $ \ev -> do
        freeObject conn r
        unless (ev == RiverLibinputResultV1Success) $
          irWarn rt (BC.unpack (dvName d) ++ " (" ++ show dev ++ "): custom acceleration points refused")
    result <- riverLibinputDeviceV1ApplyAccelConfig conn lib cfg
    riverLibinputAccelConfigV1Destroy conn cfg
    track lib result
  _ -> forM_ (dvLib d) $ \lib -> do
    mResult <- setter conn lib f v
    forM_ mResult (track lib)
  where
    conn = irConn rt
    track _ result = do
      riverLibinputResultV1Listen conn result (onResult rt result)
      modifyIORef' (irPending rt) (M.insert result (Pending dev f v gen))
      adjustDevice rt dev $ \d' -> d' { dvInflight = S.insert f (dvInflight d') }
    accelType = \case
      AccelFallback -> riverLibinputAccelConfigV1AccelTypeFallback
      AccelMotion -> riverLibinputAccelConfigV1AccelTypeMotion
      AccelScroll -> riverLibinputAccelConfigV1AccelTypeScroll
    sent = adjustDevice rt dev $ \d' -> d' { dvSnapshot = (dvSnapshot d')
      { snCurrents = M.insert f v (snCurrents (dvSnapshot d')) } }

-- | The setter and its result object; 'Nothing' for a value of the wrong kind.
setter :: Connection -> ObjectId -> Field -> Value -> IO (Maybe ObjectId)
setter conn lib f v = case f of
  FSendEvents -> uint riverLibinputDeviceV1SetSendEvents
  FTap -> uint riverLibinputDeviceV1SetTap
  FTapButtonMap -> uint riverLibinputDeviceV1SetTapButtonMap
  FDrag -> uint riverLibinputDeviceV1SetDrag
  FDragLock -> uint riverLibinputDeviceV1SetDragLock
  FThreeFingerDrag -> uint riverLibinputDeviceV1SetThreeFingerDrag
  FAccelProfile -> uint riverLibinputDeviceV1SetAccelProfile
  FAccelSpeed -> case v of
    VDouble x -> Just <$> riverLibinputDeviceV1SetAccelSpeed conn lib (doubleBytes x)
    _ -> pure Nothing
  FNaturalScroll -> uint riverLibinputDeviceV1SetNaturalScroll
  FLeftHanded -> uint riverLibinputDeviceV1SetLeftHanded
  FClickMethod -> uint riverLibinputDeviceV1SetClickMethod
  FClickfingerButtonMap -> uint riverLibinputDeviceV1SetClickfingerButtonMap
  FMiddleEmulation -> uint riverLibinputDeviceV1SetMiddleEmulation
  FScrollMethod -> uint riverLibinputDeviceV1SetScrollMethod
  FScrollButton -> uint riverLibinputDeviceV1SetScrollButton
  FScrollButtonLock -> uint riverLibinputDeviceV1SetScrollButtonLock
  FDwt -> uint riverLibinputDeviceV1SetDwt
  FDwtp -> uint riverLibinputDeviceV1SetDwtp
  FRotation -> uint riverLibinputDeviceV1SetRotation
  FCalibration -> case v of
    VFloats fs -> Just <$> riverLibinputDeviceV1SetCalibrationMatrix conn lib (floatsBytes fs)
    _ -> pure Nothing
  -- Not libinput settings: 'send' answers these before asking here.
  FAccelCustom -> pure Nothing
  FScrollFactor -> pure Nothing
  FMapToOutput -> pure Nothing
  FRepeat -> pure Nothing
  FMapToRectangle -> pure Nothing
  FSeat -> pure Nothing
  where
    uint k = case v of
      VUInt x -> Just <$> k conn lib x
      _ -> pure Nothing

-- | The event destroys the object; the id comes back with @delete_id@.
onResult :: InputRuntime -> ObjectId -> RiverLibinputResultV1Event -> IO ()
onResult rt result ev = do
  freeObject (irConn rt) result
  mPending <- atomicModifyIORef' (irPending rt) (\m -> (M.delete result m, M.lookup result m))
  forM_ mPending $ \p -> do
    let dev = pdDevice p
    devs <- readIORef (irDevices rt)
    forM_ (M.lookup dev devs) $ \_ -> do
      adjustDevice rt dev $ \d -> d { dvInflight = S.delete (pdField p) (dvInflight d) }
      current <- (== Just (pdGeneration p)) . fmap fst <$> readIORef (irConfig rt)
      -- A stale success is recorded by the broadcast that follows it.
      when current $ case ev of
        RiverLibinputResultV1Success -> adjustDevice rt dev $ \d -> d
          { dvSnapshot = (dvSnapshot d)
              { snCurrents = M.insert (pdField p) (pdValue p) (snCurrents (dvSnapshot d)) } }
        RiverLibinputResultV1Unsupported ->
          refuse rt dev (pdGeneration p) (pdField p) "the device reports it unsupported"
        RiverLibinputResultV1Invalid ->
          refuse rt dev (pdGeneration p) (pdField p) "the device reports the value invalid"
        _ -> pure ()
      -- The generation that skipped this field gets its turn.
      unless current $ reconcileDevice rt dev
