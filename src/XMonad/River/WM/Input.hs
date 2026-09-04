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
import XMonad.River.Wire (ObjectId, bytesDouble, decodeUtf8, doubleBytes)

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
  , irLibinput   :: !(Maybe ObjectId)
    -- ^ @river_libinput_config_v1@ at version 2, with the input manager;
    -- 'Nothing' and devices are left alone.
  , irXkb        :: !(Maybe ObjectId)
    -- ^ @river_xkb_config_v1@ at version 2; 'Nothing' and layouts cannot
    -- be switched.
  , irOnLayout   :: !(Maybe (Int, String) -> IO ())
    -- ^ Told the active layout whenever a keyboard reports one.
  , irKeyboards  :: !(IORef (M.Map ObjectId XkbKeyboard))
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
  -> IO InputRuntime
bindInput conn registry globals onLayout = do
  mManager <- bindGlobal conn registry globals
                riverInputManagerV1Interface 2 riverInputManagerV1Version
  case mManager of
    Nothing -> do
      warn "river_input_manager_v1 is not offered at version 2; input devices \
           \are left as they are and layouts cannot be switched"
      newInputRuntime conn warn onLayout Nothing Nothing Nothing
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
      newInputRuntime conn warn onLayout (Just im) (fst <$> mConfig) (fst <$> mXkb)
  where
    warn = hPutStrLn stderr . ("xmonad-river: input: " ++)

-- | The runtime over bound globals; separate from 'bindInput' for the tests.
newInputRuntime
  :: Connection
  -> (String -> IO ())              -- ^ where diagnostics go
  -> (Maybe (Int, String) -> IO ())  -- ^ told the active layout
  -> Maybe ObjectId                 -- ^ input manager
  -> Maybe ObjectId                 -- ^ libinput config
  -> Maybe ObjectId                 -- ^ xkb config
  -> IO InputRuntime
newInputRuntime conn warn onLayout mManager mConfig mXkb = do
  rt <- InputRuntime conn mConfig mXkb onLayout
          <$> newIORef M.empty <*> newIORef Nothing
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
      -- 1.0 stands in for the scroll-factor default river does not report.
      adjustDevice rt dev $ \d ->
        if dvType d == Just Pointer
          then d { dvSnapshot = (dvSnapshot d)
                     { snDefaults = M.insert FScrollFactor (VDouble 1) (snDefaults (dvSnapshot d)) } }
          else d
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
  _ -> sn
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
          scrollReady = dvType d == Just Pointer
                        && (libReady || (dvSynced d && isNothing (dvLib d)))
          ready = (if libReady then libinputFields else S.empty)
                  `S.union` (if scrollReady then S.singleton FScrollFactor else S.empty)
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
send rt dev d gen f v = case f of
  -- No result and nothing reported back: what was sent is what is known.
  FScrollFactor -> case v of
    VDouble x -> do
      riverInputDeviceV1SetScrollFactor conn dev x
      adjustDevice rt dev $ \d' -> d' { dvSnapshot = (dvSnapshot d')
        { snCurrents = M.insert f v (snCurrents (dvSnapshot d')) } }
    VUInt _ -> pure ()
  _ -> forM_ (dvLib d) $ \lib -> do
    mResult <- setter conn lib f v
    forM_ mResult $ \result -> do
      riverLibinputResultV1Listen conn result (onResult rt result)
      modifyIORef' (irPending rt) (M.insert result (Pending dev f v gen))
      adjustDevice rt dev $ \d' -> d' { dvInflight = S.insert f (dvInflight d') }
  where conn = irConn rt

-- | The setter and its result object; 'Nothing' for a value of the wrong kind.
setter :: Connection -> ObjectId -> Field -> Value -> IO (Maybe ObjectId)
setter conn lib f v = case (f, v) of
  (FSendEvents, VUInt x) -> Just <$> riverLibinputDeviceV1SetSendEvents conn lib x
  (FTap, VUInt x) -> Just <$> riverLibinputDeviceV1SetTap conn lib x
  (FTapButtonMap, VUInt x) -> Just <$> riverLibinputDeviceV1SetTapButtonMap conn lib x
  (FDrag, VUInt x) -> Just <$> riverLibinputDeviceV1SetDrag conn lib x
  (FDragLock, VUInt x) -> Just <$> riverLibinputDeviceV1SetDragLock conn lib x
  (FThreeFingerDrag, VUInt x) -> Just <$> riverLibinputDeviceV1SetThreeFingerDrag conn lib x
  (FAccelProfile, VUInt x) -> Just <$> riverLibinputDeviceV1SetAccelProfile conn lib x
  (FAccelSpeed, VDouble x) -> Just <$> riverLibinputDeviceV1SetAccelSpeed conn lib (doubleBytes x)
  (FNaturalScroll, VUInt x) -> Just <$> riverLibinputDeviceV1SetNaturalScroll conn lib x
  (FLeftHanded, VUInt x) -> Just <$> riverLibinputDeviceV1SetLeftHanded conn lib x
  (FClickMethod, VUInt x) -> Just <$> riverLibinputDeviceV1SetClickMethod conn lib x
  (FClickfingerButtonMap, VUInt x) -> Just <$> riverLibinputDeviceV1SetClickfingerButtonMap conn lib x
  (FMiddleEmulation, VUInt x) -> Just <$> riverLibinputDeviceV1SetMiddleEmulation conn lib x
  (FScrollMethod, VUInt x) -> Just <$> riverLibinputDeviceV1SetScrollMethod conn lib x
  (FScrollButton, VUInt x) -> Just <$> riverLibinputDeviceV1SetScrollButton conn lib x
  (FScrollButtonLock, VUInt x) -> Just <$> riverLibinputDeviceV1SetScrollButtonLock conn lib x
  (FDwt, VUInt x) -> Just <$> riverLibinputDeviceV1SetDwt conn lib x
  (FDwtp, VUInt x) -> Just <$> riverLibinputDeviceV1SetDwtp conn lib x
  (FRotation, VUInt x) -> Just <$> riverLibinputDeviceV1SetRotation conn lib x
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
