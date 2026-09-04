{-# LANGUAGE LambdaCase #-}
-- | Input-device rules and the pure half of applying them.  Rules are
-- first-order data because the loop evaluates them and runs no user code.
-- The protocol half is "XMonad.River.WM.Input"; see @LIBINPUT.md@.
module XMonad.River.Input
  ( -- * Rules
    InputType(..)
  , NameMatch(..)
  , InputMatch(..)
  , defaultInputMatch
  , touchpads
  , InputSettings(..)
  , defaultInputSettings
  , InputRule(..)
  , SendEvents(..)
  , ButtonMap(..)
  , DragLock(..)
  , ThreeFingerDrag(..)
  , AccelProfile(..)
  , AccelType(..)
  , AccelCurve(..)
  , ClickMethod(..)
  , ScrollMethod(..)
    -- * Keymaps
  , Keymap(..)
  , defaultKeymap
    -- * The validated config
  , InputConfig
  , inputRules
  , inputSeats
  , Rule(..)
  , validateInputConfig
  , forceInputConfig
    -- * A device, as the loop sees it
  , Field(..)
  , fieldName
  , libinputFields
  , Value(..)
  , Snapshot(..)
  , emptySnapshot
  , DeviceFacts(..)
  , isTouchpad
    -- * Deciding what to send
  , matchesRule
  , desiredValues
  , Outcome(..)
  , reconcile
  ) where

import Data.Bits (complement, (.&.))
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.List (foldl', nub)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.River.Protocol.LibinputConfig
import XMonad.River.Wire (encodeUtf8)

--------------------------------------------------------------------------------
-- Rules

-- | @river_input_device_v1.type@.  Switches, tablet pads and virtual
-- devices get no object.
data InputType = Keyboard | Pointer | Touch | Tablet
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | On the compositor's device name, case-sensitively.
data NameMatch
  = NameExactly String
  | NameStartsWith String
  | NameContains String
  deriving (Eq, Show, Read)

-- | Which devices a rule applies to.  Every field is a conjunct; 'Nothing'
-- matches anything.
data InputMatch = InputMatch
  { matchType     :: !(Maybe InputType)
  , matchName     :: !(Maybe NameMatch)
  , matchTouchpad :: !(Maybe Bool)
    -- ^ A pointer whose libinput object reports tap fingers.
  } deriving (Eq, Show, Read)

-- | Matches every device.
defaultInputMatch :: InputMatch
defaultInputMatch = InputMatch Nothing Nothing Nothing

-- | Matches every touchpad.
touchpads :: InputMatch
touchpads = defaultInputMatch { matchTouchpad = Just True }

-- | @libinput_config_send_events_mode@.
data SendEvents
  = SendEventsEnabled
  | SendEventsDisabled
  | SendEventsDisabledOnExternalMouse
  deriving (Eq, Show, Read, Enum, Bounded)

-- | Which buttons one, two and three fingers produce.
data ButtonMap = LeftRightMiddle | LeftMiddleRight
  deriving (Eq, Show, Read, Enum, Bounded)

data DragLock = DragLockDisabled | DragLockTimeout | DragLockSticky
  deriving (Eq, Show, Read, Enum, Bounded)

data ThreeFingerDrag = ThreeFingerDragDisabled | ThreeFingerDrag3 | ThreeFingerDrag4
  deriving (Eq, Show, Read, Enum, Bounded)

-- | The custom profile is 'accelCustom', which selects it.
data AccelProfile = AccelNone | AccelFlat | AccelAdaptive
  deriving (Eq, Show, Read, Enum, Bounded)

-- | Which movement a custom curve applies to.
data AccelType = AccelFallback | AccelMotion | AccelScroll
  deriving (Eq, Show, Read, Enum, Bounded)

-- | A custom acceleration function: output speed at @step@ intervals of
-- input speed, per @libinput_config_accel_set_points@.
data AccelCurve = AccelCurve
  { curveType   :: !AccelType
  , curveStep   :: !Double
  , curvePoints :: ![Double]
  } deriving (Eq, Show, Read)

data ClickMethod = ClickNone | ButtonAreas | ClickFinger
  deriving (Eq, Show, Read, Enum, Bounded)

data ScrollMethod = NoScroll | TwoFinger | EdgeScroll | OnButtonDown
  deriving (Eq, Show, Read, Enum, Bounded)

-- | A field no matching rule sets is reconciled to the device's default.
data InputSettings = InputSettings
  { sendEvents               :: !(Maybe SendEvents)
  , tap                      :: !(Maybe Bool)
  , tapButtonMap             :: !(Maybe ButtonMap)
  , tapDrag                  :: !(Maybe Bool)
  , dragLock                 :: !(Maybe DragLock)
  , threeFingerDrag          :: !(Maybe ThreeFingerDrag)
  , accelProfile             :: !(Maybe AccelProfile)
  , accelSpeed               :: !(Maybe Double)
    -- ^ In @[-1, 1]@.
  , naturalScroll            :: !(Maybe Bool)
  , leftHanded               :: !(Maybe Bool)
  , clickMethod              :: !(Maybe ClickMethod)
  , clickfingerButtonMap     :: !(Maybe ButtonMap)
  , middleEmulation          :: !(Maybe Bool)
  , scrollMethod             :: !(Maybe ScrollMethod)
  , scrollButton             :: !(Maybe Word32)
    -- ^ A Linux button code, for 'OnButtonDown'.
  , scrollButtonLock         :: !(Maybe Bool)
  , disableWhileTyping       :: !(Maybe Bool)
  , disableWhileTrackpointing :: !(Maybe Bool)
  , rotationAngle            :: !(Maybe Word32)
    -- ^ Degrees clockwise, in @[0, 360)@.
  , scrollFactor             :: !(Maybe Double)
    -- ^ Non-negative; @1.0@ is neutral and the default, river keeping none.
  , mapToOutput              :: !(Maybe String)
    -- ^ A connector name a pointer, touch or tablet device is confined to;
    -- @\"\"@ and the default map to the whole layout.
  , keyRepeat                :: !(Maybe (Word32, Word32))
    -- ^ Repeats per second and delay in milliseconds, for a keyboard; the
    -- default is river's @25@ and @600@, which it does not report.
  , calibrationMatrix        :: !(Maybe [Double])
    -- ^ Six values, a touchscreen's or tablet's.
  , accelCustom              :: !(Maybe [AccelCurve])
    -- ^ Custom acceleration curves; selects the @custom@ profile, so
    -- exclusive with 'accelProfile'.  Nothing restores: 'accelProfile' does.
  , mapToRectangle           :: !(Maybe (Int, Int, Int, Int))
    -- ^ @x y width height@ in the global space a pointer, touch or tablet
    -- device is confined to; wins over 'mapToOutput'.  Zero size clears.
  , seat                     :: !(Maybe String)
    -- ^ The seat the device belongs to, created if named nowhere else;
    -- the default is @\"default\"@.
  } deriving (Eq, Show, Read)

-- | Sets nothing: every field reconciles to its default.
defaultInputSettings :: InputSettings
defaultInputSettings = InputSettings
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing

data InputRule = InputRule
  { inputMatch    :: !InputMatch
  , inputSettings :: !InputSettings
  } deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- Keymaps

-- | An xkb keymap by RMLVO names, each comma-separated as xkb takes them
-- and empty for xkb's default.
data Keymap = Keymap
  { keymapRules    :: !String
  , keymapModel    :: !String
  , keymapLayouts  :: !String
  , keymapVariants :: !String
  , keymapOptions  :: !String
  } deriving (Eq, Show, Read)

-- | xkb's defaults throughout.
defaultKeymap :: Keymap
defaultKeymap = Keymap "" "" "" "" ""

--------------------------------------------------------------------------------
-- Fields and values

-- | A @river_libinput_device_v1@ setter, or a @river_input_device_v1@ one
-- from 'FScrollFactor' on, reconciled alike against a default river does
-- not report: @1.0@, no mapping, @25@/@600@.
data Field
  = FSendEvents | FTap | FTapButtonMap | FDrag | FDragLock | FThreeFingerDrag
  | FAccelProfile | FAccelSpeed | FNaturalScroll | FLeftHanded | FClickMethod
  | FClickfingerButtonMap | FMiddleEmulation | FScrollMethod | FScrollButton
  | FScrollButtonLock | FDwt | FDwtp | FRotation | FCalibration | FAccelCustom
  | FScrollFactor | FMapToOutput | FRepeat | FMapToRectangle | FSeat
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The protocol's name for a field, for diagnostics.
fieldName :: Field -> String
fieldName = \case
  FSendEvents -> "send_events"
  FTap -> "tap"
  FTapButtonMap -> "tap_button_map"
  FDrag -> "drag"
  FDragLock -> "drag_lock"
  FThreeFingerDrag -> "three_finger_drag"
  FAccelProfile -> "accel_profile"
  FAccelSpeed -> "accel_speed"
  FNaturalScroll -> "natural_scroll"
  FLeftHanded -> "left_handed"
  FClickMethod -> "click_method"
  FClickfingerButtonMap -> "clickfinger_button_map"
  FMiddleEmulation -> "middle_emulation"
  FScrollMethod -> "scroll_method"
  FScrollButton -> "scroll_button"
  FScrollButtonLock -> "scroll_button_lock"
  FDwt -> "dwt"
  FDwtp -> "dwtp"
  FRotation -> "rotation"
  FCalibration -> "calibration_matrix"
  FAccelCustom -> "accel_config"
  FScrollFactor -> "scroll_factor"
  FMapToOutput -> "map_to_output"
  FRepeat -> "repeat_info"
  FMapToRectangle -> "map_to_rectangle"
  FSeat -> "seat"

-- | The fields that need the libinput snapshot; the rest are the input
-- device's own.
libinputFields :: S.Set Field
libinputFields = S.fromList [ f | f <- [minBound .. maxBound], f < FScrollFactor ]

-- | A value on the wire: a uint, a double the protocol carries in an array,
-- a name, a repeat rate and delay, a matrix, curves, or a rectangle.
data Value
  = VUInt !Word32 | VDouble !Double | VText !ByteString | VPair !Word32 !Word32
  | VFloats ![Float] | VCurves ![AccelCurve] | VRect !Int32 !Int32 !Int32 !Int32
  deriving (Eq, Show)

-- | Equal enough not to re-send.
sameValue :: Value -> Value -> Bool
sameValue (VDouble a) (VDouble b) = abs (a - b) < 1e-9
sameValue a b = a == b

--------------------------------------------------------------------------------
-- The validated config

-- | A rule with its strings encoded and its settings projected onto fields.
data Rule = Rule
  { ruleType     :: !(Maybe InputType)
  , ruleName     :: !(Maybe NameMatch')
  , ruleTouchpad :: !(Maybe Bool)
  , ruleValues   :: !(M.Map Field Value)
  } deriving (Eq, Show)

-- | 'NameMatch' over the bytes the compositor sends.
data NameMatch'
  = Exactly !ByteString
  | StartsWith !ByteString
  | Contains !ByteString
  deriving (Eq, Show)

-- | Rules the loop can evaluate: validated, normalised, fully evaluated;
-- and the seats they name, for the loop to create.
data InputConfig = InputConfig
  { inputRules :: ![Rule]
  , inputSeats :: ![ByteString]
  } deriving (Eq, Show)

-- | The config, or which rule is wrong and why.  A negative scroll factor
-- is a protocol error, not an @invalid@ result, so it is refused here.
validateInputConfig :: [InputRule] -> Either String InputConfig
validateInputConfig rules = do
  rs <- sequence (zipWith check [1 :: Int ..] rules)
  pure (InputConfig rs (nub [ s | r <- rs, Just (VText s) <- [M.lookup FSeat (ruleValues r)], s /= encodeUtf8 "default" ]))
  where
    check n (InputRule m s) = do
      let at what = Left ("rule " ++ show n ++ ": " ++ what)
      mapM_ (\f -> if f < -1 || f > 1 || isNaN f then at "accelSpeed is outside [-1, 1]" else Right ())
        (accelSpeed s)
      mapM_ (\a -> if a >= 360 then at "rotationAngle is outside [0, 360)" else Right ())
        (rotationAngle s)
      mapM_ (\f -> if isNaN f || isInfinite f || f < 0 || f > fixedMax
                     then at "scrollFactor is negative, not finite, or too large for a Wayland fixed"
                     else Right ())
        (scrollFactor s)
      mapM_ (\(r, d) -> if r > 0x7fffffff || d > 0x7fffffff
                          then at "keyRepeat does not fit a protocol int"
                          else Right ())
        (keyRepeat s)
      mapM_ (\cal -> if length cal /= 6 || any bad cal then at "calibrationMatrix is not six finite values" else Right ())
        (calibrationMatrix s)
      mapM_ (\cs -> if null cs || any (\c -> bad (curveStep c) || curveStep c <= 0 || null (curvePoints c) || any bad (curvePoints c)) cs
                      then at "accelCustom needs a positive step and finite points per curve"
                      else if accelProfile s /= Nothing then at "accelCustom selects the custom profile; accelProfile must be unset"
                      else Right ())
        (accelCustom s)
      mapM_ (\(x, y, w, h) -> if w < 0 || h < 0 || any (\v -> v > 0x7fffffff || v < -0x80000000) [x, y, w, h]
                                then at "mapToRectangle needs a non-negative size that fits a protocol int"
                                else Right ())
        (mapToRectangle s)
      mapM_ (\sn -> if null sn then at "seat is empty" else Right ()) (seat s)
      Right Rule
        { ruleType = matchType m
        , ruleName = fmap encodeName (matchName m)
        , ruleTouchpad = matchTouchpad m
        , ruleValues = settingsValues s
        }
    -- Wayland fixed is signed 24.8.
    fixedMax = 8388607
    bad v = isNaN v || isInfinite v
    encodeName = \case
      NameExactly x -> Exactly (encodeUtf8 x)
      NameStartsWith x -> StartsWith (encodeUtf8 x)
      NameContains x -> Contains (encodeUtf8 x)

-- | Forces every rule; the fields are strict.
forceInputConfig :: InputConfig -> ()
forceInputConfig (InputConfig rs ss) = foldr seq (foldr seq () ss) rs

-- | The fields a settings record sets, as wire values.
settingsValues :: InputSettings -> M.Map Field Value
settingsValues s = M.fromList $ concat
  [ enumV FSendEvents sendEvents $ \case
      SendEventsEnabled -> riverLibinputDeviceV1SendEventsModesEnabled
      SendEventsDisabled -> riverLibinputDeviceV1SendEventsModesDisabled
      SendEventsDisabledOnExternalMouse -> riverLibinputDeviceV1SendEventsModesDisabledOnExternalMouse
  , boolV FTap tap riverLibinputDeviceV1TapStateEnabled riverLibinputDeviceV1TapStateDisabled
  , enumV FTapButtonMap tapButtonMap $ \case
      LeftRightMiddle -> riverLibinputDeviceV1TapButtonMapLrm
      LeftMiddleRight -> riverLibinputDeviceV1TapButtonMapLmr
  , boolV FDrag tapDrag riverLibinputDeviceV1DragStateEnabled riverLibinputDeviceV1DragStateDisabled
  , enumV FDragLock dragLock $ \case
      DragLockDisabled -> riverLibinputDeviceV1DragLockStateDisabled
      DragLockTimeout -> riverLibinputDeviceV1DragLockStateEnabledTimeout
      DragLockSticky -> riverLibinputDeviceV1DragLockStateEnabledSticky
  , enumV FThreeFingerDrag threeFingerDrag $ \case
      ThreeFingerDragDisabled -> riverLibinputDeviceV1ThreeFingerDragStateDisabled
      ThreeFingerDrag3 -> riverLibinputDeviceV1ThreeFingerDragStateEnabled3fg
      ThreeFingerDrag4 -> riverLibinputDeviceV1ThreeFingerDragStateEnabled4fg
  , enumV FAccelProfile accelProfile $ \case
      AccelNone -> riverLibinputDeviceV1AccelProfileNone
      AccelFlat -> riverLibinputDeviceV1AccelProfileFlat
      AccelAdaptive -> riverLibinputDeviceV1AccelProfileAdaptive
  , [ (FAccelSpeed, VDouble d) | Just d <- [accelSpeed s] ]
  , boolV FNaturalScroll naturalScroll riverLibinputDeviceV1NaturalScrollStateEnabled riverLibinputDeviceV1NaturalScrollStateDisabled
  , boolV FLeftHanded leftHanded riverLibinputDeviceV1LeftHandedStateEnabled riverLibinputDeviceV1LeftHandedStateDisabled
  , enumV FClickMethod clickMethod $ \case
      ClickNone -> riverLibinputDeviceV1ClickMethodNone
      ButtonAreas -> riverLibinputDeviceV1ClickMethodButtonAreas
      ClickFinger -> riverLibinputDeviceV1ClickMethodClickfinger
  , enumV FClickfingerButtonMap clickfingerButtonMap $ \case
      LeftRightMiddle -> riverLibinputDeviceV1ClickfingerButtonMapLrm
      LeftMiddleRight -> riverLibinputDeviceV1ClickfingerButtonMapLmr
  , boolV FMiddleEmulation middleEmulation riverLibinputDeviceV1MiddleEmulationStateEnabled riverLibinputDeviceV1MiddleEmulationStateDisabled
  , enumV FScrollMethod scrollMethod $ \case
      NoScroll -> riverLibinputDeviceV1ScrollMethodNoScroll
      TwoFinger -> riverLibinputDeviceV1ScrollMethodTwoFinger
      EdgeScroll -> riverLibinputDeviceV1ScrollMethodEdge
      OnButtonDown -> riverLibinputDeviceV1ScrollMethodOnButtonDown
  , [ (FScrollButton, VUInt b) | Just b <- [scrollButton s] ]
  , boolV FScrollButtonLock scrollButtonLock riverLibinputDeviceV1ScrollButtonLockStateEnabled riverLibinputDeviceV1ScrollButtonLockStateDisabled
  , boolV FDwt disableWhileTyping riverLibinputDeviceV1DwtStateEnabled riverLibinputDeviceV1DwtStateDisabled
  , boolV FDwtp disableWhileTrackpointing riverLibinputDeviceV1DwtpStateEnabled riverLibinputDeviceV1DwtpStateDisabled
  , [ (FRotation, VUInt a) | Just a <- [rotationAngle s] ]
  , [ (FScrollFactor, VDouble f) | Just f <- [scrollFactor s] ]
  , [ (FMapToOutput, VText (encodeUtf8 o)) | Just o <- [mapToOutput s] ]
  , [ (FRepeat, VPair r d) | Just (r, d) <- [keyRepeat s] ]
  , [ (FCalibration, VFloats (map realToFrac m)) | Just m <- [calibrationMatrix s] ]
  , [ (FAccelCustom, VCurves cs) | Just cs <- [accelCustom s] ]
  , [ (FMapToRectangle, VRect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
    | Just (x, y, w, h) <- [mapToRectangle s] ]
  , [ (FSeat, VText (encodeUtf8 n)) | Just n <- [seat s] ]
  ]
  where
    enumV f get code = [ (f, VUInt (code x)) | Just x <- [get s] ]
    boolV f get yes no = [ (f, VUInt (if b then yes else no)) | Just b <- [get s] ]

--------------------------------------------------------------------------------
-- A device

-- | What a device advertised.  A field is supported exactly when its
-- default is here.
data Snapshot = Snapshot
  { snDefaults        :: !(M.Map Field Value)
  , snCurrents        :: !(M.Map Field Value)
    -- ^ Advertised, confirmed or broadcast; for 'FScrollFactor', last sent.
  , snSendEventsModes :: !Word32
  , snAccelProfiles   :: !Word32
  , snClickMethods    :: !Word32
  , snScrollMethods   :: !Word32
  , snTapFingers      :: !Int32
  } deriving (Eq, Show)

emptySnapshot :: Snapshot
emptySnapshot = Snapshot M.empty M.empty 0 0 0 0 0

-- | What the matcher sees; a type this build does not know is 'Nothing'.
data DeviceFacts = DeviceFacts
  { dfType     :: !(Maybe InputType)
  , dfName     :: !ByteString
  , dfTouchpad :: !Bool
  } deriving (Eq, Show)

-- | A pointer whose libinput object reports tap-to-click fingers.
isTouchpad :: Maybe InputType -> Snapshot -> Bool
isTouchpad t sn = t == Just Pointer && snTapFingers sn > 0

--------------------------------------------------------------------------------
-- Deciding what to send

matchesRule :: Rule -> DeviceFacts -> Bool
matchesRule r d =
     maybe True (\t -> dfType d == Just t) (ruleType r)
  && maybe True (`nameMatches` dfName d) (ruleName r)
  && maybe True (== dfTouchpad d) (ruleTouchpad r)
  where
    nameMatches = \case
      Exactly x -> (== x)
      StartsWith x -> BS.isPrefixOf x
      Contains x -> BS.isInfixOf x

-- | The fields the matching rules set, a later rule's value winning.
desiredValues :: InputConfig -> DeviceFacts -> M.Map Field Value
desiredValues cfg d =
  foldl' (\acc r -> if matchesRule r d then M.union (ruleValues r) acc else acc)
         M.empty (inputRules cfg)

-- | What a reconciliation pass decided.
data Outcome = Outcome
  { toSend      :: ![(Field, Value)]
    -- ^ In 'Field' order.
  , unsupported :: ![Field]
    -- ^ Configured explicitly, but the device cannot do it: not sent.
  } deriving (Eq, Show)

-- | What to send: over the ready fields, the wanted value (explicit, else
-- the default) where it differs from the current one and nothing is in flight.
reconcile
  :: S.Set Field           -- ^ fields the device is ready for
  -> S.Set Field           -- ^ fields with a result in flight
  -> Snapshot
  -> M.Map Field Value     -- ^ the explicit values, from 'desiredValues'
  -> Outcome
reconcile ready inflight sn explicit0 = foldr step (Outcome [] []) [minBound .. maxBound]
  where
    -- Curves select the custom profile; the profile field must agree or it
    -- would put the default back after every apply.
    explicit
      | M.member FAccelCustom explicit0 = M.insert FAccelProfile (VUInt 4) explicit0
      | otherwise = explicit0
    step f o
      | not (S.member f ready) = o
      -- No default event: supported with the custom bit, nothing to restore.
      | f == FAccelCustom = case M.lookup f explicit of
          Nothing -> o
          Just v
            | snAccelProfiles sn .&. 4 == 0 -> o { unsupported = f : unsupported o }
            | S.member f inflight || M.lookup f (snCurrents sn) == Just v -> o
            | otherwise -> o { toSend = (f, v) : toSend o }
      | otherwise = case (M.lookup f explicit, M.lookup f (snDefaults sn)) of
          (Nothing, Nothing) -> o
          (Just _, Nothing) -> o { unsupported = f : unsupported o }
          (Just v, Just _) | not (allowed f v) -> o { unsupported = f : unsupported o }
          (mv, Just d) ->
            let want = fromMaybe d mv
                have = M.lookup f (snCurrents sn)
            in if S.member f inflight || maybe False (sameValue want) have
                 then o
                 else o { toSend = (f, want) : toSend o }
    -- A bitfield's default says the setting exists; the mask says which values.
    allowed f (VUInt v) = case f of
      FSendEvents -> v .&. complement (snSendEventsModes sn) == 0
      FAccelProfile -> v == 0 || v .&. snAccelProfiles sn /= 0
      FClickMethod -> v == 0 || v .&. snClickMethods sn /= 0
      FScrollMethod -> v == 0 || v .&. snScrollMethods sn /= 0
      _ -> True
    allowed _ _ = True
