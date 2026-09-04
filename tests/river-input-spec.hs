{-# LANGUAGE OverloadedStrings #-}
-- | Input-device configuration: the rules as values, and the loop's half
-- against a fake compositor on the other end of a socketpair.
module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word (Word16, Word32)
import System.Exit (exitFailure)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NBS

import XMonad.River.Connection
import XMonad.River.Input
import XMonad.River.Plan (KeyboardLayoutRequest (..))
import XMonad.River.WM.Input
import XMonad.River.Wire

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
        unless ok (modifyIORef' failures (+ 1))
  mapM_ (uncurry check) pureTests
  harnessTests check
  n <- readIORef failures
  when (n > 0) exitFailure

--------------------------------------------------------------------------------
-- Rules as values

touchpad, mouse, keyboard :: DeviceFacts
touchpad = DeviceFacts (Just Pointer) "PIXA3854:00 093A:0274 Touchpad" True
mouse = DeviceFacts (Just Pointer) "Logitech USB Receiver Mouse" False
keyboard = DeviceFacts (Just Keyboard) "AT Translated Set 2 keyboard" False

rule :: InputMatch -> InputSettings -> InputRule
rule = InputRule

config :: [InputRule] -> InputConfig
config rs = either error id (validateInputConfig rs)

firstRule :: [InputRule] -> Rule
firstRule rs = case inputRules (config rs) of
  (r : _) -> r
  [] -> error "no rule"

-- | A touchpad's snapshot: tap and click method supported, both at their
-- defaults.
touchpadSnapshot :: Snapshot
touchpadSnapshot = emptySnapshot
  { snDefaults = M.fromList [ (FTap, VUInt 0), (FClickMethod, VUInt 1), (FDwt, VUInt 1)
                            , (FAccelSpeed, VDouble 0), (FScrollFactor, VDouble 1) ]
  , snCurrents = M.fromList [ (FTap, VUInt 0), (FClickMethod, VUInt 1), (FDwt, VUInt 1)
                            , (FAccelSpeed, VDouble 0) ]
  , snClickMethods = 3
  , snTapFingers = 2
  }

allReady :: S.Set Field
allReady = S.fromList [minBound .. maxBound]

pureTests :: [(String, Bool)]
pureTests =
  [ ( "name: exact, prefix and substring"
    , let m s = matchesRule (firstRule [rule defaultInputMatch { matchName = Just s } defaultInputSettings])
      in m (NameExactly "Logitech USB Receiver Mouse") mouse
         && not (m (NameExactly "Logitech") mouse)
         && m (NameStartsWith "Logitech") mouse
         && not (m (NameStartsWith "USB") mouse)
         && m (NameContains "USB") mouse
         && not (m (NameContains "usb") mouse) )

  , ( "touchpad: a pointer with tap fingers, and nothing else"
    , isTouchpad (Just Pointer) touchpadSnapshot
        && not (isTouchpad (Just Pointer) emptySnapshot)
        && not (isTouchpad (Just Keyboard) touchpadSnapshot)
        && not (isTouchpad Nothing touchpadSnapshot) )

  , ( "touchpads matches only touchpads"
    , let r = firstRule [rule touchpads defaultInputSettings]
      in matchesRule r touchpad && not (matchesRule r mouse) && not (matchesRule r keyboard) )

  , ( "a later rule's field wins; others are kept"
    , let cfg = config
            [ rule defaultInputMatch defaultInputSettings { tap = Just True, naturalScroll = Just True }
            , rule touchpads defaultInputSettings { tap = Just False } ]
      in desiredValues cfg touchpad == M.fromList [ (FTap, VUInt 0), (FNaturalScroll, VUInt 1) ]
         && desiredValues cfg mouse == M.fromList [ (FTap, VUInt 1), (FNaturalScroll, VUInt 1) ] )

  , ( "an absent field goes back to the advertised default"
    , let sn = touchpadSnapshot { snCurrents = M.insert FTap (VUInt 1) (snCurrents touchpadSnapshot) }
      in reconcile allReady S.empty sn M.empty
           == Outcome [ (FTap, VUInt 0), (FScrollFactor, VDouble 1) ] [] )

  , ( "nothing differs, nothing is sent"
    , let sn = touchpadSnapshot { snCurrents = M.insert FScrollFactor (VDouble 1) (snCurrents touchpadSnapshot) }
      in reconcile allReady S.empty sn M.empty == Outcome [] [] )

  , ( "an explicit value differing from current is sent"
    , toSend (reconcile allReady S.empty touchpadSnapshot (M.fromList [ (FTap, VUInt 1) ]))
        == [ (FTap, VUInt 1), (FScrollFactor, VDouble 1) ] )

  , ( "a field with a result in flight is left alone"
    , toSend (reconcile allReady (S.singleton FTap) touchpadSnapshot (M.fromList [ (FTap, VUInt 1) ]))
        == [ (FScrollFactor, VDouble 1) ] )

  , ( "a field the device is not ready for is not considered"
    , reconcile (S.singleton FScrollFactor) S.empty touchpadSnapshot (M.fromList [ (FTap, VUInt 1) ])
        == Outcome [ (FScrollFactor, VDouble 1) ] [] )

  , ( "an explicit unsupported field is diagnosed, not sent"
    , reconcile allReady S.empty touchpadSnapshot (M.fromList [ (FRotation, VUInt 90) ])
        == Outcome [ (FScrollFactor, VDouble 1) ] [ FRotation ] )

  , ( "a value outside the support mask is unsupported"
    , unsupported (reconcile allReady S.empty touchpadSnapshot { snClickMethods = 1 }
                     (M.fromList [ (FClickMethod, VUInt 2) ])) == [ FClickMethod ] )

  , ( "a double already at its value is not re-sent"
    , toSend (reconcile allReady S.empty touchpadSnapshot (M.fromList [ (FAccelSpeed, VDouble 0) ]))
        == [ (FScrollFactor, VDouble 1) ] )

  , ( "validation: accel speed, rotation and scroll factor ranges"
    , isLeft (validateInputConfig [rule touchpads defaultInputSettings { accelSpeed = Just 1.5 }])
        && isLeft (validateInputConfig [rule touchpads defaultInputSettings { rotationAngle = Just 360 }])
        && isLeft (validateInputConfig [rule touchpads defaultInputSettings { scrollFactor = Just (-0.5) }])
        && isLeft (validateInputConfig [rule touchpads defaultInputSettings { scrollFactor = Just (1 / 0) }])
        && isLeft (validateInputConfig [rule touchpads defaultInputSettings { scrollFactor = Just 1e9 }])
        && not (isLeft (validateInputConfig [rule touchpads defaultInputSettings { scrollFactor = Just 0.5, accelSpeed = Just (-1) }])) )

  , ( "validation names the rule"
    , either (== "rule 2: accelSpeed is outside [-1, 1]") (const False)
        (validateInputConfig [ rule touchpads defaultInputSettings
                             , rule touchpads defaultInputSettings { accelSpeed = Just 2 } ]) )

  , ( "wire: fixed 0.5 is 128, a double is eight bytes, six floats are 24"
    , runEncoded (argFixed 0.5) == BS.pack [128, 0, 0, 0]
        && BS.length (doubleBytes 0.25) == 8
        && bytesDouble (doubleBytes 0.25) == Just 0.25
        && bytesDouble (BS.replicate 4 0) == Nothing
        && BS.length (floatsBytes [1, 0, 0, 0, 1, 0]) == 24
        && bytesFloats (floatsBytes [1, 0, 0, 0, 1, 0]) == Just [1, 0, 0, 0, 1, 0]
        && bytesFloats (BS.replicate 5 0) == Nothing )
  ]
  where
    isLeft = either (const True) (const False)

--------------------------------------------------------------------------------
-- The fake compositor

-- | Wire opcodes, in XML order.
opInputDevice, opLibinputDevice, opXkbKeyboard :: Word16
opXkbKeyboard = 1

opKbInputDevice, opKbLayout, opKbDone :: Word16
opKbInputDevice = 1
opKbLayout = 2
opKbDone = 7

reqSetLayoutByIndex, reqSetLayoutByName, reqSetKeymap, reqCreateKeymap :: Word16
reqSetKeymap = 1
reqSetLayoutByIndex = 2
reqSetLayoutByName = 3
reqCreateKeymap = 2

opKeymapSuccess, opKeymapFailure :: Word16
opKeymapSuccess = 0
opKeymapFailure = 1

opInputDevice = 1
opLibinputDevice = 1

opDevRemoved, opDevType, opDevName, opDevDone :: Word16
opDevRemoved = 0
opDevType = 1
opDevName = 2
opDevDone = 3

opLibRemoved, opLibInputDevice, opLibTapSupport, opLibTapDefault, opLibTapCurrent
  , opLibAccelProfilesSupport, opLibAccelProfileDefault, opLibAccelProfileCurrent
  , opLibAccelSpeedDefault, opLibAccelSpeedCurrent
  , opLibClickMethodSupport, opLibClickMethodDefault, opLibClickMethodCurrent
  , opLibDwtSupport, opLibDwtDefault, opLibDwtCurrent, opLibDone :: Word16
opLibRemoved = 0
opLibInputDevice = 1
opLibTapSupport = 5
opLibTapDefault = 6
opLibTapCurrent = 7
opLibAccelProfilesSupport = 20
opLibAccelProfileDefault = 21
opLibAccelProfileCurrent = 22
opLibAccelSpeedDefault = 23
opLibAccelSpeedCurrent = 24
opLibClickMethodSupport = 31
opLibClickMethodDefault = 32
opLibClickMethodCurrent = 33
opLibDwtSupport = 46
opLibDwtDefault = 47
opLibDwtCurrent = 48
opLibDone = 55

reqDevDestroy, reqSetScrollFactor, reqSetRepeatInfo, reqMapToOutput, reqLibDestroy, reqSetTap, reqSetAccelSpeed
  , reqSetClickMethod, reqBind, reqSync :: Word16
reqDevDestroy = 0
reqSetRepeatInfo = 2
reqSetScrollFactor = 3
reqMapToOutput = 4
reqLibDestroy = 0
reqSetTap = 2
reqSetAccelSpeed = 9
reqSetClickMethod = 13
reqBind = 0
reqSync = 0

resSuccess, resUnsupported :: Word16
resSuccess = 0
resUnsupported = 1

registry :: ObjectId
registry = ObjectId 2

-- | The globals are allocated through the connection, as bound ones would
-- be: a fixed id would collide with a sync callback's.
data Fake = Fake
  { fkConn    :: Connection
  , fkPeer    :: N.Socket
  , fkManager :: ObjectId
  , fkConfig  :: ObjectId
  , fkXkb     :: ObjectId
  , fkRuntime :: InputRuntime
  , fkWarned  :: IORef [String]
  , fkLayouts :: IORef [Maybe (Int, String)]
  , fkOutputs :: IORef (M.Map ByteString ObjectId)
  , fkBuffer  :: IORef ByteString
  }

type Req = (ObjectId, Word16, ByteString)

newFake :: IO Fake
newFake = do
  (a, b) <- N.socketPair N.AF_UNIX N.Stream N.defaultProtocol
  conn <- connectSocket a
  im <- newObject conn
  lc <- newObject conn
  xk <- newObject conn
  warned <- newIORef []
  layouts <- newIORef []
  outputs <- newIORef M.empty
  rt <- newInputRuntime conn (\m -> modifyIORef' warned (m :)) (\l -> modifyIORef' layouts (l :))
          (\n -> M.lookup n <$> readIORef outputs) (Just im) (Just lc) (Just xk)
  Fake conn b im lc xk rt warned layouts outputs <$> newIORef BS.empty

-- | A server event, queued; 'settle' delivers.
event :: Fake -> ObjectId -> Word16 -> Encoded -> IO ()
event fk oid op args = NBS.sendAll (fkPeer fk) (runEncoded (encodeMessage oid op args))

deleteId :: Fake -> ObjectId -> IO ()
deleteId fk oid = event fk (ObjectId 1) 1 (argUInt (unObjectId oid))

-- | Deliver everything queued, answer every sync, and collect the requests
-- that went out, until the runtime is quiet.
settle :: Fake -> IO [Req]
settle fk = go True []
  where
    -- A round: a marker sync, the requests before it, every sync answered,
    -- then the queued events delivered.  Quiet is a round after the first
    -- with nothing.
    go first acc = do
      marker <- newObject conn
      request conn (ObjectId 1) reqSync (argObject marker)
      flush conn
      reqs <- readUntil marker
      let (syncs, rest) = partitionSyncs reqs
      forM_ (marker : syncs) $ \cb -> do
        event fk cb 0 (argUInt 0)
        deleteId fk cb
      dispatch conn
      if null syncs && null rest && not first then pure acc else go False (acc ++ rest)
    conn = fkConn fk
    partitionSyncs reqs =
      ( [ ObjectId cb | (ObjectId 1, op, body) <- reqs, op == reqSync
                      , Right cb <- [decodeBody getWord32 body] ]
      , [ r | r@(oid, _, _) <- reqs, oid /= ObjectId 1 ] )
    readUntil marker = do
      buf <- readIORef (fkBuffer fk)
      let (msgs, _) = splitMessages buf
          isMarker (h, body) =
            msgObject h == ObjectId 1 && msgOpcode h == reqSync
              && decodeBody getWord32 body == Right (unObjectId marker)
      case break isMarker msgs of
        (before, m : _) -> do
          writeIORef (fkBuffer fk) (BS.drop (sum (map (msgSize . fst) (before ++ [m]))) buf)
          pure [ (msgObject h, msgOpcode h, body) | (h, body) <- before ]
        _ -> do
          more <- NBS.recv (fkPeer fk) 65536
          writeIORef (fkBuffer fk) (buf <> more)
          readUntil marker

-- | Run one manage-side install: what the worker's op would do.
install :: Fake -> [InputRule] -> IO [Req]
install fk rules = installInputConfig (fkRuntime fk) (config rules) >> settle fk

-- | Announce a device: type, name, done.
announceDevice :: Fake -> ObjectId -> Word32 -> ByteString -> IO ()
announceDevice fk dev ty name = do
  event fk (fkManager fk) opInputDevice (argObject dev)
  event fk dev opDevType (argUInt ty)
  event fk dev opDevName (argString (Just name))
  event fk dev opDevDone mempty

-- | Announce the libinput half of a touchpad, tap off, clickfinger
-- available but button areas current, dwt on.
announceTouchpadLib :: Fake -> ObjectId -> ObjectId -> IO ()
announceTouchpadLib fk lib dev = do
  event fk (fkConfig fk) opLibinputDevice (argObject lib)
  event fk lib opLibInputDevice (argObject dev)
  event fk lib opLibTapSupport (argInt 2)
  event fk lib opLibTapDefault (argUInt 0)
  event fk lib opLibTapCurrent (argUInt 0)
  event fk lib opLibAccelProfilesSupport (argUInt 3)
  event fk lib opLibAccelProfileDefault (argUInt 2)
  event fk lib opLibAccelProfileCurrent (argUInt 2)
  event fk lib opLibAccelSpeedDefault (argArray (doubleBytes 0))
  event fk lib opLibAccelSpeedCurrent (argArray (doubleBytes 0))
  event fk lib opLibClickMethodSupport (argUInt 3)
  event fk lib opLibClickMethodDefault (argUInt 1)
  event fk lib opLibClickMethodCurrent (argUInt 1)
  event fk lib opLibDwtSupport (argInt 1)
  event fk lib opLibDwtDefault (argUInt 1)
  event fk lib opLibDwtCurrent (argUInt 1)
  event fk lib opLibDone mempty

-- | Answer a setter as river does: the result, the broadcast, done.
answer :: Fake -> Req -> Word16 -> Word16 -> IO ()
answer fk (lib, _, body) res currentOp =
  case decodeBody ((,) <$> getObject <*> getWord32) body of
    Left _ -> pure ()
    Right (result, value) -> do
      event fk result res mempty
      deleteId fk result
      when (res == resSuccess) $ do
        event fk lib currentOp (argUInt value)
        event fk lib opLibDone mempty

-- | The setters among the requests: what has a result to answer.
setters :: ObjectId -> [Req] -> [Req]
setters lib = filter (\(o, _, _) -> o == lib)

ofKind :: ObjectId -> Word16 -> [Req] -> [Req]
ofKind oid op = filter (\(o, p, _) -> o == oid && p == op)

dev1, lib1, dev2 :: ObjectId
dev1 = ObjectId 0xff000001
lib1 = ObjectId 0xff000002
dev2 = ObjectId 0xff000003

pointerType, keyboardType :: Word32
keyboardType = 0
pointerType = 1

touchpadRules :: [InputRule]
touchpadRules =
  [ rule touchpads defaultInputSettings
      { tap = Just True, clickMethod = Just ClickFinger, disableWhileTyping = Just True } ]

harnessTests :: (String -> Bool -> IO ()) -> IO ()
harnessTests check = do
  -- Device before config: the snapshot lands, nothing is sent until rules are.
  do fk <- newFake
     announceDevice fk dev1 pointerType "Touchpad"
     announceTouchpadLib fk lib1 dev1
     quiet <- settle fk
     check "device before config: nothing is sent" $ null quiet
     sent <- install fk touchpadRules
     check "install sends tap, click method, scroll factor 1.0 and no mapping; not dwt, already on" $
       map (\(o, p, _) -> (o, p)) sent
         == [ (lib1, reqSetTap), (lib1, reqSetClickMethod), (dev1, reqSetScrollFactor), (dev1, reqMapToOutput) ]
     check "set_tap carries a client new_id then the enum" $
       case sent of
         ((_, _, body) : _) -> case decodeBody ((,) <$> getObject <*> getWord32) body of
           Right (ObjectId r, 1) -> r < 0xff000000
           _ -> False
         _ -> False
     forM_ (setters lib1 sent) $ \r@(_, p, _) -> answer fk r resSuccess (if p == reqSetTap then opLibTapCurrent else opLibClickMethodCurrent)
     after <- settle fk
     check "results confirmed, the broadcast done reconciles to nothing" $ null after
     n <- inputPendingCount (fkRuntime fk)
     check "no result left pending" $ n == 0
     -- Replace the config: the fields go back to their defaults.
     reverted <- install fk []
     check "an empty config restores tap and click method to their defaults" $
       [ (o, p, decodeBody ((,) <$> getObject <*> getWord32) b) | (o, p, b) <- reverted ]
         `matchesValues` [ (lib1, reqSetTap, 0), (lib1, reqSetClickMethod, 1) ]

  -- Config before device: it is applied when the device becomes ready.
  do fk <- newFake
     _ <- install fk touchpadRules
     announceDevice fk dev1 pointerType "Touchpad"
     announceTouchpadLib fk lib1 dev1
     sent <- settle fk
     check "config before device: applied at readiness, the device's own fields included" $
       map (\(o, p, _) -> (o, p)) sent
         == [ (lib1, reqSetTap), (lib1, reqSetClickMethod), (dev1, reqSetScrollFactor), (dev1, reqMapToOutput) ]

  -- A keyboard and a libinput-less mouse: the touchpad rule leaves them alone.
  do fk <- newFake
     _ <- install fk touchpadRules
     announceDevice fk dev1 keyboardType "Keyboard"
     announceDevice fk dev2 pointerType "Virtual Mouse"
     sent <- settle fk
     check "a keyboard gets the repeat default; a pointer without libinput its own fields after the sync" $
       sent == [ (dev1, reqSetRepeatInfo, runEncoded (argInt 25 <> argInt 600))
               , (dev2, reqSetScrollFactor, runEncoded (argFixed 1))
               , (dev2, reqMapToOutput, runEncoded (argObject nullObject)) ]

  -- Output mapping waits for the output; repeat is a keyboard's.
  do fk <- newFake
     _ <- install fk [ rule defaultInputMatch { matchType = Just Touch } defaultInputSettings { mapToOutput = Just "eDP-1" }
                     , rule defaultInputMatch { matchType = Just Keyboard } defaultInputSettings { keyRepeat = Just (30, 250) } ]
     let touch = ObjectId 0xff000005
         eDP = ObjectId 0xff000020
     announceDevice fk dev1 keyboardType "Keyboard"
     announceDevice fk touch 2 "Touchscreen"
     sent <- settle fk
     check "repeat is sent; a mapping to an unknown output waits" $
       sent == [ (dev1, reqSetRepeatInfo, runEncoded (argInt 30 <> argInt 250)) ]
     writeIORef (fkOutputs fk) (M.fromList [ ("eDP-1", eDP) ])
     outputsChanged (fkRuntime fk)
     mapped <- settle fk
     check "the mapping is sent once the output is there" $
       mapped == [ (touch, reqMapToOutput, runEncoded (argObject eDP)) ]
     outputsChanged (fkRuntime fk)
     again <- settle fk
     check "an output change re-sends the mapping, nothing else" $
       again == [ (touch, reqMapToOutput, runEncoded (argObject eDP)) ]
     cleared <- install fk []
     check "removing the rules clears the mapping and restores the repeat default" $
       cleared == [ (dev1, reqSetRepeatInfo, runEncoded (argInt 25 <> argInt 600))
                  , (touch, reqMapToOutput, runEncoded (argObject nullObject)) ]

  -- Scroll factor and acceleration speed on the wire.
  do fk <- newFake
     _ <- install fk [ rule touchpads defaultInputSettings { scrollFactor = Just 0.5, accelSpeed = Just (-0.25) } ]
     announceDevice fk dev1 pointerType "Touchpad"
     announceTouchpadLib fk lib1 dev1
     sent <- settle fk
     check "scroll factor 0.5 is fixed 128; accel speed is one double in an array" $
       ofKind dev1 reqSetScrollFactor sent == [ (dev1, reqSetScrollFactor, BS.pack [128, 0, 0, 0]) ]
         && case ofKind lib1 reqSetAccelSpeed sent of
              [(_, _, body)] -> case decodeBody ((,) <$> getObject <*> getArray) body of
                Right (_, arr) -> bytesDouble arr == Just (-0.25)
                _ -> False
              _ -> False

  -- A stale result: the generation that skipped the field gets its turn.
  do fk <- newFake
     announceDevice fk dev1 pointerType "Touchpad"
     announceTouchpadLib fk lib1 dev1
     _ <- settle fk
     first <- install fk touchpadRules
     second <- install fk []
     check "a field in flight is not re-sent by the next generation" $
       null (ofKind lib1 reqSetTap second) && null (ofKind lib1 reqSetClickMethod second)
     forM_ (setters lib1 first) $ \r@(_, p, _) -> answer fk r resSuccess (if p == reqSetTap then opLibTapCurrent else opLibClickMethodCurrent)
     late <- settle fk
     check "the stale results trigger the newer generation's sends" $
       [ (o, p, decodeBody ((,) <$> getObject <*> getWord32) b) | (o, p, b) <- late ]
         `matchesValues` [ (lib1, reqSetTap, 0), (lib1, reqSetClickMethod, 1) ]

  -- Removal with results pending.
  do fk <- newFake
     announceDevice fk dev1 pointerType "Touchpad"
     announceTouchpadLib fk lib1 dev1
     _ <- settle fk
     sent <- install fk touchpadRules
     n0 <- inputPendingCount (fkRuntime fk)
     event fk dev1 opDevRemoved mempty
     event fk lib1 opLibRemoved mempty
     reqs <- settle fk
     n1 <- inputPendingCount (fkRuntime fk)
     d <- inputDeviceCount (fkRuntime fk)
     check "removal drops the pending results and destroys both objects" $
       n0 == length (setters lib1 sent) && n1 == 0 && d == 0
         && ofKind dev1 reqDevDestroy reqs == [ (dev1, reqDevDestroy, "") ]
         && ofKind lib1 reqLibDestroy reqs == [ (lib1, reqLibDestroy, "") ]
     -- A result arriving anyway names an object with no listener.
     forM_ (setters lib1 sent) $ \r -> answer fk r resSuccess opLibTapCurrent
     ignored <- settle fk
     check "a late result for a removed device is ignored" $ null ignored

  -- Unsupported: diagnosed once per generation, never sent.
  do fk <- newFake
     announceDevice fk dev1 pointerType "Touchpad"
     announceTouchpadLib fk lib1 dev1
     _ <- settle fk
     sent <- install fk [ rule touchpads defaultInputSettings { rotationAngle = Just 90 } ]
     -- Two more dones, as broadcasts from elsewhere would be.
     event fk lib1 opLibDone mempty
     event fk lib1 opLibDone mempty
     more <- settle fk
     warned <- readIORef (fkWarned fk)
     check "an unsupported field is never sent and diagnosed once" $
       null (filter (\(o, _, _) -> o == lib1) (sent ++ more)) && length warned == 1
     -- The device says no: same treatment.
     sent2 <- install fk touchpadRules
     forM_ (setters lib1 sent2) $ \r@(_, p, _) -> answer fk r (if p == reqSetTap then resUnsupported else resSuccess) opLibClickMethodCurrent
     event fk lib1 opLibDone mempty
     more2 <- settle fk
     warned2 <- readIORef (fkWarned fk)
     check "a refused result is diagnosed once and not retried in its generation" $
       null (ofKind lib1 reqSetTap more2) && length warned2 == 2

  -- Only version 2 of both globals; nothing half-bound.
  do (a, b) <- N.socketPair N.AF_UNIX N.Stream N.defaultProtocol
     conn <- connectSocket a
     let globalsV1 = [ Global 10 "river_input_manager_v1" 1, Global 11 "river_libinput_config_v1" 2 ]
         globalsHalf = [ Global 10 "river_input_manager_v1" 2, Global 11 "river_libinput_config_v1" 1 ]
     _ <- bindInput conn registry globalsV1 (const (pure ())) (const (pure Nothing))
     flush conn
     _ <- bindInput conn registry globalsHalf (const (pure ())) (const (pure Nothing))
     flush conn
     bytes <- NBS.recv b 65536
     let (msgs, _) = splitMessages bytes
         reqs = [ (msgObject h, msgOpcode h) | (h, _) <- msgs ]
     check "v1 input manager: nothing bound; v1 libinput: only the manager is bound" $
       reqs == [ (registry, reqBind) ]

  -- Keyboards: the active layout is reported; next wraps by a sync.
  do fk <- newFake
     let kb1 = ObjectId 0xff000010
     announceDevice fk dev1 keyboardType "Keyboard"
     event fk (fkXkb fk) opXkbKeyboard (argObject kb1)
     event fk kb1 opKbInputDevice (argObject dev1)
     event fk kb1 opKbLayout (argUInt 1 <> argString (Just "German"))
     event fk kb1 opKbDone mempty
     _ <- settle fk
     reported <- readIORef (fkLayouts fk)
     check "a keyboard's layout is reported" $ reported == [ Just (1, "German") ]
     setKeyboardLayout (fkRuntime fk) KeyboardLayoutNext
     wrapped <- settle fk
     check "next past the last layout wraps to 0" $
       [ (o, p, b) | (o, p, b) <- wrapped, o == kb1 ]
         == [ (kb1, reqSetLayoutByIndex, runEncoded (argInt 2)), (kb1, reqSetLayoutByIndex, runEncoded (argInt 0)) ]
     setKeyboardLayout (fkRuntime fk) KeyboardLayoutNext
     event fk kb1 opKbLayout (argUInt 2 <> argString (Just "French"))
     moved <- settle fk
     reported2 <- readIORef (fkLayouts fk)
     check "next that lands does not wrap, and is reported" $
       [ (o, p, b) | (o, p, b) <- moved, o == kb1 ] == [ (kb1, reqSetLayoutByIndex, runEncoded (argInt 2)) ]
         && take 1 reported2 == [ Just (2, "French") ]
     setKeyboardLayout (fkRuntime fk) (KeyboardLayoutName "de")
     named <- settle fk
     check "a layout by name" $
       named == [ (kb1, reqSetLayoutByName, runEncoded (argString (Just "de"))) ]
     -- A keymap: created from a memfd, set on every keyboard once accepted,
     -- with the layout index put back; keyboards to come get it at done.
     setKeymap (fkRuntime fk) "xkb_keymap { }"
     created <- settle fk
     km <- case created of
       [(o, p, body)] | o == fkXkb fk && p == reqCreateKeymap
                      , Right (i, 1) <- decodeBody ((,) <$> getObject <*> getWord32) body -> pure i
       _ -> pure (ObjectId 0)
     check "create_keymap carries a new id and the text_v1 format" $ km /= ObjectId 0
     event fk km opKeymapSuccess mempty
     applied <- settle fk
     check "on success the keymap is set and the layout index restored" $
       applied == [ (kb1, reqSetKeymap, runEncoded (argObject km)), (kb1, reqSetLayoutByIndex, runEncoded (argInt 2)) ]
     let kb2 = ObjectId 0xff000011
     event fk (fkXkb fk) opXkbKeyboard (argObject kb2)
     event fk kb2 opKbLayout (argUInt 0 <> argString (Just "English"))
     event fk kb2 opKbDone mempty
     late <- settle fk
     check "a keyboard arriving later gets the keymap at done, index 0 left alone" $
       late == [ (kb2, reqSetKeymap, runEncoded (argObject km)) ]
     setKeymap (fkRuntime fk) "xkb_keymap { broken"
     created2 <- settle fk
     case created2 of
       [(_, _, body)] | Right (i, _) <- decodeBody ((,) <$> getObject <*> getWord32) body ->
         event fk i opKeymapFailure (argString (Just "syntax"))
       _ -> pure ()
     rejected <- settle fk
     warned <- readIORef (fkWarned fk)
     check "a rejected keymap is destroyed and reported; keyboards keep the old one" $
       length (filter (\(_, p, _) -> p == reqSetKeymap) rejected) == 0
         && any (\m -> take 15 m == "keymap rejected") warned

-- | Compare sends by object, opcode and value, ignoring the result ids.
matchesValues :: [(ObjectId, Word16, Either e (ObjectId, Word32))] -> [(ObjectId, Word16, Word32)] -> Bool
matchesValues actual expected =
  [ (o, p, v) | (o, p, Right (_, v)) <- actual ] == expected
