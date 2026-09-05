{-# LANGUAGE ScopedTypeVariables #-}

-- | Drive 'XMonad.River.Client.startClient' against a live compositor.
--
-- Run by @tests/headless-prompt.sh@, which supplies the river, starts a window
-- manager beside it and reads what this prints.  This process is an ordinary
-- Wayland client: it drives the client half of a prompt directly, with no
-- xmonad in it, because everything it checks is about the layer surface a
-- prompt puts up.  (The window manager still has to exist next door -- river
-- closes a layer surface whose namespace no window manager has claimed.)
--
-- The guarantee under test is the one in the module header of
-- "XMonad.River.Client": a surface that asks for exclusive keyboard
-- interactivity must never outlive the thread that services it, and must not be
-- taken away from someone who is using it.  That is worth a test rather than a
-- reading of the code, because every path through that code /looked/ like it
-- terminated on the day the bug was there.
--
-- Each case prints PASS or FAIL with a name; a single failure fails the run.
module Main (main) where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, threadDelay,
                           tryPutMVar, tryTakeMVar)
import Control.Monad (forM, void)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout (timeout)
import qualified Control.Exception as E

import Data.Bits ((.&.))
import Data.IORef (newIORef, readIORef, writeIORef)

import XMonad.River.Client
import XMonad.River.Protocol.Core (WlKeyboardEvent (WlKeyboardKeymap),
                                   WlSeatEvent (WlSeatCapabilities),
                                   wlKeyboardListen, wlSeatCapabilityKeyboard,
                                   wlSeatGetKeyboard, wlSeatInterface,
                                   wlSeatListen, wlSeatVersion)
import System.IO.Error (isEOFError)
import System.Posix.IO (closeFd)
import System.Posix.IO.ByteString (fdRead)
import System.Posix.Types (Fd)
import qualified Data.ByteString as BS
import qualified XMonad.River.Connection as C

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    ["--keymap-probe"]            -> keymapProbe readFdText "pread (as shipped)"
    ["--keymap-probe-sequential"] -> keymapProbe readSequential
                                       "sequential read (as it was)"
    _                             -> runCases

-- | Read the keymap twice, on two connections, and report how many bytes each
-- got.
--
-- Safe to point at a live session: it creates no surface, takes no grab and
-- shows nothing.  A @wl_keyboard@ needs neither.  That makes it the way to
-- check the shared-offset bug on real hardware, where it actually bites --
-- a headless seat has no keyboard and cannot exhibit it at all.
--
-- Before the fix the second number is 0.  After it, both are the keymap's size.
keymapProbe :: (Fd -> Int -> IO BS.ByteString) -> String -> IO ()
keymapProbe reader label = do
  a <- keymapBytes reader
  b <- keymapBytes reader
  putStrLn ("  ....  " ++ label ++ ": first=" ++ show a ++ " second=" ++ show b)
  if a > 0 && b == a
    then putStrLn "  PASS  the second connection read the same keymap as the first"
    else do
      putStrLn "  FAIL  the second connection did not get the keymap"
      exitFailure

-- | The read this module used to do, kept so the bug can be demonstrated
-- rather than asserted.
--
-- Run it against a live compositor with @--keymap-probe-sequential@ and the
-- second number comes back 0: the first read moved the shared file offset to
-- end-of-file and there is nothing left for anyone else.
readSequential :: Fd -> Int -> IO BS.ByteString
readSequential fd size = go size BS.empty
  where
    go 0 acc = pure acc
    go n acc = do
      chunk <- E.catch (fdRead fd (fromIntegral (min n 4096)))
                       (\e -> if isEOFError e then pure BS.empty else E.throwIO e)
      if BS.null chunk then pure acc else go (n - BS.length chunk) (acc <> chunk)

runCases :: IO ()
runCases = do
  results <- forM cases $ \(name, act) -> do
    ok <- E.catch act $ \(e :: E.SomeException) -> do
      putStrLn ("  note  " ++ name ++ " raised: " ++ show e)
      pure False
    putStrLn ((if ok then "  PASS  " else "  FAIL  ") ++ name)
    pure ok
  if and results then exitSuccess else exitFailure
  where
    cases =
      [ ("a surface that wants no keyboard is left alone", noKeyboardWantedSurvives)
      , ("a prompt that can never read the keyboard is closed", unusableIsClosed)
      , ("a prompt whose draw callback throws stays alive", drawThrowsSurvives)
      , ("closeAllClients closes a prompt that is merely idle", panicCloses)
      , ("a second prompt reads the keymap as well as the first", keymapReadTwice)
      ]

-- | The false-positive guard, and the reason this file exists.
--
-- Nothing the watchdog decides may depend on anyone typing: a prompt open in
-- front of someone reading it is idle and correct, and closing it would turn a
-- rare failure into a daily annoyance.  This is the half of that which a
-- headless seat can demonstrate -- a surface that never asked for a keyboard,
-- which is exactly what a completion list is.  It has no keyboard, no focus
-- and no keymap, and none of those is a fault for it, so it must still be
-- there well past the deadline.
--
-- The other half -- an idle prompt on a seat that /does/ have a keyboard --
-- needs a keyboard capability this environment cannot provide; see the header
-- of tests/headless-prompt.sh.
noKeyboardWantedSurvives :: IO Bool
noKeyboardWantedSurvives = do
  closed <- newEmptyMVar
  h <- startClient (probe closed) { csKeyboard = False }
  -- Comfortably past startupDeadlineMicros plus its close grace period.
  threadDelay (14 * 1000 * 1000)
  stillOpen <- null <$> tryTakeMVar closed
  chClose h
  _ <- waitFor 5 closed
  pure stillOpen

-- | The watchdog doing its job.
--
-- A headless seat has no keyboard, so a surface asking for exclusive keyboard
-- interactivity on it can never read one -- and would sit there holding the
-- grab forever.  That is precisely the case the watchdog is for, so it must
-- close, and within its deadline rather than eventually.
unusableIsClosed :: IO Bool
unusableIsClosed = do
  closed <- newEmptyMVar
  _ <- startClient (probe closed)
  -- startupDeadlineMicros is 10s; allow the grace period and some slack, but
  -- not so much that "it closed for some other reason later" would pass.
  waitFor 16 closed

-- | A draw callback that throws must not take the surface with it.
--
-- @guarded@ is supposed to swallow it, so a prompt survives one bad frame
-- rather than the session losing its keyboard.  Checked the same way as the
-- case above -- the client is still there afterwards -- and then closed, which
-- a client that had already fallen over could not answer.
drawThrowsSurvives :: IO Bool
drawThrowsSurvives = do
  closed <- newEmptyMVar
  -- csKeyboard = False so that the watchdog has no opinion about it here: the
  -- question is whether a throwing draw callback kills the client, and on a
  -- headless seat a keyboard surface would be closed for an unrelated and
  -- correct reason well before this finished.
  h <- startClient (probe closed) { csKeyboard = False
                                  , csDraw = \_ -> ioError (userError "boom") }
  threadDelay (3 * 1000 * 1000)
  stillOpen <- null <$> tryTakeMVar closed
  chClose h
  answered <- waitFor 5 closed
  pure (stillOpen && answered)

-- | The escape hatch, fired once so that it is not a guess.
--
-- 'closeAllClients' is what a config binds for a prompt that has stopped
-- answering.  An idle prompt is not wedged, but it is indistinguishable from
-- one to this function, which is the point: it kills the thread rather than
-- asking, and the handler around the loop does the rest.
panicCloses :: IO Bool
panicCloses = do
  closed <- newEmptyMVar
  -- Again no keyboard, so that what closes it is unambiguously the kill and
  -- not the watchdog.
  _ <- startClient (probe closed) { csKeyboard = False }
  threadDelay (3 * 1000 * 1000)
  n <- closeAllClients
  answered <- waitFor 5 closed
  pure (answered && n >= 1)

-- | The keymap must survive being asked for twice.
--
-- wlroots hands every client the same read-only descriptor for the keymap --
-- @seat_client_send_keymap@ sends @keyboard->keymap_fd@ itself -- and passing a
-- descriptor over a Wayland socket shares the open file description, and so the
-- file offset.  A client that reads it sequentially therefore leaves that offset
-- at end-of-file, and every later client reads nothing: the first prompt of a
-- session works and no other one ever does, for the rest of the session.
--
-- This is why 'XMonad.River.Client.readFdText' uses @pread@.  Two prompts in
-- succession, and the second must be as usable as the first.
--
-- On a seat with no keyboard there is no keymap to race over, so this reports
-- itself skipped rather than passing vacuously -- which is what it would
-- otherwise do, on exactly the machine that cannot catch the bug.
keymapReadTwice :: IO Bool
keymapReadTwice = do
  first  <- promptBecameUsable
  case first of
    Nothing -> do
      putStrLn "  skip  no keyboard on this seat; cannot race the keymap fd"
      pure True
    Just False -> pure False
    Just True  -> fromMaybe False <$> promptBecameUsable

-- | Open a prompt and report whether it was still usable after the watchdog's
-- deadline: 'Nothing' if the seat has no keyboard at all, so the caller can
-- tell "not applicable" from "failed".
promptBecameUsable :: IO (Maybe Bool)
promptBecameUsable = do
  closed <- newEmptyMVar
  h <- startClient (probe closed)
  -- Past startupDeadlineMicros and its grace period: if the keymap did not
  -- arrive, the watchdog will have closed this by now and said so.
  threadDelay (12 * 1000 * 1000)
  shut <- not . null <$> tryTakeMVar closed
  chClose h
  _ <- waitFor 5 closed
  seat <- seatHasKeyboard
  pure (if seat then Just (not shut) else Nothing)

-- | Whether this seat offers a keyboard.
--
-- Asked rather than assumed, on a throwaway connection of its own.  The point
-- is to tell "this machine cannot exhibit the bug" apart from "the bug is
-- back", and a stub returning 'True' would report the second on every headless
-- run -- or, returning 'False', would quietly skip the test on real hardware,
-- which is worse.
seatHasKeyboard :: IO Bool
seatHasKeyboard = do
  conn <- C.connect
  (registry, globals) <- C.getRegistry conn
  bound <- C.bindGlobal conn registry globals wlSeatInterface 1 wlSeatVersion
  case bound of
    Nothing -> C.disconnect conn >> pure False
    Just (seat, _) -> do
      caps <- newIORef 0
      wlSeatListen conn seat $ \ev -> case ev of
        WlSeatCapabilities c -> writeIORef caps c
        _                    -> pure ()
      C.roundtrip conn
      c <- readIORef caps
      C.disconnect conn
      pure (c .&. wlSeatCapabilityKeyboard /= 0)

-- | How many bytes of keymap a fresh connection manages to read.
--
-- Calls the shipped reader, because "does a second prompt get the keymap" is
-- exactly the question, and the answer depends on how it reads.
keymapBytes :: (Fd -> Int -> IO BS.ByteString) -> IO Int
keymapBytes reader = do
  conn <- C.connect
  (registry, globals) <- C.getRegistry conn
  bound <- C.bindGlobal conn registry globals wlSeatInterface 1 wlSeatVersion
  case bound of
    Nothing -> C.disconnect conn >> pure 0
    Just (seat, _) -> do
      got <- newIORef 0
      wlSeatListen conn seat $ \ev -> case ev of
        WlSeatCapabilities c | c .&. wlSeatCapabilityKeyboard /= 0 -> do
          kb <- wlSeatGetKeyboard conn seat
          wlKeyboardListen conn kb $ \kev -> case kev of
            WlKeyboardKeymap _fmt fd size -> do
              text <- reader fd (fromIntegral size)
              closeFd fd
              writeIORef got (BS.length text)
            _ -> pure ()
        _ -> pure ()
      C.roundtrip conn
      C.roundtrip conn
      n <- readIORef got
      C.disconnect conn
      pure n

-- | A prompt-shaped client: exclusive keyboard, nothing painted.
--
-- The buffer is shared memory the compositor zeroed, so drawing nothing gives
-- a transparent surface -- which is a perfectly good surface.  Nothing here
-- looks at pixels.
probe :: MVar () -> ClientSpec
probe closed = ClientSpec
  { csWidth    = 400
  , csHeight   = 40
  , csAnchor   = AnchorTop
  , csMargin   = (0, 0, 0, 0)
  , csKeyboard = True
  , csDraw     = \_ -> pure ()
  , csOnKey    = \_ _ _ -> pure ()
  , csOnClose  = void (tryPutMVar closed ())
  }

-- | Wait up to @secs@ for the client to report that it has closed.
waitFor :: Int -> MVar () -> IO Bool
waitFor secs v = maybe False (const True) <$> timeout (secs * 1000 * 1000) (takeMVar v)
