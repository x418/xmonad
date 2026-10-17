{-# LANGUAGE LambdaCase #-}
-- | A window manager that grants fullscreen with 'setFullscreen', the way
-- "XMonad.Hooks.EwmhDesktops" does, and then on a timer takes it back, gives
-- it again and closes the window while it is fullscreen.
--
-- A window manager rather than a client, for the reason
-- @tests\/river-submap-spec.hs@ gives: the code under test is the window
-- manager's.  @tests\/headless-fullscreen.sh@ starts a client that asks for
-- fullscreen, and asserts on river's protocol log and the log this writes.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.IORef (readIORef)
import Data.Monoid (All (..))
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO
import qualified Data.Set as S

import XMonad
import XMonad.River (postAction, setFullscreen)
import XMonad.River.State (riverFullscreen)
import qualified XMonad.StackSet as W

logPathEnv :: String
logPathEnv = "XMONAD_FULLSCREEN_LOG"

note :: FilePath -> String -> X ()
note path msg = io $ do
  appendFile path (msg ++ "\n")
  hPutStrLn stderr ("river-fullscreen-wm: " ++ msg)

main :: IO ()
main = do
  path <- lookupEnv logPathEnv >>= \case
    Just p  -> pure p
    Nothing -> hPutStrLn stderr
      ("river-fullscreen-wm: " ++ logPathEnv ++ " is not set") >> exitFailure
  writeFile path ""
  dirs <- getDirectories
  launch (testConfig path) dirs

testConfig :: FilePath -> XConfig (Choose Tall (Choose (Mirror Tall) Full))
testConfig path = def
  { handleEventHook = events path
  , startupHook = ask >>= io . void . forkIO . drive path
  }

events :: FilePath -> Event -> X All
events path = \case
  WindowFullscreenChanged w full -> do
    note path ("requested " ++ show full)
    grant w full
    pure (All True)
  DestroyWindowEvent _ -> do
    note path "destroyed"
    pure (All True)
  _ -> pure (All True)

-- | Leaving floats the window over the middle quarter, so that its size out
-- of fullscreen cannot be mistaken for the output's.
grant :: Window -> Bool -> X ()
grant w full = do
  windows (W.float w (if full then W.RationalRect 0 0 1 1 else W.RationalRect 0.25 0.25 0.5 0.5))
  setFullscreen w full

-- | The schedule.  Sleeps, because nothing observable from here says when
-- the client has asked; the script's assertions fail loudly if they are short.
drive :: FilePath -> XConf -> IO ()
drive path c = do
  threadDelay (6 * second)
  postAction c $ withFocused $ \w -> note path "leave" >> grant w False
  threadDelay (3 * second)
  postAction c $ withFocused $ \w -> note path "enter" >> grant w True
  threadDelay (3 * second)
  postAction c $ note path "close" >> kill
  threadDelay (3 * second)
  postAction c $ do
    granted <- io . readIORef =<< asks (riverFullscreen . riverState)
    note path ("granted " ++ show (S.size granted))
  where
    second = 1000 * 1000
