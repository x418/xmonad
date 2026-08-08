{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Main
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, uses mtl, X11, posix
--
-- xmonad, a minimalist, tiling window manager for Wayland
--
-----------------------------------------------------------------------------
--
-- River backend notes.
--
-- The X11 version is 471 lines, most of it an event handler translating Xlib
-- events into calls on "XMonad.Operations".  There is no equivalent here: river
-- delivers window management as a protocol with a manage\/render sequence
-- rather than as a stream of events to interpret, and that state machine lives
-- in "XMonad.River.WM".  What remains in this module is argument handling and
-- startup.
--
-- @--replace@ is absent.  It has no meaning: river permits exactly one window
-- manager at a time and answers a second connection with @unavailable@, so
-- there is no selection to steal and nothing to replace.  Swapping window
-- managers is what @--restart@ does, and river keeps every client alive across
-- it.
--
-----------------------------------------------------------------------------

module XMonad.Main (xmonad, launch, sendRestart) where

import System.Locale.SetLocale
import Control.Monad (unless)
import Data.Char (isSpace)
import System.Environment (getArgs, getProgName, withArgs)
import System.Exit (exitFailure)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Info
import System.Posix.Signals (sigUSR1, signalProcess)
import qualified Control.Exception as E

import Paths_xmonad (version)
import Data.Version (showVersion)

import XMonad.Core
import XMonad.River.Runtime (pidFilePath, sendRestart)
import XMonad.River.WM (riverMain)

------------------------------------------------------------------------

-- | The entry point into xmonad.
xmonad :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
xmonad conf = do
    installSignalHandlers -- important to ignore SIGCHLD to avoid zombies

    dirs <- getDirectories
    let launch' args = do
              conf'@XConfig { layoutHook = Layout l }
                  <- handleExtraArgs conf args conf{ layoutHook = Layout (layoutHook conf) }
              withArgs [] $ launch (conf' { layoutHook = l }) dirs

    args <- getArgs
    case args of
        ["--help"]            -> usage
        ["--recompile"]       -> recompile dirs True >>= flip unless exitFailure
        ["--restart"]         -> requestRestart dirs
        ["--version"]         -> putStrLn $ unwords shortVersion
        ["--verbose-version"] -> putStrLn . unwords $ shortVersion ++ longVersion
        _                     -> launch' args
 where
    shortVersion = ["xmonad", showVersion version]
    longVersion  = [ "compiled by", compilerName, showVersion compilerVersion
                   , "for",  arch ++ "-" ++ os
                   , "\nBackend: river (river-window-management-v1)" ]

-- | Ask the running window manager to restart, from a separate process.
--
-- This is @xmonad --restart@, and it does not go through 'sendRestart': that
-- throws to /this/ process's event loop, and this process has none -- it was
-- started to deliver a message and exit.  Calling it here was a real bug, and
-- a quiet one, since the failure looked like "nothing happened".
--
-- X11 got the rendezvous for free by putting a client message on the root
-- window.  river has no equivalent channel between two window manager
-- processes, so the running one leaves its pid in 'pidFilePath' and this sends
-- it @SIGUSR1@.
--
-- A stale pid file -- from a window manager that was killed rather than asked
-- to stop -- is reported rather than silently ignored, because the thing the
-- caller wanted did not happen.
requestRestart :: Directories -> IO ()
requestRestart dirs = do
    let path = pidFilePath (dataDir dirs)
    raw <- try (readFile path)
    case raw >>= \s -> maybe (Left (userError "unparseable")) Right (readMaybe s) of
      Left _ -> die $ "xmonad-river: no running window manager found (" <> path <> ")"
      Right pid -> do
        ok <- try (signalProcess sigUSR1 (fromIntegral (pid :: Integer)))
        case ok of
          Right () -> pure ()
          Left e | isDoesNotExistError e ->
                     die $ "xmonad-river: no process " <> show pid
                         <> "; the pid file at " <> path <> " is stale"
                 | otherwise -> die $ "xmonad-river: " <> show e
  where
    try :: IO a -> IO (Either IOError a)
    try = E.try
    readMaybe s = case reads s of [(n, r)] | all isSpace r -> Just n; _ -> Nothing
    die msg = hPutStrLn stderr msg >> exitFailure

usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        [ "Usage: " <> self <> " [OPTION]"
        , "Options:"
        , "  --help                       Print this message"
        , "  --version                    Print the version number"
        , "  --recompile                  Recompile your xmonad.hs"
        , "  --restart                    Request a running xmonad process to restart"
        ]

-- | Entry point into xmonad for custom builds.
--
-- This function isn't meant to be called by the typical xmonad user
-- because it:
--
--   * Does not process any command line arguments.
--
--   * Therefore doesn't know how to restart a running xmonad.
--
--   * Does not compile your configuration file since it assumes it's
--     actually running from within your compiled configuration.
--
-- Unless you know what you are doing, you should probably be using
-- the 'xmonad' function instead.
launch :: (LayoutClass l Window, Read (l Window)) => XConfig l -> Directories -> IO ()
launch initxmc drs = do
    -- setup locale information from environment
    setLocale LC_ALL (Just "")
    -- ignore SIGPIPE and SIGCHLD
    installSignalHandlers
    hSetBuffering stdout NoBuffering
    -- Wrap the layout in an existential, so every workspace can hold a
    -- different one.  XMonad.River.WM relies on this having been done.
    let xmc = initxmc { layoutHook = Layout $ layoutHook initxmc }
    riverMain xmc drs
