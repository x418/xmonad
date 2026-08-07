{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Main
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- The entry point, shadowing @src\/XMonad\/Main.hs@.
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
import System.Environment (getArgs, getProgName, withArgs)
import System.Exit (exitFailure)
import System.IO
import System.Info

import Paths_xmonad (version)
import Data.Version (showVersion)

import XMonad.Core
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
        ["--restart"]         -> sendRestart
        ["--version"]         -> putStrLn $ unwords shortVersion
        ["--verbose-version"] -> putStrLn . unwords $ shortVersion ++ longVersion
        _                     -> launch' args
 where
    shortVersion = ["xmonad", showVersion version]
    longVersion  = [ "compiled by", compilerName, showVersion compilerVersion
                   , "for",  arch ++ "-" ++ os
                   , "\nBackend: river (river-window-management-v1)" ]

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
