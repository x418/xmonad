--------------------------------------------------------------------
-- |
-- Module    : XMonad
-- Copyright : (c) Don Stewart
-- License   : BSD3
--
-- Useful exports for configuration files.
--
-- This is the river backend's copy, shadowing @src\/XMonad.hs@.
--
-- The one difference is the important one: it does __not__ re-export
-- @Graphics.X11@ or @Graphics.X11.Xlib.Extras@.  The X11 build re-exports
-- 1458 names from those two modules -- keysyms, masks, geometry types, and
-- several hundred Xlib calls.  Reproducing that surface over Wayland would
-- mean several hundred functions that typecheck and do nothing, and a config
-- that compiles is then a config that looks correct while silently failing.
--
-- The rule for this backend is the opposite: anything that cannot be
-- faithfully ported is not exported, so a config or contrib module reaching
-- for it fails at the call site, at compile time, naming the file and line.
-- What that costs is recorded name by name in tests\/api\/unportable.txt, and
-- tests\/api\/check-api.sh fails if the set drifts without justification.
--
--------------------------------------------------------------------

module XMonad (

    module XMonad.Main,
    module XMonad.Core,
    module XMonad.Config,
    module XMonad.Layout,
    module XMonad.ManageHook,
    module XMonad.Operations,
    (.|.),
    MonadState(..), gets, modify,
    MonadReader(..), asks,
    MonadIO(..)

 ) where

-- core modules
import XMonad.Main
import XMonad.Core
import XMonad.Config
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Operations
-- import XMonad.StackSet -- conflicts with 'workspaces' defined in XMonad.hs

-- modules needed to get basic configuration working
import Data.Bits

import Control.Monad.State
import Control.Monad.Reader
