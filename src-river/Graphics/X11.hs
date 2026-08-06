-- | Compatibility shim, internal to this package.
--
-- River does /not/ re-export @Graphics.X11@ as part of its API: almost nothing
-- that name promises can be faithfully provided over Wayland, and a name that
-- is present but inert is worse than one that is absent -- absent fails at the
-- call site, at compile time, naming the file and line.
--
-- This module exists for exactly one reason: @src\/XMonad\/Layout.hs@ opens
-- with @import Graphics.X11 (Rectangle(..))@, and that file is 258 lines of
-- layout arithmetic both backends want.  Sharing it costs this shim; vendoring
-- it costs a second copy of every geometry bug.  The shim is cheaper.
--
-- 'Rectangle' is one of the few X11 types that ports without compromise: four
-- numbers describing a screen region, and river's coordinate space means what
-- X11's did.
module Graphics.X11
  ( Rectangle(..)
  , Position
  , Dimension
  , Window
  ) where

import XMonad.River.Types (Dimension, Position, Rectangle (..), Window)
