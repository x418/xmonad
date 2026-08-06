-- | Compatibility shim, internal to this package.  See "Graphics.X11" for why
-- these two modules exist and why river does not export them.
--
-- @src\/XMonad\/Layout.hs@ opens with
-- @import Graphics.X11.Xlib.Extras (Event(DestroyWindowEvent))@ and matches on
-- that constructor in one place: @Choose@'s 'handleMessage', which forwards a
-- window's disappearance to both branches so a stateful layout can drop its
-- per-window bookkeeping.  @river_window_v1.closed@ means exactly that, so the
-- shared code is right as written and needs no river-specific variant.
module Graphics.X11.Xlib.Extras
  ( Event(..)
  ) where

import XMonad.River.Types (Event (..))
