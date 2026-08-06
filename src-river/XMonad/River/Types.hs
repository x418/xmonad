-- | The vocabulary shared between the river backend and the modules it shares
-- with the X11 backend.
--
-- Three of these names -- 'Rectangle', 'Window' and 'Event' -- are re-exported
-- under their X11 spellings by the "Graphics.X11" and "Graphics.X11.Xlib.Extras"
-- shims, purely so that @src\/XMonad\/Layout.hs@ compiles unmodified under both
-- backends.  They are the only X11 names river reproduces, and they are
-- reproduced because they port without compromise, not for appearance's sake.
module XMonad.River.Types
  ( -- * Geometry
    Rectangle(..)
  , Position
  , Dimension
    -- * Windows
  , Window
    -- * Events
  , Event(..)
    -- * Accumulated compositor state
  , RiverWindow(..)
  , RiverOutput(..)
  , RiverSeat(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Word (Word32)

import XMonad.River.Wire (ObjectId)

--------------------------------------------------------------------------------
-- Geometry

-- | X11 used @Int16@ positions and @Word16@ dimensions; river uses @Int32@
-- throughout.  The wider types are strictly more permissive, so layout code
-- written against xmonad's 'Rectangle' still typechecks.
type Position = Int32

type Dimension = Word32

data Rectangle = Rectangle
  { rect_x      :: !Position
  , rect_y      :: !Position
  , rect_width  :: !Dimension
  , rect_height :: !Dimension
  } deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- Windows

-- | A managed window.  Under X11 this was a server-side resource id; here it
-- is the @river_window_v1@ protocol object.
--
-- The distinction matters in one place: river object ids are recycled after
-- @wl_display.delete_id@, so a 'Window' must never be retained past the
-- @closed@ event.  A window's @identifier@ event provides a genuinely unique
-- string for anything that needs to outlive the object -- which is also why
-- river does not export xmonad's state-file machinery: an id serialised now
-- means nothing to the process that reads it back.
type Window = ObjectId

--------------------------------------------------------------------------------
-- Events

-- | What the river layer surfaces to @handleEventHook@ and to layouts.
--
-- Deliberately small.  Most of what an X11 window manager learns from raw
-- events, river delivers as accumulated state on 'RiverWindow' instead, so
-- there is nothing to translate.
--
-- 'DestroyWindowEvent' keeps its X11 spelling because @src\/XMonad\/Layout.hs@
-- matches on it by name to release a layout's per-window state, and
-- @river_window_v1.closed@ means exactly what the X11 event meant.  The rest
-- are named for what river actually reports.
data Event
  = DestroyWindowEvent { ev_window :: !Window }
    -- ^ @river_window_v1.closed@: the window is gone and its id may be reused.
  | WindowAdded        { ev_window :: !Window }
  | WindowTitleChanged { ev_window :: !Window, ev_text :: !(Maybe ByteString) }
  | WindowAppIdChanged { ev_window :: !Window, ev_text :: !(Maybe ByteString) }
  | OutputAdded        { ev_output :: !ObjectId }
  | OutputRemoved      { ev_output :: !ObjectId }
  | SeatAdded          { ev_seat   :: !ObjectId }
  | SeatRemoved        { ev_seat   :: !ObjectId }
  | SessionLocked
  | SessionUnlocked
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Accumulated compositor state

-- | Everything the compositor has told us about one window.
--
-- @river_window_v1@ delivers these as separate events, accumulated here.  Note
-- that 'rwPid' is river's @unreliable_pid@: racy under PID reuse and not
-- one-to-one with windows, but exactly what manage hooks keying on a spawned
-- process already assume.
data RiverWindow = RiverWindow
  { rwObject     :: !ObjectId
  , rwNode       :: !ObjectId
  , rwAppId      :: !(Maybe ByteString)
  , rwTitle      :: !(Maybe ByteString)
  , rwPid        :: !(Maybe Int32)
  , rwIdentifier :: !(Maybe ByteString)
  , rwParent     :: !(Maybe ObjectId)
  , rwDimensions :: !(Int32, Int32)
  , rwNew        :: !Bool
  , rwClosed     :: !Bool
  , rwHidden     :: !Bool
  } deriving (Eq, Show)

data RiverOutput = RiverOutput
  { roObject   :: !ObjectId
  , roPosition :: !(Int32, Int32)
  , roSize     :: !(Int32, Int32)
  , roRemoved  :: !Bool
  } deriving (Eq, Show)

data RiverSeat = RiverSeat
  { rsObject  :: !ObjectId
  , rsRemoved :: !Bool
  } deriving (Eq, Show)
