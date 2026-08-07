-- | The vocabulary the river backend is built on.
--
-- Several names here -- 'Rectangle', 'Window', 'Event', 'SizeHints' -- keep
-- their X11 spellings.  That is not imitation: each is a concept river has
-- too, and keeping the name means the layout arithmetic and the manage-hook
-- algebra are the same code on both backends rather than two copies that drift.
-- Where river's version is narrower than X11's, the type says so and the
-- haddock says why.
module XMonad.River.Types
  ( -- * Geometry
    Rectangle(..)
  , BorderColor
  , parseColor, parseColorMaybe
  , Position
  , Dimension
    -- * Input
  , KeyMask
  , KeySym
  , Button
  , ButtonMask
    -- * Windows
  , Window
    -- * Events
  , Event(..)
    -- * Size hints
  , SizeHints(..)
  , noSizeHints
    -- * Window attributes
  , WindowAttributes(..)
  , waIsUnmapped, waIsUnviewable, waIsViewable
    -- * Accumulated compositor state
  , RiverWindow(..)
  , RiverOutput(..)
  , RiverSeat(..)
  , LayerFocus(..)
  , layerHasFocus
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

-- | An RGBA border colour, in the 32-bit-per-channel form
-- @river_window_v1.set_borders@ takes.
--
-- X11 stored a 'Pixel' resolved against the window's colormap.  Wayland has no
-- colormaps and no pixel values, so the honest representation is the one the
-- protocol asks for.
type BorderColor = (Word32, Word32, Word32, Word32)

-- | Parse @\"#rrggbb\"@ into the 32-bit channel values river's @set_borders@
-- takes.
--
-- 'Nothing' for anything that is not a colour, which is what lets
-- 'XMonad.Operations.setWindowBorderWithFallback' have a fallback to fall back
-- to.  'parseColor' is the total version, for the config's own border colours:
-- a typo there should not stop the window manager starting.
-- Trailing characters are ignored rather than rejected, which is what the
-- total version has always done: @\"#rrggbbaa\"@ is a colour a config might
-- reasonably write, and dropping the alpha beats refusing the whole string.
parseColorMaybe :: String -> Maybe BorderColor
parseColorMaybe ('#':r1:r2:g1:g2:b1:b2:_) =
    case traverse hexPair [[r1,r2],[g1,g2],[b1,b2]] of
      Just [r, g, b] -> Just (scale r, scale g, scale b, maxBound)
      _              -> Nothing
  where
    -- river takes 32-bit channels; 8-bit values are widened by replication so
    -- that 0xff maps to 0xffffffff rather than 0xff000000.
    scale v = v * 0x01010101
    hexPair [a, b] = (\x y -> x * 16 + y) <$> hexDigit a <*> hexDigit b
    hexPair _ = Nothing
    hexDigit c
      | c >= '0' && c <= '9' = Just (fromIntegral (fromEnum c - fromEnum '0'))
      | c >= 'a' && c <= 'f' = Just (fromIntegral (fromEnum c - fromEnum 'a' + 10))
      | c >= 'A' && c <= 'F' = Just (fromIntegral (fromEnum c - fromEnum 'A' + 10))
      | otherwise = Nothing
parseColorMaybe _ = Nothing

-- | 'parseColorMaybe', with unparseable colours becoming opaque black.
parseColor :: String -> BorderColor
parseColor s = case parseColorMaybe s of
  Just c  -> c
  Nothing -> (0, 0, 0, maxBound)

--------------------------------------------------------------------------------
-- Input

-- | Modifier masks and keysyms are numerically identical between X11 and
-- xkbcommon, and @river_seat_v1.modifiers@ assigns shift=1, ctrl=4, mod1=8,
-- mod3=32, mod4=64, mod5=128 -- exactly X11's values.  So a key description
-- like @"M-S-\<Return\>"@ means the same thing on both sides, and these are
-- plain numbers rather than anything river-specific.
type KeyMask = Word32
type KeySym  = Word32
type Button  = Word32

-- | X11 spelled the same type two ways depending on what was being masked.
-- Kept so that 'XMonad.Core.XConfig''s field types read exactly as upstream's.
type ButtonMask = KeyMask

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
  | ScreenLayoutChanged
    -- ^ The set of screen rectangles changed: an output appeared or went
    -- away, or one of them moved or changed size.
    --
    -- X11 made a config work this out for itself, from @ConfigureNotify@ on
    -- the root window versus @RRScreenChangeNotify@, and clear the burst of
    -- duplicates Xorg emitted with them.  river reports each output's position
    -- and dimensions directly, so the window manager can just compare the
    -- result -- this is sent only when the rectangles genuinely differ from
    -- what the 'WindowSet' already had.  See "XMonad.Hooks.Rescreen".
  | SessionLocked
  | SessionUnlocked
  | KeyPressed { ev_state :: !KeyMask, ev_keysym :: !KeySym }
    -- ^ A key captured by "XMonad.River.Keyboard".
    --
    -- Not something river volunteers: the window manager only learns about
    -- keys it has created a binding for, so this reaches a config only while
    -- something -- a prompt, a submap -- is holding a grab.  Ordinary
    -- keybindings never arrive this way; they run their action directly.
  | TimerFired !Int
    -- ^ A timer started with @XMonad.Util.Timer.startTimer@ has expired.
    --
    -- Not something the compositor sends: the window manager posts it to
    -- itself when a timer thread reports in.  X11 did the same thing by
    -- sending a client message to the root window, which needed an interned
    -- atom and a round trip through the server; here it is a constructor.
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Size hints

-- | What river tells the window manager about a window's preferred size.
--
-- Shaped like X11's @SizeHints@ so that the size-hint arithmetic in
-- "XMonad.Operations" is the same pure code on both backends, and deliberately
-- narrower: 'river_window_v1.dimensions_hint' carries a minimum and a maximum
-- and nothing else.
--
-- The three fields that are always 'Nothing' are not oversights.  Wayland has
-- no resize increments, no aspect ratio hint and no base size; there is
-- nothing for them to be read from, on any compositor.  They are kept so that
-- the arithmetic -- which already does the right thing with an absent hint --
-- needs no river-specific variant, and so that a layout written against
-- xmonad's 'SizeHints' still typechecks.
--
-- X11's @sh_win_gravity@ is absent entirely: gravity describes how a window
-- moves when its parent resizes, and Wayland has neither the parent
-- relationship nor the concept.
data SizeHints = SizeHints
  { sh_min_size    :: Maybe (Dimension, Dimension)
  , sh_max_size    :: Maybe (Dimension, Dimension)
  , sh_resize_inc  :: Maybe (Dimension, Dimension)
    -- ^ Always 'Nothing'; Wayland has no resize increment hint.
  , sh_aspect      :: Maybe ((Dimension, Dimension), (Dimension, Dimension))
    -- ^ Always 'Nothing'; Wayland has no aspect ratio hint.
  , sh_base_size   :: Maybe (Dimension, Dimension)
    -- ^ Always 'Nothing'; Wayland has no base size.
  } deriving (Eq, Show, Read)

-- | A window that has told us nothing about its preferred size.
noSizeHints :: SizeHints
noSizeHints = SizeHints
  { sh_min_size   = Nothing
  , sh_max_size   = Nothing
  , sh_resize_inc = Nothing
  , sh_aspect     = Nothing
  , sh_base_size  = Nothing
  }

--------------------------------------------------------------------------------
-- Window attributes

-- | What X11 answered @XGetWindowAttributes@ with, restricted to the parts
-- river can answer for.
--
-- Same reasoning as 'SizeHints': keeping the X11 name and field names means a
-- module that only wants a window's geometry compiles unchanged, and there are
-- a lot of those.  The fields that are here are real answers.
--
-- The ones that are not here are the X11 server's own bookkeeping, and there
-- is no server: @wa_colormap@, @wa_visual@, @wa_depth@, @wa_backing_store@,
-- @wa_save_under@, @wa_your_event_mask@ and the rest describe how a window is
-- stored and drawn by X, which under Wayland is between the client and the
-- compositor and no business of the window manager's.  A module reaching for
-- one of those gets a compile error, which is the right answer: it is doing
-- X11 drawing, not window management.
--
-- One caveat on 'wa_x' and 'wa_y'.  X11 reported where the window /was/;
-- these report where the last layout run /put/ it, because river never says
-- where a window is -- the window manager is what decided.  For a window the
-- layout did not place, both are zero and 'wa_map_state' is 'waIsUnmapped'.
data WindowAttributes = WindowAttributes
  { wa_x                 :: !Position
  , wa_y                 :: !Position
  , wa_width             :: !Dimension
  , wa_height            :: !Dimension
  , wa_border_width      :: !Dimension
    -- ^ The configured border width, which is what river was asked to draw.
  , wa_map_state         :: !Int
  , wa_override_redirect :: !Bool
    -- ^ Always 'False'.  Override-redirect was an X client's way of asking the
    -- window manager to keep out; the Wayland equivalent is to use layer shell
    -- instead, and river does not offer layer surfaces as windows at all.
  } deriving (Eq, Show, Read)

-- | Values of 'wa_map_state', matching X11's.
waIsUnmapped, waIsUnviewable, waIsViewable :: Int
waIsUnmapped   = 0
waIsUnviewable = 1
waIsViewable   = 2

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
  , rwSizeHints  :: !SizeHints
    -- ^ From @river_window_v1.dimensions_hint@.  A zero or negative bound
    -- means the window did not state one, and becomes 'Nothing'.
  , rwNew        :: !Bool
  , rwClosed     :: !Bool
  , rwFullscreen :: !Bool
    -- ^ The window has asked to be fullscreen and has not asked to stop.
    --
    -- X11 answered this from @_NET_WM_STATE@, which a window manager could
    -- read whenever it liked.  river reports it as a pair of events instead,
    -- so the answer has to be accumulated -- and each arrives before the
    -- manage sequence it triggers, which is what lets a manage hook ask.
  , rwHidden     :: !Bool
  } deriving (Eq, Show)

data RiverOutput = RiverOutput
  { roObject      :: !ObjectId
  , roPosition    :: !(Int32, Int32)
  , roSize        :: !(Int32, Int32)
  , roRemoved     :: !Bool
  , roLayerObject :: !(Maybe ObjectId)
    -- ^ The @river_layer_shell_output_v1@ for this output, when the compositor
    -- offers layer shell at all.
  , roLayerArea   :: !(Maybe Rectangle)
    -- ^ The area left over once layer surfaces have claimed their exclusive
    -- zones.  Preferring this to the raw output rectangle is what stops a bar
    -- or dock being tiled over.
  } deriving (Eq, Show)

data RiverSeat = RiverSeat
  { rsObject      :: !ObjectId
  , rsRemoved     :: !Bool
  , rsLayerObject :: !(Maybe ObjectId)
  , rsLayerFocus  :: !LayerFocus
  , rsPointer     :: !(Position, Position)
    -- ^ Latest @river_seat_v1.pointer_position@, in the compositor's logical
    -- coordinate space.  Recorded because an interactive drag reports a delta
    -- from where it started, and 'XMonad.Operations.mouseDrag' promises its
    -- caller an absolute position.
  , rsXkbSeat     :: !(Maybe ObjectId)
    -- ^ The seat's @river_xkb_bindings_seat_v1@, which is what
    -- @ensure_next_key_eaten@ is requested on.  A submap needs it to learn
    -- that a key it does not know about was pressed, and so to give up rather
    -- than stay armed with the window manager's own bindings disabled.
    --
    -- 'Nothing' on a compositor offering @river_xkb_bindings_v1@ version 1,
    -- where the request does not exist.  See 'XMonad.River.submapNextKey' for
    -- what is lost.
  } deriving (Eq, Show)

-- | Whether a seat's keyboard currently belongs to a layer surface.
--
-- This has to be tracked because focus requests must not be made while it
-- does: river discards them outright when focus is exclusive, and in the
-- non-exclusive case re-focusing a window in the same manage sequence silently
-- steals the keyboard back.  That is the difference between a prompt you can
-- type into and one you cannot.
data LayerFocus
  = LayerFocusNone
  | LayerFocusNonExclusive
  | LayerFocusExclusive
  deriving (Eq, Show)

layerHasFocus :: LayerFocus -> Bool
layerHasFocus LayerFocusNone = False
layerHasFocus _              = True
