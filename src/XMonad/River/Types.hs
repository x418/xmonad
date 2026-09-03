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
  , centredRect
  , BorderColor
  , Pixel, pixelColor
  , parseColor, parseColorMaybe
  , Position
  , Dimension
    -- * Input
  , KeyMask
  , KeySym
  , Button
  , ButtonMask
  , shiftMask, lockMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask
  , mod5Mask, noModMask
  , button1, button2, button3, button4, button5
  , riverModifiers
  , EventType, keyPress, keyRelease
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

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Ratio ((%))
import Data.Word (Word32)

import XMonad.River.Protocol.WindowManagement
  ( riverSeatV1ModifiersCtrl, riverSeatV1ModifiersMod1, riverSeatV1ModifiersMod3
  , riverSeatV1ModifiersMod4, riverSeatV1ModifiersMod5, riverSeatV1ModifiersShift )
import XMonad.River.Wire (ObjectId)
import qualified XMonad.StackSet as W

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

-- | A rectangle of the given size in logical pixels, centred on the screen
-- and expressed as a fraction of it, the form 'XMonad.StackSet.floating'
-- holds.  Never under a pixel a side.  The origin is never negative: a window
-- larger than the screen keeps its size and starts at the top left, so its
-- top edge stays reachable.
centredRect :: Rectangle -> Integer -> Integer -> W.RationalRect
centredRect sr width height =
  W.RationalRect (max 0 (1 % 2 - rw / 2)) (max 0 (1 % 2 - rh / 2)) rw rh
  where
    sw = max 1 (toInteger (rect_width sr))
    sh = max 1 (toInteger (rect_height sr))
    rw = max 1 width  % sw
    rh = max 1 height % sh

-- | An RGBA border colour, in the 32-bit-per-channel form
-- @river_window_v1.set_borders@ takes.
--
-- X11 stored a 'Pixel' resolved against the window's colormap.  Wayland has no
-- colormaps and no pixel values, so the honest representation is the one the
-- protocol asks for.
type BorderColor = (Word32, Word32, Word32, Word32)

-- | A packed @0xRRGGBB@ colour.  X11's was a colormap index; there is no
-- colormap, so this is the colour itself.  Kept because contrib signatures
-- name it.
type Pixel = Word32

-- | Widen a packed @0xRRGGBB@ colour into the form @set_borders@ takes.
pixelColor :: Pixel -> BorderColor
pixelColor p = (chan 16, chan 8, chan 0, maxBound)
  where
    -- Replicated rather than shifted, so 0xff becomes 0xffffffff rather than
    -- 0xff000000 -- the same widening 'parseColorMaybe' does.
    chan n = ((p `div` (256 ^ (n `div` 8 :: Int))) `mod` 256) * 0x01010101

-- | Parse @\"#rrggbb\"@ into the 32-bit channel values river's @set_borders@
-- takes, packed into a 'Pixel'.
--
-- 'Nothing' for anything that is not a colour, which is what lets
-- 'XMonad.Operations.setWindowBorderWithFallback' have a fallback to fall back
-- to.  'parseColor' is the total version, for the config's own border colours:
-- a typo there should not stop the window manager starting.
-- Trailing characters are ignored rather than rejected, which is what the
-- total version has always done: @\"#rrggbbaa\"@ is a colour a config might
-- reasonably write, and dropping the alpha beats refusing the whole string.
parseColorMaybe :: String -> Maybe Pixel
parseColorMaybe ('#':r1:r2:g1:g2:b1:b2:_) =
    case traverse hexPair [[r1,r2],[g1,g2],[b1,b2]] of
      Just [r, g, b] -> Just (r * 65536 + g * 256 + b)
      _              -> Nothing
  where
    hexPair [a, b] = (\x y -> x * 16 + y) <$> hexDigit a <*> hexDigit b
    hexPair _ = Nothing
    hexDigit c
      | c >= '0' && c <= '9' = Just (fromIntegral (fromEnum c - fromEnum '0'))
      | c >= 'a' && c <= 'f' = Just (fromIntegral (fromEnum c - fromEnum 'a' + 10))
      | c >= 'A' && c <= 'F' = Just (fromIntegral (fromEnum c - fromEnum 'A' + 10))
      | otherwise = Nothing
parseColorMaybe _ = Nothing

-- | 'parseColorMaybe', with unparseable colours becoming black.
parseColor :: String -> Pixel
parseColor s = case parseColorMaybe s of
  Just c  -> c
  Nothing -> 0

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

-- | A managed window: the @river_window_v1@ object.  Ids are recycled after
-- @closed@, so a 'Window' must not outlive the object; @identifier@ is the
-- name that does.
type Window = ObjectId

--------------------------------------------------------------------------------
-- Events

-- | What reaches @handleEventHook@ and layouts.  Small: most of what X11
-- delivered as events, river delivers as state on 'RiverWindow'.
-- 'DestroyWindowEvent' keeps its X11 spelling because @XMonad.Layout@
-- matches on it by name.
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
    -- ^ The set of screen rectangles changed.  Sent only when they genuinely
    -- differ from what the 'WindowSet' had; see "XMonad.Hooks.Rescreen".
  | SessionLocked
  | SessionUnlocked
  | KeyPressed { ev_state :: !KeyMask, ev_keysym :: !KeySym }
    -- ^ A captured key.  Reaches a config only while a prompt or submap
    -- holds keys; ordinary bindings run their action directly.
  | TimerFired !Int
    -- ^ A timer started with @XMonad.Util.Timer.startTimer@ has expired.
    -- Posted by the window manager to itself.
  | WindowFullscreenChanged { ev_window :: !Window, ev_fullscreen :: !Bool }
    -- ^ A client asked to become fullscreen, or to stop being.  A request:
    -- the flag behind 'XMonad.Hooks.ManageHelpers.isFullscreen' is already
    -- updated, nothing has been resized.  "XMonad.Layout.Fullscreen" acts on
    -- it.
  | SurfaceClicked { ev_window :: !Window, ev_x :: !Position, ev_y :: !Position }
    -- ^ A surface the window manager drew was pressed: X11's @ButtonPress@
    -- on a decoration.  The id is the shell surface
    -- @XMonad.Util.XUtils.createNewWindow@ returned; the position is global,
    -- as of this manage sequence.  No button number: it may have been touch.
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Size hints

-- | A window's preferred size, in X11's shape so the size-hint arithmetic in
-- "XMonad.Operations" is shared.  @dimensions_hint@ carries a minimum and a
-- maximum; Wayland has no increments, aspect or base size, so those are
-- always 'Nothing'.
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

-- | What @XGetWindowAttributes@ answered, restricted to what river can
-- answer: geometry as the last layout put it (zero and unmapped for a window
-- it did not place), border width, map state.  The server's own bookkeeping
-- fields are absent.
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
    -- ^ From @river_window_v1.dimensions@, which river sends only once the
    -- window has mapped; @(0, 0)@ until then, and never afterwards.
  , rwSizeHints  :: !SizeHints
    -- ^ From @river_window_v1.dimensions_hint@.  A zero or negative bound
    -- means the window did not state one, and becomes 'Nothing'.
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
  , roWlOutput    :: !(Maybe ObjectId)
    -- ^ The bound @wl_output@, from @river_output_v1.wl_output@.
  , roName        :: !(Maybe ByteString)
    -- ^ The connector name (@eDP-1@, @DP-3@), from @wl_output.name@; needs
    -- @wl_output@ version 4.
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

-- | The modifier masks, with X11's values, which are also river's:
-- @river_seat_v1.modifiers@ assigns shift=1, ctrl=4, mod1=8, mod3=32,
-- mod4=64, mod5=128.  'lockMask' and 'mod2Mask' have no river bit; no
-- binding matches on them.
shiftMask, lockMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask,
  mod5Mask, noModMask :: KeyMask
shiftMask   = 1
lockMask    = 2
controlMask = 4
mod1Mask    = 8
mod2Mask    = 16
mod3Mask    = 32
mod4Mask    = 64
mod5Mask    = 128
noModMask   = 0

-- | The part of a 'KeyMask' river can bind on: lock and mod2 (caps and num
-- lock) have no bit in @river_seat_v1.modifiers@.
riverModifiers :: KeyMask -> Word32
riverModifiers mask = mask .&. supported
  where
    supported = riverSeatV1ModifiersShift
            + riverSeatV1ModifiersCtrl
            + riverSeatV1ModifiersMod1
            + riverSeatV1ModifiersMod3
            + riverSeatV1ModifiersMod4
            + riverSeatV1ModifiersMod5

-- | X11 button numbers.
--
-- river's pointer bindings take Linux input event codes instead, so these are
-- translated at the point of use rather than being the same numbers.  They
-- keep X11's spelling because that is what a config writes.
button1, button2, button3, button4, button5 :: Button
button1 = 1
button2 = 2
button3 = 3
button4 = 4
button5 = 5

-- | X11's event type tag, kept for the handful of signatures that name it.
type EventType = Word32

keyPress, keyRelease :: EventType
keyPress   = 2
keyRelease = 3
