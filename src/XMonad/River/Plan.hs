-- | What the window manager decided, as a value.
--
-- The worker computes a 'Plan' and the loop transmits it.  Most of what a
-- sequence sends restates state and is a value that may be re-sent; a few
-- requests are one-shot effects ('Op') and must go exactly once.
module XMonad.River.Plan
  ( Plan(..)
  , emptyPlan
  , FocusTarget(..)
  , Op(..)
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.River.Types (Dimension, KeyMask, KeySym, Position, Rectangle, Window)
import XMonad.River.Wire (ObjectId)

-- | Where a seat's keyboard should go.
data FocusTarget
  = FocusWindow !Window
  | ClearFocus
  deriving (Eq, Show)

-- | Everything restated on each sequence: safe to transmit again unchanged,
-- which is what lets the loop answer a @manage_start@ it has nothing new for.
data Plan = Plan
  { planSerial     :: !Int
    -- ^ Monotonic.  Lets whoever published a plan tell when it has landed.
  , planPlacements :: ![(Window, Rectangle)]
    -- ^ Topmost first, floats before tiles, which is upstream's convention.
    -- The render sequence reverses this before @place_top@ing it.
  , planFloating   :: !(S.Set Window)
    -- ^ Which of the placements are floating, so the rest can be told they
    -- are tiled.  A window that is not told draws itself as though it were
    -- floating: its own decorations, and drop shadows outside the size it
    -- was given, which it then subtracts from its content.
  , planBorders    :: !(M.Map Window (Dimension, (Word32, Word32, Word32, Word32)))
    -- ^ Width and RGBA per placed window, already resolved against any
    -- per-window override.  Borders are rendering state, so river forgets them
    -- between frames and they have to be restated rather than set once.
  , planVisible    :: !(S.Set Window)
    -- ^ What the layout placed.  Anything river knows about and this does not
    -- contain belongs to a workspace that is off screen, and is hidden --
    -- which is how workspaces exist at all, river having no such concept.
  , planRaised     :: ![Window]
    -- ^ Standing "keep this above the layout" requests, re-applied every frame
    -- because the render sequence would otherwise have just undone them.
  , planFocus      :: !FocusTarget
    -- ^ Applied to every seat whose keyboard has not gone to a layer surface.
    -- One target rather than one per seat: xmonad has a single focus, and a
    -- second seat pointed somewhere else would have no way to be described.
  , planLayerDefault :: !(Maybe (ObjectId, Maybe ObjectId))
    -- ^ The output that should host layer surfaces which name none, and its
    -- @river_layer_shell_output_v1@.  Held as the choice rather than as a
    -- request because the request is only reissued when the choice changes,
    -- and what has been sent is the transmitting side's business.
  }

-- | The plan of a window manager that has decided nothing yet.
emptyPlan :: Plan
emptyPlan = Plan
  { planSerial     = 0
  , planPlacements = []
  , planBorders    = M.empty
  , planFloating   = S.empty
  , planVisible    = S.empty
  , planRaised     = []
  , planFocus      = ClearFocus
  , planLayerDefault = Nothing
  }

-- | Effects that happen once and are then forgotten.  Drained as they are
-- transmitted: @close@ twice kills a second window.
data Op
  = OpClose !Window
    -- ^ @river_window_v1.close@, from 'XMonad.Operations.kill'.
  | OpWarpPointer !ObjectId !Position !Position
    -- ^ @river_seat_v1.pointer_warp@ on a named seat.
  | OpPointerOpStart !ObjectId
    -- ^ @river_seat_v1.op_start_pointer@, which begins an interactive drag.
  | OpPointerOpEnd !ObjectId
    -- ^ @river_seat_v1.op_end@.  Manage-sequence only, like the start.
  | OpUseDecorations !Window !Bool
    -- ^ @use_ssd@ when 'True', @use_csd@ when 'False'.
  | OpSetCapabilities !Window !Word32
    -- ^ @set_capabilities@: which of maximize, minimize, fullscreen and the
    -- window menu the client should offer.
  | OpInformFullscreen !Window !Bool
    -- ^ @inform_fullscreen@ or @inform_not_fullscreen@: tell the client its
    -- state, without river touching its geometry.
  | OpInformResize !Window !Bool
    -- ^ @inform_resize_start@ or @inform_resize_end@, around an interactive
    -- resize.
  | OpSetPosition !Window !Position !Position
    -- ^ Where an interactive drag has put a window.  Rendering state, so legal
    -- in either sequence, but it arrives from user code and so takes the same
    -- route as the rest.
  | OpProposeDimensions !Window !Dimension !Dimension
    -- ^ Likewise from an interactive resize.
  | OpExitSession
    -- ^ End the Wayland session.
  | OpStop
    -- ^ @river_window_manager_v1.stop@: ask river to release this window
    -- manager so a successor may connect.
  | OpSetXcursorTheme !ObjectId !ByteString !Word32
    -- ^ The cursor theme and size, on one seat.
  | OpGrabKeys !Int ![(KeyMask, KeySym)]
    -- ^ Bind these for as long as the config wants them, reporting presses and
    -- releases by index.  Unlike 'OpCaptureInput' this disables nothing and
    -- ends only when 'OpUngrabKeys' says so -- it is "XMonad.River.grabKeys",
    -- which is a standing capture rather than an interaction.  Tagged with
    -- the generation of the action table the indices are into.
  | OpUngrabKeys
    -- ^ Release everything 'OpGrabKeys' bound.
  | OpCaptureInput ![(KeyMask, KeySym)] !KeyMask !Bool !Int
    -- ^ Take the keyboard: disable the config's bindings and listen for these
    -- keys instead, ending when the modifier mask goes up (zero to watch none)
    -- or, if the flag is set, after the first key.  Tagged with the generation
    -- identifying it.
    --
    -- The keys rather than the actions: what to run is the config's business,
    -- and this says only what to listen for.  Drained inside a manage
    -- sequence, which is what makes arming atomic with the key press that
    -- asked for it.
  deriving (Eq, Show)
