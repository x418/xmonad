-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.River.State
-- License     :  BSD3-style (see LICENSE)
--
-- The compositor-connection state the window manager carries around.
--
-- Upstream's 'XMonad.Core.XConf' needs three things from X11: the display, the
-- root window, and two border pixels.  River needs rather more than that --
-- the bound globals, the accumulated view of windows, outputs and seats, and a
-- handful of 'Data.IORef.IORef's that the event loop and the 'XMonad.Core.X'
-- monad both reach.  Holding those as seventeen fields on @XConf@ made that
-- record four times the size of upstream's, which is the one place an upstream
-- change to @XConf@ would be certain to conflict.
--
-- So they live here instead, behind a single @riverState@ field.  Nothing is
-- hidden by the move: the field names are unchanged, and a call site says
-- @asks (riverWindows . riverState)@ where it used to say @asks riverWindows@.
--
-- The record is parameterised over the monad because several fields hold
-- actions, and 'XMonad.Core.X' is defined in "XMonad.Core", which imports this
-- module.  "XMonad.Core" instantiates it as @RiverState X@.
--
-----------------------------------------------------------------------------

module XMonad.River.State (RiverState(..), InputCapture(..)) where

import Data.IORef (IORef)
import qualified Data.Map as M

import XMonad.River.Mailbox (Mailbox)
import XMonad.River.Types (KeyMask, KeySym, Position, Rectangle, RiverOutput, RiverSeat, RiverWindow, Window)
import XMonad.River.Wire (ObjectId)

-- | Everything the window manager knows about its connection to river.
-- | An interaction that has taken the keyboard for a while.
--
-- A submap and a hold-to-cycle -- @M-Tab@ -- are the same thing at this level:
-- both disable every one of the window manager's bindings, install a set of
-- their own, and end.  They differ only in what ends them, which is what
-- 'icOneShot' and 'icMods' say.  One type rather than two because they are
-- also /mutually exclusive/: each disables the whole binding set, so two open
-- at once would have one teardown re-enabling bindings the other still thinks
-- it has disabled.  Sharing a slot makes that exclusion structural rather than
-- something to remember.
--
-- Held as data rather than as actions to run, because arming and tearing down
-- are the event loop's work while the actions are the config's: the loop
-- reports which key fired by index, and this turns that index back into
-- something to run.  See @DESIGN.md@ on input routing being loop state.
data InputCapture m = InputCapture
    { icKeys       :: ![(KeyMask, KeySym)]
      -- ^ What to listen for.  Position is identity: the loop knows only
      -- indices into this list.
    , icOnKey      :: !(Bool -> Int -> m ())
      -- ^ Run when one of them fires.  'True' for a press, 'False' for a
      -- release; the 'Int' indexes 'icKeys'.
    , icOnEnd      :: !(m ())
      -- ^ Run when the interaction ends -- an unbound key, the watched
      -- modifier being released, or the deadline.
    , icMods       :: !KeyMask
      -- ^ Modifiers whose release ends this, or zero to watch none.
    , icOneShot    :: !Bool
      -- ^ Whether the first key that fires also ends it.  True for a submap,
      -- false for a hold-to-cycle, where keys fire until the modifier goes up.
    , icGeneration :: !Int
      -- ^ Distinguishes this from a later one, so that a deadline belonging to
      -- an earlier interaction cannot close the current one.
    }

data RiverState m = RiverState
    { riverManager  :: !ObjectId               -- ^ the @river_window_manager_v1@ global
    , riverBindings :: !ObjectId               -- ^ the @river_xkb_bindings_v1@ global
    , riverCompositor :: !(Maybe ObjectId)
      -- ^ the @wl_compositor@ global, for surfaces the window manager draws
      -- itself.  'Nothing' on a compositor that does not advertise one, which
      -- should not happen but is not worth crashing over -- a session without
      -- decorations beats no session.
    , riverShm      :: !(Maybe ObjectId)
      -- ^ the @wl_shm@ global, for the buffers those surfaces are drawn into.
    , riverWindows  :: !(IORef (M.Map ObjectId RiverWindow))
    , riverOutputs  :: !(IORef (M.Map ObjectId RiverOutput))
    , riverSeats    :: !(IORef (M.Map ObjectId RiverSeat))
    , riverDirty    :: !(IORef Bool)
      -- ^ set when state changed outside a manage sequence, so that one must
      -- be requested with @manage_dirty@
    , inManageSeq   :: !(IORef Bool)
      -- ^ guards requests river only permits during a manage sequence
    , riverRestart  :: !(IORef (Maybe (FilePath, [String])))
      -- ^ program and arguments to exec once river confirms this window
      -- manager has stopped.  Not a shell command string: @sh -c@ forks
      -- rather than execs for anything but the simplest word, so routing the
      -- restart through a shell left one behind on every @M-q@, each the
      -- parent of the next.
    , riverMailbox :: !(Mailbox (m ()))
      -- ^ How a thread that is not the event loop gets work done.  X11 let a
      -- background thread post a client message to the root window; there is
      -- no such relay here, so the channel is ours.  See
      -- "XMonad.River.Mailbox".
    , riverKeyBindings :: !(IORef (M.Map ObjectId (m ())))
      -- ^ The @river_xkb_binding_v1@ object created for each of the config's
      -- key bindings, and what it runs.  Shared with the event loop rather
      -- than private to it because a submap has to disable the whole set for
      -- as long as it is open: river leaves it to compositor policy which of
      -- several bindings matching one physical key gets the press, so two
      -- live bindings for the same key is undefined rather than layered.
    , riverPlacements :: !(IORef [(Window, Rectangle)])
      -- ^ Where the last layout run put each window, in river's global
      -- coordinate space.  This is the only record of a window's position:
      -- river reports a window's size but never where it is, because the
      -- window manager is the thing that decided.  See
      -- 'XMonad.River.windowRect'.
    , riverExtraKeys :: !(IORef [(m (), m ())])
      -- ^ Bindings installed at runtime, over and above the config's.
      --
      -- X11 called this grabbing a key: a window manager could ask the server
      -- for one at any moment and give it back later.  River has no grab, so
      -- what stands in for one is a @river_xkb_binding_v1@ created on demand;
      -- this is where those live so they can be destroyed again.  See
      -- 'XMonad.River.grabKeys'.
    , riverRestack :: !(IORef [Window])
      -- ^ Windows to raise above the layout's own order, bottom-to-top.
      --
      -- The render sequence restacks from the layout on every frame, so a
      -- request made anywhere else -- a logHook raising the current
      -- workspace, say -- is overwritten before anyone sees it.  This is
      -- where such a request is kept so that it is re-applied every frame
      -- instead, which is what "raise it and have it stay raised" has to
      -- mean when something else owns the order.  Windows that are no longer
      -- placed are dropped as they go.
    , riverCapture :: !(IORef (Maybe (InputCapture m)))
      -- ^ The interaction currently holding the keyboard, or 'Nothing'.
      -- Written by 'XMonad.River.submapNextKey' and
      -- 'XMonad.River.whileModifiersHeld'; taken -- atomically, so exactly one
      -- of several racing claimants wins -- by whichever of a key, an unbound
      -- key, a modifier release or the deadline gets there first.
    , riverDragOrigin :: !(IORef (Position, Position))
      -- ^ Where the pointer was when the current interactive operation began.
      -- river reports a drag as a delta from its start; 'mouseDrag' promises
      -- its caller an absolute position, so the origin has to be remembered.
    }
