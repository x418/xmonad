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

module XMonad.River.State
  ( RiverState(..)
  , InputCapture(..)
  , Display'(..)
  , updatePlacement
    -- * One-shot requests
  , queueOp, queueNow, takeOps, takeNowOps, nowOpsPending
    -- * Border overrides
  , borderOverride, overrideBorderWidth, overrideBorderColor, forgetBorderOverride
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', readTVar, stateTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', readIORef)
import qualified Data.Map as M

import XMonad.River.Connection (Connection)
import XMonad.River.Mailbox (Mailbox)
import XMonad.River.Plan (Op)
import XMonad.River.Types (BorderColor, Dimension, KeyMask, KeySym, Position, Rectangle, RiverOutput, RiverSeat, RiverWindow, SizeHints, Window, WindowAttributes)
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
    , riverDirty    :: !(TVar Bool)
      -- ^ set when state changed outside a manage sequence, so that one must
      -- be requested with @manage_dirty@.  A 'TVar' because the event loop
      -- waits on it: a write here wakes the loop.
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
    , riverOverlays :: !(IORef [ObjectId])
      -- ^ Nodes of the window-manager surfaces that are currently mapped,
      -- bottom-to-top.  These are @river_shell_surface_v1@ nodes --
      -- decorations, EasyMotion's chord overlays -- and the render sequence
      -- stacks them above the windows.  Written by xmonad-contrib, whose
      -- drawable registry this package cannot import.
    , riverOverlayPos :: !(IORef (M.Map ObjectId (Position, Position)))
      -- ^ Where each of those nodes goes.  @river_node_v1.set_position@ is
      -- rendering state and legal only inside a sequence, so the surface's
      -- owner records the position here and the render sequence applies it.
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
    , riverAfterLayout :: !(IORef [m ()])
      -- ^ Actions waiting for the layout to run, newest first.
      --
      -- X11 needed no such queue: @windows@ moved and resized the windows
      -- before it returned, so a binding could change the 'WindowSet' and
      -- immediately ask where something had ended up.  Here the layout runs at
      -- the end of the manage sequence, /after/ every binding action, so the
      -- same code reads the geometry from before its own change.  This is
      -- where an action that needs the answer waits for it.  See
      -- 'XMonad.River.afterLayout'.
    , riverGeometry :: !(IORef (M.Map Window WindowAttributes))
      -- ^ What the last layout decided, for 'XMonad.Core.getWindowAttributes'.
      -- Every window river knows; one the layout did not place is unmapped
      -- at the origin, as X11 would have said.
    , riverSizeHints :: !(IORef (M.Map Window SizeHints))
      -- ^ Likewise for 'XMonad.Core.getWMNormalHints'.
    , riverBorders :: !(IORef (M.Map Window (Maybe Dimension, Maybe BorderColor)))
      -- ^ Per-window overrides of border width and colour.  Sticky until the
      -- window goes: river keeps no border state, so the render sequence
      -- restates borders from the plan and an override has to be remembered.
    , riverSubmapGen :: !(IORef Int)
      -- ^ Numbers each keyboard capture, so a deadline for an earlier one
      -- cannot close the current one.
    , riverOps :: !(IORef [Op])
      -- ^ One-shot requests awaiting the next manage sequence, newest first.
      -- Drained as transmitted: each is an effect river performs once.
    , riverNowOps :: !(TVar [Op])
      -- ^ Requests that need no sequence and must not wait for one.  A
      -- 'TVar' so the loop, which waits on it, wakes.
    }

-- | What 'XMonad.Core.withDisplay' hands out.  X11's was the server
-- connection; the queries contrib makes on it -- 'XMonad.Core.getWindowAttributes',
-- 'XMonad.Core.setWindowBorder' -- are answered from the window manager's own
-- state here, so it carries that too.  "XMonad.Core" instantiates it as
-- @Display' X@ under the name @Display@.
data Display' m = Display'
    { dpyConn  :: !Connection
    , dpyState :: !(RiverState m)
    }

--------------------------------------------------------------------------------
-- One-shot requests

-- | Ask for a request to go out with the next manage sequence.
queueOp :: MonadIO m => RiverState n -> Op -> m ()
queueOp rs op = liftIO (atomicModifyIORef' (riverOps rs) (\ops -> (op : ops, ())))

-- | Ask for a request to go out on the event loop's next pass.
queueNow :: MonadIO m => RiverState n -> Op -> m ()
queueNow rs op = liftIO (atomically (modifyTVar' (riverNowOps rs) (op :)))

-- | Take everything queued, oldest first, leaving none.
takeOps :: RiverState n -> IO [Op]
takeOps rs = atomicModifyIORef' (riverOps rs) (\ops -> ([], reverse ops))

takeNowOps :: RiverState n -> IO [Op]
takeNowOps rs = atomically (stateTVar (riverNowOps rs) (\ops -> (reverse ops, [])))

-- | Retries until a now-op is queued; for the loop's wait.
nowOpsPending :: RiverState n -> STM ()
nowOpsPending rs = readTVar (riverNowOps rs) >>= check . not . null

--------------------------------------------------------------------------------
-- Border overrides

type Borders = IORef (M.Map Window (Maybe Dimension, Maybe BorderColor))

-- | What has been overridden for one window; no override is @(Nothing, Nothing)@.
borderOverride :: Borders -> Window -> IO (Maybe Dimension, Maybe BorderColor)
borderOverride ref w = M.findWithDefault (Nothing, Nothing) w <$> readIORef ref

overrideBorderWidth :: Borders -> Window -> Dimension -> IO ()
overrideBorderWidth ref w n = atomicModifyIORef' ref $ \m ->
    (M.alter (\o -> Just (Just n, maybe Nothing snd o)) w m, ())

overrideBorderColor :: Borders -> Window -> BorderColor -> IO ()
overrideBorderColor ref w c = atomicModifyIORef' ref $ \m ->
    (M.alter (\o -> Just (maybe Nothing fst o, Just c)) w m, ())

-- | Drop a window's overrides once it is gone: river recycles object ids.
forgetBorderOverride :: Borders -> Window -> IO ()
forgetBorderOverride ref w = atomicModifyIORef' ref (\m -> (M.delete w m, ()))

-- | Correct the recorded geometry of a window that something has just moved or
-- resized outside a layout run.
--
-- X11 had no equivalent because it needed none: @moveResizeWindow@ changed
-- what the server would report, and a caller reading the geometry straight
-- back got the new one.  'riverPlacements' is this backend's stand-in for that
-- report, so anything that moves a window has to say so here or the next
-- reader -- 'XMonad.Operations.floatLocation', in particular, which is how a
-- drag records where it got to -- answers with the position from the last
-- layout run and undoes the move.
--
-- A window with no placement is left alone rather than added.  It has no
-- geometry for this to correct, and inventing one would tell
-- 'XMonad.River.windowUnderPointer' that a window the layout never placed is
-- under the pointer.
--
-- Lives here, rather than beside either of its callers, because both
-- "XMonad.Operations" and "XMonad.River" need it and the latter imports the
-- former.
updatePlacement :: MonadIO m => IORef [(Window, Rectangle)] -> Window -> Rectangle -> m ()
updatePlacement ref w r = liftIO $ modifyIORef' ref $
    map (\e -> if fst e == w then (w, r) else e)
