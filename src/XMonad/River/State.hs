-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.River.State
-- License     :  BSD3-style (see LICENSE)
--
-- What the backend keeps beyond 'XMonad.Core.XState': the bound globals,
-- the compositor's view of windows, outputs and seats, and the queues the
-- loop and the worker meet at.  One field on @XConf@ rather than seventeen,
-- so an upstream change to @XConf@ merges.  Parameterised over the monad
-- because "XMonad.Core" instantiates it as @RiverState X@.
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
import Data.Word (Word32)
import qualified Data.Map as M

import XMonad.River.Connection (Connection)
import XMonad.River.Mailbox (Mailbox)
import XMonad.River.Plan (Op)
import XMonad.River.Types (BorderColor, Dimension, KeyMask, KeySym, Position, Rectangle, RiverOutput, RiverSeat, RiverWindow, Window)
import XMonad.River.Wire (ObjectId)

-- | An interaction that has taken the keyboard: a submap or a hold-to-cycle.
-- Both disable the config's bindings and install their own; they differ in
-- what ends them.  One slot for both, since two open at once would tear each
-- other down.  The loop arms it and reports keys by index; the config's
-- actions run on the worker.
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
    , riverXkbVersion :: !Word32
      -- ^ The @river_xkb_bindings_v1@ version negotiated.  @modifiers_watch@
      -- arrived in 3; sending it to an older object is a protocol error, so
      -- whatever wants a modifier release has to check first.
    , riverBorderWidth :: !Dimension
      -- ^ The config's border width, which is what @wa_border_width@ answers.
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
      -- manager has stopped.  Exec'd directly: @sh -c@ would fork and stay.
    , riverMailbox :: !(Mailbox (m ()))
      -- ^ How a thread that is not the event loop gets an action run.
    , riverPlacements :: !(IORef [(Window, Rectangle)])
      -- ^ Where the last layout put each window: the only record of a
      -- window's position, since river never reports one.  Topmost first.
    , riverExtraKeys :: !(IORef (Int, [(m (), m ())]))
      -- ^ Press and release actions for 'XMonad.River.grabKeys', by index,
      -- tagged with the generation the bindings were created for.  The loop
      -- fires a binding only if its generation is the table's: the bindings
      -- of an earlier grab stay alive until a sequence destroys them, and
      -- must not index a table they were not built against.
    , riverRestack :: !(IORef [Window])
      -- ^ Windows to keep above the layout's order, bottom-to-top.  Dropped
      -- as they stop being placed.
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
      -- ^ The interaction holding the keyboard, if any.  Written by the
      -- config; claimed atomically by whichever end wins.
    , riverDragOrigin :: !(IORef (Position, Position))
      -- ^ Where the pointer was when the interactive operation began; river
      -- reports deltas, 'XMonad.Operations.mouseDrag' promises positions.
    , riverAfterLayout :: !(IORef [m ()])
      -- ^ Actions waiting for the layout, newest first; see
      -- 'XMonad.River.afterLayout'.
    , riverGeometry :: !(IORef (M.Map Window Rectangle))
      -- ^ Where the last layout put each placed window, as a map: what
      -- 'XMonad.Core.getWindowAttributes' answers from.  A window river knows
      -- and this does not hold is unmapped at the origin, as X11 would have
      -- said; the attributes are built when asked, not for every window on
      -- every pass.
    , riverLogDue :: !(IORef Bool)
      -- ^ 'XMonad.Operations.windows' ran since the last sequence's log hook.
      -- X11 ran the log hook on every @windows@; here it runs once, at the end
      -- of the sequence, if anything asked for it or the set changed on its
      -- own.  Worker only.
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

-- | Correct the recorded geometry of a window something just moved outside a
-- layout run, so 'XMonad.Operations.floatLocation' reads the move back rather
-- than undoing it.  A window with no placement is left alone.
updatePlacement :: MonadIO m => IORef [(Window, Rectangle)] -> Window -> Rectangle -> m ()
updatePlacement ref w r = liftIO $ modifyIORef' ref $
    map (\e -> if fst e == w then (w, r) else e)
