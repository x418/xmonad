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
-- Two records, by who may touch them: 'Shared' is what both threads reach,
-- each field saying which side writes it and how the other reads; and
-- 'WorkerState' is the worker's alone.  The loop's own state is
-- 'XMonad.River.WM.Runtime.Runtime', which holds a 'Shared' and cannot name
-- a 'WorkerState'.  Ownership is a type, not a comment.
--
-----------------------------------------------------------------------------

module XMonad.River.State
  ( RiverState(..)
  , Shared(..)
  , WorkerState(..)
  , InputCapture(..)
  , Display'(..)
    -- * The names the rest of xmonad reads the state by
  , riverManager, riverBindings, riverXkbVersion, riverBorderWidth, riverCompositor, riverShm
  , riverWindows, riverOutputs, riverSeats, riverDirty, riverRestart, riverMailbox, riverLoopJobs
  , riverExtraKeys, riverOverlays, riverOverlayPos, riverCapture, riverOps, riverNowOps
  , riverKeyboardLayout
  , inManageSeq, riverPlaced, riverRestack, riverDragOrigin, riverAfterLayout
  , riverUnsized, riverLogDue, riverBorders, riverSubmapGen
    -- * Where the last layout put things
  , Placed(..), rectOf, placedRects, placedOrder, updatePlacement
    -- * One-shot requests
  , queueOp, queueNow, takeOps, takeNowOps, nowOpsPending
    -- * Border overrides
  , borderOverride, overrideBorderWidth, overrideBorderColor, clearBorderColor, forgetBorderOverride
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', readTVar, stateTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', readIORef)
import Data.Int (Int32)
import Data.Word (Word32)
import qualified Data.Map as M
import qualified Data.Set as S

import XMonad.River.Connection (Connection)
import XMonad.River.Mailbox (Mailbox)
import XMonad.River.Plan (Op, Plan)
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

-- | What both threads may reach.  Which thread writes a field, and how the
-- other reads it, is stated per group; the loop's own state lives in
-- "XMonad.River.WM.Runtime" and the worker's in 'WorkerState', so that
-- naming the wrong thread's state does not typecheck.
data Shared m = Shared
    { -- * Fixed for the connection's life
      shManager  :: !ObjectId               -- ^ the @river_window_manager_v1@ global
    , shBindings :: !ObjectId               -- ^ the @river_xkb_bindings_v1@ global
    , shXkbVersion :: !Word32
      -- ^ The @river_xkb_bindings_v1@ version negotiated.  @modifiers_watch@
      -- arrived in 3; sending it to an older object is a protocol error, so
      -- whatever wants a modifier release has to check first.
    , shBorderWidth :: !Dimension
      -- ^ The config's border width, which is what @wa_border_width@ answers.
    , shCompositor :: !(Maybe ObjectId)
      -- ^ the @wl_compositor@ global, for surfaces the window manager draws
      -- itself.  'Nothing' on a compositor that does not advertise one, which
      -- should not happen but is not worth crashing over -- a session without
      -- decorations beats no session.
    , shShm      :: !(Maybe ObjectId)
      -- ^ the @wl_shm@ global, for the buffers those surfaces are drawn into.
    , shLayerShell :: !(Maybe ObjectId)
      -- ^ the @river_layer_shell_v1@ global; 'Nothing' and there are no
      -- usable areas to wait for and no default output to nominate.

      -- * The loop writes, the worker reads
    , shWindows  :: !(IORef (M.Map ObjectId RiverWindow))
    , shOutputs  :: !(IORef (M.Map ObjectId RiverOutput))
    , shSeats    :: !(IORef (M.Map ObjectId RiverSeat))
    , shHovered  :: !(IORef (Maybe Window))
      -- ^ The window under the pointer, as river last said; the worker
      -- checks it is still there before focus follows the mouse.
    , shKeyboardLayout :: !(IORef (Maybe (Int, String)))
      -- ^ The active xkb layout, index and name, as last reported.  'Nothing'
      -- without @river_xkb_config_v1@ or before the first keyboard.

      -- * Transactional: a write wakes the loop
    , shDirty    :: !(TVar Bool)
      -- ^ set when state changed outside a manage sequence, so that one must
      -- be requested with @manage_dirty@.
    , shMailbox :: !(Mailbox (m ()))
      -- ^ How a thread that is not the event loop gets an action run.
    , shLoopJobs :: !(Mailbox (IO ()))
      -- ^ Protocol lifecycle work that must run on the event loop after any
      -- render currently in progress. Drawable destruction uses this ordering.
    , shNowOps :: !(TVar [Op])
      -- ^ Requests that need no sequence and must not wait for one.
    , shPlan    :: !(TVar Plan)
      -- ^ The last plan the layout produced.  'planSerial' is monotonic.
    , shSeqDone :: !(TVar Int)
      -- ^ The highest manage-sequence number the worker has finished.

      -- * Atomic, written from either side
    , shOps :: !(IORef [Op])
      -- ^ One-shot requests awaiting the next manage sequence, newest first.
      -- Queued by the worker, drained as transmitted: each is an effect
      -- river performs once.
    , shCapture :: !(IORef (Maybe (InputCapture m)))
      -- ^ The interaction holding the keyboard, if any.  Written by the
      -- config; claimed atomically by whichever end wins.
    , shExtraKeys :: !(IORef (Int, [(m (), m ())]))
      -- ^ Press and release actions for 'XMonad.River.grabKeys', by index,
      -- tagged with the generation the bindings were created for.  The loop
      -- fires a binding only if its generation is the table's: the bindings
      -- of an earlier grab stay alive until a sequence destroys them, and
      -- must not index a table they were not built against.
    , shOverlays :: !(IORef [ObjectId])
      -- ^ Nodes of the window-manager surfaces that are currently mapped,
      -- bottom-to-top.  These are @river_shell_surface_v1@ nodes --
      -- decorations, EasyMotion's chord overlays -- and the render sequence
      -- stacks them above the windows.  Written by xmonad-contrib, whose
      -- drawable registry this package cannot import.
    , shOverlayPos :: !(IORef (M.Map ObjectId (Position, Position)))
      -- ^ Where each of those nodes goes.  @river_node_v1.set_position@ is
      -- rendering state and legal only inside a sequence, so the surface's
      -- owner records the position here and the render sequence applies it.
    , shRestart  :: !(IORef (Maybe (FilePath, [String])))
      -- ^ program and arguments to exec once river confirms this window
      -- manager has stopped.  Exec'd directly: @sh -c@ would fork and stay.
    , shLayoutMoved :: !(IORef Bool)
      -- ^ The last layout pass moved a window.  Worker writes, loop takes;
      -- the next @pointer_enter@ is then the layout's doing, not the
      -- pointer's.
    }

-- | The worker's own.  Reached through 'XMonad.Core.riverState' from 'X'
-- code and from nowhere else: the loop's 'XMonad.River.WM.Runtime.Runtime'
-- holds a 'Shared', not this.
data WorkerState m = WorkerState
    { wsInManageSeq :: !(IORef Bool)
      -- ^ guards requests river only permits during a manage sequence
    , wsPlaced :: !(IORef Placed)
      -- ^ Where the last layout put things: the stacking order, for what is
      -- under the pointer, and the rectangles by window, which is what
      -- 'XMonad.Core.getWindowAttributes' answers from.  A window river knows
      -- and this does not hold is unmapped at the origin, as X11 would have
      -- said; the attributes are built when asked, not for every window on
      -- every pass.
    , wsRestack :: !(IORef [Window])
      -- ^ Windows to keep above the layout's order, bottom-to-top.  Dropped
      -- as they stop being placed.
    , wsDragOrigin :: !(IORef (Position, Position))
      -- ^ Where the pointer was when the interactive operation began; river
      -- reports deltas, 'XMonad.Operations.mouseDrag' promises positions.
    , wsAfterLayout :: !(IORef [m ()])
      -- ^ Actions waiting for the layout, newest first; see
      -- 'XMonad.River.afterLayout'.
    , wsUnsized :: !(IORef (S.Set Window))
      -- ^ Floats adopted before river had sized them, whose rectangle is
      -- 'XMonad.Operations.floatLocation''s fallback rather than anyone's
      -- choice.  Proposed 0x0, river's "the window decides", and centred at
      -- the size it decided on the sequence its first @dimensions@ event
      -- asks for.
    , wsLogDue :: !(IORef Bool)
      -- ^ 'XMonad.Operations.windows' ran since the last sequence's log hook.
      -- X11 ran the log hook on every @windows@; here it runs once, at the end
      -- of the sequence, if anything asked for it or the set changed on its
      -- own.
    , wsBorders :: !(IORef (M.Map Window (Maybe Dimension, Maybe BorderColor)))
      -- ^ Per-window overrides of border width and colour.  Sticky until the
      -- window goes: river keeps no border state, so the render sequence
      -- restates borders from the plan and an override has to be remembered.
    , wsSubmapGen :: !(IORef Int)
      -- ^ Numbers each keyboard capture, so a deadline for an earlier one
      -- cannot close the current one.
    , wsAdopted :: !(IORef (S.Set Window))
      -- ^ Windows the manage hook has run for.
    , wsRestored :: !(IORef Bool)
      -- ^ The predecessor's state file has been looked for.
    , wsFloatSizes :: !(IORef (M.Map Window (Int32, Int32)))
      -- ^ Each float's size as last seen, so a size the client changed itself
      -- can be told from one this side proposed and is waiting on.
    }

-- | What @XConf@ carries: the two halves.  The names below are the ones
-- "XMonad.Core", "XMonad.Operations" and contrib read them by.
data RiverState m = RiverState
    { rsShared :: !(Shared m)
    , rsWorker :: !(WorkerState m)
    }

riverManager, riverBindings :: RiverState m -> ObjectId
riverManager = shManager . rsShared
riverBindings = shBindings . rsShared
riverXkbVersion :: RiverState m -> Word32
riverXkbVersion = shXkbVersion . rsShared
riverBorderWidth :: RiverState m -> Dimension
riverBorderWidth = shBorderWidth . rsShared
riverCompositor, riverShm :: RiverState m -> Maybe ObjectId
riverCompositor = shCompositor . rsShared
riverShm = shShm . rsShared
riverWindows :: RiverState m -> IORef (M.Map ObjectId RiverWindow)
riverWindows = shWindows . rsShared
riverOutputs :: RiverState m -> IORef (M.Map ObjectId RiverOutput)
riverOutputs = shOutputs . rsShared
riverSeats :: RiverState m -> IORef (M.Map ObjectId RiverSeat)
riverSeats = shSeats . rsShared
riverDirty :: RiverState m -> TVar Bool
riverDirty = shDirty . rsShared
riverRestart :: RiverState m -> IORef (Maybe (FilePath, [String]))
riverRestart = shRestart . rsShared
riverMailbox :: RiverState m -> Mailbox (m ())
riverMailbox = shMailbox . rsShared
riverLoopJobs :: RiverState m -> Mailbox (IO ())
riverLoopJobs = shLoopJobs . rsShared
riverExtraKeys :: RiverState m -> IORef (Int, [(m (), m ())])
riverExtraKeys = shExtraKeys . rsShared
riverOverlays :: RiverState m -> IORef [ObjectId]
riverOverlays = shOverlays . rsShared
riverOverlayPos :: RiverState m -> IORef (M.Map ObjectId (Position, Position))
riverOverlayPos = shOverlayPos . rsShared
riverCapture :: RiverState m -> IORef (Maybe (InputCapture m))
riverCapture = shCapture . rsShared
riverOps :: RiverState m -> IORef [Op]
riverOps = shOps . rsShared
riverNowOps :: RiverState m -> TVar [Op]
riverNowOps = shNowOps . rsShared
riverKeyboardLayout :: RiverState m -> IORef (Maybe (Int, String))
riverKeyboardLayout = shKeyboardLayout . rsShared

inManageSeq :: RiverState m -> IORef Bool
inManageSeq = wsInManageSeq . rsWorker
riverPlaced :: RiverState m -> IORef Placed
riverPlaced = wsPlaced . rsWorker
riverRestack :: RiverState m -> IORef [Window]
riverRestack = wsRestack . rsWorker
riverDragOrigin :: RiverState m -> IORef (Position, Position)
riverDragOrigin = wsDragOrigin . rsWorker
riverAfterLayout :: RiverState m -> IORef [m ()]
riverAfterLayout = wsAfterLayout . rsWorker
riverUnsized :: RiverState m -> IORef (S.Set Window)
riverUnsized = wsUnsized . rsWorker
riverLogDue :: RiverState m -> IORef Bool
riverLogDue = wsLogDue . rsWorker
riverBorders :: RiverState m -> IORef (M.Map Window (Maybe Dimension, Maybe BorderColor))
riverBorders = wsBorders . rsWorker
riverSubmapGen :: RiverState m -> IORef Int
riverSubmapGen = wsSubmapGen . rsWorker
{-# INLINE riverManager #-}
{-# INLINE riverBindings #-}
{-# INLINE riverXkbVersion #-}
{-# INLINE riverBorderWidth #-}
{-# INLINE riverCompositor #-}
{-# INLINE riverShm #-}
{-# INLINE riverWindows #-}
{-# INLINE riverOutputs #-}
{-# INLINE riverSeats #-}
{-# INLINE riverDirty #-}
{-# INLINE riverRestart #-}
{-# INLINE riverMailbox #-}
{-# INLINE riverLoopJobs #-}
{-# INLINE riverExtraKeys #-}
{-# INLINE riverOverlays #-}
{-# INLINE riverOverlayPos #-}
{-# INLINE riverCapture #-}
{-# INLINE riverOps #-}
{-# INLINE riverNowOps #-}
{-# INLINE riverKeyboardLayout #-}
{-# INLINE inManageSeq #-}
{-# INLINE riverPlaced #-}
{-# INLINE riverRestack #-}
{-# INLINE riverDragOrigin #-}
{-# INLINE riverAfterLayout #-}
{-# INLINE riverUnsized #-}
{-# INLINE riverLogDue #-}
{-# INLINE riverBorders #-}
{-# INLINE riverSubmapGen #-}

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

-- | Back to the plan's colour, keeping a width override.
clearBorderColor :: Borders -> Window -> IO ()
clearBorderColor ref w = atomicModifyIORef' ref $ \m ->
    (M.update (\(wd, _) -> (\d -> (Just d, Nothing)) <$> wd) w m, ())

-- | Drop a window's overrides once it is gone: river recycles object ids.
forgetBorderOverride :: Borders -> Window -> IO ()
forgetBorderOverride ref w = atomicModifyIORef' ref (\m -> (M.delete w m, ()))

-- | What the last layout placed, once: the order for hit-testing and the
-- rectangles for lookups, written together by @applyLayout@.
data Placed = Placed
    { pdOrder :: ![Window]
      -- ^ Topmost first.
    , pdMap   :: !(M.Map Window Rectangle)
    }

-- | Where the last layout put a window, if it placed it.
rectOf :: MonadIO m => RiverState n -> Window -> m (Maybe Rectangle)
rectOf rs w = liftIO (M.lookup w . pdMap <$> readIORef (riverPlaced rs))

-- | Every placed window's rectangle.
placedRects :: MonadIO m => RiverState n -> m (M.Map Window Rectangle)
placedRects rs = liftIO (pdMap <$> readIORef (riverPlaced rs))

-- | The placed windows with their rectangles, topmost first.
placedOrder :: MonadIO m => RiverState n -> m [(Window, Rectangle)]
placedOrder rs = liftIO $ do
    Placed order rects <- readIORef (riverPlaced rs)
    pure [ (w, r) | w <- order, Just r <- [M.lookup w rects] ]

-- | Correct the recorded geometry of a window something just moved outside a
-- layout run, so 'XMonad.Operations.floatLocation' reads the move back rather
-- than undoing it.  A window with no placement is left alone.
updatePlacement :: MonadIO m => RiverState n -> Window -> Rectangle -> m ()
updatePlacement rs w r = liftIO $
    modifyIORef' (riverPlaced rs) $ \p -> p { pdMap = M.adjust (const r) w (pdMap p) }
