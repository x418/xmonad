{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The event loop's state, shared with the worker where it must be.
--
-- Each field has one writing thread.  The loop owns the connection and the
-- fields under /loop only/; the worker owns 'XState' and the fields under
-- /worker only/; 'Shared' is what the worker publishes and the loop waits on.
-- The compositor's view -- windows, outputs, seats -- lives in 'RiverState',
-- written by the loop and read by both.
module XMonad.River.WM.Runtime
  ( Runtime(..)
  , Shared(..)
  , queueAction
  , queueActions
  , claimCapture
  , liveSeats
  , adjust
  , broadcastEvent
  , screensOf
  , allEdges
  , sizeBound
  , linuxButton
  ) where

import Control.Monad.Reader (asks)
import Data.IORef
import Data.Int (Int32)
import Data.Monoid (All(..))
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.Core
import XMonad.River.Connection (Connection, Global)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Protocol.WindowManagement
import XMonad.River.State (InputCapture(..), Shared(..))
import XMonad.River.Types
import XMonad.River.WM.Input (InputRuntime)
import XMonad.River.Wire (ObjectId)
import qualified XMonad.StackSet as W

-- | What the worker publishes and the loop waits on.  Worker-written only.
-- | The loop's state.  Each field has one writing thread; the comment says
-- which when it is not the loop.
data Runtime = Runtime
  { -- constants
    rtConn           :: !Connection
  , rtRegistry       :: !ObjectId
  , rtManager        :: !ObjectId
  , rtBindingsGlobal :: !ObjectId
  , rtXkbVersion     :: !Word32
    -- ^ Negotiated @river_xkb_bindings_v1@ version; @get_seat@ and
    -- @ensure_next_key_eaten@ arrived in 2.
  , rtInput          :: !InputRuntime
    -- ^ Input devices and their configuration; loop only, see
    -- "XMonad.River.WM.Input".
  , rtFollowsMouse   :: !Bool
  , rtKeyActions     :: !(M.Map (KeyMask, KeySym) (X ()))
  , rtButtonActions  :: !(M.Map (KeyMask, Button) (Window -> X ()))
  , rtSubmit         :: !(X () -> IO ())
    -- ^ Hand an action to the worker.
  , rtShared         :: !(Shared X)
    -- ^ What the worker sees too.  The worker's own state is not here: the
    -- loop holds 'X' actions but never evaluates one, and cannot name what
    -- only they may touch.
    -- loop only
  , rtPending        :: !(IORef [X ()])
    -- ^ Binding actions awaiting the next manage sequence, newest first.
  , rtDirtySent      :: !(IORef Bool)
    -- ^ A @manage_dirty@ has gone out and its @manage_start@ has not yet
    -- arrived.  One request per wait: river starts one sequence for any
    -- number of them, and a second only adds a round trip.
  , rtSeqNo          :: !(IORef Int)
  , rtSent           :: !(IORef Int)
    -- ^ 'planSerial' of the last plan transmitted in a manage sequence.
  , rtAsked          :: !(IORef Int)
    -- ^ 'planSerial' a @manage_dirty@ has already been sent for.
  , rtBindings       :: !(IORef (M.Map ObjectId (X ())))
    -- ^ The config's key bindings, by binding object.
  , rtBindingKeys    :: !(IORef (M.Map ObjectId (KeyMask, KeySym)))
  , rtPrefixKeys     :: !(IORef (S.Set (KeyMask, KeySym)))
    -- ^ Keys the config declared as submap prefixes.
  , rtPrefixPressed  :: !(IORef Bool)
    -- ^ A prefix was pressed in the batch before this manage_start.
  , rtPrefixHeld     :: !(IORef Bool)
    -- ^ The config's bindings are disabled for a prefix whose action the
    -- worker has not run yet; released with the plan that ran it.
  , rtPointerBind    :: !(IORef (M.Map ObjectId (Window -> X ())))
  , rtBoundSeats     :: !(IORef (S.Set ObjectId))
  , rtGrabbed        :: !(IORef [ObjectId])
    -- ^ Bindings 'XMonad.River.grabKeys' asked for.
  , rtGrabbedKeys    :: !(IORef (Maybe (Int, [(KeyMask, KeySym)])))
    -- ^ Desired grabbed keys, retained so a newly added seat receives them.
  , rtArmed          :: !(IORef [ObjectId])
    -- ^ Bindings an open capture installed.
  , rtArmedGen       :: !(IORef Int)
    -- ^ The generation of the capture those bindings belong to.  A teardown
    -- the loop initiates -- an unbound key eaten, the deadline -- claims the
    -- capture only if it is still this one, so that a capture armed since
    -- cannot be closed by the end of its predecessor.
  , rtDisarm         :: !(IORef Bool)
    -- ^ A capture ended and its bindings are still installed; torn down in
    -- the next manage sequence, the only place @enable@ is legal.
  , rtLayerDefault   :: !(IORef (Maybe ObjectId))
    -- ^ The output last nominated for layer surfaces that name none.
  , rtStartupSent    :: !(IORef Bool)
  , rtModWatcher     :: !(IORef (Maybe (Word32 -> Word32 -> IO Bool)))
    -- ^ What to run when the watched modifiers change.  One slot:
    -- @modifiers_watch@ is one mask per seat. The result says whether the
    -- watched transition ended the interaction; unrelated transitions retain
    -- the watcher.
  , rtModWatched     :: !(IORef Bool)
    -- ^ A @modifiers_watch@ with a non-zero mask is in force and has to be
    -- withdrawn when the capture ends, or river reports -- and starts a
    -- manage sequence for -- every modifier change for the rest of the session.
  , rtEatGenerations :: !(IORef (M.Map ObjectId Int))
    -- ^ Outstanding @ensure_next_key_eaten@ request per xkb-seat object. The
    -- generation prevents a late event for a superseded capture ending its
    -- replacement.
  , rtGlobals        :: !(IORef (M.Map Word32 Global))
    -- ^ The registry, kept current, so an output's @wl_output@ can be bound
    -- by the name @river_output_v1.wl_output@ carries.
  , rtWindowsGen     :: !(IORef Int)
    -- ^ Counts the windows river has announced; part of what decides whether
    -- a render sequence has anything new to send.
    -- what the last transmission said, so the next sends only what changed
  , rtLastManage     :: !(IORef (M.Map Window ((Dimension, Dimension, Bool), (Int32, Int32))))
    -- ^ Per placed window: the dimensions and tiled-ness last proposed, and
    -- the size the client had when that was decided.
  , rtLastRender     :: !(IORef (M.Map Window (Rectangle, (Dimension, BorderColor))))
    -- ^ Position and border, per shown window.
  , rtLastStack      :: !(IORef [ObjectId])
    -- ^ The node order last placed, bottom to top.
  , rtLastOverlayPos :: !(IORef (M.Map ObjectId (Position, Position)))
    -- ^ Where each listed overlay was last put.
  , rtLastRendered   :: !(IORef (Int, Int, [ObjectId], M.Map ObjectId (Position, Position)))
    -- ^ What the last render sequence was given: plan serial, window count,
    -- overlays and their positions.  A render sequence given the same again
    -- -- river starts one whenever a client changes its own size -- sends
    -- nothing.
  }

-- | Queue an action for the next manage sequence.  Loop thread only.
--
-- Nothing is asked of river here.  Every event that queues an action -- a
-- binding's press, a click, the pointer entering a window, a drag's motion --
-- is one river sends in the batch that precedes a @manage_start@, and that
-- @manage_start@ drains the queue in the same pass.  Asking for a sequence
-- as well would buy an empty one after every key press.  What survives a pass
-- undrained -- posted from another thread, or a batch split across two reads
-- -- the loop asks for at the end of the pass.
queueAction :: Runtime -> X () -> IO ()
queueAction rt act = queueActions rt [act]

queueActions :: Runtime -> [X ()] -> IO ()
queueActions _ [] = pure ()
queueActions rt acts = modifyIORef' (rtPending rt) (reverse acts ++)

-- | Take the open capture if it is the one of the given generation, leaving
-- it otherwise.  Whoever takes it owns its teardown.  Loop thread only.
claimCapture :: Runtime -> Int -> IO (Maybe (InputCapture X))
claimCapture rt gen = atomicModifyIORef' (shCapture (rtShared rt)) $ \case
  Just cap | icGeneration cap == gen -> (Nothing, Just cap)
  other -> (other, Nothing)

-- | The seats river has not removed.  A request to a removed seat is ignored
-- by the server, but the object it created would be ours to leak.
liveSeats :: M.Map ObjectId RiverSeat -> [RiverSeat]
liveSeats = filter (not . rsRemoved) . M.elems

adjust :: IORef (M.Map ObjectId a) -> ObjectId -> (a -> a) -> IO ()
adjust ref k f = modifyIORef' ref (M.adjust f k)

broadcastEvent :: Event -> X All
broadcastEvent ev = do
  hook <- asks (handleEventHook . config)
  userCodeDef (All True) (hook ev)

-- | The screens a 'WindowSet' currently has, current first.
screensOf :: WindowSet -> [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
screensOf ws = W.current ws : W.visible ws

allEdges :: Word32
allEdges = riverWindowV1EdgesTop + riverWindowV1EdgesBottom
         + riverWindowV1EdgesLeft + riverWindowV1EdgesRight

-- | A dimension bound river reports as zero or less was not stated.
sizeBound :: Int32 -> Int32 -> Maybe (Dimension, Dimension)
sizeBound w h
  | w > 0 && h > 0 = Just (fromIntegral w, fromIntegral h)
  | otherwise      = Nothing

-- | X11 button numbers to Linux input event codes.
linuxButton :: Button -> Word32
linuxButton = \case
  1 -> 0x110  -- BTN_LEFT
  2 -> 0x112  -- BTN_MIDDLE
  3 -> 0x111  -- BTN_RIGHT
  4 -> 0x113  -- BTN_SIDE
  5 -> 0x114  -- BTN_EXTRA
  n -> 0x110 + fromIntegral n
