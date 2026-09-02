{-# LANGUAGE LambdaCase #-}

-- | Process-level plumbing that needs no window manager state.
--
-- Kept separate from "XMonad.Core" so that "XMonad.River" can re-export it
-- without "XMonad.Core" having to, and separate from "XMonad.River" so that
-- "XMonad.Core" can use 'sendRestart' -- which xmonad exports from
-- @XMonad.Main@ on both backends -- without importing it back.
module XMonad.River.Runtime
  ( RestartRequested(..)
  , sendRestart
  , exitLoopWith
  , setMainThread
  , warnUnimplemented
  , publishGeometry
  , lookupGeometry
  , publishSizeHints
  , lookupSizeHints
  , setBorderWidth
  , setBorderColor
  , getWindowAttributes
  , getWMNormalHints
  , getGeometry
  , setWindowBorderWidth
  , setWindowBorder
  , lookupBorderOverride
  , forgetBorderOverride
  , nextSubmapGeneration
  , currentSubmapGeneration
  , setModifierWatcher
  , takeModifierWatcher
  , emitOp
  , takeOps
  , emitNow
  , takeNowOps
  , nowOpsPending
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Word (Word32)
import System.Exit (ExitCode)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.River.Types (BorderColor, Dimension, Pixel, pixelColor, Position, Window,
                           WindowAttributes(..), SizeHints, noSizeHints)
import XMonad.River.Connection (Display)
import XMonad.River.Plan (Op)
import XMonad.River.Wire (ObjectId, nullObject)

{-# NOINLINE geometryRef #-}
geometryRef :: IORef (M.Map Window WindowAttributes)
geometryRef = unsafePerformIO (newIORef M.empty)

-- | Record what is known about every window's geometry, for the benefit of the
-- @IO@-shaped queries below.
--
-- A process-level 'IORef' rather than a field of 'XMonad.Core.XConf', because
-- the callers it exists for are shaped like @getWindowAttributes dpy w@ and run
-- in 'IO' with nothing but the connection to go on.  Under X11 that was fine --
-- the answer was the server's, and asking it was an @IO@ action.  Here the
-- answer is the window manager's own, and there is exactly one window manager
-- per process, so a global is a faithful stand-in for what the server used to
-- be.  It is the same reasoning as 'setMainThread' above.
--
-- Written once per manage sequence, by "XMonad.River.WM".
publishGeometry :: M.Map Window WindowAttributes -> IO ()
publishGeometry = writeIORef geometryRef

-- | What is known about one window, or 'Nothing' if river has never mentioned
-- it.
lookupGeometry :: Window -> IO (Maybe WindowAttributes)
lookupGeometry w = M.lookup w <$> readIORef geometryRef

{-# NOINLINE hintsRef #-}
hintsRef :: IORef (M.Map Window SizeHints)
hintsRef = unsafePerformIO (newIORef M.empty)

-- | Record every window's size hints, for the same reason as
-- 'publishGeometry': @getWMNormalHints dpy w@ runs in 'IO'.
publishSizeHints :: M.Map Window SizeHints -> IO ()
publishSizeHints = writeIORef hintsRef

-- | A window's size hints, or 'noSizeHints' if river has not mentioned it.
--
-- Total, where 'lookupGeometry' is partial: X11's @getWMNormalHints@ answered
-- with an empty hint structure for a window that had set none, so there is a
-- correct total answer here and no reason to make callers handle a failure.
lookupSizeHints :: Window -> IO SizeHints
lookupSizeHints w = M.findWithDefault noSizeHints w <$> readIORef hintsRef

{-# NOINLINE bordersRef #-}
bordersRef :: IORef (M.Map Window (Maybe Dimension, Maybe BorderColor))
bordersRef = unsafePerformIO (newIORef M.empty)

-- | Override the border width of one window, or its colour.
--
-- X11 had no state to keep for this: @setWindowBorderWidth@ was a call on the
-- server, and the value stuck until something set it again.  river has no such
-- memory -- borders are rendering state, reapplied from scratch during every
-- render sequence -- so the override has to be remembered here for
-- "XMonad.River.WM" to apply.
--
-- One consequence is worth stating: an override set here is /sticky/, where
-- X11's colour was overwritten by the next @windows@ call.  It has to be.  A
-- non-sticky override under river would last until the next render sequence,
-- which is to say no time at all.
setBorderWidth :: Window -> Dimension -> IO ()
setBorderWidth w n = atomicModifyIORef' bordersRef $ \m ->
  (M.alter (\o -> Just (Just n, maybe Nothing snd o)) w m, ())

setBorderColor :: Window -> BorderColor -> IO ()
setBorderColor w c = atomicModifyIORef' bordersRef $ \m ->
  (M.alter (\o -> Just (maybe Nothing fst o, Just c)) w m, ())

-- | What has been overridden for one window.  Total: no override is
-- @(Nothing, Nothing)@.
lookupBorderOverride :: Window -> IO (Maybe Dimension, Maybe BorderColor)
lookupBorderOverride w = M.findWithDefault (Nothing, Nothing) w <$> readIORef bordersRef

-- | Drop a window's overrides, once river says it is gone.
--
-- Not housekeeping that can be skipped: river recycles object ids, so an entry
-- left behind would reappear on an unrelated window as a border nobody asked
-- for.
forgetBorderOverride :: Window -> IO ()
forgetBorderOverride w = atomicModifyIORef' bordersRef (\m -> (M.delete w m, ()))

{-# NOINLINE submapGenRef #-}
submapGenRef :: IORef Int
submapGenRef = unsafePerformIO (newIORef 0)

-- | Claim the next submap generation, for a submap that is about to open.
--
-- Exists so that a submap's abandonment timer can tell whether the submap it
-- was started for is still the one that is open.  Without it, a timer left
-- over from a submap that ended normally would cancel whichever submap
-- happened to be open when it fired.
nextSubmapGeneration :: IO Int
nextSubmapGeneration = atomicModifyIORef' submapGenRef (\n -> (n + 1, n + 1))

currentSubmapGeneration :: IO Int
currentSubmapGeneration = readIORef submapGenRef

{-# NOINLINE modWatcherRef #-}
modWatcherRef :: IORef (Maybe (Word32 -> Word32 -> IO ()))
modWatcherRef = unsafePerformIO (newIORef Nothing)

-- | Register what to run when the seat's watched modifiers change.
--
-- One slot, not a list: @modifiers_watch@ is a single mask per seat and the
-- second caller would silently replace the first's mask anyway.  A process
-- global rather than a field of the 'Runtime' because the listener that feeds
-- it is installed once per seat, in 'IO', while the thing that wants it --
-- 'XMonad.River.whileModifiersHeld' -- runs in 'X' much later.
setModifierWatcher :: Maybe (Word32 -> Word32 -> IO ()) -> IO ()
setModifierWatcher = writeIORef modWatcherRef

-- | Take the current watcher, leaving none.
--
-- Taking rather than reading, so that the modifier release which concludes an
-- interaction cannot be delivered twice.
takeModifierWatcher :: IO (Maybe (Word32 -> Word32 -> IO ()))
takeModifierWatcher = atomicModifyIORef' modWatcherRef (\w -> (Nothing, w))

-- | Thrown into the event loop thread to ask for a restart.
data RestartRequested = RestartRequested deriving (Show)

instance E.Exception RestartRequested

{-# NOINLINE mainThreadRef #-}
mainThreadRef :: IORef (Maybe ThreadId)
mainThreadRef = unsafePerformIO (newIORef Nothing)

-- | Record the thread running the event loop, so 'sendRestart' can reach it.
setMainThread :: IO ()
setMainThread = writeIORef mainThreadRef . Just =<< myThreadId

-- | Ask the window manager to end the session, from any thread.
--
-- 'System.Exit.exitWith' from the worker only unwinds the worker: its
-- 'ExitCode' is caught by the handler that keeps one bad action from taking
-- the window manager down with it, and the session carries on. The loop is
-- the thread whose exit ends the process, and the one that can tell river
-- first, so hand it there the same way 'sendRestart' does.
exitLoopWith :: ExitCode -> IO ()
exitLoopWith code = readIORef mainThreadRef >>= \case
  Just tid -> E.throwTo tid code
  Nothing -> E.throwIO code

-- | Ask the window manager to restart itself, from any thread.
--
-- This exists for the same reason xmonad's does: @restart@ runs in @X@, which
-- is a @StateT@ over the event loop's own state, so a forked thread cannot
-- call it -- and @M-q@ typically forks to run a rebuild script and then wants
-- a restart.
--
-- xmonad solved it by posting a client message to the X11 event queue.  There
-- is no equivalent queue here, but Haskell offers something better: an
-- asynchronous exception thrown into the event loop's thread.  Under the
-- threaded runtime the loop's blocking socket read is interruptible, so this
-- takes effect immediately rather than at the next event.
sendRestart :: IO ()
sendRestart = readIORef mainThreadRef >>= \case
  Just tid -> E.throwTo tid RestartRequested
  Nothing -> hPutStrLn stderr
    "xmonad-river: sendRestart called before the event loop started"

-- | Complain, once per process, that something is doing less than it says.
--
-- The rule in this backend is that anything which cannot be faithfully ported
-- is not exported at all, so this is deliberately rare -- it is for the cases
-- where the name must stay because it is load-bearing in shared code, but the
-- behaviour is partial.  Silence would be the wrong default: a rule that never
-- fires looks like a bug in the config, and the person debugging it has no
-- reason to suspect the backend.
warnUnimplemented
  :: MonadIO m
  => String  -- ^ what is partial, e.g. @"mouseDrag"@
  -> String  -- ^ what happens instead, and what to do about it
  -> m ()
warnUnimplemented name explanation = liftIO $ do
  already <- atomicModifyIORef' warnedRef $ \seen ->
    (S.insert name seen, S.member name seen)
  unless already $
    hPutStrLn stderr ("xmonad-river: " ++ name ++ " is not implemented. " ++ explanation)

{-# NOINLINE warnedRef #-}
warnedRef :: IORef (S.Set String)
warnedRef = unsafePerformIO (newIORef S.empty)

-- | A window's attributes.
--
-- Kept in 'IO' with the same signature the X11 version had, so that the
-- @io $ getWindowAttributes d w@ spelling used throughout xmonad-contrib still
-- compiles.  There is no server to ask, so the answer comes from what the last
-- layout run decided; see 'XMonad.River.Types.WindowAttributes'.
--
-- Throws for a window river has never mentioned, as @XGetWindowAttributes@
-- did.  Callers that would rather not, and most should not, can use
-- 'withWindowAttributes'.
getWindowAttributes :: Display -> Window -> IO WindowAttributes
getWindowAttributes _ win = lookupGeometry win >>= \case
    Just wa -> pure wa
    Nothing -> ioError . userError $
        "getWindowAttributes: no such window: " ++ show win

-- | A window's size hints.
--
-- Same signature as X11's, so @io $ getWMNormalHints d w@ still compiles.
-- River reports a minimum and a maximum and nothing else; see
-- 'XMonad.River.Types.SizeHints' for what the remaining fields do.
getWMNormalHints :: Display -> Window -> IO SizeHints
getWMNormalHints _ = lookupSizeHints

-- | Override how wide a border the window manager draws around one window.
--
-- Zero removes it, which is what "XMonad.Layout.NoBorders" is built on.
--
-- The 'Display' is accepted and unused, as elsewhere: there is one connection
-- and 'XConf' already has it.  Keeping the parameter lets a call site written
-- for X11 compile unchanged.
setWindowBorderWidth :: Display -> Window -> Dimension -> IO ()
setWindowBorderWidth _ = setBorderWidth

-- | Override the colour of one window's border.
--
-- Takes a 'Pixel', as X11 did.  What a 'Pixel' /is/ differs -- there is no
-- colormap to index into, so it is the packed colour itself -- but the
-- signature and the meaning at the call site are unchanged.  Border colours
-- reach the compositor as RGBA, so this widens; a caller that wants to say
-- something a 'Pixel' cannot, such as a transparent border, wants
-- 'XMonad.River.Types.BorderColor' and 'XMonad.River.Runtime.setBorderColor'.
setWindowBorder :: Display -> Window -> Pixel -> IO ()
setWindowBorder _ w = setBorderColor w . pixelColor

-- | Geometry in the tuple shape X11's @getGeometry@ returned:
-- @(root, x, y, width, height, border width, depth)@.
--
-- The first component is 'nullWindow' rather than a root window id, because
-- there is no root window; the last is zero, because there is no visual depth
-- to report.  Callers in xmonad-contrib discard both.
getGeometry :: Display -> Window
            -> IO (Window, Position, Position, Dimension, Dimension, Dimension, Int)
getGeometry dpy win = do
    wa <- getWindowAttributes dpy win
    pure ( nullObject
         , wa_x wa, wa_y wa, wa_width wa, wa_height wa, wa_border_width wa, 0 )

--------------------------------------------------------------------------------
-- One-shot requests from user code

-- | Requests that user code asked for and the event loop has not yet sent.
--
-- A process global for the reason 'publishGeometry' is one: the callers are
-- ordinary @X@ actions -- 'XMonad.Operations.kill', 'XMonad.Operations.warpPointer'
-- -- which have an 'XMonad.Core.XConf' but no route to the event loop, and
-- there is exactly one window manager per process.
--
-- Queued rather than sent, because river accepts most of these only during a
-- manage sequence and user code does not run inside one.  Unlike the plan,
-- which is restated in full every sequence, these are effects that must happen
-- exactly once -- sending @close@ twice kills a second window -- so they are
-- drained as they are transmitted rather than kept.
{-# NOINLINE opsRef #-}
opsRef :: IORef [Op]
opsRef = unsafePerformIO (newIORef [])

-- | Ask for a request to go out with the next sequence.  Newest first; 'takeOps'
-- pays the one reversal.
emitOp :: MonadIO m => Op -> m ()
emitOp op = liftIO (atomicModifyIORef' opsRef (\ops -> (op : ops, ())))

-- | Take everything queued, in the order it was asked for, leaving none.
takeOps :: IO [Op]
takeOps = atomicModifyIORef' opsRef (\ops -> ([], reverse ops))

-- | Requests that need no manage sequence, and must not wait for one.
--
-- Separate from 'opsRef' because those are drained while a sequence is being
-- transmitted, and these have to happen whether or not one is coming: ending
-- the session, or asking river to release this window manager, would otherwise
-- wait on a sequence that nothing is going to request.  The event loop drains
-- these on every pass.
{-# NOINLINE nowOpsRef #-}
nowOpsRef :: TVar [Op]
nowOpsRef = unsafePerformIO (newTVarIO [])

-- | Ask for a request to go out on the event loop's next pass.  A 'TVar' so
-- that the loop, which waits on it, wakes up.
emitNow :: MonadIO m => Op -> m ()
emitNow op = liftIO (atomically (modifyTVar' nowOpsRef (op :)))

-- | Take everything queued, oldest first, leaving none.
takeNowOps :: IO [Op]
takeNowOps = atomically (stateTVar nowOpsRef (\ops -> (reverse ops, [])))

-- | Retries until something is queued; for the loop's wait.
nowOpsPending :: STM ()
nowOpsPending = readTVar nowOpsRef >>= check . not . null
