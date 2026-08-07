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
  , setMainThread
  , pidFilePath
  , warnUnimplemented
  , publishGeometry
  , lookupGeometry
  , publishSizeHints
  , lookupSizeHints
  , setBorderWidth
  , setBorderColor
  , lookupBorderOverride
  , forgetBorderOverride
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.River.Types (BorderColor, Dimension, Window, WindowAttributes, SizeHints, noSizeHints)

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
setBorderWidth w n = modifyIORef' bordersRef $
  M.alter (\o -> Just (Just n, maybe Nothing snd o)) w

setBorderColor :: Window -> BorderColor -> IO ()
setBorderColor w c = modifyIORef' bordersRef $
  M.alter (\o -> Just (maybe Nothing fst o, Just c)) w

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
forgetBorderOverride w = modifyIORef' bordersRef (M.delete w)

-- | Thrown into the event loop thread to ask for a restart.
data RestartRequested = RestartRequested deriving (Show)

instance E.Exception RestartRequested

{-# NOINLINE mainThreadRef #-}
mainThreadRef :: IORef (Maybe ThreadId)
mainThreadRef = unsafePerformIO (newIORef Nothing)

-- | Record the thread running the event loop, so 'sendRestart' can reach it.
setMainThread :: IO ()
setMainThread = writeIORef mainThreadRef . Just =<< myThreadId

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

-- | Where the running window manager records its process id, given the data
-- directory.
--
-- This exists because @xmonad --restart@ has to reach a window manager it is
-- not part of, and under river there is nothing between the two processes to
-- carry the request.  X11 had one for free: the second process put a client
-- message on the root window and the server delivered it.  river mediates the
-- @stop@/@finished@ handover but offers no channel for anything else, so the
-- rendezvous has to be on the filesystem, and @SIGUSR1@ carries the request.
--
-- Deliberately not in "XMonad.Core" next to 'XMonad.Core.stateFileName'.  That
-- module's exports are required to be a subset of the X11 build's, and X11 has
-- no such file because it never needed one.
pidFilePath :: FilePath -> FilePath
pidFilePath dir = dir </> "xmonad-river.pid"

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
