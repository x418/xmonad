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
  , warnUnimplemented
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as E
import qualified Data.Set as S

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
