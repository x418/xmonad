{-# LANGUAGE LambdaCase #-}

-- | Process-level plumbing that needs no window manager state.
--
-- Two things here are process globals, deliberately.  'sendRestart' keeps
-- upstream's argument-free signature and is called from threads that hold
-- nothing; 'warnUnimplemented' is a once-per-process diagnostic reached from
-- @MonadIO@ code with nothing to thread it through.  Everything else the
-- backend keeps lives in "XMonad.River.State".
module XMonad.River.Runtime
  ( RestartRequested(..)
  , sendRestart
  , exitLoopWith
  , setMainThread
  , warnUnimplemented
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import System.Exit (ExitCode)
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

-- | Ask the loop to end the process with this code, from any thread.
--
-- 'System.Exit.exitWith' on the worker only unwinds the worker; the loop is
-- the thread whose exit ends the process, and the one that tells river first.
exitLoopWith :: ExitCode -> IO ()
exitLoopWith code = readIORef mainThreadRef >>= \case
  Just tid -> E.throwTo tid code
  Nothing -> E.throwIO code

-- | Ask the window manager to restart itself, from any thread.
--
-- An asynchronous exception into the loop's thread: under the threaded
-- runtime it interrupts the blocking socket read, so this takes effect at
-- once rather than at the next event.
sendRestart :: IO ()
sendRestart = readIORef mainThreadRef >>= \case
  Just tid -> E.throwTo tid RestartRequested
  Nothing -> hPutStrLn stderr
    "xmonad-river: sendRestart called before the event loop started"

-- | Complain, once per process, that something is doing less than it says.
--
-- Rare by design: anything that cannot be faithfully ported is not exported.
-- This is for a name that must stay because shared code depends on it, but
-- whose behaviour is partial.
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
