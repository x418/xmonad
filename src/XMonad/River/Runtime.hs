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
  , restartPending
  , takeRestart
  , exitLoopWith
  , setMainThread
  , warnUnimplemented
  ) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, check, newTVarIO, readTVar, swapTVar, writeTVar)
import Control.Monad (unless, void, when)
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
-- A flag the loop waits on and takes between passes, where no batch is half
-- dispatched and no request half written.  A loop that has not taken it
-- within 'restartGraceMicros' is wedged, and gets 'RestartRequested' as an
-- asynchronous exception instead, which is what @xmonad --restart@ is for.
sendRestart :: IO ()
sendRestart = readIORef mainThreadRef >>= \case
  Just tid -> do
    atomically (writeTVar restartWanted True)
    void . forkIO $ do
      threadDelay restartGraceMicros
      stuck <- atomically (swapTVar restartWanted False)
      when stuck (E.throwTo tid RestartRequested)
  Nothing -> hPutStrLn stderr
    "xmonad-river: sendRestart called before the event loop started"

{-# NOINLINE restartWanted #-}
restartWanted :: TVar Bool
restartWanted = unsafePerformIO (newTVarIO False)

-- | Retries until a restart has been asked for; for the loop's wait.
restartPending :: STM ()
restartPending = readTVar restartWanted >>= check

-- | Whether a restart has been asked for, clearing the request.
takeRestart :: IO Bool
takeRestart = atomically (swapTVar restartWanted False)

restartGraceMicros :: Int
restartGraceMicros = 2 * 1000 * 1000

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
