-- | Handing work to an event loop from another thread.
--
-- An event loop here owns a 'XMonad.River.Connection.Connection' outright and
-- spends its life blocked on the compositor socket.  Other threads -- a timer,
-- a prompt's client thread, the worker -- need to hand it work and have it
-- notice now rather than at the next unrelated Wayland event.  X11 gave that
-- away for free with client messages to the root window; Wayland relays
-- nothing between a window manager and its own threads, so the channel is
-- ours.
--
-- A mailbox is a 'TVar' queue.  Posting is a transaction, and a loop waiting
-- in 'waitSocketOr' has that transaction's commit as its wakeup: no pipe, no
-- watcher threads.
--
-- Deliberately not parameterised over the monad: this module knows nothing
-- about 'XMonad.Core.X', so that "XMonad.Core" can hold a @Mailbox (X ())@
-- without a circular import.
module XMonad.River.Mailbox
  ( Mailbox
  , newMailbox
  , post
  , drain
  , awaitMail
  , waitSocketOr
  ) where

import Control.Concurrent.STM
import GHC.Conc (threadWaitReadSTM)
import System.Posix.Types (Fd)

-- | Newest first; 'drain' pays the one reversal.
newtype Mailbox a = Mailbox (TVar [a])

newMailbox :: IO (Mailbox a)
newMailbox = Mailbox <$> newTVarIO []

-- | Post work from any thread.
post :: Mailbox a -> a -> IO ()
post (Mailbox q) x = atomically (modifyTVar' q (x :))

-- | Take everything posted so far, oldest first.
drain :: Mailbox a -> IO [a]
drain (Mailbox q) = atomically (stateTVar q (\xs -> (reverse xs, [])))

-- | Retries until something has been posted.  Composes with 'orElse'.
awaitMail :: Mailbox a -> STM ()
awaitMail (Mailbox q) = readTVar q >>= check . not . null

-- | Block until the descriptor is readable ('Left') or the alternative
-- commits ('Right').
--
-- Every event loop in this backend waits like this: on its socket and on
-- whatever else may wake it, in one transaction.  The socket registration is
-- cancelled when the alternative wins, so registrations do not pile up in the
-- IO manager.
waitSocketOr :: Fd -> STM a -> IO (Either () a)
waitSocketOr fd alt = do
  (ready, cancel) <- threadWaitReadSTM fd
  r <- atomically ((Left <$> ready) `orElse` (Right <$> alt))
  case r of
    Left ()  -> pure ()
    Right _  -> cancel
  pure r
