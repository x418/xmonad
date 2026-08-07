-- | Handing work to the event loop from another thread.
--
-- The event loop is single-threaded and is the sole owner of the connection:
-- request buffering is plain 'IORef's, so a background thread that touched it
-- would corrupt the request stream rather than race benignly.  But plenty of
-- useful things happen off that thread -- a timer expiring, a subprocess
-- finishing -- and they need a way in.
--
-- X11 gave this away for free.  Any process could @sendEvent@ a client message
-- to the root window, and the window manager's loop was already reading that
-- socket, so a background thread simply posted an event and the loop woke up.
-- @XMonad.Util.Timer@ is built entirely out of that trick: it forks, sleeps,
-- and posts.  There is no equivalent under Wayland -- the compositor will not
-- relay messages between a window manager and its own threads -- so the
-- channel has to exist here.
--
-- A mailbox is that channel: a queue plus a pipe.  The queue carries the work
-- and the pipe carries the fact that there is work, because the loop is parked
-- in a blocking read and a queue alone cannot wake it.  Waiting on the pipe
-- alongside the socket is what turns "something was posted" into "the loop
-- notices now" rather than "the loop notices at the next unrelated Wayland
-- event", which for an idle desktop could be minutes.
--
-- Deliberately not parameterised over the monad: this module knows nothing
-- about 'XMonad.Core.X', so that "XMonad.Core" can hold a @Mailbox (X ())@
-- without a circular import.
module XMonad.River.Mailbox
  ( Mailbox
  , newMailbox
  , post
  , drain
  , mailboxFd
  , clearWakeups
  ) where

import Control.Monad (void)
import Data.IORef
import System.Posix.IO (createPipe, fdReadBuf, fdWriteBuf)
import System.Posix.Types (Fd)
import Foreign.Marshal.Alloc (allocaBytes)

data Mailbox a = Mailbox
  { mbQueue :: !(IORef [a])
    -- ^ Newest first; 'drain' pays the one reversal.
  , mbRead  :: !Fd
  , mbWrite :: !Fd
  }

newMailbox :: IO (Mailbox a)
newMailbox = do
  (r, w) <- createPipe
  q <- newIORef []
  pure (Mailbox q r w)

-- | The descriptor to wait on alongside the Wayland socket.
mailboxFd :: Mailbox a -> Fd
mailboxFd = mbRead

-- | Post work from any thread.
--
-- Both halves matter and in this order: the queue is updated before the pipe
-- is written, so the loop can never be woken by a byte and then find nothing
-- to do.  The reverse order would be a race that shows up as a timer that
-- fires one event late.
post :: Mailbox a -> a -> IO ()
post mb x = do
  atomicModifyIORef' (mbQueue mb) $ \xs -> (x : xs, ())
  allocaBytes 1 $ \p -> void (fdWriteBuf (mbWrite mb) p 1)

-- | Take everything posted so far, oldest first.
drain :: Mailbox a -> IO [a]
drain mb = atomicModifyIORef' (mbQueue mb) $ \xs -> ([], reverse xs)

-- | Consume the wakeup bytes.
--
-- One byte per 'post', and several posts may be coalesced into a single
-- wakeup, so this reads whatever is there rather than one byte per drain.  It
-- must not block: the pipe is only ever read after the loop has been told it
-- is readable.
clearWakeups :: Mailbox a -> IO ()
clearWakeups mb = allocaBytes 64 $ \p -> void (fdReadBuf (mbRead mb) p 64)
