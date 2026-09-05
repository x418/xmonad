{-# LANGUAGE ForeignFunctionInterface #-}

-- | Sending and receiving file descriptors over the Wayland socket.
--
-- Wayland attaches a descriptor as @SCM_RIGHTS@ ancillary data to the same
-- @sendmsg@ that carries the request bytes, and the receiver correlates them
-- by order.  @network@ cannot express that: its @sendMsg@ and @sendBufMsg@
-- both take a mandatory 'N.SockAddr' and always set @msg_name@, which Linux
-- rejects with @EISCONN@ on a connected socket, and its @sendFd@ uses a
-- different single-byte protocol of its own.  So the @msghdr@ work happens in
-- @src\/cbits\/wl-fd.c@, where the @CMSG_*@ macros exist.
--
-- This is the only C in the package and it exists only under @-f river@.  It
-- is here because drawing needs it: a @wl_buffer@ comes from a @wl_shm_pool@,
-- which comes from @wl_shm.create_pool@, which takes a descriptor.  Exactly
-- one request in the whole drawing path does -- everything else is ordinary
-- wire format the codec already handles -- but without that one, a window
-- manager cannot put a pixel on the screen.
module XMonad.River.Socket
  ( sendWithFds
  , recvWithFds
  , recvWithFdsInto
  ) where

import Control.Monad (when)
import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIfMinus1RetryMayBlock)
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Conc (threadWaitRead, threadWaitWrite)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import System.Posix.Types (Fd (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Network.Socket as N

foreign import ccall unsafe "hs_wl_sendmsg_fds"
  c_sendmsg_fds :: CInt -> Ptr Word8 -> CSize -> Ptr CInt -> CSize -> IO CInt

foreign import ccall unsafe "hs_wl_recvmsg_fds"
  c_recvmsg_fds :: CInt -> Ptr Word8 -> CSize -> Ptr CInt -> CSize -> Ptr CSize
                -> IO CInt

-- | The most descriptors one message may carry.  Kept in step with the array
-- sizes in @src\/cbits\/wl-fd.c@, which rejects more.
maxFds :: Int
maxFds = 16

-- | Write bytes, attaching descriptors to the same message.
--
-- Returns the number of bytes actually written, which may be fewer than
-- offered; the caller retries the remainder.  The descriptors go with the
-- first such write, which is what Wayland's ordering rule requires.
sendWithFds :: N.Socket -> BS.ByteString -> [Fd] -> IO Int
sendWithFds sock bs fds = do
  when (length fds > maxFds) $
    ioError (userError ("sendWithFds: more than " ++ show maxFds ++ " descriptors"))
  N.withFdSocket sock $ \fd ->
    BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
      withArrayLen [n | Fd n <- fds] $ \nfds fdp ->
        -- MayBlock, not plain Retry.  GHC's IO manager puts every socket it
        -- owns into non-blocking mode, so a full send buffer comes back as
        -- EAGAIN rather than waiting -- and a plain retry would treat that as
        -- a fatal error.  The recovery is to wait for writability the way the
        -- IO manager does and go again.
        fmap fromIntegral
          . throwErrnoIfMinus1RetryMayBlock "sendWithFds"
              (c_sendmsg_fds (fromIntegral fd) (castPtr p) (fromIntegral len)
                             fdp (fromIntegral nfds))
          $ threadWaitWrite (fromIntegral fd)

-- | Read up to @n@ bytes, collecting any descriptors that arrive with them.
--
-- Received descriptors have @CLOEXEC@ set.  A window manager forks on every
-- @spawn@, and a buffer descriptor leaking into a child would keep the shared
-- mapping alive past its owner.
recvWithFds :: N.Socket -> Int -> IO (BS.ByteString, [Fd])
recvWithFds sock n = allocaBytes n $ \buf -> recvWithFdsBuf sock buf n

-- | As 'recvWithFds', into a buffer the caller keeps: a connection reads
-- into the same pinned block every time rather than allocating one per
-- read, and only the bytes that arrived are copied out.
recvWithFdsInto :: N.Socket -> ForeignPtr Word8 -> Int -> IO (BS.ByteString, [Fd])
recvWithFdsInto sock fptr n = withForeignPtr fptr $ \buf -> recvWithFdsBuf sock buf n

recvWithFdsBuf :: N.Socket -> Ptr Word8 -> Int -> IO (BS.ByteString, [Fd])
recvWithFdsBuf sock buf n =
  N.withFdSocket sock $ \fd ->
      allocaArray maxFds $ \fdp ->
        alloca $ \nfdsp -> do
          -- As sendWithFds: an empty socket answers EAGAIN rather than
          -- blocking, and the fix is to wait for readability.  This is what
          -- the network package's recv does internally, and losing it was the
          -- first thing that broke when the window manager was finally run.
          got <- throwErrnoIfMinus1RetryMayBlock "recvWithFds"
            (c_recvmsg_fds (fromIntegral fd) buf (fromIntegral n)
                           fdp (fromIntegral maxFds) nfdsp)
            (threadWaitRead (fromIntegral fd))
          nfds <- peek nfdsp
          fds <- peekArray (fromIntegral nfds) fdp
          bs <- BS.packCStringLen (castPtr buf, fromIntegral got)
          pure (bs, map Fd fds)
