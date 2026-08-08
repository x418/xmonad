{-# LANGUAGE ForeignFunctionInterface #-}

-- | Shared-memory pixel buffers.
--
-- A @wl_buffer@'s pixels live in memory the compositor can read, which means an
-- anonymous file both processes map.  The chain is
-- @memfd@ -> 'wlShmCreatePool' -> @wl_shm_pool.create_buffer@, and the first
-- step is the only place in the whole drawing path that passes a file
-- descriptor; see "XMonad.River.Socket" for why that took work.
--
-- This module deliberately knows nothing about drawing.  It hands out a
-- 'Ptr' to writable, correctly-strided memory and lets the caller fill it.
-- Under X11 the equivalent split is that xmonad core owns no font or graphics
-- context code either -- all of that is xmonad-contrib's, and the same applies
-- here: cairo and pango belong on the contrib side of the line, not in the
-- window manager.
module XMonad.River.Buffer
  ( Buffer(..)
  , newBuffer
  , destroyBuffer
  , bufferSize
  ) where

import Control.Monad (when)
import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIf, throwErrnoIfNull, throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import System.Posix.Types (Fd (..))

import XMonad.River.Connection (Connection)
import XMonad.River.Protocol.Core
import XMonad.River.Wire (ObjectId)

foreign import ccall unsafe "hs_wl_memfd"
  c_memfd :: CString -> CSize -> IO CInt

foreign import ccall unsafe "hs_wl_mmap"
  c_mmap :: CInt -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "hs_wl_munmap"
  c_munmap :: Ptr Word8 -> CSize -> IO CInt

-- | A mapped @wl_buffer@, ready to be drawn into and attached to a surface.
data Buffer = Buffer
  { bufObject :: !ObjectId
    -- ^ The @wl_buffer@ to attach.
  , bufPool   :: !ObjectId
    -- ^ The @wl_shm_pool@ it came from, destroyed with it.
  , bufPixels :: !(Ptr Word8)
    -- ^ Writable, @bufHeight * bufStride@ bytes, in the format below.
  , bufWidth  :: !Int
  , bufHeight :: !Int
  , bufStride :: !Int
    -- ^ Bytes per row.  Four times the width for the 32-bit formats, but kept
    -- explicit because cairo picks its own stride and the two must agree.
  }

-- | Bytes the mapping occupies.
bufferSize :: Buffer -> Int
bufferSize b = bufHeight b * bufStride b

-- | Allocate a buffer of the given size and hand back its mapped pixels.
--
-- The format is @wl_shm.format.argb8888@: premultiplied alpha, native-endian
-- 32-bit words, which is exactly cairo's @ARGB32@ so a caller can point cairo
-- straight at 'bufPixels' with no conversion.
--
-- The descriptor is handed to the connection, which closes it once the request
-- has actually been written.  Closing it here instead would be the natural
-- thing to write and would be wrong: 'wlShmCreatePool' only queues, so the
-- descriptor is still needed at the next flush.  The mapping stays valid
-- either way -- it does not depend on the descriptor staying open.
newBuffer :: Connection -> ObjectId -> Int -> Int -> IO Buffer
newBuffer conn shm width height = do
  let stride = width * 4
      size   = stride * height
  when (width <= 0 || height <= 0) $
    ioError (userError "newBuffer: dimensions must be positive")

  fd <- fmap Fd . withCString "xmonad-river" $ \name ->
    throwErrnoIf (< 0) "newBuffer: memfd_create" (c_memfd name (fromIntegral size))

  pixels <- throwErrnoIfNull "newBuffer: mmap" $
    c_mmap (case fd of Fd n -> n) (fromIntegral size)

  pool <- wlShmCreatePool conn shm fd (fromIntegral size)

  buf <- wlShmPoolCreateBuffer conn pool 0
           (fromIntegral width) (fromIntegral height)
           (fromIntegral stride) wlShmFormatArgb8888

  pure Buffer
    { bufObject = buf
    , bufPool   = pool
    , bufPixels = pixels
    , bufWidth  = width
    , bufHeight = height
    , bufStride = stride
    }

-- | Release a buffer and unmap its pixels.
--
-- Only safe once the compositor is done with it -- that is what
-- @wl_buffer.release@ announces.  Unmapping under a compositor still reading
-- the pixels is a use-after-free on its side of the socket.
destroyBuffer :: Connection -> Buffer -> IO ()
destroyBuffer conn b = do
  wlBufferDestroy conn (bufObject b)
  wlShmPoolDestroy conn (bufPool b)
  throwErrnoIfMinus1_ "destroyBuffer: munmap" $
    c_munmap (bufPixels b) (fromIntegral (bufferSize b))
