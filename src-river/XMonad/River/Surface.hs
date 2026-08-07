-- | Surfaces the window manager owns and draws.
--
-- A window manager under X11 could create a window whenever it liked, because
-- it was an ordinary X client that also happened to manage. Under river the
-- window manager is not a client at all: it speaks a management protocol, and
-- a surface it created without saying so would be treated as somebody's
-- window and handed straight back to itself to lay out.
--
-- @river_window_manager_v1.get_shell_surface@ is how that is avoided. It
-- assigns the @river_shell_surface_v1@ role to a plain @wl_surface@, which
-- tells river the surface is window manager UI: not a window, never laid out,
-- positioned by the window manager through its own 'riverShellSurfaceV1GetNode'.
-- That is what a prompt or a decoration is made of.
--
-- Note this is /not/ layer shell. @river_layer_shell_v1@ is how the window
-- manager learns about *clients'* layer surfaces -- bars, notifications,
-- wallpaper. It offers no way to create one.
--
-- This module stops at "here is a surface and some pixels to fill". What goes
-- in the pixels is xmonad-contrib's business, exactly as fonts and graphics
-- contexts are under X11.
module XMonad.River.Surface
  ( Surface(..)
  , newSurface
  , destroySurface
  , withSurfaceBuffer
  , moveSurface
  ) where

import Data.IORef

import XMonad.River.Buffer
import XMonad.River.Connection (Connection)
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Types (Position)
import XMonad.River.Wire (ObjectId)

-- | A window-manager-owned surface and its current backing store.
data Surface = Surface
  { surfWl    :: !ObjectId
    -- ^ The @wl_surface@.
  , surfShell :: !ObjectId
    -- ^ Its @river_shell_surface_v1@ role object.
  , surfNode  :: !ObjectId
    -- ^ Its node, which is how it gets positioned and stacked -- the same
    -- mechanism ordinary windows are placed with.
  , surfBuffer :: !(IORef (Maybe Buffer))
    -- ^ Reallocated whenever the requested size changes.
  }

-- | Create a surface for window manager UI.
--
-- Nothing is visible until something is drawn into it and committed; a
-- surface with no buffer attached is not mapped.
newSurface :: Connection -> ObjectId -> ObjectId -> IO Surface
newSurface conn compositor manager = do
  wl <- wlCompositorCreateSurface conn compositor
  shell <- riverWindowManagerV1GetShellSurface conn manager wl
  node <- riverShellSurfaceV1GetNode conn shell
  ref <- newIORef Nothing
  pure Surface { surfWl = wl, surfShell = shell, surfNode = node, surfBuffer = ref }

-- | Position the surface, in the compositor's global coordinate space.
moveSurface :: Connection -> Surface -> Position -> Position -> IO ()
moveSurface conn s x y = riverNodeV1SetPosition conn (surfNode s) x y

-- | Draw into the surface at the given size, then present it.
--
-- The buffer is reallocated only when the size changes, because a resize is
-- rare and reallocating per frame would mean a @memfd@, an @mmap@ and a
-- round trip for every keystroke in a prompt.
--
-- The caller fills 'bufPixels' -- @width * 4@ bytes per row, ARGB32
-- premultiplied, which is cairo's native format so a cairo image surface can
-- be pointed straight at it.
--
-- Damage is reported over the whole surface. Finer damage is an optimisation
-- worth making only once there is something whose repaints are measurably
-- expensive, and a prompt's are not.
withSurfaceBuffer
  :: Connection -> Surface -> ObjectId -> Int -> Int -> (Buffer -> IO a) -> IO a
withSurfaceBuffer conn s shm width height draw = do
  existing <- readIORef (surfBuffer s)
  buf <- case existing of
    Just b | bufWidth b == width && bufHeight b == height -> pure b
    Just b -> do
      -- Safe only because nothing else is in flight: the previous buffer was
      -- attached and committed, and river has finished with it by the time we
      -- are asked to draw again.  A double-buffered UI would have to wait for
      -- wl_buffer.release instead.
      destroyBuffer conn b
      fresh <- newBuffer conn shm width height
      writeIORef (surfBuffer s) (Just fresh)
      pure fresh
    Nothing -> do
      fresh <- newBuffer conn shm width height
      writeIORef (surfBuffer s) (Just fresh)
      pure fresh

  a <- draw buf

  wlSurfaceAttach conn (surfWl s) (bufObject buf) 0 0
  wlSurfaceDamageBuffer conn (surfWl s) 0 0
    (fromIntegral width) (fromIntegral height)
  wlSurfaceCommit conn (surfWl s)
  pure a

-- | Tear down a surface and its backing store.
destroySurface :: Connection -> Surface -> IO ()
destroySurface conn s = do
  readIORef (surfBuffer s) >>= mapM_ (destroyBuffer conn)
  writeIORef (surfBuffer s) Nothing
  riverNodeV1Destroy conn (surfNode s)
  riverShellSurfaceV1Destroy conn (surfShell s)
  wlSurfaceDestroy conn (surfWl s)
