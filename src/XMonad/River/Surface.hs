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
--
-- == Presenting with the frame
--
-- The worker draws; the loop commits.  A drawn surface is queued in
-- 'Presents', and the loop presents it inside a manage or render sequence
-- with @sync_next_commit@, so river shows the new contents at the same
-- @render_finish@ as the window positions they go with: a tab bar moves with
-- its windows rather than a frame before them.  The loop is also the only
-- side that destroys a buffer, so it never attaches one already destroyed.
module XMonad.River.Surface
  ( Surface(..)
  , Presents
  , newPresents
  , newSurface
  , destroySurface
  , withSurfaceBuffer
  , hideSurface
  , presentPending
  ) where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map.Strict as M

import XMonad.River.Buffer
import XMonad.River.Connection (Connection)
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Wire (ObjectId, nullObject)

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
    -- ^ Reallocated whenever the requested size changes.  Worker-written.
  , surfPresents :: !Presents
  }

data Present = Show !Buffer | Hide

-- | Surfaces drawn and not yet presented, by shell surface, with the buffers
-- the drawing replaced; and how to ask for a sequence to present them in.
data Presents = Presents
  { prQueue :: !(IORef (M.Map ObjectId (Surface, Present), [Buffer]))
  , prWake  :: !(IO ())
    -- ^ Run after queueing: asks for a manage sequence when the worker is
    -- not in one.
  }

newPresents :: IO () -> IO Presents
newPresents wake = (`Presents` wake) <$> newIORef (M.empty, [])

-- | Create a surface for window manager UI.
--
-- Nothing is visible until something is drawn into it and presented; a
-- surface with no buffer attached is not mapped.
newSurface :: Connection -> ObjectId -> ObjectId -> Presents -> IO Surface
newSurface conn compositor manager presents = do
  wl <- wlCompositorCreateSurface conn compositor
  shell <- riverWindowManagerV1GetShellSurface conn manager wl
  node <- riverShellSurfaceV1GetNode conn shell
  ref <- newIORef Nothing
  pure Surface { surfWl = wl, surfShell = shell, surfNode = node, surfBuffer = ref
               , surfPresents = presents }

-- | Draw into the surface at the given size, and queue it to be presented.
--
-- The buffer is reallocated only when the size changes, because a resize is
-- rare and reallocating per frame would mean a @memfd@ and an @mmap@ for
-- every keystroke in a prompt.  The one it replaces is the loop's to destroy,
-- after it has presented the new one.
--
-- The caller fills 'bufPixels' -- @width * 4@ bytes per row, ARGB32
-- premultiplied, which is cairo's native format so a cairo image surface can
-- be pointed straight at it.
withSurfaceBuffer
  :: Connection -> Surface -> ObjectId -> Int -> Int -> (Buffer -> IO a) -> IO a
withSurfaceBuffer conn s shm width height draw = do
  existing <- readIORef (surfBuffer s)
  (buf, replaced) <- case existing of
    Just b | bufWidth b == width && bufHeight b == height -> pure (b, [])
    _ -> do
      fresh <- newBuffer conn shm width height
      writeIORef (surfBuffer s) (Just fresh)
      pure (fresh, maybe [] pure existing)
  a <- draw buf
  queue s (Show buf) replaced
  pure a

-- | Unmap the surface without destroying it, at the next presentation.
--
-- Attaching a null buffer is how a @wl_surface@ is unmapped; there is no
-- separate hide request.  The backing store is kept, because a surface that is
-- hidden is usually about to be shown again -- a decoration on a workspace
-- being switched away from -- and reallocating shared memory for that is work
-- with nothing to show for it.
hideSurface :: Surface -> IO ()
hideSurface s = queue s Hide []

queue :: Surface -> Present -> [Buffer] -> IO ()
queue s p replaced = do
  let presents = surfPresents s
  atomicModifyIORef' (prQueue presents) $ \(pending, retired) ->
    ((M.insert (surfShell s) (s, p) pending, replaced ++ retired), ())
  prWake presents

-- | Present everything queued, then destroy the buffers it replaced.  Loop
-- thread only, inside a manage or render sequence: @sync_next_commit@ is
-- rendering state, and holds each commit for the next @render_finish@.
presentPending :: Connection -> Presents -> IO ()
presentPending conn presents = do
  (pending, retired) <- atomicModifyIORef' (prQueue presents) (\q -> ((M.empty, []), q))
  forM_ (M.elems pending) $ \(s, p) -> do
    riverShellSurfaceV1SyncNextCommit conn (surfShell s)
    case p of
      Show buf -> do
        wlSurfaceAttach conn (surfWl s) (bufObject buf) 0 0
        wlSurfaceDamageBuffer conn (surfWl s) 0 0
          (fromIntegral (bufWidth buf)) (fromIntegral (bufHeight buf))
      Hide -> wlSurfaceAttach conn (surfWl s) nullObject 0 0
    wlSurfaceCommit conn (surfWl s)
  mapM_ (destroyBuffer conn) retired

-- | Tear down a surface and its backing store.  Loop thread only; a present
-- still queued for it is dropped.
destroySurface :: Connection -> Surface -> IO ()
destroySurface conn s = do
  atomicModifyIORef' (prQueue (surfPresents s)) $ \(pending, retired) ->
    ((M.delete (surfShell s) pending, retired), ())
  readIORef (surfBuffer s) >>= mapM_ (destroyBuffer conn)
  writeIORef (surfBuffer s) Nothing
  riverNodeV1Destroy conn (surfNode s)
  riverShellSurfaceV1Destroy conn (surfShell s)
  wlSurfaceDestroy conn (surfWl s)
