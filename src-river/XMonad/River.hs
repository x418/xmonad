-- | Everything the river backend offers that xmonad's API does not.
--
-- This module exists so that nothing river-specific leaks into "XMonad".  A
-- config that imports @XMonad@ sees exactly the names the X11 build offers,
-- minus the ones river cannot faithfully provide -- never more.  Anything that
-- has no X11 counterpart is here instead, and importing this module is a
-- config saying, explicitly, that it is a river config.
--
-- That separation is what makes the API test meaningful: it compares the eight
-- modules xmonad has always exported, and river is required to add nothing to
-- them.  A name that turns up in "XMonad.Core" without an X11 counterpart is a
-- test failure, and the fix is to move it here.
module XMonad.River (
    -- * Compositor state
    --
    -- | What river has told us about each object it manages.  Accumulated from
    -- its events, because river reports state rather than answering queries.
    RiverWindow(..), RiverOutput(..), RiverSeat(..),
    LayerFocus(..), layerHasFocus,
    BorderColor,
    noSizeHints,

    -- * Working off the event loop
    --
    -- | The event loop owns the connection outright, so a timer thread or a
    -- subprocess watcher cannot touch it.  'postAction' is how such a thread
    -- gets an action run: it is queued and executed at the start of the next
    -- manage sequence, which the loop requests as soon as it wakes.
    postAction,

    -- * Driving the manage sequence
    --
    -- | X11 let a window manager act at any moment.  river permits window
    -- management state to change only during a manage sequence, so a config
    -- acting from a timer or a forked thread has to ask for one.
    manageDirty,

    -- * Lifecycle
    RestartRequested(..), setMainThread, exitSession,

    -- * Keysym tables
    keysymTable, reverseKeysymTable,

    -- * Diagnostics
    warnUnimplemented,
  ) where

import Data.IORef (writeIORef)
import Control.Monad.Reader (asks)

import XMonad.Core
import XMonad.River.Keysym.Table (keysymTable, reverseKeysymTable)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Protocol.WindowManagement (riverWindowManagerV1ExitSession)
import XMonad.River.Runtime (RestartRequested(..), setMainThread, warnUnimplemented)
import XMonad.River.Types

-- | Run an action on the event loop, from any thread.
--
-- The action is queued and runs at the start of the next manage sequence.
-- That is not a delay to work around: river permits window management state to
-- change only during a sequence, so it is the earliest moment the action could
-- legally do anything.
postAction :: XConf -> X () -> IO ()
postAction c = MB.post (riverMailbox c)

-- | Ask the compositor to start a manage sequence, because state it cannot see
-- has changed.
--
-- This is what makes actions triggered from forked threads and timers take
-- effect.
manageDirty :: X ()
manageDirty = do
  ref <- asks riverDirty
  io (writeIORef ref True)

-- | End the Wayland session, taking the compositor with it.
--
-- X11's equivalent was exiting the process and letting the server notice.
-- Under river that only hands the seat to the next window manager: the
-- compositor keeps running, and every client with it.
exitSession :: X ()
exitSession = do
    conn <- asks display
    manager <- asks riverManager
    io (riverWindowManagerV1ExitSession conn manager)
