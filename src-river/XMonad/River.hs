{-# LANGUAGE LambdaCase #-}
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
    -- | @\"#rrggbb\"@ to the RGBA form river wants.  X11 resolved a colour
    -- name against the server's colormap and could fail for reasons a config
    -- could not see; here the only failure is a string that is not a colour,
    -- and 'parseColor' answers with opaque black rather than failing at all.
    parseColor, parseColorMaybe,
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

    -- * Window geometry
    --
    -- | X11 answered @XGetWindowAttributes@ for any window; river never
    -- reports where a window is, because the window manager is what decided.
    windowRect, moveResizeWindow, pointerPosition,

    -- * Decorations
    --
    -- | Server-side decoration is requested for every new window before the
    -- manage hook runs, because river's default is the opposite.  These are
    -- how a manage hook overrides that.
    useServerDecorations, useClientDecorations,

    -- * Reading a key
    --
    -- | What a submap is built on.  River has no keyboard grab, so this
    -- installs a binding per key instead -- and, unlike X11's, it cannot wait
    -- for the answer.  See 'submapNextKey'.
    submapNextKey,

    -- * Lifecycle
    RestartRequested(..), setMainThread, exitSession,

    -- * Keysym tables
    keysymTable, reverseKeysymTable,

    -- * Diagnostics
    warnUnimplemented,
  ) where

import Data.Bits ((.&.))
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word32)
import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (ask, asks)
import qualified Data.Map.Strict as M

import XMonad.Core
import XMonad.Operations (applySizeHintsContents)
import XMonad.River.Keyboard (riverModifiers)
import XMonad.River.Keysym.Table (keysymTable, reverseKeysymTable)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
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

-- | Where a window is, in river's global coordinate space.
--
-- This is what @withWindowAttributes@ was reached for under X11, and it is not
-- a query: river reports a window's size but never its position, since the
-- window manager is the thing that chose it.  So the answer is whatever the
-- last layout run decided, which is also what was sent to the compositor.
--
-- 'Nothing' for a window that is not currently placed -- one on a workspace
-- that is not on screen, or one that has just appeared and not yet been laid
-- out.  Under X11 those still had geometry, so a caller that assumed an answer
-- has to be given one here.
windowRect :: Window -> X (Maybe Rectangle)
windowRect w = do
    placements <- io . readIORef =<< asks riverPlacements
    pure (lookup w placements)

-- | Put a window at an absolute position and size.
--
-- X11's @moveResizeWindow@ in one call, and it needs two here because river
-- splits the state: a position is rendering state and may be set at any time,
-- while dimensions are window management state and may only be proposed during
-- a manage sequence.  Calling this outside one silently drops the resize, so
-- callers belong in a binding action or a 'XMonad.Operations.mouseDrag'
-- callback, both of which river runs inside a sequence.
--
-- Size hints are applied, as the X11 callers all did by hand.
moveResizeWindow :: Window -> Rectangle -> X ()
moveResizeWindow w r = do
    conn <- asks display
    known <- io . readIORef =<< asks riverWindows
    forM_ (M.lookup w known) $ \rw -> do
        let (width, height) = applySizeHintsContents (rwSizeHints rw)
                (rect_width r, rect_height r)
        io $ riverNodeV1SetPosition conn (rwNode rw) (rect_x r) (rect_y r)
        io $ riverWindowV1ProposeDimensions conn w
               (fromIntegral width) (fromIntegral height)

-- | Where the pointer is, in river's global coordinate space.
--
-- X11 had @queryPointer@, which asked the server.  river reports pointer
-- motion as it happens and answers no questions, so this is the last position
-- it sent.  'Nothing' when there is no seat, or before the pointer has moved
-- at all.
pointerPosition :: X (Maybe (Position, Position))
pointerPosition = do
    seats <- io . readIORef =<< asks riverSeats
    pure $ case M.elems seats of
        (s:_) -> Just (rsPointer s)
        []    -> Nothing

-- | Ask a window to let the window manager draw its frame.
--
-- The default for every new window, and the reason it has to be asked for is
-- that river's default is @use_csd@: a window manager that says nothing gets
-- clients drawing their own title bars and close buttons.  On a tiling desktop
-- that is decoration on every window that nobody asked for.
--
-- \"Server side\" does not mean river draws a title bar.  It draws what this
-- window manager asks for, which is the border from
-- 'XMonad.Core.borderWidth' and nothing else.
--
-- Has no effect on a client that only supports client-side decoration; river
-- documents that, which is why this is not conditional on the decoration hint.
--
-- Only legal during a manage sequence, so a manage hook is the place for it.
useServerDecorations :: Window -> X ()
useServerDecorations w = do
    conn <- asks display
    io (riverWindowV1UseSsd conn w)

-- | Let a window draw its own title bar and borders.
--
-- Undoes 'useServerDecorations' for one window, from a manage hook:
--
-- > manageHook = className =? \"Gimp\" --> liftX (ask >>= useClientDecorations) <> idHook
--
-- Only legal during a manage sequence.
useClientDecorations :: Window -> X ()
useClientDecorations w = do
    conn <- asks display
    io (riverWindowV1UseCsd conn w)

-- | Read one key press and run the action it selects.
--
-- This is what "XMonad.Actions.Submap" is built on, and the reason it lives
-- here rather than in contrib is that its shape is dictated by river.
--
-- __It does not block, and it cannot.__  Under X11 a submap grabbed the
-- keyboard and sat in @maskEvent@ until a key arrived, so @submap@ returned
-- only once the chosen action had run.  That is not merely awkward here, it is
-- impossible: a binding may only be created during a manage sequence, and no
-- binding fires until that sequence has been finished.  Waiting inside the
-- sequence for a key would be waiting for something the compositor is not
-- permitted to send yet.  So the bindings are installed, this returns
-- immediately, and the selected action runs when the key actually arrives.
--
-- The difference is invisible when a submap is the last thing an action does,
-- which is how @submap@ has always been used and what
-- 'XMonad.Util.EZConfig.mkKeymap' generates.  Anything sequenced after a call
-- to this will run /before/ the key is pressed rather than after.
--
-- While the submap is open every one of the window manager's own bindings is
-- disabled.  river leaves it to compositor policy which of several bindings
-- matching one physical key receives the press, so leaving the config's
-- bindings live alongside the submap's would make a prefix key that is also a
-- submap key -- @M-m m@, say -- do something undefined.  They are restored by
-- 'reset', which every exit path runs.
submapNextKey
    :: M.Map (KeyMask, KeySym) (X ())  -- ^ what each key runs
    -> X ()                            -- ^ run instead if the key is unbound
    -> X ()
submapNextKey subKeys onUnbound = do
    conf <- ask
    let conn = display conf
        bindingsGlobal = riverBindings conf
    seats <- io (readIORef (riverSeats conf))
    globals <- io (readIORef (riverKeyBindings conf))

    io $ forM_ (M.keys globals) (riverXkbBindingV1Disable conn)

    temps <- io . fmap concat . forM (M.elems seats) $ \seat ->
        forM (M.toList subKeys) $ \((mask, keysym), _) -> do
            b <- riverXkbBindingsV1GetXkbBinding conn bindingsGlobal
                   (rsObject seat) keysym (riverModifiers mask)
            pure (b, (mask, keysym))

    -- Restoring the config's bindings and destroying the submap's, whichever
    -- way the submap ends.  Runs in the manage sequence that follows the key
    -- press, which is the earliest either request is legal.
    let reset = do
            io (writeIORef (riverSubmap conf) Nothing)
            io $ forM_ temps $ \(b, _) -> do
                riverXkbBindingV1Disable conn b
                riverXkbBindingV1Destroy conn b
            io $ forM_ (M.keys globals) (riverXkbBindingV1Enable conn)

    io $ forM_ temps $ \(b, k) -> do
        riverXkbBindingV1Listen conn b $ \case
            RiverXkbBindingV1Pressed -> do
                -- Take the submap slot, so that this and ate_unbound_key
                -- cannot both fire a teardown.
                taken <- atomicModifyIORef' (riverSubmap conf) (\s -> (Nothing, s))
                when (isJust taken) $ MB.post (riverMailbox conf) $ do
                    reset
                    fromMaybe (pure ()) (M.lookup k subKeys)
            _ -> pure ()
        riverXkbBindingV1Enable conn b

    io (writeIORef (riverSubmap conf) (Just (reset >> onUnbound)))

    -- Ask to be told about a key that is not in the submap, so it can be
    -- abandoned.  Without this -- on version 1, where the request does not
    -- exist -- an unknown key does nothing and the submap stays open with the
    -- config's bindings disabled, which is a session with no shortcuts until a
    -- submap key is pressed.
    io $ forM_ (M.elems seats) $ \s ->
        forM_ (rsXkbSeat s) (riverXkbBindingsSeatV1EnsureNextKeyEaten conn)
