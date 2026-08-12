{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

    -- * Waiting for the layout
    --
    -- | X11's @windows@ placed every window before it returned, so a caller
    -- could change the layout and immediately ask what it had done.  Here the
    -- layout runs at the end of the manage sequence; this is how to be asked
    -- afterwards.
    afterLayout,

    -- * Window geometry
    --
    -- | X11 answered @XGetWindowAttributes@ for any window; river never
    -- reports where a window is, because the window manager is what decided.
    windowRect, moveResizeWindow, pointerPosition, windowUnderPointer,

    -- * Stacking
    --
    -- | X11 let a window manager restack at any moment and the order stuck
    -- until something else changed it.  River's render sequence re-applies the
    -- layout's own order on every frame, so "raise this" has to be a standing
    -- request rather than a one-off; these record one.
    raiseWindow, restackWindows,

    -- * Window relationships
    --
    -- | What @WM_TRANSIENT_FOR@ answered, under the name Wayland gives it.
    windowParent,

    -- * Counting outputs before there is a window manager
    --
    -- | X11 let anything open a second connection and ask Xinerama how many
    -- screens there were.  This is the Wayland equivalent, and the only thing
    -- here that does not need a running xmonad.
    countOutputs,

    -- * Cursor
    --
    -- | X11 set a cursor glyph on the root window and every window inherited
    -- it.  Wayland has neither, and offers a theme instead.
    setCursorTheme,

    -- * Decorations
    --
    -- | Server-side decoration is requested for every new window before the
    -- manage hook runs, because river's default is the opposite.  These are
    -- how a manage hook overrides that.
    useServerDecorations, useClientDecorations,

    -- * Holding a modifier
    --
    -- | What Alt-Tab is built on: capture some keys for as long as a modifier
    -- is held, and finish when it is let go.
    whileModifiersHeld,

    -- * Capturing keys for as long as you like
    --
    -- | What X11 called grabbing a key.  River has no grab; a binding is an
    -- object, so this creates and destroys them.  Unlike 'submapNextKey' these
    -- stand until removed, and coexist with the config's own bindings.
    grabKeys, grabKeysUpDown, ungrabKeys,

    -- * Reading a key
    --
    -- | What a submap is built on.  River has no keyboard grab, so this
    -- installs a binding per key instead -- and, unlike X11's, it cannot wait
    -- for the answer.  See 'submapNextKey'.
    submapNextKey,

    -- * Getting the keyboard back
    --
    -- | A prompt holds the keyboard with an exclusive layer-shell grab.  If it
    -- stops answering, every keystroke goes to it and nothing can be typed
    -- anywhere.  This is the way out, and it is worth binding.
    closeAllPrompts,

    -- * Lifecycle
    RestartRequested(..), setMainThread, exitSession,

    -- * Keysym tables
    keysymTable, reverseKeysymTable,

    -- * Diagnostics
    warnUnimplemented,
  ) where

import Data.Bits ((.&.))
import System.IO (hPutStrLn, stderr)
import Data.IORef (atomicModifyIORef', modifyIORef', readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word32)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask, asks)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M

import XMonad.Core
import Data.List (find)
import XMonad.Operations (applySizeHintsContents, pointWithin)
import XMonad.River.Keyboard (riverModifiers)
import XMonad.River.Keysym.Table (keysymTable, reverseKeysymTable)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Client (closeAllClients)
import qualified XMonad.River.Connection as C
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Plan (Op(..))
import XMonad.River.Runtime (emitNow, emitOp, RestartRequested(..), currentSubmapGeneration, nextSubmapGeneration, setMainThread, setModifierWatcher, warnUnimplemented)
import XMonad.River.Types
import XMonad.River.State (InputCapture(..), RiverState(..), updatePlacement)

-- | Run an action on the event loop, from any thread.
--
-- The action is queued and runs at the start of the next manage sequence.
-- That is not a delay to work around: river permits window management state to
-- change only during a sequence, so it is the earliest moment the action could
-- legally do anything.
postAction :: XConf -> X () -> IO ()
postAction c = MB.post (riverMailbox (riverState c))

-- | Ask the compositor to start a manage sequence, because state it cannot see
-- has changed.
--
-- This is what makes actions triggered from forked threads and timers take
-- effect.
manageDirty :: X ()
manageDirty = do
  ref <- asks (riverDirty . riverState)
  io (writeIORef ref True)

-- | Run an action once the layout has been applied, rather than now.
--
-- For code that changes the 'XMonad.Core.WindowSet' and then needs to know
-- where something ended up.  Under X11 that needed nothing: @windows@ moved
-- and resized every window before it returned, so the server could be asked
-- immediately and would answer about the new arrangement.  Here the layout is
-- the last thing a manage sequence does, so a binding that calls @windows@ and
-- then 'windowRect' -- or 'XMonad.Core.getWindowAttributes', or anything else
-- resting on those -- reads geometry from before its own change.
--
-- The symptom is quiet and easy to misread, because it is /correct/ for every
-- action that moves the focus without moving a window: @focusUp@ leaves both
-- windows where they were, so last sequence's answer is still true.  It is
-- only wrong when the window itself moves.  Swapping two windows and warping
-- the pointer to the focused one -- @windows W.swapUp >> warpToWindow@ -- puts
-- the pointer where that window used to be, which is where it already is, so
-- nothing appears to happen at all.
--
-- Still in the same sequence, so there is no visible delay: the queue is
-- drained at the end of 'XMonad.River.WM.applyLayout', after both
-- 'windowRect' and @getWindowAttributes@ have been brought up to date, and
-- before anything is transmitted.  An action queued from outside a manage
-- sequence asks for one, since otherwise nothing would come to run it.
--
-- Changing the 'XMonad.Core.WindowSet' from here works but is a sequence late:
-- the layout that would act on it has already run.  This is for reading the
-- result of a change, not for making another one.
afterLayout :: X () -> X ()
afterLayout act = do
  ref <- asks (riverAfterLayout . riverState)
  io (modifyIORef' ref (act :))
  inSeq <- io . readIORef =<< asks (inManageSeq . riverState)
  unless inSeq manageDirty

-- | End the Wayland session, taking the compositor with it.
--
-- X11's equivalent was exiting the process and letting the server notice.
-- Under river that only hands the seat to the next window manager: the
-- compositor keeps running, and every client with it.
exitSession :: X ()
exitSession = emitNow OpExitSession

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
    placements <- io . readIORef =<< asks (riverPlacements . riverState)
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
--
-- The recorded geometry is updated too, and that is not bookkeeping.  Under
-- X11 this call changed what the server would report, immediately, and callers
-- rely on reading it straight back: 'XMonad.Actions.FlexibleManipulate' moves
-- the window and then calls 'XMonad.Operations.float', which asks where the
-- window is in order to record it.  Nothing here reaches a server, so the
-- record has to be kept by hand -- and without it 'XMonad.Operations.float'
-- answers with the position from the last layout run and puts the window
-- straight back, once per motion event.  See 'updatePlacement'.
moveResizeWindow :: Window -> Rectangle -> X ()
moveResizeWindow w r = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    forM_ (M.lookup w known) $ \rw -> do
        let (width, height) = applySizeHintsContents (rwSizeHints rw)
                (rect_width r, rect_height r)
        emitOp (OpSetPosition w (rect_x r) (rect_y r))
        emitOp (OpProposeDimensions w width height)
        ref <- asks (riverPlacements . riverState)
        updatePlacement ref w r { rect_width = width, rect_height = height }

-- | Where the pointer is, in river's global coordinate space.
--
-- X11 had @queryPointer@, which asked the server.  river reports pointer
-- motion as it happens and answers no questions, so this is the last position
-- it sent.  'Nothing' when there is no seat, or before the pointer has moved
-- at all.
pointerPosition :: X (Maybe (Position, Position))
pointerPosition = do
    seats <- io . readIORef =<< asks (riverSeats . riverState)
    pure $ case M.elems seats of
        (s:_) -> Just (rsPointer s)
        []    -> Nothing

-- | Which window the pointer is over.
--
-- X11's @queryPointer@ answered this as its @child@ result, because the server
-- owned the window tree and could hit-test it.  River owns no such tree to
-- ask: the window manager is what decided where every window went.  So the
-- answer is computed from the placements the last layout run produced, which
-- is the same information the server would have been reporting back.
--
-- 'Nothing' when the pointer is over no managed window, and when there is no
-- pointer at all.  Windows the layout did not place -- on a workspace that is
-- not on screen -- are not candidates, which is right: they are not under
-- anything.
windowUnderPointer :: X (Maybe Window)
windowUnderPointer = pointerPosition >>= \case
    Nothing -> pure Nothing
    Just (px, py) -> do
        placements <- io . readIORef =<< asks (riverPlacements . riverState)
        pure $ fst <$> find (pointWithin px py . snd) placements

-- | How many outputs the compositor has, over a connection of its own.
--
-- Every other query here answers from what river has told the running window
-- manager.  This one does not need one: it opens an ordinary Wayland client
-- connection, counts the @wl_output@ globals the registry advertises, and
-- closes it again.  That is what a config calling @countScreens@ in @main@ --
-- before xmonad starts, to size its workspace list -- needs, and it is exactly
-- what the X11 version did with @openDisplay ""@ and Xinerama.
--
-- river permits one /window manager/, not one client, so this does not
-- conflict with a session already running.  Answers @0@ if there is no
-- compositor to ask, rather than throwing: a caller in @main@ has nowhere
-- useful to catch.
countOutputs :: MonadIO m => m Int
countOutputs = io $ handle (\(_ :: SomeException) -> pure 0) $ do
    conn <- C.connect
    (_, globals) <- C.getRegistry conn
    C.disconnect conn
    pure $ length [ () | g <- globals, C.globalInterface g == BC.pack "wl_output" ]

-- | Capture these keys until 'ungrabKeys', running the given action for each.
--
-- The river answer to X11's @grabKey@.  There is no grab to take: a key
-- reaches a window manager because a @river_xkb_binding_v1@ exists for it, so
-- "grabbing" is creating one and "ungrabbing" is destroying it.  Two
-- consequences follow from that, and both are improvements:
--
-- * it is per keysym and modifier mask, never per keycode.  X11's @grabKey@
--   took a @KeyCode@, which is why callers had to run 'mkGrabs' first; river
--   binds the keysym directly, so the keymap never enters into it.
-- * a captured key does not shadow the config's binding for the same key by
--   accident -- both bindings exist, and river fires both.  A caller that
--   wants exclusivity should say so by not choosing keys the config uses.
--
-- Replaces any previous set: this is the whole standing capture, not an
-- addition to it, which is what a caller recomputing its keymap on every
-- change wants.  Removing the last one restores the plain configuration.
grabKeys :: M.Map (KeyMask, KeySym) (X ()) -> X ()
grabKeys keymap = grabKeysUpDown (M.map (, pure ()) keymap)

-- | As 'grabKeys', but with an action for the key going up as well as down.
--
-- X11 gave a window manager key releases only if it had asked for
-- @keyReleaseMask@, and told press from release by the event type.  A river
-- binding reports both without being asked, so this is the same capture with
-- the second half wired up -- which is all "XMonad.Actions.UpKeys" ever
-- wanted.
grabKeysUpDown :: M.Map (KeyMask, KeySym) (X (), X ()) -> X ()
grabKeysUpDown keymap = do
    conf <- ask
    ungrabKeys
    let entries = M.toList keymap
    -- The actions, in the order the loop will index them.  Written before the
    -- op, so the table is there by the time a binding can fire.
    io $ writeIORef (riverExtraKeys (riverState conf)) (map snd entries)
    emitOp (OpGrabKeys (map fst entries))
    manageDirty

-- | Release everything 'grabKeys' captured.
ungrabKeys :: X ()
ungrabKeys = do
    conf <- ask
    io (writeIORef (riverExtraKeys (riverState conf)) [])
    emitOp OpUngrabKeys

-- | Keep a window above the ones the layout placed.
--
-- X11's @raiseWindow@ was a request the server obeyed until someone else
-- restacked.  Here the render sequence restacks from the layout every frame,
-- so a one-off request would be undone before it was seen; this is recorded
-- and re-applied each frame instead.  It lapses when the window stops being
-- placed -- closed, or moved to a workspace that is not on screen -- so
-- nothing has to remember to undo it.
raiseWindow :: Window -> X ()
raiseWindow w = restackWindows [w]

-- | Keep these windows above the ones the layout placed, topmost first.
--
-- X11's @restackWindows@ took the same order: the head ends up on top.  The
-- list replaces any previous request rather than adding to it, which is what
-- a caller recomputing the whole order every 'logHook' wants.
restackWindows :: [Window] -> X ()
restackWindows ws = do
    ref <- asks (riverRestack . riverState)
    -- Stored bottom-to-top, because that is the order the render sequence
    -- walks; the argument is topmost-first, as X11's was.
    io (writeIORef ref (reverse ws))
    manageDirty

-- | The window this one is a dialog for, if any.
--
-- X11 spelled this @WM_TRANSIENT_FOR@ and answered it with
-- @getTransientForHint@; Wayland spells it @xdg_toplevel.set_parent@ and river
-- reports it as @river_window_v1.parent@.  Same relationship, same use --
-- deciding that a window is a dialog and belongs on top of, or focused
-- instead of, the window that raised it.
--
-- Unlike the X11 call this asks nothing: the answer is what river last said.
windowParent :: Window -> X (Maybe Window)
windowParent w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    pure (M.lookup w known >>= rwParent)

-- | Choose the XCursor theme the compositor draws with.
--
-- This is what replaces X11's @setDefaultCursor@, and it is a different
-- question with a different answer.  X11 named one glyph from the cursor font
-- and set it on the root window; every window that did not override it
-- inherited that shape.  Wayland has no root window and no cursor font: a
-- client picks its own cursor from a theme, and the compositor draws the
-- cursor wherever no client does.  So the choice available to a window manager
-- is which /theme/ the compositor draws from, not which shape it draws.
--
-- > startupHook = setCursorTheme "Adwaita" 24
--
-- Applies to cursors the compositor renders, and not necessarily to those a
-- client renders for itself.  river's own documentation notes the consequence:
-- a window manager generally wants @XCURSOR_THEME@ and @XCURSOR_SIZE@ in the
-- environment of the programs it spawns as well, so that clients drawing their
-- own cursors agree with the compositor about which theme that is.
--
-- Applied to every seat.  Requires river offering @river_window_management_v1@
-- version 2 or better; on version 1 the request does not exist and this warns
-- rather than failing.
setCursorTheme :: String -> Int -> X ()
setCursorTheme name size = do
    seats <- io . readIORef =<< asks (riverSeats . riverState)
    forM_ (M.elems seats) $ \s ->
        emitNow (OpSetXcursorTheme (rsObject s) (BC.pack name) (fromIntegral size))

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
useServerDecorations w = emitOp (OpUseDecorations w True)

-- | Let a window draw its own title bar and borders.
--
-- Undoes 'useServerDecorations' for one window, from a manage hook:
--
-- > manageHook = className =? \"Gimp\" --> liftX (ask >>= useClientDecorations) <> idHook
--
-- Only legal during a manage sequence.
useClientDecorations :: Window -> X ()
useClientDecorations w = emitOp (OpUseDecorations w False)

-- | Close every prompt, releasing any keyboard grab one is holding.
--
-- Bind this.  It is the only thing that reliably gets a wedged session back,
-- and the reason it works is a detail of river worth knowing: river matches
-- xkb bindings /before/ it consults keyboard focus, so a window manager
-- binding still fires while a layer surface holds an exclusive keyboard grab.
-- A prompt that has stopped reading its keyboard cannot be escaped by typing
-- at it, but it can still be killed by a binding.
--
-- > , ((modMask .|. shiftMask, xK_Escape), closeAllPrompts)
--
-- Closing is not polite: the client threads are killed rather than asked, on
-- the grounds that a thread which is not answering is exactly the case this
-- exists for.  Each unwinds through its own teardown, so the surfaces go and
-- the connections drop.  A prompt that was working simply disappears, which is
-- what pressing an escape hatch should do.
--
-- Reports how many were closed, so that pressing it when nothing is stuck says
-- so rather than appearing to do nothing.
closeAllPrompts :: X ()
closeAllPrompts = io $ do
  n <- closeAllClients
  hPutStrLn stderr $ "xmonad-river: closed " <> show n <> " prompt(s)"

-- | Capture keys until a modifier is released.
--
-- This is what "XMonad.Actions.Repeatable" is built on, and it is the
-- Alt-Tab shape: an action is invoked with a modifier held, keeps responding
-- to keys while it stays held, and concludes when it is let go.
--
-- __It does not block__, for the same reason 'submapNextKey' does not: a
-- binding may only be created during a manage sequence and cannot fire until
-- that sequence has finished, so waiting inside the sequence would be waiting
-- for something the compositor is not permitted to send.  X11 grabbed the
-- keyboard and sat in @maskEvent@; here the bindings are installed, this
-- returns, and the handler runs as keys arrive.
--
-- The visible consequence is that a caller cannot compute a value from the
-- interaction, and anything sequenced after this runs /before/ the keys are
-- pressed rather than after -- which is why there is a separate conclusion
-- action rather than a return value.
--
-- While the interaction is open the config's own bindings are disabled, as
-- for a submap: the key that invoked this is typically also a global binding
-- (@M-Tab@), and river leaves it to compositor policy which of several
-- matching bindings receives a press.
--
-- Requires @river_xkb_bindings_v1@ version 3, where @modifiers_watch@ arrived.
-- On anything older there is no way to learn that a modifier was released, so
-- the handler is never installed, the conclusion runs immediately, and a
-- diagnostic says so -- which degrades Alt-Tab to "acts once", rather than to
-- a session whose bindings are disabled forever.
whileModifiersHeld
    :: KeyMask                        -- ^ concludes when any of these is released
    -> [(KeyMask, KeySym)]            -- ^ keys to capture while held
    -> (Bool -> KeySym -> X ())       -- ^ @True@ for a press, @False@ for a release
    -> X ()                           -- ^ run once, when the modifier is released
    -> X ()
whileModifiersHeld mods captures onKey onDone = do
    conf <- ask
    gen <- io nextSubmapGeneration
    seats <- io (readIORef (riverSeats (riverState conf)))
    -- @modifiers_watch@ arrived in version 3 of river_xkb_bindings_v1, and
    -- without it there is no way to learn that a modifier was released.  Doing
    -- nothing but the conclusion degrades Alt-Tab to "acts once", rather than
    -- to a session whose bindings are disabled forever.
    let xkbSeats = [ x | s <- M.elems seats, Just x <- [rsXkbSeat s] ]
    if null xkbSeats
      then do
        io $ hPutStrLn stderr
          "xmonad-river: river_xkb_bindings_v1 is too old for modifier \
          \watching; a hold-to-cycle action will act once and stop"
        onDone
      else do
        io $ writeIORef (riverCapture (riverState conf)) $ Just InputCapture
            { icKeys       = captures
            , icOnKey      = \pressed i -> case drop i captures of
                ((_, sym):_) -> onKey pressed sym
                []           -> pure ()
            , icOnEnd      = onDone
            , icMods       = mods
            , icOneShot    = False
            , icGeneration = gen
            }
        emitOp (OpCaptureInput captures mods False gen)

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
    gen <- io nextSubmapGeneration
    let entries = M.toList subKeys
        keys' = map fst entries
        acts = map snd entries
    -- Written before the op is emitted, so the slot is populated by the time
    -- the event loop can possibly report a key: arming happens when the op is
    -- drained, which is later than this and on another path.
    io $ writeIORef (riverCapture (riverState conf)) $ Just InputCapture
        { icKeys       = keys'
        , icOnKey      = \_ i -> case drop i acts of
            (a:_) -> a
            []    -> onUnbound
        , icOnEnd      = onUnbound
        , icMods       = 0
        , icOneShot    = True
        , icGeneration = gen
        }
    emitOp (OpCaptureInput keys' 0 True gen)

