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
    postAction, postLoop,

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

    -- * Fullscreen
    --
    -- | Geometry stays the layout's; this tells the client what it is.
    informFullscreen,

    -- * Outputs
    --
    -- | Connector names, for a bar that wants to know which screen is which.
    outputName, outputNames,

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

    -- * The display
    --
    -- | What 'XMonad.Core.withDisplay' hands out: the connection, and the
    -- state the queries on it are answered from.
    Display'(..),

    -- * Lifecycle
    RestartRequested(..), setMainThread, exitSession,

    -- * Input devices
    --
    -- | What @xinput@ did: rules over closed data, matched by the loop.
    setInputConfig,
    InputRule(..), InputMatch(..), defaultInputMatch, touchpads,
    NameMatch(..), InputType(..),
    InputSettings(..), defaultInputSettings,
    SendEvents(..), ButtonMap(..), DragLock(..), ThreeFingerDrag(..),
    AccelProfile(..), ClickMethod(..), ScrollMethod(..),

    -- * Keysym tables
    keysymTable, reverseKeysymTable,

    -- * Diagnostics
    warnUnimplemented,
  ) where

import Data.Bits ((.&.))
import System.IO (hPutStrLn, stderr)
import Control.Concurrent.STM (atomically, writeTVar)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef', readIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word32)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, evaluate, handle)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask, asks)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M

import XMonad.Core
import Data.List (find, sortOn)
import XMonad.Operations (applySizeHintsContents, pointWithin)
import XMonad.River.Keysym.Table (keysymTable, reverseKeysymTable)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Client (closeAllClients)
import XMonad.River.Input
import qualified XMonad.River.Connection as C
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Ops (emitNow, emitOp)
import XMonad.River.Plan (Op(..))
import XMonad.River.Runtime (RestartRequested(..), setMainThread, warnUnimplemented)
import XMonad.River.Types
import XMonad.River.State (Display'(..), InputCapture(..), RiverState(..), updatePlacement)

-- | Run an action on the worker, from any thread.  It runs at the start of
-- the next manage sequence, the earliest moment it could legally change
-- anything.
postAction :: XConf -> X () -> IO ()
postAction c = MB.post (riverMailbox (riverState c))

-- | Queue protocol lifecycle work on the event loop.
--
-- The action runs before the loop dispatches another compositor event. This is
-- for destruction that must follow any render already using the object.
postLoop :: IO () -> X ()
postLoop action = asks (riverLoopJobs . riverState) >>= io . (`MB.post` action)

-- | Ask river for a manage sequence, because state it cannot see has
-- changed.  What makes actions from timers and forked threads take effect.
manageDirty :: X ()
manageDirty = do
  ref <- asks (riverDirty . riverState)
  io (atomically (writeTVar ref True))

-- | Run an action once the layout has been applied, rather than now.
--
-- Under X11 @windows@ moved every window before it returned, so a binding
-- could change the 'WindowSet' and ask where something ended up.  Here the
-- layout runs at the end of the sequence, so the same code would read the
-- geometry from before its own change -- @windows W.swapUp >> warpToWindow@
-- warps to where the window used to be.  Still the same sequence: the queue
-- is drained at the end of the layout, before anything is transmitted.  For
-- reading the result of a change, not for making another one.
afterLayout :: X () -> X ()
afterLayout act = do
  ref <- asks (riverAfterLayout . riverState)
  io (modifyIORef' ref (act :))
  inSeq <- io . readIORef =<< asks (inManageSeq . riverState)
  unless inSeq manageDirty

-- | End the Wayland session.  Exiting the process would only hand the seat
-- to the next window manager; the compositor and every client keep running.
exitSession :: X ()
exitSession = emitNow OpExitSession

-- | Where a window is: what the last layout decided, which is also what was
-- sent.  river never reports a position, since the window manager chose it.
-- 'Nothing' for a window not currently placed.
windowRect :: Window -> X (Maybe Rectangle)
windowRect w = do
    placements <- io . readIORef =<< asks (riverPlacements . riverState)
    pure (lookup w placements)

-- | Put a window at an absolute position and size, size hints applied.
--
-- Position is rendering state and dimensions are window management state,
-- so the resize is dropped outside a manage sequence; callers belong in a
-- binding or a 'XMonad.Operations.mouseDrag' callback.  The recorded
-- geometry is updated too, so 'XMonad.Operations.float' reads the new
-- position back rather than the last layout's.
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

-- | Where the pointer is, as river last reported it.  'Nothing' before it has
-- moved, or with no seat.
pointerPosition :: X (Maybe (Position, Position))
pointerPosition = do
    seats <- io . readIORef =<< asks (riverSeats . riverState)
    pure $ case M.elems seats of
        (s:_) -> Just (rsPointer s)
        []    -> Nothing

-- | Which placed window the pointer is over, from the last layout's
-- placements: the first rectangle containing it, which is the topmost.
windowUnderPointer :: X (Maybe Window)
windowUnderPointer = pointerPosition >>= \case
    Nothing -> pure Nothing
    Just (px, py) -> do
        placements <- io . readIORef =<< asks (riverPlacements . riverState)
        pure $ fst <$> find (pointWithin px py . snd) placements

-- | How many outputs the compositor has, over a connection of its own -- for
-- a config calling @countScreens@ in @main@, before xmonad starts.  @0@ if
-- there is no compositor to ask.
countOutputs :: MonadIO m => m Int
countOutputs = io $ handle (\(_ :: SomeException) -> pure 0) $ do
    conn <- C.connect
    (_, globals) <- C.getRegistry conn
    C.disconnect conn
    pure $ length [ () | g <- globals, C.globalInterface g == BC.pack "wl_output" ]

-- | Capture these keys until 'ungrabKeys', running the action for each.
--
-- X11's @grabKey@: a key reaches the window manager because a binding
-- exists for it, so grabbing is creating one.  Per keysym and modifier
-- mask, never per keycode.  The config's binding for the same key still
-- fires; river fires both.  Replaces any previous set.
grabKeys :: M.Map (KeyMask, KeySym) (X ()) -> X ()
grabKeys keymap = grabKeysUpDown (M.map (, pure ()) keymap)

-- | As 'grabKeys', with an action for the key going up as well as down.
grabKeysUpDown :: M.Map (KeyMask, KeySym) (X (), X ()) -> X ()
grabKeysUpDown keymap = do
    conf <- ask
    emitOp OpUngrabKeys
    let entries = M.toList keymap
    -- The actions, in the order the loop will index them, under a fresh
    -- generation.  Written before the op, so the table is there by the time a
    -- binding can fire; tagged, so the previous grab's bindings -- alive until
    -- the sequence destroys them -- cannot index this table with their own
    -- positions.
    gen <- io (nextGeneration (riverSubmapGen (riverState conf)))
    io $ atomicWriteIORef (riverExtraKeys (riverState conf)) (gen, map snd entries)
    emitOp (OpGrabKeys gen (map fst entries))
    manageDirty

-- | Release everything 'grabKeys' captured.
ungrabKeys :: X ()
ungrabKeys = do
    conf <- ask
    gen <- io (nextGeneration (riverSubmapGen (riverState conf)))
    io (atomicWriteIORef (riverExtraKeys (riverState conf)) (gen, []))
    emitOp OpUngrabKeys
    manageDirty

-- | Keep a window above the ones the layout placed.  The render sequence
-- restacks from the layout every frame, so this is a standing request; it
-- lapses when the window stops being placed.
raiseWindow :: Window -> X ()
raiseWindow w = restackWindows [w]

-- | Keep these windows above the ones the layout placed, topmost first.
-- Replaces any previous request.
restackWindows :: [Window] -> X ()
restackWindows ws = do
    ref <- asks (riverRestack . riverState)
    -- Stored bottom-to-top, because that is the order the render sequence
    -- walks; the argument is topmost-first, as X11's was.
    io (atomicWriteIORef ref (reverse ws))
    manageDirty

-- | The window this one is a dialog for: @xdg_toplevel.set_parent@, what
-- @WM_TRANSIENT_FOR@ answered.
windowParent :: Window -> X (Maybe Window)
windowParent w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    pure (M.lookup w known >>= rwParent)

-- | The XCursor theme the compositor draws with, on every seat.  Clients
-- draw their own cursors from @XCURSOR_THEME@ and @XCURSOR_SIZE@, which the
-- programs this spawns should have in their environment too.
setCursorTheme :: String -> Int -> X ()
setCursorTheme name size = do
    seats <- io . readIORef =<< asks (riverSeats . riverState)
    forM_ (M.elems seats) $ \s ->
        emitNow (OpSetXcursorTheme (rsObject s) (BC.pack name) (fromIntegral size))

-- | Ask a window to let the window manager draw its frame.  The default for
-- every new window (river's is @use_csd@); a CSD-only client ignores it.
-- Manage sequence only, so a manage hook is the place.
useServerDecorations :: Window -> X ()
useServerDecorations w = emitOp (OpUseDecorations w True)

-- | Let a window draw its own title bar and borders; undoes
-- 'useServerDecorations' for one window, from a manage hook.
useClientDecorations :: Window -> X ()
useClientDecorations w = emitOp (OpUseDecorations w False)

-- | Tell a window it is fullscreen, or that it no longer is.
--
-- @inform_fullscreen@ changes what the client draws -- a browser hides its
-- toolbars -- and nothing about where river puts it; the layout still owns
-- that, which is how "XMonad.Layout.Fullscreen" has always worked.  Called
-- from its event hook.  Only legal during a manage sequence.
informFullscreen :: Window -> Bool -> X ()
informFullscreen w full = emitOp (OpInformFullscreen w full)

-- | The connector name of a screen (@eDP-1@, @DP-3@), if the compositor
-- offers @wl_output@ version 4.  Screen ids follow the outputs sorted by
-- position, as "XMonad.River.WM" assigns them.
outputName :: ScreenId -> X (Maybe String)
outputName sid = lookup sid <$> outputNames

-- | Every screen's connector name, by screen id.
outputNames :: X [(ScreenId, String)]
outputNames = do
    outs <- io . readIORef =<< asks (riverOutputs . riverState)
    let live = [ o | o <- sortOn roPosition (M.elems outs), not (roRemoved o)
                   , let (w, h) = roSize o, w > 0 && h > 0 ]
    pure [ (S i, BC.unpack n) | (i, o) <- zip [0 ..] live, Just n <- [roName o] ]

-- | Close every prompt, releasing any keyboard grab one is holding.  Bind
-- it: river matches xkb bindings before it consults keyboard focus, so this
-- fires while a wedged prompt holds an exclusive grab.  The client threads
-- are killed, not asked.
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
    gen <- io (nextGeneration (riverSubmapGen (riverState conf)))
    seats <- io (readIORef (riverSeats (riverState conf)))
    -- @modifiers_watch@ arrived in version 3 of river_xkb_bindings_v1, and
    -- without it there is no way to learn that a modifier was released.  Doing
    -- nothing but the conclusion degrades Alt-Tab to "acts once", rather than
    -- to a session whose bindings are disabled forever.  The seat object
    -- alone is not enough to ask: it exists from version 2, and sending a
    -- version-3 request to it would be a protocol error.
    let xkbSeats = [ x | s <- M.elems seats, not (rsRemoved s), Just x <- [rsXkbSeat s] ]
    if null xkbSeats || riverXkbVersion (riverState conf) < 3
      then do
        io $ hPutStrLn stderr
          "xmonad-river: river_xkb_bindings_v1 is too old for modifier \
          \watching; a hold-to-cycle action will act once and stop"
        onDone
      else do
        io $ atomicWriteIORef (riverCapture (riverState conf)) $ Just InputCapture
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
    gen <- io (nextGeneration (riverSubmapGen (riverState conf)))
    let entries = M.toList subKeys
        keys' = map fst entries
        acts = map snd entries
    -- Written before the op is emitted, so the slot is populated by the time
    -- the event loop can possibly report a key: arming happens when the op is
    -- drained, which is later than this and on another path.
    io $ atomicWriteIORef (riverCapture (riverState conf)) $ Just InputCapture
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

-- | Replace the input rules.  Authoritative: a field no rule sets returns
-- to the device's default, so run no other input-configuration client.
-- Applies to devices present and hot-plugged; an invalid rule leaves the
-- whole set uninstalled, with a line on stderr.  Needs no manage sequence.
setInputConfig :: [InputRule] -> X ()
setInputConfig rules = case validateInputConfig rules of
    Left err -> io $ hPutStrLn stderr
        ("xmonad-river: setInputConfig: " ++ err ++ "; no rules were installed")
    Right cfg -> do
        -- On the worker: the loop is never handed a thunk.
        io (evaluate (forceInputConfig cfg))
        emitNow (OpInstallInputConfig cfg)

-- | Claim the next capture generation.
nextGeneration :: IORef Int -> IO Int
nextGeneration ref = atomicModifyIORef' ref (\n -> (n + 1, n + 1))
