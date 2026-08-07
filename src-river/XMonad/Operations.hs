{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Operations
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Operations, shadowing @src\/XMonad\/Operations.hs@.
--
-- This is where the two backends diverge most, and the divergence is not a
-- matter of a few differing lines: upstream's file is 899 lines of Xlib calls,
-- this one is a fraction of that, because river does the work.  They are not
-- two implementations of the same code -- they are different programs that
-- agree on an interface.
--
-- The interface they agree on is smaller here, deliberately.  Everything X11
-- reached for through the display and cannot be reproduced over Wayland is
-- absent rather than present and inert.  Each omission is justified in
-- tests/api/unportable.txt, and tests/api/check-api.sh fails if one appears
-- that is not.
--
-- The single structural difference to understand: __'windows' does not run the
-- layout.__  river only permits window management state to change during a
-- manage sequence, so the layout runs once at the end of the current sequence,
-- after every queued action has had its say.  Called from outside a sequence
-- -- a timer, a @dbus@ callback, a key binding -- 'windows' asks river to
-- start one.  Upstream's @windows@ tiles immediately because X11 let it.
--
-----------------------------------------------------------------------------

module XMonad.Operations (
    -- * Manage One Window
    unmanage, killWindow, kill, isClient,
    hide, reveal,
    focus, isFixedSizeOrTransient,

    -- * Manage Windows
    windows, refresh, rescreen, modifyWindowSet, windowBracket, windowBracket_,
    withFocused, withUnfocused,

    -- * Keyboard and Mouse
    cleanMask, extraModifiers, cacheNumlockMask,

    -- * Messages
    sendMessage, broadcastMessage, sendMessageWithNoRefresh,

    -- * Floating Layer
    float, floatLocation,

    -- * Window Size Hints
    D, mkAdjust, applySizeHints, applySizeHints', applySizeHintsContents,
    applyAspectHint, applyResizeIncHint, applyMaxSizeHint,

    -- * Rectangles
    containedIn, nubScreens, pointWithin, scaleRationalRect,
    getCleanedScreenInfo,

    -- * Pointer
    warpPointer,
    mouseDrag, mouseMoveWindow, mouseResizeWindow,

    -- * Lifecycle
    restart, exitSession,

    -- * Other Utilities
    pointScreen, screenWorkspace,
    setLayout, updateLayout,
    ) where

import XMonad.Core
import XMonad.River.Protocol.WindowManagement
import qualified XMonad.StackSet as W

import Data.IORef (readIORef, writeIORef)
import Data.List            (find, nub, sortOn)
import Data.Maybe
import Data.Monoid          (Any(..))
import Data.Ratio           ((%))
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad (forM_, guard, unless, void, when)

-- ---------------------------------------------------------------------
-- Managing windows

-- | Modify the current window list with a pure function, and arrange for the
-- result to be applied.
--
-- Unlike the X11 version this does not itself run the layout.  See the module
-- header: layout belongs to the manage sequence, and this function's job is to
-- make sure one happens.
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    modify $ \st -> st { windowset = f (windowset st) }
    inSeq <- io . readIORef =<< asks inManageSeq
    unless inSeq requestManageSequence
    asks (logHook . config) >>= userCodeDef ()

-- | Ask river to start a manage sequence, because state it cannot observe has
-- changed.
requestManageSequence :: X ()
requestManageSequence = do
    conn <- asks display
    manager <- asks riverManager
    io (riverWindowManagerV1ManageDirty conn manager)

-- | Re-run the layout.  Under river this is a request for another manage
-- sequence; the layout runs there.
refresh :: X ()
refresh = windows id

-- | Modify the @WindowSet@ in state with no special handling.
modifyWindowSet :: (WindowSet -> WindowSet) -> X ()
modifyWindowSet f = modify $ \xst -> xst { windowset = f (windowset xst) }

-- | Perform an @X@ action and check its return value against a predicate p.
-- If p holds, unwind changes to the @WindowSet@ and replay them using @windows@.
windowBracket :: (a -> Bool) -> X a -> X a
windowBracket p action = withWindowSet $ \old -> do
  a <- action
  when (p a) . withWindowSet $ \new -> do
    modifyWindowSet $ const old
    windows         $ const new
  return a

-- | Perform an @X@ action. If it returns @Any True@, unwind the
-- changes to the @WindowSet@ and replay them using @windows@.
windowBracket_ :: X Any -> X ()
windowBracket_ = void . windowBracket getAny

-- | A window no longer exists; remove it from the window list, on whatever
-- workspace it is.
unmanage :: Window -> X ()
unmanage = windows . W.delete

-- | Close the given window.  Politely -- this is @xdg_toplevel.close@, which
-- the client may ignore or prompt about, exactly as an X11 @WM_DELETE_WINDOW@
-- could be.
killWindow :: Window -> X ()
killWindow w = do
    conn <- asks display
    known <- io . readIORef =<< asks riverWindows
    when (M.member w known) $ io (riverWindowV1Close conn w)

-- | Kill the currently focused client.
kill :: X ()
kill = withFocused killWindow

-- | Hide a window, by removing it from the visible set.
--
-- The compositor call happens in the render sequence -- river only applies
-- rendering state at @render_finish@ -- so this records the intent and lets
-- 'XMonad.River.WM.renderSequence' carry it out.
hide :: Window -> X ()
hide w = whenX (gets (S.member w . mapped)) $
    modify (\s -> s { mapped = S.delete w (mapped s) })

-- | Show a window.  Harmless if it was already visible.
reveal :: Window -> X ()
reveal w = whenX (isClient w) $
    modify (\s -> s { mapped = S.insert w (mapped s) })

-- | Is the window under management by xmonad?
isClient :: Window -> X Bool
isClient w = withWindowSet $ return . W.member w

-- | Set focus explicitly to window @w@ if it is managed by us.
focus :: Window -> X ()
focus w = local (\c -> c { mouseFocused = True }) $ withWindowSet $ \s ->
    when (W.member w s && W.peek s /= Just w) $ windows (W.focusWindow w)

-- | Apply an 'X' operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ \w -> whenJust (W.peek w) f

-- | Apply an 'X' operation to all unfocused windows on the current workspace.
withUnfocused :: (Window -> X ()) -> X ()
withUnfocused f = withWindowSet $ \ws ->
    whenJust (W.peek ws) $ \w ->
        let unfocusedWindows = filter (/= w) $ W.index ws
        in mapM_ f unfocusedWindows

-- ---------------------------------------------------------------------
-- Keyboard and mouse
--
-- These three have a correct total implementation under river rather than an
-- unavailable one, because the situation each guards against cannot arise.
-- They are not stubs: each establishes exactly the postcondition its caller
-- is entitled to.
--
-- Their neighbours in upstream's export list -- unGrab, setButtonGrab and
-- clearEvents -- are deliberately *not* here, even though `pure ()` would be
-- equally vacuous.  Those two are advice-shaped: unGrab's caller writes
-- @unGrab >> spawn "slock"@ believing it has handed the keyboard over, and
-- under river it has not.  The locker gets input by a different route
-- entirely, so a silent success would be true about grabs and misleading
-- about the thing the caller cares about.  See tests/api/unportable.txt.

-- | Strip numlock\/capslock from a mask.
--
-- The identity, because river delivers modifiers already resolved: the bits
-- this was written to remove are never set.  Stripping nothing is the correct
-- strip, not a skipped one.
cleanMask :: KeyMask -> X KeyMask
cleanMask = return

-- | Combinations of extra modifier masks we need to grab keys\/buttons for.
--
-- Exactly one, the empty one.  This existed so that a binding could be
-- grabbed once per numlock\/capslock combination; river's bindings are
-- protocol objects matched against resolved modifiers, so one is all there is.
extraModifiers :: X [KeyMask]
extraModifiers = return [0]

-- | Find the numlock modifier and remember it.
--
-- There is no keymap to inspect and no mask to find.  The field is set to
-- zero, which is what 'cleanMask' and 'extraModifiers' above already assume,
-- so this establishes its postcondition rather than skipping it.
cacheNumlockMask :: X ()
cacheNumlockMask = modify $ \s -> s { numberlockMask = 0 }

-- ---------------------------------------------------------------------
-- Screens

-- | The screen configuration may have changed; update the state and refresh.
--
-- Under X11 this queried xinerama.  Here the reconciliation against river's
-- outputs happens at the start of every manage sequence anyway, so this only
-- has to ask for one.
rescreen :: X ()
rescreen = refresh

-- | Returns 'True' if the first rectangle is contained within, but not equal
-- to the second.
containedIn :: Rectangle -> Rectangle -> Bool
containedIn r1@(Rectangle x1 y1 w1 h1) r2@(Rectangle x2 y2 w2 h2)
 = and [ r1 /= r2
       , x1 >= x2
       , y1 >= y2
       , fromIntegral x1 + w1 <= fromIntegral x2 + w2
       , fromIntegral y1 + h1 <= fromIntegral y2 + h2 ]

-- | Given a list of screens, remove all duplicated screens and screens that
-- are entirely contained within another.
nubScreens :: [Rectangle] -> [Rectangle]
nubScreens xs = nub . filter (\x -> not $ any (x `containedIn`) xs) $ xs

-- | The screen rectangles, cleaned according to the rules for 'nubScreens'.
--
-- Under X11 this queried xinerama through the display.  Here the outputs are
-- accumulated in 'XConf', so the 'Display' is accepted and unused, as for
-- 'isFixedSizeOrTransient'.  Removed outputs and
-- zero-sized ones are skipped, and an output's layer-shell area is preferred
-- to its raw rectangle where one has been reported, so that a bar or dock
-- claiming an exclusive zone shrinks the tiling area rather than being tiled
-- over.
--
-- Ordering is by position, which is what keeps screen ids stable across a
-- monitor being unplugged and replugged.
getCleanedScreenInfo :: Display -> X [Rectangle]
getCleanedScreenInfo _ = do
    outs <- io . readIORef =<< asks riverOutputs
    pure $ nubScreens
        [ rect
        | o <- sortOn roPosition (filter (not . roRemoved) (M.elems outs))
        , let (x, y) = roPosition o
        , let (width, height) = roSize o
        , width > 0 && height > 0
        , let rect = case roLayerArea o of
                Just a | rect_width a > 0 && rect_height a > 0 -> a
                _ -> Rectangle x y (fromIntegral width) (fromIntegral height)
        ]

-- | Given a point, determine the screen (if any) that contains it.
pointScreen :: Position -> Position
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
pointScreen x y = withWindowSet $ return . find p . W.screens
  where p = pointWithin x y . screenRect . W.screenDetail

-- | @pointWithin x y r@ returns 'True' if the @(x, y)@ co-ordinate is within
-- @r@.
pointWithin :: Position -> Position -> Rectangle -> Bool
pointWithin x y r = x >= rect_x r &&
                    x <  rect_x r + fromIntegral (rect_width r) &&
                    y >= rect_y r &&
                    y <  rect_y r + fromIntegral (rect_height r)

-- | Produce the actual rectangle from a screen and a ratio on that screen.
scaleRationalRect :: Rectangle -> W.RationalRect -> Rectangle
scaleRationalRect (Rectangle sx sy sw sh) (W.RationalRect rx ry rw rh)
 = Rectangle (sx + scale sw rx) (sy + scale sh ry) (scale sw rw) (scale sh rh)
 where scale s r = floor (toRational s * r)

-- | Return workspace visible on screen @sc@, or 'Nothing'.
screenWorkspace :: ScreenId -> X (Maybe WorkspaceId)
screenWorkspace sc = withWindowSet $ return . W.lookupWorkspace sc

------------------------------------------------------------------------
-- Message handling

-- | Throw a message to the current 'LayoutClass', possibly modifying how we
-- lay out the windows, in which case changes are handled through a refresh.
sendMessage :: Message a => a -> X ()
sendMessage a = windowBracket_ $ do
    w <- gets $ W.workspace . W.current . windowset
    ml' <- handleMessage (W.layout w) (SomeMessage a) `catchX` return Nothing
    whenJust ml' $ \l' ->
        modifyWindowSet $ \ws -> ws { W.current = (W.current ws)
                                { W.workspace = (W.workspace $ W.current ws)
                                  { W.layout = l' }}}
    return (Any $ isJust ml')

-- | Send a message to all layouts, without refreshing.
broadcastMessage :: Message a => a -> X ()
broadcastMessage a = withWindowSet $ \ws -> do
    let c = W.workspace . W.current $ ws
        v = map W.workspace . W.visible $ ws
        h = W.hidden ws
    mapM_ (sendMessageWithNoRefresh a) (c : v ++ h)

-- | Send a message to a layout, without refreshing.
sendMessageWithNoRefresh :: Message a => a -> WindowSpace -> X ()
sendMessageWithNoRefresh a w =
    handleMessage (W.layout w) (SomeMessage a) `catchX` return Nothing >>=
    updateLayout  (W.tag w)

-- | Update the layout field of a workspace.
updateLayout :: WorkspaceId -> Maybe (Layout Window) -> X ()
updateLayout i ml = whenJust ml $ \l ->
    runOnWorkspaces $ \ww -> return $ if W.tag ww == i then ww { W.layout = l} else ww

-- | Set the layout of the currently viewed workspace.
setLayout :: Layout Window -> X ()
setLayout l = do
    ss@W.StackSet{ W.current = c@W.Screen{ W.workspace = ws }} <- gets windowset
    _ <- handleMessage (W.layout ws) (SomeMessage ReleaseResources)
    windows $ const $ ss{ W.current = c{ W.workspace = ws{ W.layout = l } } }

------------------------------------------------------------------------
-- Floating layer support

-- | Given a window, find the screen it is located on, and compute the geometry
-- of that window with respect to that screen.
--
-- X11 read this from the window's attributes and size hints.  River reports a
-- window's current dimensions directly, and nothing else: there is no position
-- to read, because under Wayland the compositor owns placement and a client
-- never had one to report.  So the fraction is computed from the dimensions
-- against the current screen, and the window is centred.
floatLocation :: Window -> X (ScreenId, W.RationalRect)
floatLocation w = do
    ws <- gets windowset
    known <- io . readIORef =<< asks riverWindows
    let sc = W.current ws
        sr = screenRect (W.screenDetail sc)
        sw = max 1 (fromIntegral (rect_width sr))
        sh = max 1 (fromIntegral (rect_height sr))
    pure $ case M.lookup w known of
        Nothing -> (W.screen sc, W.RationalRect 0 0 1 1)
        Just rw ->
            let (width, height) = rwDimensions rw
                rwidth  = fromIntegral (max 1 width)  % sw
                rheight = fromIntegral (max 1 height) % sh
            in ( W.screen sc
               , W.RationalRect (0.5 - rwidth / 2) (0.5 - rheight / 2) rwidth rheight )

-- | Make a tiled window floating, using its suggested rectangle.
float :: Window -> X ()
float w = do
    (sc, rr) <- floatLocation w
    windows $ \ws -> W.float w rr . fromMaybe ws $ do
        i  <- W.findTag w ws
        guard $ i `elem` map (W.tag . W.workspace) (W.screens ws)
        f  <- W.peek ws
        sw <- W.lookupWorkspace sc ws
        return (W.focusWindow f . W.shiftWin sw w $ ws)

-- ---------------------------------------------------------------------
-- Pointer

-- | Move the pointer, in river's global coordinate space.
--
-- Wayland forbids ordinary clients from warping the pointer, but the window
-- manager is not an ordinary client: @river_seat_v1.pointer_warp@ exists
-- precisely for this.
warpPointer :: Position -> Position -> X ()
warpPointer x y = do
    conn <- asks display
    seats <- io . readIORef =<< asks riverSeats
    forM_ (M.keys seats) $ \seat -> io (riverSeatV1PointerWarp conn seat x y)

-- | Accumulate mouse motion events.
--
-- river drives interactive operations through the seat rather than by letting
-- the window manager grab the pointer: @op_start_pointer@ begins one,
-- @op_delta@ reports the total offset since it began, and @op_release@ fires
-- when the button goes up.  "XMonad.River.WM" routes those two events back
-- here, so the @(motion, cleanup)@ contract is the one xmonad has always had.
--
-- The offset is turned back into an absolute position by adding the pointer
-- position recorded when the operation started, so the callback still receives
-- root coordinates.
mouseDrag :: (Position -> Position -> X ()) -> X () -> X ()
mouseDrag f done = do
    drag <- gets dragging
    case drag of
        Just _ -> return () -- already dragging
        Nothing -> do
            conn <- asks display
            seats <- io . readIORef =<< asks riverSeats
            case M.elems seats of
                [] -> return ()
                (s:_) -> do
                    origin <- asks riverDragOrigin
                    io (writeIORef origin (rsPointer s))
                    io (riverSeatV1OpStartPointer conn (rsObject s))
                    modify $ \st -> st { dragging = Just (f, cleanup) }
  where
    cleanup = do
        modify $ \st -> st { dragging = Nothing }
        done

-- | Drag the window under the cursor with the mouse while it is dragged.
mouseMoveWindow :: Window -> X ()
mouseMoveWindow w = whenX (isClient w) $ do
    known <- io . readIORef =<< asks riverWindows
    ws <- gets windowset
    let sr = screenRect (W.screenDetail (W.current ws))
    forM_ (M.lookup w known) $ \_ -> do
        (ox, oy) <- io . readIORef =<< asks riverDragOrigin
        mouseDrag
            (\ex ey -> do
                node <- io . readIORef =<< asks riverWindows
                conn <- asks display
                forM_ (M.lookup w node) $ \rw ->
                    io $ riverNodeV1SetPosition conn (rwNode rw)
                           (rect_x sr + (ex - ox)) (rect_y sr + (ey - oy))
                float w)
            (float w)

-- | Resize the window under the cursor with the mouse while it is dragged.
mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = whenX (isClient w) $ do
    known <- io . readIORef =<< asks riverWindows
    forM_ (M.lookup w known) $ \rw0 -> do
        let (w0, h0) = rwDimensions rw0
            hints = rwSizeHints rw0
        (ox, oy) <- io . readIORef =<< asks riverDragOrigin
        mouseDrag
            (\ex ey -> do
                conn <- asks display
                let (width, height) = applySizeHintsContents hints
                        ( fromIntegral w0 + (ex - ox)
                        , fromIntegral h0 + (ey - oy) )
                io $ riverWindowV1ProposeDimensions conn w
                       (fromIntegral width) (fromIntegral height)
                float w)
            (float w)

-- ---------------------------------------------------------------------
-- Lifecycle

-- | @restart name resume@ restarts the window manager by executing @name@.
--
-- This is the river analogue of xmonad's @restart@, and the reason @M-q@
-- survives the move to Wayland.  The compositor owns the windows, not the
-- window manager, so tearing the window manager down disturbs nothing: river
-- supports hot-swapping window managers without restarting itself or any
-- client.
--
-- The sequence is @stop@, wait for @finished@, then exec.  Overlapping the two
-- is not allowed -- river answers the second connection with @unavailable@ --
-- so the handover has to be ordered this way.
--
-- @resume@ is accepted and ignored.  There is no state file: a river object id
-- serialised by this process means nothing to its successor, because ids are
-- per-connection and recycled after @wl_display.delete_id@.  See
-- README.river.md, which records this as something worth fixing upstream.
restart :: String -> Bool -> X ()
restart cmd _resume = do
    conn <- asks display
    manager <- asks riverManager
    ref <- asks riverRestart
    broadcastMessage ReleaseResources
    io (writeIORef ref (Just cmd))
    io (riverWindowManagerV1Stop conn manager)

-- | End the Wayland session, taking the compositor with it.
exitSession :: X ()
exitSession = do
    conn <- asks display
    manager <- asks riverManager
    io (riverWindowManagerV1ExitSession conn manager)

-- ---------------------------------------------------------------------
-- Support for window size hints
--
-- The arithmetic below is exactly upstream's, unchanged, and that is the
-- point: it is pure, it already treats every hint as optional, and river's
-- 'SizeHints' simply has fewer of them populated.  A hint river cannot report
-- leaves the corresponding function as the identity, which is what it always
-- did for a window that declared no such hint under X11 either.

-- | An alias for a (width, height) pair
type D = (Dimension, Dimension)

-- | Detect whether a window has fixed size or is transient.
--
-- The 'Display' is accepted and unused: there is only one, 'XConf' already has
-- it, and what river reports about a window is accumulated there rather than
-- queried over the wire.  Keeping the parameter costs nothing and lets a call
-- site written for X11 compile unchanged.
--
-- Fixed size is @dimensions_hint@ reporting an equal minimum and maximum;
-- transient is @river_window_v1.parent@, which is @xdg_toplevel.set_parent@ --
-- the faithful translation of X11's @WM_TRANSIENT_FOR@.
isFixedSizeOrTransient :: Display -> Window -> X Bool
isFixedSizeOrTransient _ w = do
    known <- io . readIORef =<< asks riverWindows
    pure $ case M.lookup w known of
        Nothing -> False
        Just rw ->
            let sh = rwSizeHints rw
                isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
                isTransient = isJust (rwParent rw)
            in isFixedSize || isTransient

-- | Given a window, build an adjuster function that will reduce the given
-- dimensions according to the window's border width and size hints.
mkAdjust :: Window -> X (D -> D)
mkAdjust w = do
    known <- io . readIORef =<< asks riverWindows
    bw <- asks (borderWidth . config)
    pure $ case M.lookup w known of
        Nothing -> id
        Just rw -> applySizeHints bw (rwSizeHints rw)

-- | Reduce the dimensions if needed to comply to the given SizeHints, taking
-- window borders into account.
applySizeHints :: Integral a => Dimension -> SizeHints -> (a, a) -> D
applySizeHints bw sh =
    tmap (+ 2 * bw) . applySizeHintsContents sh . tmap (subtract $ 2 * fromIntegral bw)
    where
    tmap f (x, y) = (f x, f y)

-- | Reduce the dimensions if needed to comply to the given SizeHints.
applySizeHintsContents :: Integral a => SizeHints -> (a, a) -> D
applySizeHintsContents sh (w, h) =
    applySizeHints' sh (fromIntegral $ max 1 w, fromIntegral $ max 1 h)

-- | Use size hints to scale a pair of dimensions.
applySizeHints' :: SizeHints -> D -> D
applySizeHints' sh =
      maybe id applyMaxSizeHint                   (sh_max_size   sh)
    . maybe id (\(bw, bh) (w, h) -> (w+bw, h+bh)) (sh_base_size  sh)
    . maybe id applyResizeIncHint                 (sh_resize_inc sh)
    . maybe id applyAspectHint                    (sh_aspect     sh)
    . maybe id (\(bw,bh) (w,h)   -> (w-bw, h-bh)) (sh_base_size  sh)

-- | Reduce the dimensions so their aspect ratio falls between the two given
-- aspect ratios.
--
-- Correct as written, and under river it never fires: Wayland has no aspect
-- ratio hint, so 'sh_aspect' is always 'Nothing'.
applyAspectHint :: (D, D) -> D -> D
applyAspectHint ((minx, miny), (maxx, maxy)) x@(w,h)
    | or [minx < 1, miny < 1, maxx < 1, maxy < 1] = x
    | w * maxy > h * maxx                         = (h * maxx `div` maxy, h)
    | w * miny < h * minx                         = (w, w * miny `div` minx)
    | otherwise                                   = x

-- | Reduce the dimensions so they are a multiple of the size increments.
--
-- As 'applyAspectHint': correct, and never fires under river.
applyResizeIncHint :: D -> D -> D
applyResizeIncHint (iw,ih) x@(w,h) =
    if iw > 0 && ih > 0 then (w - w `mod` iw, h - h `mod` ih) else x

-- | Reduce the dimensions if they exceed the given maximum dimensions.
--
-- This one does fire: @dimensions_hint@ carries a maximum.
applyMaxSizeHint  :: D -> D -> D
applyMaxSizeHint (mw,mh) x@(w,h) =
    if mw > 0 && mh > 0 then (min w mw,min h mh) else x
