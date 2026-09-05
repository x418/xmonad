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
-- Operations, on the river backend.  Everything upstream did through Xlib is
-- either absent -- each omission is justified in tests/api/unportable.txt --
-- or answered from the window manager's own state.
--
-- The one structural difference: 'windows' does not run the layout.  river
-- permits window management state to change only during a manage sequence,
-- so the layout runs once at the end of the current one, after every queued
-- action; called from outside a sequence, 'windows' asks river for one.
--
-----------------------------------------------------------------------------

module XMonad.Operations (
    -- * Manage One Window
    unmanage, killWindow, kill, isClient,
    hide, reveal, setWindowBorderWithFallback,
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
    StateFile (..), writeStateToFile, readStateFile, restart,

    -- * Other Utilities
    pointScreen, screenWorkspace,
    setLayout, updateLayout,
    ) where

import XMonad.Core
import XMonad.River.Ops (emitNow, emitOp)
import XMonad.River.Plan (Op(..))
import XMonad.River.State
import XMonad.River.Types
import XMonad.River.Protocol.WindowManagement
import qualified XMonad.StackSet as W

import Control.Arrow        (second)
import Control.Concurrent.STM (atomically, writeTVar)
import Data.IORef (atomicWriteIORef, readIORef, writeIORef)
import Data.List            (find, nub, sortOn)
import Data.Maybe
import Data.Monoid          (Any(..))
import Data.Ratio           ((%))
import System.Directory     (removeFile)
import System.Environment   (getArgs)
import System.IO            (Handle, IOMode (ReadMode), hGetContents, hPutStrLn, stderr, withFile)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad (forM, forM_, guard, join, unless, void, when)

-- | Whether a window should float on adoption: fixed size (equal minimum and
-- maximum in @dimensions_hint@) or transient (@river_window_v1.parent@, the
-- Wayland spelling of @WM_TRANSIENT_FOR@).  The 'Display' is unused; the
-- signature is upstream's.
isFixedSizeOrTransient :: Display -> Window -> X Bool
isFixedSizeOrTransient _ w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    pure $ case M.lookup w known of
        Nothing -> False
        Just rw ->
            let sh = rwSizeHints rw
                isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
                isTransient = isJust (rwParent rw)
            in isFixedSize || isTransient

-- | A window no longer exists; remove it from the window list, on whatever
-- workspace it is.
unmanage :: Window -> X ()
unmanage = windows . W.delete

-- | Ask the window to close: @river_window_v1.close@, which the client may
-- ignore or prompt about, as @WM_DELETE_WINDOW@ could be.  Queued: @close@ is
-- legal only inside a manage sequence.
killWindow :: Window -> X ()
killWindow w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    -- Queued rather than sent: @close@ is legal only during a manage sequence,
    -- and this runs from a keybinding, which is outside one.
    when (M.member w known) $ emitOp (OpClose w)

-- | Kill the currently focused client.
kill :: X ()
kill = withFocused killWindow

-- | Modify the current window list with a pure function, and arrange for the
-- result to be applied.
--
-- Unlike upstream this does not run the layout: river permits window
-- management state to change only inside a manage sequence, so the layout
-- runs once at the end of the current one, after every queued action.
-- Called outside a sequence, this asks river for one.
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    old <- gets windowset
    let ws = f old
    modify $ \st -> st { windowset = ws }

    -- Tell a workspace that has just left the screen, as upstream does.  A
    -- layout that draws its own surfaces -- Tabbed, and everything else built
    -- on "XMonad.Layout.Decoration" -- unmaps them on 'Hide', and nothing
    -- else ever tells it to: the decorations are shell surfaces this process
    -- owns, not river windows, so 'transmitRender' hiding the windows of a
    -- workspace that is no longer visible does not touch them.  Without this
    -- the tab bar of the workspace you left stays on screen over the one you
    -- switched to, until some later relayout happens to redraw it.
    let tagsOldVisible = map (W.tag . W.workspace) (W.current old : W.visible old)
        gottenHidden = filter ((`elem` tagsOldVisible) . W.tag) (W.hidden ws)
    mapM_ (sendMessageWithNoRefresh Hide) gottenHidden

    inSeq <- io . readIORef =<< asks (inManageSeq . riverState)
    unless inSeq requestManageSequence
    -- The log hook runs once, at the end of the sequence, rather than here
    -- and there both; see 'XMonad.River.WM.Sequence.manageSequence'.
    io . flip writeIORef True =<< asks (riverLogDue . riverState)

-- | Ask river for a manage sequence.  A flag the loop drains, because the
-- loop owns the connection and this runs wherever an 'X' action does; the
-- flag is a 'TVar' so the loop wakes.
requestManageSequence :: X ()
requestManageSequence = do
    ref <- asks (riverDirty . riverState)
    io (atomically (writeTVar ref True))

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
-- changes to the @WindowSet@ and replay them using @windows@. This is
-- a version of @windowBracket@ that discards the return value and
-- handles an @X@ action that reports its need for refresh via @Any@.
windowBracket_ :: X Any -> X ()
windowBracket_ = void . windowBracket getAny

-- | Produce the actual rectangle from a screen and a ratio on that screen.
scaleRationalRect :: Rectangle -> W.RationalRect -> Rectangle
scaleRationalRect (Rectangle sx sy sw sh) (W.RationalRect rx ry rw rh)
 = Rectangle (sx + scale sw rx) (sy + scale sh ry) (scale sw rw) (scale sh rh)
 where scale s r = floor (toRational s * r)

-- | Set a window's border colour from a colour string, falling back to the
-- 'Pixel' if it does not parse.  X11's fallback covered a full colormap; here
-- the only failure is a string that is not @\"#rrggbb\"@.
setWindowBorderWithFallback :: Display -> Window -> String -> Pixel -> X ()
setWindowBorderWithFallback _ w color fallback = do
    ref <- asks (riverBorders . riverState)
    io (overrideBorderColor ref w (pixelColor (fromMaybe fallback (parseColorMaybe color))))

-- | Hide a window: drop it from the mapped set.  The compositor request is
-- the render sequence's, from the plan.
hide :: Window -> X ()
hide w = whenX (gets (S.member w . mapped)) $
    modify (\s -> s { mapped = S.delete w (mapped s) })

-- | Show a window.  Harmless if the window was already visible.
reveal :: Window -> X ()
reveal w = whenX (isClient w) $
    modify (\s -> s { mapped = S.insert w (mapped s) })

-- | Re-run the layout: under river, a request for another manage sequence.
refresh :: X ()
refresh = windows id

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

-- | The screen rectangles, cleaned according to the rules for 'nubScreens':
-- river's live outputs, sorted by position (which keeps screen ids stable
-- across a replug), each preferring its layer-shell usable area so a bar's
-- exclusive zone is not tiled over.  The 'Display' is unused.
getCleanedScreenInfo :: Display -> X [Rectangle]
getCleanedScreenInfo _ = do
    outs <- io . readIORef =<< asks (riverOutputs . riverState)
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

-- | The screen configuration may have changed; update the state and refresh.
-- The reconciliation against river's outputs happens at the start of every
-- manage sequence, so this only asks for one.
rescreen :: X ()
rescreen = refresh

-- | Set focus explicitly to window @w@ if it is managed by us.
focus :: Window -> X ()
focus w = local (\c -> c { mouseFocused = True }) $ withWindowSet $ \s ->
    when (W.member w s && W.peek s /= Just w) $ windows (W.focusWindow w)

-- | Find the numlock modifier and remember it.  There is no keymap to
-- inspect: river resolves lock modifiers before a binding fires, so the mask
-- is zero, which is what 'cleanMask' and 'extraModifiers' assume.
cacheNumlockMask :: X ()
cacheNumlockMask = modify $ \s -> s { numberlockMask = 0 }

-- | Throw a message to the current 'LayoutClass' possibly modifying how we
-- layout the windows, in which case changes are handled through a refresh.
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
    handleMessage (W.layout ws) (SomeMessage ReleaseResources)
    windows $ const $ ss{ W.current = c{ W.workspace = ws{ W.layout = l } } }

-- | Return workspace visible on screen @sc@, or 'Nothing'.
screenWorkspace :: ScreenId -> X (Maybe WorkspaceId)
screenWorkspace sc = withWindowSet $ return . W.lookupWorkspace sc

-- | Apply an 'X' operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ \w -> whenJust (W.peek w) f

-- | Apply an 'X' operation to all unfocused windows on the current workspace, if there are any.
withUnfocused :: (Window -> X ()) -> X ()
withUnfocused f = withWindowSet $ \ws ->
    whenJust (W.peek ws) $ \w ->
        let unfocusedWindows = filter (/= w) $ W.index ws
        in mapM_ f unfocusedWindows

-- | Is the window is under management by xmonad?
isClient :: Window -> X Bool
isClient w = withWindowSet $ return . W.member w

-- | Combinations of extra modifier masks we need to grab keys\/buttons for.
-- Exactly one, the empty one: river bindings are matched against resolved
-- modifiers, so there is no numlock or capslock variant to grab.
extraModifiers :: X [KeyMask]
extraModifiers = return [0]

-- | Strip numlock\/capslock from a mask.  The identity: river never sets
-- those bits.
cleanMask :: KeyMask -> X KeyMask
cleanMask = return

-- | Write the current window state (and extensible state) to a file so that
-- xmonad can resume with that state intact.  Windows are keyed by
-- @river_window_v1.identifier@, which outlives the connection; one river has
-- not given an identifier is left out and comes back through the manage hook.
writeStateToFile :: X ()
writeStateToFile = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    let identOf w = B8.unpack <$> (rwIdentifier =<< M.lookup w known)

        maybeShow (t, Right (PersistentExtension ext)) = Just (t, show ext)
        maybeShow (t, Left str) = Just (t, str)
        maybeShow _ = Nothing

        wsData   = retagWindows identOf . W.mapLayout show . windowset
        extState = mapMaybe maybeShow . M.toList . extensibleState

    path <- asks $ stateFileName . directories
    stateData <- gets (\s -> StateFile (wsData s) (extState s))
    catchIO (writeFile path $ show stateData)

-- | Re-key a 'W.StackSet', dropping anything the function has no answer for.
-- A dropped focus is replaced by the nearest survivor below it, then above,
-- which is what closing the focused window does.
retagWindows :: Ord b => (a -> Maybe b) -> W.StackSet i l a s sd -> W.StackSet i l b s sd
retagWindows f (W.StackSet cur vis hid flt) = W.StackSet
    (onScreen cur)
    (map onScreen vis)
    (map onWorkspace hid)
    (M.fromList [ (b, r) | (a, r) <- M.toList flt, Just b <- [f a] ])
  where
    onScreen (W.Screen ws sid sd) = W.Screen (onWorkspace ws) sid sd
    onWorkspace (W.Workspace t l st) = W.Workspace t l (onStack =<< st)
    -- 'up' runs outwards from the focus, so its head is the nearest window
    -- above and no reversal is needed to pick a replacement.
    onStack (W.Stack fo u d) = case (f fo, mapMaybe f u, mapMaybe f d) of
      (Just b, u', d')      -> Just (W.Stack b u' d')
      (Nothing, u', x : d') -> Just (W.Stack x u' d')
      (Nothing, x : u', []) -> Just (W.Stack x u' [])
      (Nothing, [], [])     -> Nothing

-- | Read the state of a previous xmonad instance from a file and return that
-- state.  The state file is removed after reading it.
--
-- Must run once river has advertised its windows, because the identifiers
-- in the file are resolved against them: the first manage sequence, see
-- @restoreState@ in "XMonad.River.WM.Sequence".  Parsed once; the note
-- compares what the file claimed with what resolved, because the two
-- differing means the identifiers did not match what river now reports,
-- and the symptom -- windows back on the wrong workspace -- is otherwise
-- indistinguishable from a missing file.
readStateFile :: XConfig Layout -> X (Maybe XState)
readStateFile xmc = do
    path <- asks $ stateFileName . directories
    msf <- parseStateFile path
    forM msf $ \sf -> do
        st <- restoreStateFile xmc sf
        io $ hPutStrLn stderr $ "xmonad-river: note: restored "
          <> show (length (W.allWindows (windowset st))) <> " of "
          <> show (length (W.allWindows (sfWins sf))) <> " windows from " <> path
        return st

-- | The state file's contents, and the file removed.  'Nothing' if it does
-- not parse, or cannot be read.
parseStateFile :: FilePath -> X (Maybe StateFile)
parseStateFile path = do
    -- I'm trying really hard here to make sure we read the entire
    -- contents of the file before it is removed from the file system.
    sf' <- userCode . io $ do
        raw <- withFile path ReadMode readStrict
        return $! maybeRead reads raw
    io (removeFile path)
    return (join sf')
  where
    readStrict :: Handle -> IO String
    readStrict h = hGetContents h >>= \s -> length s `seq` return s

-- | The 'XState' a parsed state file describes, its window identifiers
-- resolved against the windows river has advertised.
restoreStateFile :: XConfig Layout -> StateFile -> X XState
restoreStateFile xmc sf = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    let byIdent = M.fromList
          [ (B8.unpack i, rwObject w)
          | w <- M.elems known, Just i <- [rwIdentifier w] ]
        winset = retagWindows (`M.lookup` byIdent)
               . W.ensureTags layout (workspaces xmc)
               $ W.mapLayout (fromMaybe layout . maybeRead lreads) (sfWins sf)
        extState = M.fromList . map (second Left) $ sfExt sf
    return XState { windowset       = winset
                  , numberlockMask  = 0
                  , mapped          = S.empty
                  , waitingUnmap    = M.empty
                  , dragging        = Nothing
                  , extensibleState = extState
                  }
  where
    layout = layoutHook xmc
    lreads = readsLayout layout

-- | A whole parse, and nothing left over.
maybeRead :: ReadS a -> String -> Maybe a
maybeRead reads' s = case reads' s of
    [(x, "")] -> Just x
    _         -> Nothing

-- | @restart name resume@ restarts the window manager by executing @name@.
--
-- river hot-swaps window managers without restarting itself or any client:
-- @stop@, wait for @finished@, exec.  @resume@ writes the state file for the
-- successor, keyed on @river_window_v1.identifier@.
restart :: String -> Bool -> X ()
restart cmd resume = do
    ref <- asks (riverRestart . riverState)
    broadcastMessage ReleaseResources
    when resume writeStateToFile
    -- With the arguments this process was started with, as @--restart@
    -- keeps them: a config started with flags stays started with them.
    args <- io getArgs
    io (atomicWriteIORef ref (Just (cmd, args)))
    -- Queued for the event loop, which owns the connection.  Ordering holds:
    -- this action finishes before the loop's next pass, so the state file is
    -- written before river is asked to let go.
    emitNow OpStop

-- | An alias for a (width, height) pair
type D = (Dimension, Dimension)

-- | Where a window should go when it becomes floating: the screen it is on
-- and its rectangle as a fraction of that screen.
--
-- river never reports where a window is, so the answer is the last layout's
-- placement ('riverPlacements'), which is also what a drag has just written
-- there -- a float that ignored it would be undone by the next sequence.  A
-- window with no placement (one being floated by a manage hook, before any
-- layout) is centred at its own size, its minimum, or half the screen.
floatLocation :: Window -> X (ScreenId, W.RationalRect)
floatLocation w = do
    ws <- gets windowset
    placements <- io . readIORef =<< asks (riverGeometry . riverState)
    case M.lookup w placements of
        Just r -> do
            -- The screen the window is on, not the focused one.  Upstream
            -- takes the same care, and for the same reason: floating a window
            -- that lives on another screen must not haul it onto this one.
            msc <- pointScreen (rect_x r) (rect_y r)
            let sc = fromMaybe (W.current ws) msc
            pure (W.screen sc, relativeRect (screenRect (W.screenDetail sc)) r)
        Nothing -> do
            known <- io . readIORef =<< asks (riverWindows . riverState)
            let sc = W.current ws
                sr = screenRect (W.screenDetail sc)
                sw = max 1 (fromIntegral (rect_width sr))
                sh = max 1 (fromIntegral (rect_height sr))
            pure $ case M.lookup w known of
                Nothing -> (W.screen sc, W.RationalRect 0 0 1 1)
                Just rw ->
                    -- A window river has not sized yet reports (0, 0), and a
                    -- window being floated by a manage hook is exactly that:
                    -- the hook runs when it is adopted, before any layout has
                    -- proposed dimensions for it.  Clamping that to one pixel
                    -- made doCenterFloat centre a 1x1 rectangle, and a client
                    -- that will not be that small -- one with min size hints,
                    -- like JetBrains' project selector -- drew itself at its
                    -- own size from that centre point, hanging off the bottom
                    -- right of the screen.
                    --
                    -- Fall back to what it says it wants: its minimum, or
                    -- failing that half the screen, which is what a window
                    -- with nothing to say gets from most tiling layouts
                    -- anyway.
                    -- All as 'Integer': rwDimensions is signed and the size
                    -- hints are not, so the two do not meet in either of
                    -- their own types.
                    let hinted = case sh_min_size (rwSizeHints rw) of
                          Just (mw, mh) -> (toInteger mw, toInteger mh)
                          Nothing -> (sw `div` 2, sh `div` 2)
                        (width, height) = case rwDimensions rw of
                          (0, 0)   -> hinted
                          (dw, dh) -> (toInteger dw, toInteger dh)
                    in (W.screen sc, centredRect sr width height)

-- | A rectangle as a fraction of a screen: the exact inverse of
-- 'scaleRationalRect', so a placed window survives the round trip unchanged
-- and floating it is a no-op on screen.
relativeRect :: Rectangle -> Rectangle -> W.RationalRect
relativeRect sr r = W.RationalRect
    (fromIntegral (rect_x r - rect_x sr) % sw)
    (fromIntegral (rect_y r - rect_y sr) % sh)
    (fromIntegral (rect_width r)  % sw)
    (fromIntegral (rect_height r) % sh)
  where
    sw = max 1 (fromIntegral (rect_width sr))
    sh = max 1 (fromIntegral (rect_height sr))

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

-- | Make a tiled window floating, using its suggested rectangle
float :: Window -> X ()
float w = do
    (sc, rr) <- floatLocation w
    windows $ \ws -> W.float w rr . fromMaybe ws $ do
        i  <- W.findTag w ws
        guard $ i `elem` map (W.tag . W.workspace) (W.screens ws)
        f  <- W.peek ws
        sw <- W.lookupWorkspace sc ws
        return (W.focusWindow f . W.shiftWin sw w $ ws)

-- | Accumulate mouse motion events.
--
-- river drives interactive operations through the seat: @op_start_pointer@
-- begins one, @op_delta@ reports the total offset since it began, and
-- @op_release@ fires when the button goes up; "XMonad.River.WM.Events"
-- routes those back here, so the @(motion, cleanup)@ contract is upstream's.
mouseDrag :: (Position -> Position -> X ()) -> X () -> X ()
mouseDrag f done = void (startMouseDrag f done)

startMouseDrag :: (Position -> Position -> X ()) -> X () -> X Bool
startMouseDrag f done = do
    drag <- gets dragging
    case drag of
        Just _ -> return False -- already dragging
        Nothing -> do
            seats <- io . readIORef =<< asks (riverSeats . riverState)
            case filter (not . rsRemoved) (M.elems seats) of
                [] -> return False
                (s:_) -> do
                    origin <- asks (riverDragOrigin . riverState)
                    io (writeIORef origin (rsPointer s))
                    emitOp (OpPointerOpStart (rsObject s))
                    modify $ \st -> st { dragging = Just (f, cleanup) }
                    pure True
  where
    cleanup = do
        modify $ \st -> st { dragging = Nothing }
        done

-- | Move the pointer, in river's global coordinate space:
-- @river_seat_v1.pointer_warp@, which exists because a window manager is not
-- an ordinary client.
warpPointer :: Position -> Position -> X ()
warpPointer x y = do
    seats <- io . readIORef =<< asks (riverSeats . riverState)
    forM_ (M.keys seats) $ \seat -> emitOp (OpWarpPointer seat x y)

-- | Where the pointer was when the drag began.  Read from inside the motion
-- callback only; 'mouseDrag' writes it.
dragOrigin :: X (Position, Position)
dragOrigin = io . readIORef =<< asks (riverDragOrigin . riverState)

-- | Put a window where a drag has decided it goes, and keep the recorded
-- geometry in step so that 'float' can read the result back.  Size hints are
-- the caller's business.
dragWindowTo :: Window -> Rectangle -> X ()
dragWindowTo w r = do
    emitOp (OpSetPosition w (rect_x r) (rect_y r))
    emitOp (OpProposeDimensions w (rect_width r) (rect_height r))
    rs <- asks riverState
    updatePlacement rs w r

-- | Drag the window under the cursor with the mouse while it is dragged.
mouseMoveWindow :: Window -> X ()
mouseMoveWindow w = whenX (isClient w) $ do
    placements <- io . readIORef =<< asks (riverGeometry . riverState)
    -- The window's own origin is the thing the pointer's movement is added to.
    -- This used to add it to the /screen's/ origin, which meant a window
    -- jumped to wherever in the screen the pointer had travelled from the
    -- corner rather than following the pointer.
    forM_ (M.lookup w placements) $ \r ->
        mouseDrag
            (\ex ey -> do
                (ox, oy) <- dragOrigin
                dragWindowTo w r { rect_x = rect_x r + (ex - ox)
                                 , rect_y = rect_y r + (ey - oy) }
                float w)
            (float w)

-- | Resize the window under the cursor with the mouse while it is dragged.
-- The size follows the pointer's displacement from where the drag began,
-- rather than warping the pointer to the corner first as X11 did.
mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = whenX (isClient w) $ do
    placements <- io . readIORef =<< asks (riverGeometry . riverState)
    known <- io . readIORef =<< asks (riverWindows . riverState)
    forM_ ((,) <$> M.lookup w placements <*> M.lookup w known) $ \(r, rw) -> do
        started <- startMouseDrag
            (\ex ey -> do
                (ox, oy) <- dragOrigin
                let (width, height) =
                        applySizeHintsContents (rwSizeHints rw)
                            ( fromIntegral (rect_width r)  + (ex - ox)
                            , fromIntegral (rect_height r) + (ey - oy) )
                dragWindowTo w r { rect_width = width, rect_height = height }
                float w)
            (emitOp (OpInformResize w False) >> float w)
        -- Told only after claiming the drag, so every start has the release
        -- callback above as its matching end.
        when started (emitOp (OpInformResize w True))

-- | A type to help serialize xmonad's state to a file.
--
-- Windows are river @identifier@s rather than object ids, which are
-- per-connection and recycled: the identifier belongs to the window and
-- survives a window manager restart.
data StateFile = StateFile
  { sfWins :: W.StackSet WorkspaceId String String ScreenId ScreenDetail
  , sfExt  :: [(String, String)]
  } deriving (Show, Read)

-- | Given a window, build an adjuster function that will reduce the given
-- dimensions according to the window's border width and size hints.
mkAdjust :: Window -> X (D -> D)
mkAdjust w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
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

-- | Use size hints to scale a pair of dimensions.  Upstream's arithmetic,
-- unchanged: every hint is optional, and river reports only a minimum and a
-- maximum.
applySizeHints' :: SizeHints -> D -> D
applySizeHints' sh =
      maybe id applyMaxSizeHint                   (sh_max_size   sh)
    . maybe id (\(bw, bh) (w, h) -> (w+bw, h+bh)) (sh_base_size  sh)
    . maybe id applyResizeIncHint                 (sh_resize_inc sh)
    . maybe id applyAspectHint                    (sh_aspect     sh)
    . maybe id (\(bw,bh) (w,h)   -> (w-bw, h-bh)) (sh_base_size  sh)

-- | Reduce the dimensions so their aspect ratio falls between the two given aspect ratios.
applyAspectHint :: (D, D) -> D -> D
applyAspectHint ((minx, miny), (maxx, maxy)) x@(w,h)
    | or [minx < 1, miny < 1, maxx < 1, maxy < 1] = x
    | w * maxy > h * maxx                         = (h * maxx `div` maxy, h)
    | w * miny < h * minx                         = (w, w * miny `div` minx)
    | otherwise                                   = x

-- | Reduce the dimensions so they are a multiple of the size increments.
applyResizeIncHint :: D -> D -> D
applyResizeIncHint (iw,ih) x@(w,h) =
    if iw > 0 && ih > 0 then (w - w `mod` iw, h - h `mod` ih) else x

applyMaxSizeHint  :: D -> D -> D
applyMaxSizeHint (mw,mh) x@(w,h) =
    if mw > 0 && mh > 0 then (min w mw,min h mh) else x
