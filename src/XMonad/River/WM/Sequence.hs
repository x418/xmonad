{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The manage sequence, as the worker runs it.
--
-- In order: restore a predecessor's state, drop what river closed, reconcile
-- screens with outputs, adopt new windows through the manage hook, run the
-- queued actions, lay out, publish the 'Plan'.  All of it is 'X' code and
-- none of it touches the connection.
module XMonad.River.WM.Sequence
  ( manageSequence
  , runStartupHook
  , rescreen
  , sameWindows
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, handle)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.IORef
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid (appEndo)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import XMonad.Core
import XMonad.Operations (StateFile (..), floatLocation, isFixedSizeOrTransient, readStateFile, scaleRationalRect)
import XMonad.River.Ops (emitOp)
import XMonad.River.Plan
import XMonad.River.Protocol.WindowManagement (riverWindowV1CapabilitiesFullscreen)
import XMonad.River.State (RiverState(..), borderOverride, forgetBorderOverride)
import XMonad.River.Types
import XMonad.River.WM.Runtime
import qualified XMonad.StackSet as W

manageSequence :: Runtime -> Int -> [X ()] -> X ()
manageSequence rt n acts = do
  let inSeq = inManageSeq (rtState rt)
  io (writeIORef inSeq True)
  body `catchX` pure ()
  io (writeIORef inSeq False)
  io (atomically (modifyTVar' (shSeqDone (rtShared rt)) (max n)))
  where
    body = do
      before <- gets windowset
      restoreState rt
      reapClosed rt
      syncScreens
      nominateLayerOutput rt
      adoptNewWindows rt
      mapM_ userCode acts
      applyLayout rt
      -- 'windows' runs the log hook; a window opening or closing on its own,
      -- a restore and a rescreen do not go through it.
      after <- gets windowset
      unless (sameWindows before after) $
        asks (logHook . config) >>= userCodeDef ()

-- | Whether two 'WindowSet's would produce the same status line.  Not 'Eq':
-- that would compare layouts, which may rewrite their state on every pass.
sameWindows :: WindowSet -> WindowSet -> Bool
sameWindows a b =
     map wsOf (W.workspaces a) == map wsOf (W.workspaces b)
  && W.currentTag a == W.currentTag b
  && map (W.tag . W.workspace) (W.visible a) == map (W.tag . W.workspace) (W.visible b)
  where
    wsOf w = (W.tag w, W.integrate' (W.stack w), fmap W.focus (W.stack w))

-- | Pick up where the previous window manager left off, if it left a state
-- file.  First in the first sequence and never again: river sends every
-- existing window, identifier included, before the first @manage_start@, so
-- that is the earliest the file's identifiers can be resolved.
restoreState :: Runtime -> X ()
restoreState rt = do
  done <- io (readIORef (rtRestored rt))
  unless done $ do
    io (writeIORef (rtRestored rt) True)
    path <- asks (stateFileName . directories)
    exists <- io (doesFileExist path)
    when exists $ do
      xmc <- asks config
      wanted <- io (windowsInStateFile path)
      mst <- readStateFile xmc
      whenJust mst $ \st -> do
        modify $ \s -> s { windowset = windowset st
                         , extensibleState = extensibleState st }
        let restored = W.allWindows (windowset st)
        -- The two numbers differing means the identifiers did not match what
        -- river now reports; the symptom is windows back on the wrong
        -- workspace, otherwise indistinguishable from a missing file.
        io $ hPutStrLn stderr $ "xmonad-river: note: restored "
          <> show (length restored) <> " of " <> show wanted
          <> " windows from " <> path

-- | How many windows the state file claims, from the raw text.
windowsInStateFile :: FilePath -> IO Int
windowsInStateFile path = handle (\(_ :: SomeException) -> pure 0) $ do
  raw <- readFile path
  case reads raw of
    [(sf, _)] -> pure (length (W.allWindows (sfWins sf)))
    _         -> pure 0

-- | Drop what river has closed from the 'WindowSet', and tell the config.
-- The objects are destroyed by 'reapObjects' on the loop, afterwards.
reapClosed :: Runtime -> X ()
reapClosed rt = do
  ws <- io (readIORef (riverWindows rs))
  forM_ [ w | w <- M.elems ws, rwClosed w ] $ \w -> do
    modify $ \st -> st { windowset = W.delete (rwObject w) (windowset st) }
    io $ modifyIORef' (rtAdopted rt) (S.delete (rwObject w))
    -- river recycles ids; an override left behind would land on an
    -- unrelated window.
    io $ forgetBorderOverride (riverBorders rs) (rwObject w)

  outs <- io (readIORef (riverOutputs rs))
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o ->
    void (broadcastEvent (OutputRemoved (roObject o)))

  seats <- io (readIORef (riverSeats rs))
  let gone = [ s | s <- M.elems seats, rsRemoved s ]
  forM_ gone $ \s -> void (broadcastEvent (SeatRemoved (rsObject s)))
  -- A drag's @op_release@ never arrives once its seat is gone; end it here
  -- or every later drag is ignored.
  unless (null gone) $ gets dragging >>= mapM_ snd
  where rs = rtState rt

-- | Nominate the output layer surfaces that name none go to: the one under
-- the current screen, so a launcher opens where the work is.  Matched by
-- position, the only link between a 'W.Screen' and its output.
nominateLayerOutput :: Runtime -> X ()
nominateLayerOutput rt = forM_ (rtLayerShell rt) $ \_ -> do
  outs <- io (readIORef (riverOutputs (rtState rt)))
  ws <- gets windowset
  let SD current = W.screenDetail (W.current ws)
      live = filter (not . roRemoved) (M.elems outs)
      onScreen o = let (x, y) = roPosition o
                   in x == rect_x current && y == rect_y current
      chosen = case filter onScreen live of
        (o:_) -> Just o
        []    -> case sortOn roPosition live of
          (o:_) -> Just o
          []    -> Nothing
  io $ atomically $ modifyTVar' (shPlan (rtShared rt)) $ \p ->
    p { planLayerDefault = (\o -> (roObject o, roLayerObject o)) <$> chosen }

-- | Reconcile the 'WindowSet'\'s screens with river's outputs: xmonad's
-- @rescreen@, driven by the output list.  Outputs are ordered by position so
-- that screen ids are stable across reconnects, which
-- "XMonad.Actions.PhysicalScreens" relies on.
syncScreens :: X ()
syncScreens = do
  outs <- io . readIORef =<< asks (riverOutputs . riverState)
  let rects =
        [ rect
        | o <- sortOn roPosition (filter (not . roRemoved) (M.elems outs))
        , let (x, y) = roPosition o
        , let (width, height) = roSize o
        , width > 0 && height > 0
          -- The area layer shell reports, so a bar's exclusive zone shrinks
          -- the tiling area rather than being tiled over.
        , let rect = case roLayerArea o of
                Just a | rect_width a > 0 && rect_height a > 0 -> a
                _ -> Rectangle x y (fromIntegral width) (fromIntegral height)
        ]
  unless (null rects) $ do
    -- Compared in screen-id order: current-first order changes on every view.
    before <- gets (map (screenRect . W.screenDetail)
                    . sortOn W.screen . screensOf . windowset)
    -- Only when the outputs changed.  'rescreen' re-seats the current
    -- workspace as screen 0; run on every sequence it pinned focus, and
    -- everything that follows W.current, to the leftmost output.
    when (before /= rects) $ do
      modify $ \st -> st { windowset = rescreen rects (windowset st) }
      void (broadcastEvent ScreenLayoutChanged)

-- | Lay the given screen rectangles over the current workspaces, keeping
-- each workspace on its screen where possible.  A screen with no workspace of
-- its own takes a distinct one from the hidden pool.
rescreen :: [Rectangle] -> WindowSet -> WindowSet
rescreen rects ws = ws
    { W.current = (W.current ws) { W.screen = 0, W.screenDetail = SD firstRect }
    , W.visible = zipWith reseat [1 ..] restRects
    , W.hidden = newHidden
    }
  where
    (firstRect, restRects) = case rects of
      (r:rs) -> (r, rs)
      []     -> (Rectangle 0 0 0 0, [])
    oldVisible = W.visible ws
    extras = drop (length oldVisible) restRects
    reseat i r = case drop (i - 1) oldVisible of
      (s:_) -> s { W.screen = fromIntegral i, W.screenDetail = SD r }
      [] -> case drop (i - 1 - length oldVisible) pool of
        (h:_) -> W.Screen h (fromIntegral i) (SD r)
        []    -> W.Screen (W.workspace (W.current ws)) (fromIntegral i) (SD r)
    surplus = drop (length restRects) oldVisible
    pool = map W.workspace surplus ++ W.hidden ws
    newHidden = drop (length extras) pool

-- | Run the manage hook for windows river has told us about since the last
-- sequence, and insert them.  Before the window has been rendered, which is
-- the ordering guarantee xmonad's manage hook has always had.
adoptNewWindows :: Runtime -> X ()
adoptNewWindows rt = do
  ws <- io (readIORef (riverWindows (rtState rt)))
  adopted <- io (readIORef (rtAdopted rt))
  let fresh = [ w | w <- M.elems ws, not (rwClosed w)
                  , not (S.member (rwObject w) adopted) ]
  forM_ fresh $ \w -> do
    io $ modifyIORef' (rtAdopted rt) (S.insert (rwObject w))
    -- river's default is CSD.  Asked before the manage hook, so a hook can
    -- override it; a CSD-only client ignores it, which river documents.
    emitOp (OpUseDecorations (rwObject w) True)
    -- What the client may ask for and expect an answer to.  Maximize,
    -- minimize and a window menu mean nothing to a tiling layout; river's
    -- default would tell the client all four are supported.
    emitOp (OpSetCapabilities (rwObject w) riverWindowV1CapabilitiesFullscreen)
    -- A window restored from the state file is already managed -- by the
    -- process that wrote the file -- and gets the setup above only.
    managed <- gets (W.allWindows . windowset)
    unless (rwObject w `elem` managed) $ do
      mh <- asks (manageHook . config)
      g <- userCodeDef mempty (runQuery mh (rwObject w))
      -- As upstream's 'manage': a fixed-size or transient window floats.
      shouldFloat <- withDisplay $ \d -> isFixedSizeOrTransient d (rwObject w)
      rr <- snd <$> floatLocation (rwObject w)
      ws' <- gets windowset
      let clamp (W.RationalRect x y wid h)
            | x + wid > 1 || y + h > 1 || x < 0 || y < 0 =
                W.RationalRect (0.5 - wid / 2) (0.5 - h / 2) wid h
          clamp r = r
          inserted = W.insertUp (rwObject w) ws'
          placed
            | shouldFloat = W.float (rwObject w) (clamp rr) inserted
            | otherwise = inserted
      modify $ \st -> st { windowset = appEndo g placed }
      void (broadcastEvent (WindowAdded (rwObject w)))

-- | The user's startup hook, once, after the first manage sequence has been
-- answered.  @XMONAD_RIVER_NO_STARTUP_HOOK@ skips it, so a real config can run
-- under a test compositor without reaching into the desktop it belongs to.
runStartupHook :: X ()
runStartupHook = do
  skip <- io (lookupEnv "XMONAD_RIVER_NO_STARTUP_HOOK")
  case skip of
    Just _ -> io $ hPutStrLn stderr
      "xmonad-river: note: startup hook skipped \
      \(XMONAD_RIVER_NO_STARTUP_HOOK is set)"
    Nothing -> asks (startupHook . config) >>= void . userCode

-- | Run the layout for every visible screen and publish the result as a
-- 'Plan'.  Floats are withheld from the layout and placed from the rectangles
-- 'XMonad.Operations.float' recorded, first, which is upstream's order.
applyLayout :: Runtime -> X ()
applyLayout rt = do
  ws <- gets windowset
  perScreen <- forM (screensOf ws) $ \scr -> do
    let wsp = W.workspace scr
        SD rect = W.screenDetail scr
        floats = W.floating ws
        onWs = W.integrate' (W.stack wsp)
        tiled = W.stack wsp >>= W.filter (`M.notMember` floats)
        flt = [ (fw, scaleRationalRect rect rr)
              | fw <- onWs, Just rr <- [M.lookup fw floats] ]
    (rs, mLayout) <- userCodeDef ([], Nothing) (runLayout wsp { W.stack = tiled } rect)
    forM_ mLayout $ \l' -> modify $ \st ->
      st { windowset = updateLayout (W.tag wsp) l' (windowset st) }
    pure (flt ++ rs, map fst flt)

  let placements = concatMap fst perScreen
      floating = S.fromList (concatMap snd perScreen)
      placed = S.fromList (map fst placements)
      mFocus = W.peek ws

  -- Borders are decided here.  The focused window takes the focused colour
  -- whatever an override says: X11 repainted it on every 'windows', and a
  -- colour WindowNavigation had set would otherwise hide the focus forever.
  bw <- asks (borderWidth . config)
  focusedCol <- asks focusedBorder
  normalCol <- asks normalBorder
  borders <- fmap M.fromList $ forM placements $ \(win, _) -> do
    (mWidth, mColor) <- io (borderOverride (riverBorders (rtState rt)) win)
    let rgba = case mColor of
          Just c | Just win /= mFocus -> c
          _ -> pixelColor (if Just win == mFocus then focusedCol else normalCol)
    pure (win, (fromMaybe bw mWidth, rgba))

  restackRef <- asks (riverRestack . riverState)
  raised <- io (readIORef restackRef)
  let stillUp = filter (`S.member` placed) raised
  io (atomicWriteIORef restackRef stillUp)

  -- X11 kept 'mapped' from map and unmap events; here it is what this pass
  -- placed, which is what contrib (EasyMotion, for one) asks.
  modify $ \st -> st { mapped = placed }

  placeRef <- asks (riverPlacements . riverState)
  old <- io (readIORef placeRef)
  io (atomicWriteIORef (rtLayoutMoved rt) (old /= placements))
  io (writeIORef placeRef placements)

  io $ atomically $ modifyTVar' (shPlan (rtShared rt)) $ \p -> p
    { planSerial     = planSerial p + 1
    , planPlacements = placements
    , planFloating   = floating
    , planBorders    = borders
    , planVisible    = placed
    , planRaised     = stillUp
    , planFocus      = maybe ClearFocus FocusWindow mFocus
    }

  -- What 'getWindowAttributes' and 'getWMNormalHints' answer with.  Every
  -- known window, as X11 did: one off screen is unmapped at the origin.
  allKnown <- io (readIORef (riverWindows (rtState rt)))
  let placedMap = M.fromList placements
      attrs w rw = case M.lookup w placedMap of
        Just r -> WindowAttributes
          { wa_x = rect_x r, wa_y = rect_y r
          , wa_width = rect_width r, wa_height = rect_height r
          , wa_border_width = bw, wa_map_state = waIsViewable
          , wa_override_redirect = False }
        Nothing -> let (dw, dh) = rwDimensions rw in WindowAttributes
          { wa_x = 0, wa_y = 0
          , wa_width = fromIntegral dw, wa_height = fromIntegral dh
          , wa_border_width = bw, wa_map_state = waIsUnmapped
          , wa_override_redirect = False }
  -- Lazily: only a window somebody asks about has its attributes built.
  io $ writeIORef (riverGeometry (rtState rt)) (ML.mapWithKey attrs allKnown)
  io $ writeIORef (riverSizeHints (rtState rt)) (ML.map rwSizeHints allKnown)

  -- Last, so 'XMonad.River.afterLayout' sees everything above.  Drained once:
  -- an action these queue waits for the next layout.
  ref <- asks (riverAfterLayout . riverState)
  queued <- io (atomicModifyIORef' ref (\as -> ([], reverse as)))
  mapM_ userCode queued

updateLayout :: WorkspaceId -> Layout Window -> WindowSet -> WindowSet
updateLayout i l = W.mapWorkspace $ \wsp ->
  if W.tag wsp == i then wsp { W.layout = l } else wsp
