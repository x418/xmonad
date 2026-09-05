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
import Control.Exception (finally)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets, modify, put)
import Data.IORef
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid (appEndo)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import XMonad.Core
import XMonad.Operations (floatLocation, getCleanedScreenInfo, isFixedSizeOrTransient, readStateFile, scaleRationalRect)
import XMonad.River.Ops (emitOp)
import XMonad.River.Plan
import XMonad.River.Protocol.WindowManagement (riverWindowV1CapabilitiesFullscreen)
import XMonad.River.State
import XMonad.River.Types
import XMonad.River.WM.Runtime (broadcastEvent, screensOf)
import XMonad.River.Wire (ObjectId)
import qualified XMonad.StackSet as W

-- | The worker cannot name the loop's state: this takes the sequence number
-- and the actions, and everything else comes through 'riverState'.
manageSequence :: Int -> [X ()] -> X ()
manageSequence n acts = do
  c <- ask
  st <- get
  rs <- asks riverState
  let inSeq = inManageSeq rs
      -- Whatever becomes of the body, the flag comes down and the loop is
      -- told the sequence is over.  An asynchronous exception -- the worker
      -- being killed -- would otherwise leave every later 'windows' believing
      -- it is inside a sequence, and never asking for one.
      finished = do
        writeIORef inSeq False
        atomically (modifyTVar' (shSeqDone (rsShared rs)) (max n))
  io (writeIORef inSeq True)
  (_, st') <- io (runX c st (body `catchX` pure ()) `finally` finished)
  put st'
  where
    body = do
      rs <- asks riverState
      before <- gets windowset
      restoreState
      reapClosed
      syncScreens
      layerDefault <- nominateLayerOutput
      adoptNewWindows
      settleFloats
      mapM_ userCode acts
      -- Before every output's usable area is known the layout would cover
      -- the bar and be undone a sequence later, resizing every window twice;
      -- river follows the area with a sequence of its own.  The plan is
      -- written once either way.
      ready <- areasKnown
      if ready
        then applyLayout layerDefault
        else io $ atomically $ modifyTVar' (shPlan (rsShared rs)) $ \p ->
               p { planLayerDefault = layerDefault }
      -- The log hook, once: for every 'windows' the actions ran, which X11
      -- ran it after each of, and for a set that changed on its own -- a
      -- window opening or closing, a restore, a rescreen.
      after <- gets windowset
      let logDue = riverLogDue rs
      due <- io (readIORef logDue)
      io (writeIORef logDue False)
      when (due || not (sameWindows before after)) $
        asks (logHook . config) >>= userCodeDef ()

-- | Every live output has reported its @non_exclusive_area@, or there is no
-- layer shell to report one.
areasKnown :: X Bool
areasKnown = asks (rsShared . riverState) >>= \sh -> case shLayerShell sh of
  Nothing -> pure True
  Just _ -> do
    outs <- io (readIORef (shOutputs sh))
    pure $ and [ roLayerArea o /= Nothing
               | o <- M.elems outs, not (roRemoved o)
               , let (w, h) = roSize o, w > 0 && h > 0 ]

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
restoreState :: X ()
restoreState = do
  restored <- asks (wsRestored . rsWorker . riverState)
  done <- io (readIORef restored)
  unless done $ do
    io (writeIORef restored True)
    path <- asks (stateFileName . directories)
    exists <- io (doesFileExist path)
    -- readStateFile parses once and notes how many windows resolved.
    when exists $ do
      xmc <- asks config
      mst <- readStateFile xmc
      whenJust mst $ \st ->
        modify $ \s -> s { windowset = windowset st
                         , extensibleState = extensibleState st }

-- | Drop what river has closed from the 'WindowSet', and tell the config.
-- The objects are destroyed by 'reapObjects' on the loop, afterwards.
reapClosed :: X ()
reapClosed = do
  rs <- asks riverState
  ws <- io (readIORef (riverWindows rs))
  forM_ [ w | w <- M.elems ws, rwClosed w ] $ \w -> do
    modify $ \st -> st { windowset = W.delete (rwObject w) (windowset st) }
    io $ modifyIORef' (wsAdopted (rsWorker rs)) (S.delete (rwObject w))
    io $ modifyIORef' (riverUnsized rs) (S.delete (rwObject w))
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

-- | Nominate the output layer surfaces that name none go to: the one under
-- the current screen, so a launcher opens where the work is.  Matched by
-- position, the only link between a 'W.Screen' and its output: the output
-- whose area contains the screen's origin.  Containment rather than
-- equality, because the screen rectangle is the layer shell's usable area
-- when there is one, and a bar on the left or top edge moves its origin off
-- the output's.  Returns the choice; 'applyLayout' publishes it with the
-- rest of the plan.
nominateLayerOutput :: X (Maybe (ObjectId, Maybe ObjectId))
nominateLayerOutput = asks (rsShared . riverState) >>= \sh -> case shLayerShell sh of
 Nothing -> pure Nothing
 Just _ -> do
  outs <- io (readIORef (shOutputs sh))
  ws <- gets windowset
  let SD current = W.screenDetail (W.current ws)
      live = filter (not . roRemoved) (M.elems outs)
      onScreen o = let (x, y) = roPosition o
                       (w, h) = roSize o
                       cx = rect_x current
                       cy = rect_y current
                   in x <= cx && cx < x + fromIntegral w
                   && y <= cy && cy < y + fromIntegral h
      chosen = case filter onScreen live of
        (o:_) -> Just o
        []    -> case sortOn roPosition live of
          (o:_) -> Just o
          []    -> Nothing
  pure ((\o -> (roObject o, roLayerObject o)) <$> chosen)

-- | Reconcile the 'WindowSet'\'s screens with river's outputs: xmonad's
-- @rescreen@, driven by the output list.  Outputs are ordered by position so
-- that screen ids are stable across reconnects, which
-- "XMonad.Actions.PhysicalScreens" relies on.
syncScreens :: X ()
syncScreens = do
  screens <- withDisplay getCleanedScreenInfo
  unless (null screens) $ do
    -- Compared in screen-id order: current-first order changes on every view.
    before <- gets (map (screenRect . W.screenDetail)
                    . sortOn W.screen . screensOf . windowset)
    -- Only when the outputs changed.  'rescreen' re-seats the current
    -- workspace as screen 0; run on every sequence it pinned focus, and
    -- everything that follows W.current, to the leftmost output.
    ws <- gets windowset
    let updated = rescreen screens ws
        applied = map (screenRect . W.screenDetail)
                . sortOn W.screen . screensOf $ updated
    when (before /= applied) $ do
      modify $ \st -> st { windowset = updated }
      void (broadcastEvent ScreenLayoutChanged)

-- | Lay the given screen rectangles over the current workspaces, keeping
-- each workspace on its screen where possible.  A screen with no workspace of
-- its own takes a distinct one from the hidden pool.
rescreen :: [Rectangle] -> WindowSet -> WindowSet
rescreen rects ws = ws
    { W.current = (W.current ws) { W.screen = 0, W.screenDetail = SD firstRect }
    , W.visible = zipWith3 reseat [1 :: Int ..] restRects available
    , W.hidden = newHidden
    }
  where
    (firstRect, restRects) = case rects of
      (r:rs) -> (r, rs)
      []     -> (Rectangle 0 0 0 0, [])
    oldVisible = W.visible ws
    requested = length restRects
    keptVisible = take requested oldVisible
    surplus = drop requested oldVisible
    pool = map W.workspace surplus ++ W.hidden ws
    extras = take (requested - length keptVisible) pool
    available = map Left keptVisible ++ map Right extras
    reseat i r = \case
      Left s  -> s { W.screen = fromIntegral i, W.screenDetail = SD r }
      Right h -> W.Screen h (fromIntegral i) (SD r)
    newHidden = drop (length extras) pool

-- | Run the manage hook for windows river has told us about since the last
-- sequence, and insert them.  Before the window has been rendered, which is
-- the ordering guarantee xmonad's manage hook has always had.
adoptNewWindows :: X ()
adoptNewWindows = do
  rs <- asks riverState
  let adoptedRef = wsAdopted (rsWorker rs)
  ws <- io (readIORef (riverWindows rs))
  adopted <- io (readIORef adoptedRef)
  let fresh = [ w | w <- M.elems ws, not (rwClosed w)
                  , not (S.member (rwObject w) adopted) ]
  -- Once, not per window: a restart brings every window at once.
  managed <- gets (S.fromList . W.allWindows . windowset)
  forM_ fresh $ \w -> do
    io $ modifyIORef' adoptedRef (S.insert (rwObject w))
    -- river's default is CSD.  Asked before the manage hook, so a hook can
    -- override it; a CSD-only client ignores it, which river documents.
    emitOp (OpUseDecorations (rwObject w) True)
    -- What the client may ask for and expect an answer to.  Maximize,
    -- minimize and a window menu mean nothing to a tiling layout; river's
    -- default would tell the client all four are supported.
    emitOp (OpSetCapabilities (rwObject w) riverWindowV1CapabilitiesFullscreen)
    -- A window restored from the state file is already managed -- by the
    -- process that wrote the file -- and gets the setup above only.
    unless (S.member (rwObject w) managed) $ do
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
          final = appEndo g placed
          -- A float still at the fallback's size, before river has sized
          -- it, has a rectangle nobody chose: doFloat and doCenterFloat only
          -- move that rectangle, doRectFloat picks its own.
          W.RationalRect _ _ fw fh = rr
          unsized = rwDimensions w == (0, 0) &&
            case M.lookup (rwObject w) (W.floating final) of
              Just (W.RationalRect _ _ fw' fh') -> (fw', fh') == (fw, fh)
              Nothing -> False
      when unsized $ io $ modifyIORef' (riverUnsized rs) (S.insert (rwObject w))
      modify $ \st -> st { windowset = final }
      void (broadcastEvent (WindowAdded (rwObject w)))

-- | Give a float adopted before river had sized it the size the client chose:
-- centred on its screen at the dimensions river has now reported.  Once per
-- window, in the sequence its first @dimensions@ event asks for
-- ("XMonad.River.WM.Events"); a window sunk meanwhile is left alone.  Plain
-- 'modify', not 'windows': floats are not the log hook's business.
settleFloats :: X ()
settleFloats = do
  rs <- asks riverState
  let sizesRef = wsFloatSizes (rsWorker rs)
  pending <- io (readIORef (riverUnsized rs))
  known <- io (readIORef (riverWindows rs))
  forM_ (S.toList pending) $ \w -> forM_ (M.lookup w known) $ \rw ->
    case rwDimensions rw of
      (0, 0) -> pure ()
      (dw, dh) -> do
        io (modifyIORef' (riverUnsized rs) (S.delete w))
        ws <- gets windowset
        when (M.member w (W.floating ws)) $ do
          scr <- screenOf w
          let rr = centredRect (screenRect (W.screenDetail scr)) (toInteger dw) (toInteger dh)
          modify $ \st -> st { windowset = W.float w rr (windowset st) }
  -- A float that sized itself keeps the size, as X11's granted
  -- ConfigureRequest let it.  Only a size that changed since last seen: one
  -- this side proposed is the placement's already, and the client's report
  -- of it is not a change of mind.
  geometry <- io (readIORef (riverGeometry rs))
  seen <- io (readIORef sizesRef)
  floats <- gets (M.keys . W.floating . windowset)
  let sized = [ (w, rwDimensions rw) | w <- floats, Just rw <- [M.lookup w known]
              , rwDimensions rw /= (0, 0), not (S.member w pending) ]
  forM_ sized $ \(w, (dw, dh)) ->
    when (M.lookup w seen /= Just (dw, dh)) $
      forM_ (M.lookup w geometry) $ \r ->
        when ((rect_width r, rect_height r) /= (fromIntegral dw, fromIntegral dh)) $ do
          scr <- screenOf w
          let r' = r { rect_width = fromIntegral dw, rect_height = fromIntegral dh }
          modify $ \st -> st { windowset = W.float w (relativeRect (screenRect (W.screenDetail scr)) r') (windowset st) }
  io (writeIORef sizesRef (M.fromList sized))
  where
    screenOf :: Window -> X (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
    screenOf w = do
      ws <- gets windowset
      pure $ fromMaybe (W.current ws) $
        find ((== W.findTag w ws) . Just . W.tag . W.workspace) (screensOf ws)
    -- 'XMonad.Operations.relativeRect', which is not exported.
    relativeRect (Rectangle sx sy sw sh) (Rectangle x y w h) = W.RationalRect
      (fromIntegral (x - sx) / fromIntegral (max 1 sw)) (fromIntegral (y - sy) / fromIntegral (max 1 sh))
      (fromIntegral w / fromIntegral (max 1 sw)) (fromIntegral h / fromIntegral (max 1 sh))

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
applyLayout :: Maybe (ObjectId, Maybe ObjectId) -> X ()
applyLayout layerDefault = do
  rs <- asks riverState
  ws <- gets windowset
  perScreen <- forM (screensOf ws) $ \scr -> do
    let wsp = W.workspace scr
        SD rect = W.screenDetail scr
        floats = W.floating ws
        onWs = W.integrate' (W.stack wsp)
        tiled = W.stack wsp >>= W.filter (`M.notMember` floats)
        flt = [ (fw, scaleRationalRect rect rr)
              | fw <- onWs, Just rr <- [M.lookup fw floats] ]
    (rects, mLayout) <- userCodeDef ([], Nothing) (runLayout wsp { W.stack = tiled } rect)
    forM_ mLayout $ \l' -> modify $ \st ->
      st { windowset = updateLayout (W.tag wsp) l' (windowset st) }
    pure (flt ++ rects, map fst flt)

  let placements = concatMap fst perScreen
      floating = S.fromList (concatMap snd perScreen)
      placedMap = M.fromList placements
      placed = M.keysSet placedMap
      mFocus = W.peek ws

  -- Borders are decided here.  The focused window takes the focused colour
  -- whatever an override says: X11 repainted it on every 'windows', and a
  -- colour WindowNavigation had set would otherwise hide the focus forever.
  bw <- asks (borderWidth . config)
  focusedCol <- asks focusedBorder
  normalCol <- asks normalBorder
  overrides <- io (readIORef (riverBorders rs))
  let borders = M.fromList
        [ (win, (fromMaybe bw mWidth, rgba))
        | (win, _) <- placements
        , let (mWidth, mColor) = M.findWithDefault (Nothing, Nothing) win overrides
        , let rgba = case mColor of
                Just c | Just win /= mFocus -> c
                _ -> pixelColor (if Just win == mFocus then focusedCol else normalCol)
        ]

  restackRef <- asks (riverRestack . riverState)
  raised <- io (readIORef restackRef)
  let stillUp = filter (`S.member` placed) raised
  io (atomicWriteIORef restackRef stillUp)

  -- X11 kept 'mapped' from map and unmap events; here it is what this pass
  -- placed, which is what contrib (EasyMotion, for one) asks.
  modify $ \st -> st { mapped = placed }

  placeRef <- asks (riverPlacements . riverState)
  old <- io (readIORef placeRef)
  io (atomicWriteIORef (shLayoutMoved (rsShared rs)) (old /= placements))
  io (atomicWriteIORef placeRef placements)
  -- What 'getWindowAttributes' answers from; the attributes themselves are
  -- built when asked, in "XMonad.Core".  Forced: a map left as a thunk
  -- keeps the previous pass alive until somebody reads it.
  io (atomicWriteIORef (riverGeometry rs) $! placedMap)

  unsized <- io (readIORef (riverUnsized rs))
  io $ atomically $ modifyTVar' (shPlan (rsShared rs)) $ \p -> p
    { planSerial     = planSerial p + 1
    , planLayerDefault = layerDefault
    , planPlacements = placements
    , planFloating   = floating
    , planUnsized    = S.filter (`S.member` floating) unsized
    , planBorders    = borders
    , planVisible    = placed
    , planRaised     = stillUp
    , planFocus      = maybe ClearFocus FocusWindow mFocus
    }

  -- Last, so 'XMonad.River.afterLayout' sees everything above.  Drained once:
  -- an action these queue waits for the next layout.
  ref <- asks (riverAfterLayout . riverState)
  queued <- io (atomicModifyIORef' ref (\as -> ([], reverse as)))
  mapM_ userCode queued

updateLayout :: WorkspaceId -> Layout Window -> WindowSet -> WindowSet
updateLayout i l = W.mapWorkspace $ \wsp ->
  if W.tag wsp == i then wsp { W.layout = l } else wsp
