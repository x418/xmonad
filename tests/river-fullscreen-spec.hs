module Main (main) where

import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.Core (Layout (..), ScreenDetail (..), WindowSet)
import XMonad.Layout (Full (..))
import XMonad.River.Plan (FocusTarget (..), Placement (..), Border (..), Plan (..), emptyPlan, fullscreenChanges)
import XMonad.River.Types (Rectangle (..), RiverOutput (..), Window)
import XMonad.River.WM.Sequence (fullscreenOutputs, outputAt, rescreen)
import XMonad.River.WM.Transmit (stackOrder)
import XMonad.River.Wire (ObjectId (..))
import qualified XMonad.StackSet as W

main :: IO ()
main = do
  check "a screen below a top bar is on the output under it" $
    fmap roObject (outputAt outputs (Rectangle 1920 30 1920 1050)) == Just right
  check "a rectangle on no output has none" $
    outputAt outputs (Rectangle 5000 0 10 10) == Nothing

  check "a shown window goes to its screen's output, whatever its hint" $
    fullscreenOutputs outputs twoScreens (const (Just left)) M.empty (S.singleton onRight)
      == M.singleton onRight right
  check "a hidden window goes to its hint when the hint is live" $
    fullscreenOutputs outputs twoScreens (const (Just left)) M.empty (S.singleton hidden)
      == M.singleton hidden left
  check "a dead hint falls back to where the window was" $
    fullscreenOutputs outputs twoScreens (const (Just gone)) (M.singleton hidden left) (S.singleton hidden)
      == M.singleton hidden left
  check "with nothing live to go back to, the current screen's output" $
    fullscreenOutputs outputs twoScreens (const (Just gone)) (M.singleton hidden gone) (S.singleton hidden)
      == M.singleton hidden right
  check "a window the WindowSet does not have is dropped" $
    fullscreenOutputs outputs twoScreens (const Nothing) M.empty (S.singleton stranger) == M.empty
  check "no outputs, no fullscreen" $
    fullscreenOutputs [] twoScreens (const Nothing) M.empty (S.singleton onRight) == M.empty

  let isLive = (`elem` [left, right])
  check "a new window enters" $
    fullscreenChanges isLive M.empty (M.singleton onRight right)
      == (M.singleton onRight right, S.empty, M.singleton onRight right)
  check "an unchanged window is not told again" $
    fullscreenChanges isLive (M.singleton onRight right) (M.singleton onRight right)
      == (M.empty, S.empty, M.singleton onRight right)
  check "a window moving output is sent the new one" $
    fullscreenChanges isLive (M.singleton onRight right) (M.singleton onRight left)
      == (M.singleton onRight left, S.empty, M.singleton onRight left)
  check "a window no longer wanted leaves" $
    fullscreenChanges isLive (M.singleton onRight right) M.empty
      == (M.empty, S.singleton onRight, M.empty)
  check "a window wanted on a dead output neither enters nor leaves" $
    fullscreenChanges isLive (M.singleton onRight right) (M.singleton onRight gone)
      == (M.empty, S.empty, M.singleton onRight right)

  -- Topmost first in the plan; bottom to top in the result.
  let plan = emptyPlan
        { planSerial = 1
        , planOrder = [onRight, onLeft]
        , planPlaced = M.fromList [ (w, placed) | w <- [onRight, onLeft] ]
        , planRaised = [onRight]
        , planFocus = FocusWindow onRight
        , planFullscreen = M.singleton onRight right
        }
      nodeOf (ObjectId w) = Just (ObjectId (w + 1000))
      bar = ObjectId 77
  check "overlays sit under fullscreen windows, and above the rest" $
    stackOrder plan nodeOf [bar] == [ObjectId 1012, bar, ObjectId 1011, ObjectId 1011]
  check "without fullscreen, overlays stay on top" $
    stackOrder plan { planFullscreen = M.empty } nodeOf [bar]
      == [ObjectId 1012, ObjectId 1011, ObjectId 1011, bar]

check :: String -> Bool -> IO ()
check name ok = unless ok (error ("river-fullscreen: " ++ name))

left, right, gone :: ObjectId
left = ObjectId 1
right = ObjectId 2
gone = ObjectId 3

onLeft, onRight, hidden, stranger :: Window
onLeft = ObjectId 12
onRight = ObjectId 11
hidden = ObjectId 13
stranger = ObjectId 14

outputs :: [RiverOutput]
outputs = [output left 0, output right 1920]
  where
    output o x = RiverOutput
      { roObject = o, roPosition = (x, 0), roSize = (1920, 1080), roRemoved = False
      , roLayerObject = Nothing, roLayerArea = Nothing, roWlOutput = Nothing
      , roName = Nothing, roCaptureSessions = 0 }

-- | "1" on the left, "2" on the right with the focus, "3" off screen; the
-- screens' rectangles start below a 30-pixel bar.
twoScreens :: WindowSet
twoScreens = W.shiftWin "3" hidden (W.insertUp hidden withScreens)
  where
    base = W.new (Layout Full) ["1", "2", "3"] [SD (Rectangle 0 30 1920 1050)]
    withScreens = W.insertUp onRight (W.view "2" (W.insertUp onLeft
      (rescreen [Rectangle 0 30 1920 1050, Rectangle 1920 30 1920 1050] base)))

placed :: Placement
placed = Placement (Rectangle 0 0 10 10) True False (Border 0 (0, 0, 0, 0))
