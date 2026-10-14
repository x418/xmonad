module Main (main) where

import Control.Monad (unless)
import Data.List (nub, sortOn)

import XMonad.Core (Layout (..), ScreenDetail (..), ScreenId, WindowSet)
import XMonad.Layout (Full (..))
import XMonad.River.Types (Rectangle (..))
import XMonad.River.WM.Runtime (screensOf)
import XMonad.River.WM.Sequence (rescreen)
import qualified XMonad.StackSet as W

main :: IO ()
main = do
  check "surplus outputs are ignored" $
    length (screensOf (rescreen threeRects (windowSet ["1"]))) == 1
  check "screens never duplicate a workspace" $
    uniqueScreenTags (rescreen threeRects (windowSet ["1", "2"]))
  check "all workspaces remain present" $
    map W.tag (W.workspaces (rescreen threeRects (windowSet ["1", "2"])))
      == ["1", "2"]
  check "removed screens return their workspaces to hidden" $
    let shrunk = rescreen (take 1 threeRects)
                   (rescreen (take 2 threeRects) (windowSet ["1", "2", "3"]))
    in map W.tag (W.hidden shrunk) == ["2", "3"]
  check "reapplying the same outputs is idempotent" $
    let once = rescreen threeRects (windowSet ["1", "2"])
    in screenState (rescreen threeRects once) == screenState once
  check "a resize keeps every workspace on its screen, and the focus" $
    let resized = rescreen (map shorter (take 2 threeRects)) focusedOnSecond
    in placement resized == placement focusedOnSecond
       && W.currentTag resized == "2"
  check "unplugging keeps the focused workspace on screen" $
    W.currentTag (rescreen (take 1 threeRects) focusedOnSecond) == "2"

check :: String -> Bool -> IO ()
check name ok = unless ok (error ("river-screen: " ++ name))

uniqueScreenTags :: WindowSet -> Bool
uniqueScreenTags ws =
  let tags = map (W.tag . W.workspace) (screensOf ws)
  in length tags == length (nub tags)

screenState :: WindowSet -> [(String, Rectangle)]
screenState = map (\screen ->
  (W.tag (W.workspace screen), screenRect (W.screenDetail screen))) . screensOf

-- | Workspace by screen id.
placement :: WindowSet -> [(ScreenId, String)]
placement = sortOn fst . map (\screen -> (W.screen screen, W.tag (W.workspace screen))) . screensOf

-- | Two screens, "1" and "2", with the second focused.
focusedOnSecond :: WindowSet
focusedOnSecond = W.view "2" (rescreen (take 2 threeRects) (windowSet ["1", "2", "3"]))

-- | What a bar's exclusive zone does to an output's usable area.
shorter :: Rectangle -> Rectangle
shorter r = r { rect_y = rect_y r + 30, rect_height = rect_height r - 30 }

windowSet :: [String] -> WindowSet
windowSet names = W.new (Layout Full) names [SD (head threeRects)]

threeRects :: [Rectangle]
threeRects =
  [ Rectangle 0 0 1920 1080
  , Rectangle 1920 0 1920 1080
  , Rectangle 3840 0 1920 1080
  ]
