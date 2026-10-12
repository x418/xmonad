module Main (main) where

import Control.Monad (unless)

import XMonad.River (edgesDragged, noSizeHints)
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Types (Rectangle (..), SizeHints (..))

main :: IO ()
main = do
  check "no edges is the bottom-right corner" $
    edgesDragged riverWindowV1EdgesNone noSizeHints r 10 20 == Rectangle 100 100 210 120
  check "the left edge moves, the right stays" $
    edgesDragged riverWindowV1EdgesLeft noSizeHints r 10 20 == Rectangle 110 100 190 100
  check "top-left grows up and left" $
    edgesDragged (riverWindowV1EdgesTop + riverWindowV1EdgesLeft) noSizeHints r (-10) (-20)
      == Rectangle 90 80 210 120
  check "a maximum size holds the opposite edge" $
    edgesDragged riverWindowV1EdgesLeft maxHints r (-100) 0 == Rectangle 50 100 250 100
  check "never below one pixel" $
    edgesDragged riverWindowV1EdgesRight noSizeHints r (-500) 0 == Rectangle 100 100 1 100
  where
    r = Rectangle 100 100 200 100
    maxHints = noSizeHints { sh_max_size = Just (250, 400) }

check :: String -> Bool -> IO ()
check name ok = unless ok (error ("river-drag: " ++ name))
