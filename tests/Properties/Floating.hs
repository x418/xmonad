{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Floating where

import Test.QuickCheck
import Instances

import XMonad.StackSet hiding (filter)
import XMonad.River.Types (Rectangle(..), centredRect)

import qualified Data.Map as M
import Data.Ratio ((%))

------------------------------------------------------------------------
-- properties for the floating layer:

prop_float_reversible (nex :: NonEmptyWindowsStackSet) = do
  let NonEmptyWindowsStackSet x = nex
  w <- arbitraryWindow nex
  return $ sink w (float w geom x) == x
        where
            geom = RationalRect 100 100 100 100

prop_float_geometry (nex :: NonEmptyWindowsStackSet) = do
    let NonEmptyWindowsStackSet x = nex
    w <- arbitraryWindow nex
    let s = float w geom x
    return $ M.lookup w (floating s) == Just geom
  where
    geom = RationalRect 100 100 100 100

prop_float_delete (nex :: NonEmptyWindowsStackSet) = do
    let NonEmptyWindowsStackSet x = nex
    w <- arbitraryWindow nex
    let s = float w geom x
        t = delete w s
    return $ not (w `member` t)
  where
    geom = RationalRect 100 100 100 100

-- | 'centredRect': the size asked for, as a fraction of the screen; centred
-- when it fits; the origin never negative, the size kept, when it does not.
prop_centred_rect :: Rectangle -> SizedPositive -> SizedPositive -> Bool
prop_centred_rect sr (SizedPositive w) (SizedPositive h) =
    x >= 0 && y >= 0
    && rw == toInteger w % sw && rh == toInteger h % sh
    && (toInteger w > sw || x == (1 - rw) / 2)
    && (toInteger h > sh || y == (1 - rh) / 2)
    && (toInteger w <= sw || x == 0)
    && (toInteger h <= sh || y == 0)
  where
    RationalRect x y rw rh = centredRect sr (toInteger w) (toInteger h)
    sw = max 1 (toInteger (rect_width sr))
    sh = max 1 (toInteger (rect_height sr))
