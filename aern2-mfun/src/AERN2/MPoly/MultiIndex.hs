module AERN2.MPoly.MultiIndex where

import MixedTypesNumPrelude

import AERN2.Util.Vector (Vector, (!), vlength)
import qualified AERN2.Util.Vector as V

import Debug.Trace

type MultiIndex = Vector Integer

size :: MultiIndex -> Integer
size = V.foldl' (+) 0

zeroIndex :: Integer -> MultiIndex
zeroIndex n = V.generate (int n) (const 0)
