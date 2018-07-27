module AERN2.Util.Vector
(
    module Data.Vector
  , (!)
  , (!.)
  , vlength
)
where

import MixedTypesNumPrelude
import Data.Vector hiding ((!))
import qualified Data.Vector as V

(!.) :: Vector a -> Integer -> a
(!.) v n = (V.!) v (int n)

(!) :: Vector a -> Int -> a
(!) = (V.!)

vlength :: Vector a -> Integer
vlength v = integer $ V.length v
