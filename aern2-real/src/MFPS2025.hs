module MFPS2025 where

import AERN2.Real
import Math.NumberTheory.Logarithms (integerLog2)
import MixedTypesNumPrelude (integer)
import Prelude

sqrt_approx_FP :: Double -> Int -> Double
sqrt_approx_FP _ 0 = 1
sqrt_approx_FP x n =
  let y = sqrt_approx_FP x (n - 1)
   in (y + x / y) / 2

sqrt_approx :: CReal -> Int -> CReal
sqrt_approx _ 0 = 1
sqrt_approx x n =
  let y = sqrt_approx x (n - 1)
   in (y + x / y) / 2

sqrt_approx_fast :: CReal -> Integer -> CReal
sqrt_approx_fast x n =
  sqrt_approx x (1 + (integerLog2 (integer $ n + 1)))

restr_sqrt :: CReal -> CReal
restr_sqrt x =
  limit (\n -> sqrt_approx_fast x n)

