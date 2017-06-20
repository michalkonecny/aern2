module AERN2.Analytic
(
  module AERN2.Analytic.Type
, anaSin
, anaGeom
)
where

import MixedTypesNumPrelude
import AERN2.Real
import Data.List

import AERN2.Analytic.Type

anaSin :: Analytic
anaSin =
  Analytic 3 1 an
  where
  iFact n = foldl' (/) (real 1) (map real (reverse [2 .. n]))
  an n =
    if even n then
      real 0 :: CauchyReal
    else
      (-1)^((n - 1)/2)*(iFact n) :: CauchyReal

anaGeom :: Analytic
anaGeom =
  Analytic 1 1 an
  where
  an n =
    if even n then
      real $ 0.5^n
    else
      real 0 :: CauchyReal
