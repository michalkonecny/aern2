module AERN2.NumPreludeCompatible
    (module Prelude, module AERN2.Num)
where

import AERN2.Num (CauchyReal, Complex(..))
import Prelude

_example1 :: CauchyReal
_example1 = 1 + 1

_example2 :: CauchyReal
_example2 = pi / 4

_example3 :: Complex CauchyReal
_example3 = pi / 4

