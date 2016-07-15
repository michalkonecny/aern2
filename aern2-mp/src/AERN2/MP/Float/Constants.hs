{-|
    Module      :  AERN2.MP.Float.Constants
    Description :  Special constants NaN, infinity etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Special constants NaN, infinity etc
-}

module AERN2.MP.Float.Constants
  (
    nan, infinity, itisNaN, itisInfinite
  )
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Data.Ratio

import AERN2.MP.Float.Type
import AERN2.MP.Float.Conversions
import AERN2.MP.Float.Operators

nan, infinity, z, one :: MPFloat
nan = z /. z
infinity = one /. z
z = mpFloat 0
one = mpFloat 1

itisNaN :: MPFloat -> Bool
itisNaN x = x *^ one /= x

itisInfinite :: MPFloat -> Bool
itisInfinite x = x *^ (mpFloat 2) == x
