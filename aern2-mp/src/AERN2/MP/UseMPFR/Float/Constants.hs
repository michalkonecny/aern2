{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.UseMPFR.Float.Constants
    Description :  Special constants NaN, infinity etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Special constants NaN, infinity etc
-}

module AERN2.MP.UseMPFR.Float.Constants
  (
    zero, one
    , nan, infinity
  )
where

import MixedTypesNumPrelude
import qualified Prelude as P
-- import Data.Ratio

import AERN2.MP.UseMPFR.Float.Type
import AERN2.MP.UseMPFR.Float.Conversions
import AERN2.MP.UseMPFR.Float.Operators

zero, one :: MPFloat
zero = mpFloat 0
one = mpFloat 1

nan, infinity :: MPFloat
nan = zero /. zero
infinity = one /. zero

itisNaN :: MPFloat -> Bool
itisNaN x = x *^ one /= x

itisInfinite :: MPFloat -> Bool
itisInfinite x =
  x *^ (mpFloat 2) P.== x
  &&
  x P./= (mpFloat 0)

instance CanTestFinite MPFloat where
  isInfinite = itisInfinite
  isFinite x = not (itisInfinite x || itisNaN x)

instance CanTestNaN MPFloat where
  isNaN = itisNaN
