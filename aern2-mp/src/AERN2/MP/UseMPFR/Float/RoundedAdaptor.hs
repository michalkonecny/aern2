{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, ExistentialQuantification, RankNTypes #-}
-- {-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.UseMPFR.Float.RoundedAdaptor
    Description :  Numeric.Rounded + variable precision
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Numeric.Rounded + variable precision
-}
module AERN2.MP.UseMPFR.Float.RoundedAdaptor
#ifndef MPFRRounded
() where
#else
(
  module AERN2.MP.UseMPFR.Float.RoundedAdaptor
, module Numeric.MPFR.LowLevel
)
where

import Prelude hiding (div, pi)
-- import qualified Prelude as P

import Numeric.MPFR.LowLevel
-- import qualified Numeric.MPFR.LowLevel as R

type RoundMode = RoundingMode

defaultPrecision :: Precision
defaultPrecision = 10

pi :: RoundMode -> Precision -> MPFR
pi = kPi

fromIntegerA :: RoundMode -> Precision -> Integer -> MPFR
fromIntegerA = fromInteger'

zero, one :: MPFR
zero = fromIntegerA Up defaultPrecision 0
one = fromIntegerA Up defaultPrecision 1

toDoubleA :: RoundMode -> MPFR -> Double
toDoubleA = toDouble

fromRationalA :: RoundMode -> Precision -> Rational -> MPFR
fromRationalA = fromRational'

toRationalA :: MPFR -> Rational
toRationalA = toRational'

add, sub, mul, div, atan2 :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
add = add_
sub = sub_
mul = mul_
div = div_
atan2 = atan2_

neg, abs, sqrt, exp, log, sin, cos :: RoundMode -> Precision -> MPFR -> MPFR
neg = negate_
abs = abs_
sqrt = sqrt_
exp = exp_
log = log_
sin = sin_
cos = cos_
-- TODO: add more ops

#endif
