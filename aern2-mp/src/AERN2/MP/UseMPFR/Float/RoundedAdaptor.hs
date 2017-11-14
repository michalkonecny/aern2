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
, module Numeric.Rounded.Simple
)
where

import Prelude hiding (div, pi)
-- import qualified Prelude as P

import Numeric.Rounded.Simple
-- import qualified Numeric.RoundedSimple as R

instance Show Rounded where
  show = show'

getPrec :: Rounded -> Int
getPrec = precision

getExp :: Rounded -> Int
getExp = exponent'

data RoundMode = Up | Down

withRoundMode :: (RoundingMode -> t) -> (RoundMode -> t)
withRoundMode op Up = op TowardInf
withRoundMode op Down = op TowardNegInf
{-# INLINE withRoundMode #-}

set :: RoundMode -> Precision -> Rounded -> Rounded
set = withRoundMode precRound

defaultPrecision :: Precision
defaultPrecision = 10

pi :: RoundMode -> Precision -> Rounded
pi = withRoundMode kPi

fromIntegerA :: RoundMode -> Precision -> Integer -> Rounded
fromIntegerA = withRoundMode fromInteger'

zero, one :: Rounded
zero = fromIntegerA Up defaultPrecision 0
one = fromIntegerA Up defaultPrecision 1

toDoubleA :: RoundMode -> Rounded -> Double
toDoubleA = withRoundMode toDouble

fromRationalA :: RoundMode -> Precision -> Rational -> Rounded
fromRationalA = withRoundMode fromRational'

toRationalA :: Rounded -> Rational
toRationalA = toRational' TowardNearest

add, sub, mul, div, atan2 :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
add = withRoundMode add_
sub = withRoundMode sub_
mul = withRoundMode mul_
div = withRoundMode div_
atan2 = withRoundMode atan2_

neg, abs, sqrt, exp, log, sin, cos :: RoundMode -> Precision -> Rounded -> Rounded
neg = withRoundMode negate_
abs = withRoundMode abs_
sqrt = withRoundMode sqrt_
exp = withRoundMode exp_
log = withRoundMode log_
sin = withRoundMode sin_
cos = withRoundMode cos_
-- TODO: add more ops

#endif
