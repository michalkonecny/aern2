{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.UseMPFR.Ball.PreludeOps
    Description :  Instances of Prelude.Num etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Instances of Prelude classes Eq, Ord, Num etc
-}
module AERN2.MP.UseMPFR.Ball.PreludeOps
(
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Dyadic (dyadic)

import AERN2.MP.UseMPFR.Ball.Type
import AERN2.MP.UseMPFR.Ball.Conversions ()
import AERN2.MP.UseMPFR.Ball.Comparisons ()
import AERN2.MP.UseMPFR.Ball.Field ()
import AERN2.MP.UseMPFR.Ball.Elementary

{- Instances of Prelude numerical classes provided for convenient use outside AERN2
   and also because Template Haskell translates (-x) to (Prelude.negate x) -}

instance P.Eq MPBall where
  a == b =
    case a == b of
      Just t -> t
      _ ->
        error "Failed to decide equality of MPBalls.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of MPBalls returns Maybe Bool instead of Bool."

instance P.Ord MPBall where
  compare a b =
    case (a < b, a == b, a > b) of
      (Just True, _, _) -> P.LT
      (_, Just True, _) -> P.EQ
      (_, _, Just True) -> P.GT
      _ ->
        error "Failed to decide order of MPBalls.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of MPBalls returns Maybe Bool instead of Bool."

instance P.Num MPBall where
    fromInteger = convertExactly
    negate = negate
    (+) = (+)
    (*) = (*)
    abs = abs
    signum = error "Prelude.signum not implemented for MPBall"

instance P.Fractional MPBall where
    fromRational = convertExactly . dyadic -- will work only for dyadic rationals
    recip = (~!) . recip
    (/) = (/!)

instance P.Floating MPBall where
    pi = error "MPBall: no pi :: MPBall, use pi ? (bitsS n) instead"
    sqrt = (~!) . sqrt
    exp = exp
    sin = sin
    cos = cos
    log = (~!) . log
    atan = error "MPBall: atan not implemented yet"
    atanh = error "MPBall: atanh not implemented yet"
    asin = error "MPBall: asin not implemented yet"
    acos = error "MPBall: acos not implemented yet"
    sinh = error "MPBall: sinh not implemented yet"
    cosh = error "MPBall: cosh not implemented yet"
    asinh = error "MPBall: asinh not implemented yet"
    acosh = error "MPBall: acosh not implemented yet"
