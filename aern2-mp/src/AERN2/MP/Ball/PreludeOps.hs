{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Ball.PreludeOps
    Description :  Instances of Prelude.Num etc
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Instances of Prelude classes Eq, Ord, Num etc
-}
module AERN2.MP.Ball.PreludeOps
(
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Dyadic (dyadic)

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons ()
import AERN2.MP.Ball.Field ()
import AERN2.MP.Ball.Elementary ()

{- Instances of Prelude numerical classes provided for convenient use outside AERN2
   and also because Template Haskell translates (-x) to (Prelude.negate x) -}

instance P.Eq MPBall where
  a == b =
    case a == b of
      CertainTrue  -> True
      CertainFalse  -> False
      _ ->
        error "Failed to decide equality of MPBalls.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of MPBalls returns Kleenean instead of Bool."

instance P.Ord MPBall where
  compare a b =
    case (a < b, a == b, a > b) of
      (CertainTrue, _, _) -> P.LT
      (_, CertainTrue, _) -> P.EQ
      (_, _, CertainTrue) -> P.GT
      _ ->
        error "Failed to decide order of MPBalls.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of MPBalls returns Kleenean instead of Bool."

instance P.Num MPBall where
    fromInteger = convertExactly
    negate = negate
    (+) = (+)
    (*) = (*)
    abs = abs
    signum = error "Prelude.signum not implemented for MPBall"

instance P.Fractional MPBall where
    fromRational = convertExactly . dyadic -- will work only for dyadic rationals
    recip = recip
    (/) = (/)

instance P.Floating MPBall where
    pi = error "There is no pi :: MPBall, use pi :: Real instead"
    sqrt = sqrt
    exp = exp
    sin = sin
    cos = cos
    log = log
    atan = error "MPBall: atan not implemented yet"
    atanh = error "MPBall: atanh not implemented yet"
    asin = error "MPBall: asin not implemented yet"
    acos = error "MPBall: acos not implemented yet"
    sinh = error "MPBall: sinh not implemented yet"
    cosh = error "MPBall: cosh not implemented yet"
    asinh = error "MPBall: asinh not implemented yet"
    acosh = error "MPBall: acosh not implemented yet"
