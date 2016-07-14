{-|
    Module      :  AERN2.MP.Ball.Conversions
    Description :  Conversions of arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions of arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Conversions
()
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Dyadic (Dyadic)
import qualified AERN2.MP.Float as MPFloat
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (errorBound)

import AERN2.MP.Ball.Type

{--- constructing a ball with a given precision ---}

instance ConvertWithPrecision Integer MPBall where
  safeConvertP p x =
    Right $ MPBall xUp (xUp `EB.subMP` xDn)
    where
    xUp = MPFloat.fromIntegerUp p x
    xDn = MPFloat.fromIntegerDown p x

instance ConvertWithPrecision Int MPBall where
  safeConvertP p = safeConvertP p . integer

instance ConvertWithPrecision Rational MPBall where
  safeConvertP p x =
    Right $ MPBall xUp (xUp `EB.subMP` xDn)
    where
    xUp = MPFloat.fromRationalUp p x
    xDn = MPFloat.fromRationalDown p x

instance ConvertWithPrecision (Rational, Rational) MPBall where
  safeConvertP p (x,e) =
    Right $ MPBall xFlt (xe + eUp)
    where
    (MPBall xFlt xe) = mpBallP p x
    eUp = errorBound e

{--- constructing an exact ball ---}

instance ConvertibleExactly MPBall MPBall where
  safeConvertExactly = Right

instance ConvertibleExactly Dyadic MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)

instance ConvertibleExactly Integer MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)

instance ConvertibleExactly Int MPBall where
  safeConvertExactly x = Right $ MPBall (convertExactly x) (errorBound 0)
