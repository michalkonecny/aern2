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
(
  integerBounds
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Numeric.CatchingExceptions (CanTestValid(..))

import Data.Convertible

import AERN2.MP.Dyadic (Dyadic)
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (mpFloat)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision
import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (errorBound)

import AERN2.MP.Ball.Type

{--- extracting from a ball ---}

integerBounds :: MPBall -> (Integer, Integer)
integerBounds b =
  (floor l, ceiling r)
  where
    (l,r) = endpointsMP b

instance Convertible MPBall EB.ErrorBound where
  safeConvert b =
    Right (errorBound (max (abs l) (abs r)))
    where
    (l,r) = endpointsMP b

{--- constructing an exact ball ---}

instance ConvertibleExactly MPBall MPBall where
  safeConvertExactly = Right

instance ConvertibleExactly Dyadic MPBall where
  safeConvertExactly x = Right $ MPBall (mpFloat x) (errorBound 0)

instance ConvertibleExactly Integer MPBall where
  safeConvertExactly x
    | isValid b = Right b
    | otherwise = convError "too large to convert to MPBall" x
    where
      b = MPBall (mpFloat x) (errorBound 0)

instance ConvertibleExactly (Integer, Integer) MPBall where
  safeConvertExactly (x,e)
    | isValid b = Right b
    | otherwise = convError "too large to convert to MPBall" x
    where
      b = MPBall (mpFloat x) (errorBound $ mpFloat e)

instance ConvertibleExactly Int MPBall where
  safeConvertExactly x = Right $ MPBall (mpFloat x) (errorBound 0)

instance ConvertibleExactly (Int, Int) MPBall where
  safeConvertExactly (x,e) = Right $ MPBall (mpFloat x) (errorBound $ mpFloat e)

{--- constructing a ball with a given precision ---}

instance ConvertibleWithPrecision Integer MPBall where
  safeConvertP p x
    | isValid b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = MPBall xUp (xUp `EB.subMP` xDn)
    xUp = MPFloat.fromIntegerUp p x
    xDn = MPFloat.fromIntegerDown p x

instance ConvertibleWithPrecision Int MPBall where
  safeConvertP p = safeConvertP p . integer

instance ConvertibleWithPrecision Rational MPBall where
  safeConvertP p x
    | isValid b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = MPBall xUp (xUp `EB.subMP` xDn)
    xUp = MPFloat.fromRationalUp p x
    xDn = MPFloat.fromRationalDown p x

instance ConvertibleWithPrecision (Rational, Rational) MPBall where
  safeConvertP p (x,e)
    | isValid b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = MPBall xFlt (xe + eUp) -- beware, precision may be too high relative to accuracy
    (MPBall xFlt xe) = mpBallP p x
    eUp = errorBound e
