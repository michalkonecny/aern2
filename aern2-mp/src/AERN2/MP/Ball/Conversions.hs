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

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Data.Typeable
-- import Data.Convertible

import AERN2.MP.Dyadic (Dyadic, dyadic)
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (mpFloat)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision
-- import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (ErrorBound, errorBound)

import AERN2.MP.Ball.Type

{--- extracting from a ball ---}

instance HasIntegerBounds MPBall where
  integerBounds b =
    (floor l, ceiling r)
    where
      (l,r) = endpointsMP b

instance Convertible MPBall ErrorBound where
  safeConvert b =
    Right (errorBound (max (abs l) (abs r)))
    where
    (l,r) = endpointsMP b

{--- constructing an exact ball ---}

instance ConvertibleExactly MPBall MPBall where
  safeConvertExactly = Right

instance ConvertibleExactly Dyadic MPBall where
  safeConvertExactly x = Right $ MPBall (mpFloat x) (errorBound 0)

instance ConvertibleExactly ErrorBound MPBall where
  safeConvertExactly eb = Right $ MPBall (mpFloat eb) (errorBound 0)

instance
  (ConvertibleExactly c Dyadic, ConvertibleExactly e Dyadic
  , Show c, Show e, Typeable c, Typeable e)
  =>
  ConvertibleExactly (c, e) MPBall
  where
  safeConvertExactly (c,e)
    | isFinite b = Right b
    | otherwise = convError "too large to convert to MPBall" (c,e)
    where
    b = MPBall (mpFloat $ dyadic c) (errorBound $ mpFloat $ dyadic e)

instance ConvertibleExactly Integer MPBall where
  safeConvertExactly x
    | isFinite b = Right b
    | otherwise = convError "too large to convert to MPBall" x
    where
      b = MPBall (mpFloat x) (errorBound 0)

instance ConvertibleExactly Int MPBall where
  safeConvertExactly x = Right $ MPBall (mpFloat x) (errorBound 0)

{--- constructing a ball with a given precision ---}

instance ConvertibleWithPrecision Integer MPBall where
  safeConvertP p x
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = MPBall xC (errorBound xErr)
    (xC, xErr) = MPFloat.ceduCentreErr $ MPFloat.fromIntegerCEDU p x

instance ConvertibleWithPrecision Int MPBall where
  safeConvertP p = safeConvertP p . integer

instance ConvertibleWithPrecision Dyadic MPBall where
  safeConvertP p x
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = mpBall x

instance ConvertibleWithPrecision Rational MPBall where
  safeConvertP p x
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = MPBall xC (errorBound xErr)
    (xC, xErr) = MPFloat.ceduCentreErr $ MPFloat.fromRationalCEDU p x

instance ConvertibleWithPrecision (Rational, Rational) MPBall where
  safeConvertP p (x,e)
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = MPBall xFlt (xe + eUp) -- beware, precision may be too high relative to accuracy
    (MPBall xFlt xe) = mpBallP p x
    eUp = errorBound e
