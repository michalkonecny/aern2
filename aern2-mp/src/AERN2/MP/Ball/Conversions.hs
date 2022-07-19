{-# OPTIONS_GHC -Wno-orphans #-}
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

-- import qualified Numeric.CollectErrors as CN

import Data.Typeable
-- import Data.Convertible

import AERN2.MP.Dyadic (Dyadic, dyadic)
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat(..), mpFloat)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision
-- import qualified AERN2.MP.ErrorBound as EB
-- import qualified AERN2.MP.ErrorBound as EB
import AERN2.MP.ErrorBound (ErrorBound, errorBound, CanBeErrorBound)

import AERN2.MP.Ball.Type

{--- extracting from a ball ---}

instance HasIntegerBounds MPBall where
  integerBounds b =
    (floor l, ceiling r)
    where
      (l,r) = endpoints b

instance Convertible MPBall ErrorBound where
  safeConvert b =
    Right (errorBound (max (abs l) (abs r)))
    where
    (l,r) = endpoints b

{--- constructing an exact ball ---}

instance ConvertibleExactly MPBall MPBall where
  safeConvertExactly = Right

instance
  (ConvertibleExactly c Dyadic, Convertible e ErrorBound
  , Show c, Show e, Typeable c, Typeable e)
  =>
  ConvertibleExactly (c, e) MPBall
  where
  safeConvertExactly (c,e)
    | isFinite b = Right b
    | otherwise = convError "too large to convert to MPBall" (c,e)
    where
    b = updateRadius (const eE) cB 
    cB = mpBall $ dyadic c 
    eE = errorBound e

instance ConvertibleExactly Dyadic MPBall where
  safeConvertExactly x = Right $ MPBall xA 
    where
    (MPFloat xA) = mpFloat x

instance ConvertibleExactly ErrorBound MPBall where
  safeConvertExactly eb = Right $ mpBall (eb, 0)


instance ConvertibleExactly Integer MPBall where
  safeConvertExactly x
    | isFinite b = Right b
    | otherwise = convError "too large to convert to MPBall" x
    where
    b = mpBall (x, 0)

instance ConvertibleExactly Int MPBall where
  safeConvertExactly x = Right $ mpBall (x, 0)

{--- constructing a ball with a given precision ---}

instance ConvertibleWithPrecision Integer MPBall where
  safeConvertP p x
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = mpBall $ MPFloat.ceduCentreErr $ MPFloat.fromIntegerCEDU p x

instance ConvertibleWithPrecision Int MPBall where
  safeConvertP p = safeConvertP p . integer

instance ConvertibleWithPrecision Dyadic MPBall where
  safeConvertP p x
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = mpBall (x, 0)

instance ConvertibleWithPrecision Rational MPBall where
  safeConvertP p x
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = mpBall $ MPFloat.ceduCentreErr $ MPFloat.fromRationalCEDU p x

instance ConvertibleWithPrecision (Rational, Rational) MPBall where
  safeConvertP p (x,e)
    | isFinite b = Right b
    | otherwise = convError ("too large to convert to MPBall with precision " ++ show p) x
    where
    b = updateRadius (+ eUp) xB -- beware, precision may be too high relative to accuracy
    xB = mpBallP p x
    eUp = errorBound e

{--- constructing a fat ball ---}

instance (CanBeErrorBound t) => CanPlusMinus MPBall t where
  plusMinus b e = updateRadius (+ (errorBound e)) b

instance (CanBeErrorBound t) => CanPlusMinus (CN MPBall) t where
  plusMinus b e = updateRadius (+ (errorBound e)) b

instance (CanBeErrorBound t, Show t, Typeable t) => CanPlusMinus MPFloat t where
  type PlusMinusType MPFloat t = MPBall
  plusMinus b e = mpBall (b, e)

instance (CanBeErrorBound t, Show t, Typeable t) => CanPlusMinus Dyadic t where
  type PlusMinusType Dyadic t = MPBall
  plusMinus b e = mpBall (b, e)

instance (CanBeErrorBound t, Show t, Typeable t) => CanPlusMinus Integer t where
  type PlusMinusType Integer t = MPBall
  plusMinus b e = mpBall (b, e)

instance (CanBeErrorBound t, Show t, Typeable t) => CanPlusMinus Int t where
  type PlusMinusType Int t = MPBall
  plusMinus b e = mpBall (b, e)

instance (CanBeErrorBound t) => CanPlusMinus Rational t where
  type PlusMinusType Rational t = MPBall
  plusMinus b e = (mpBallP p b) +- e
    where
    p = prec 100 
