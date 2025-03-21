{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.Float.Conversions
    Description :  Conversions and comparisons of arbitrary precision floats
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions and comparisons of arbitrary precision floating point numbers
-}
module AERN2.MP.Float.Conversions
  (
   -- * MPFloat to other types (see also instances)
   toDouble
   -- * MPFloat constructors (see also instances)
   , CanBeMPFloat, mpFloat
   , fromIntegerCEDU
   , fromRationalCEDU
   -- * comparisons and constants (see also instances)
   , zero, one, two
   , nan, infinity
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Data.Ratio
-- import Data.Convertible

-- import AERN2.Norm
import AERN2.MP.Precision

import qualified Data.CDAR as MPLow

import AERN2.MP.Float.Auxi
import AERN2.MP.Float.Type
import AERN2.MP.Float.Arithmetic


{- conversions to MPFloat -}

type CanBeMPFloat t = ConvertibleExactly t MPFloat
mpFloat :: (CanBeMPFloat t) => t -> MPFloat
mpFloat = convertExactly

instance ConvertibleExactly MPFloat MPFloat where
    safeConvertExactly =
      Right . id

instance ConvertibleExactly Integer MPFloat where
    safeConvertExactly =
      Right . MPFloat . P.fromInteger

instance ConvertibleExactly (WithSample MPFloat Integer) MPFloat where
    safeConvertExactly (WithSample sampleMPFloat n) =
      fmap increasePrecisionToP (safeConvertExactly n)
      where
        p = if isFinite sampleMPFloat then getPrecision sampleMPFloat else (prec 2)
        increasePrecisionToP v 
          | vp >= p = v
          | otherwise = setPrecision p v
          where
            vp = getPrecision v
           
instance ConvertibleExactly Int MPFloat where
    safeConvertExactly = safeConvertExactly . integer

instance ConvertibleExactly (WithSample MPFloat Int) MPFloat where
    safeConvertExactly (WithSample sampleMPFloat n) =
      fmap (setPrecision p) (safeConvertExactly n)
      where
        p = getPrecision sampleMPFloat

fromIntegerCEDU :: Precision -> Integer -> BoundsCEDU MPFloat
fromIntegerCEDU pp =
  setPrecisionCEDU pp . MPFloat . P.fromInteger

fromRationalCEDU :: Precision -> Rational -> BoundsCEDU MPFloat
fromRationalCEDU pp =
  setPrecisionCEDU pp . (MPFloat . MPLow.toApproxMB (p2cdarPrec pp))

{- conversions from MPFloat -}

instance ConvertibleExactly MPLow.Approx Rational where
  safeConvertExactly = Right . P.toRational

instance ConvertibleExactly MPFloat Rational where
  safeConvertExactly = safeConvertExactly . unMPFloat
    
toDouble :: MPFloat -> Double
toDouble = P.fromRational . rational

instance Convertible MPFloat Double where
  safeConvert x
    | isFinite dbl = Right dbl
    | otherwise = convError "conversion to double: out of bounds" x
    where
    dbl = toDouble x


instance CanRound MPFloat where
  properFraction (MPFloat x) = (n,f)
    where
      r = rational x
      n = (numerator r) `P.quot` (denominator r)
      f =  ceduCentre $ (MPFloat x) `subCEDU` (MPFloat $ P.fromInteger n)
  
{- comparisons -}

instance HasEqAsymmetric MPLow.Approx MPLow.Approx
instance HasEqAsymmetric MPFloat MPFloat where
  equalTo = lift2R equalTo
instance HasEqAsymmetric MPFloat Integer where
  equalTo = convertSecond equalTo
instance HasEqAsymmetric Integer MPFloat where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric MPFloat Int where
  equalTo = convertSecond equalTo
instance HasEqAsymmetric Int MPFloat where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric MPFloat Rational where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Rational MPFloat where
  equalTo = convertSecond equalTo

instance CanTestZero MPFloat

instance HasOrderAsymmetric MPLow.Approx MPLow.Approx
instance HasOrderAsymmetric MPFloat MPFloat where
  lessThan = lift2R lessThan
  leq = lift2R leq
instance HasOrderAsymmetric MPFloat Integer where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric Integer MPFloat where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPFloat Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric Int MPFloat where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Rational MPFloat where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric MPFloat Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq

instance CanTestPosNeg MPFloat

{- min, max -}

instance CanMinMaxAsymmetric MPFloat MPFloat where
  type MinMaxType MPFloat MPFloat = MPFloat
  max x y
    | isNaN x = x
    | isNaN y = y
    | otherwise = lift2 P.max x y
  min x y
    | isNaN x = x
    | isNaN y = y
    | otherwise = lift2 P.min x y

{- constants -}

zero, one, two :: MPFloat
zero = mpFloat 0
one = mpFloat 1
two = mpFloat 2

nan, infinity :: MPFloat
nan = MPFloat MPLow.Bottom
infinity = nan

itisNaN :: MPFloat -> Bool
itisNaN (MPFloat MPLow.Bottom) = True
itisNaN _ = False

instance CanTestFinite MPFloat where
  isInfinite = itisNaN
  isFinite = not . itisNaN

instance CanTestNaN MPFloat where
  isNaN = itisNaN
