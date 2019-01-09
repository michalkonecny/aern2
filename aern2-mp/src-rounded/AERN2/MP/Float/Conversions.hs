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
import Data.Convertible

import AERN2.Norm
import AERN2.MP.Precision

import AERN2.MP.Float.Aux
import AERN2.MP.Float.Type
import AERN2.MP.Float.Arithmetic

import qualified AERN2.MP.Float.RoundedAdaptor as MPLow

mpToDouble :: MPLow.RoundMode -> MPFloat -> Double
mpToDouble = MPLow.toDoubleA

mpToRational :: MPFloat -> Rational
mpToRational x
  | x == 0 = 0.0
  | otherwise = MPLow.toRationalA x

mpFromRationalA :: MPLow.RoundMode -> MPLow.Precision -> Rational -> MPFloat
mpFromRationalA = MPLow.fromRationalA

{- conversions to MPFloat -}

type CanBeMPFloat t = ConvertibleExactly t MPFloat
mpFloat :: (CanBeMPFloat t) => t -> MPFloat
mpFloat = convertExactly

instance ConvertibleExactly Integer MPFloat where
    safeConvertExactly n =
        findExact $ map (flip fromIntegerCEDU n) $ standardPrecisions initPrec
        where
        initPrec =
            case getNormLog n of
              NormBits b -> prec (b + 8)
              _ -> prec 8
        findExact [] =
            convError "integer too high to represent exactly" n
        findExact (cedu : rest)
            | ceduErr cedu P.> zero = findExact rest
            | otherwise = Right (ceduCentre cedu)

instance ConvertibleExactly Int MPFloat where
    safeConvertExactly = safeConvertExactly . integer

fromIntegerCEDU :: Precision -> Integer -> BoundsCEDU MPFloat
fromIntegerCEDU pp n =
  constCEDU (\r p -> MPLow.fromIntegerA r p n) (p2mpfrPrec pp)

fromRationalCEDU :: Precision -> Rational -> BoundsCEDU MPFloat
fromRationalCEDU pp q =
  constCEDU (\r p -> mpFromRationalA r p q) (p2mpfrPrec pp)

{- conversions from MPFloat -}

instance ConvertibleExactly MPFloat Rational where
  safeConvertExactly = Right . mpToRational

toDouble :: MPFloat -> Double
toDouble = mpToDouble MPLow.Up

instance Convertible MPFloat Double where
  safeConvert x
    | isFinite dbl = Right dbl
    | otherwise = convError "conversion to double: out of bounds" x
    where
    dbl = toDouble x

instance CanRound MPFloat where
  properFraction x = (n,f)
    where
      r = rational x
      n = (numerator r) `P.quot` (denominator r)
      f = ceduCentre $ x `subCEDU` (mpFloat n)

{- comparisons -}

instance HasEqAsymmetric MPFloat MPFloat
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

instance HasOrderAsymmetric MPFloat MPFloat
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

instance CanMinMaxAsymmetric MPFloat MPFloat

{- constants -}

zero, one, two :: MPFloat
zero = MPLow.zero
one = MPLow.one
two = MPLow.add MPLow.Up (MPLow.getPrec one) one one

nan, infinity :: MPFloat
nan = ceduCentre $ divCEDU zero zero 
infinity = ceduCentre $ divCEDU one zero 

itisNaN :: MPFloat -> Bool
itisNaN x = not $ x P.== x

itisInfinite :: MPFloat -> Bool
itisInfinite x =
  ceduCentre (mulCEDU x two) P.== x
  &&
  x P./= zero

instance CanTestFinite MPFloat where
  isInfinite = itisInfinite
  isFinite x = not (itisInfinite x || itisNaN x)

instance CanTestNaN MPFloat where
  isNaN = itisNaN


