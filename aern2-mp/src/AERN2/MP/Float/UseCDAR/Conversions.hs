{-|
    Module      :  AERN2.MP.Float.UseCDAR.Conversions
    Description :  Conversions and comparisons of arbitrary precision floats
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions and comparisons of arbitrary precision floating point numbers
-}

module AERN2.MP.Float.UseCDAR.Conversions
  (
   -- * MPFloat to other types (see also instances)
  --  toDoubleUp, toDoubleDown
  --  -- * MPFloat constructors (see also instances)
  --  , CanBeMPFloat, mpFloat
  --  , fromIntegerUp, fromIntegerDown
  --  , fromRationalUp, fromRationalDown
   )
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Data.Ratio
import Data.Convertible

import AERN2.Norm
import AERN2.MP.Precision

{-
import AERN2.MP.Float.UseRounded.Type
import AERN2.MP.Float.UseRounded.Arithmetic

import qualified AERN2.MP.Float.UseRounded.RoundedAdaptor as MPLow

mpToDouble :: MPLow.RoundMode -> MPFloat -> Double
mpToDouble = MPLow.toDoubleA

mpToRational :: MPFloat -> Rational
mpToRational x
  | x == 0 = 0.0
  | otherwise = MPLow.toRationalA x

mpFromRationalA :: MPLow.RoundMode -> MPLow.Precision -> Rational -> MPFloat
mpFromRationalA = MPLow.fromRationalA

instance HasNorm MPFloat where
  getNormLog x
    | x == 0 = NormZero
    | otherwise = NormBits (P.toInteger $ MPLow.getExp x)

{- conversions -}

instance CanRound MPFloat where
  properFraction x = (n,f)
    where
      r = rational x
      n = (numerator r) `quot` (denominator r)
      f = x `subUp` (mpFloat n)

instance ConvertibleExactly MPFloat Rational where
  safeConvertExactly = Right . mpToRational

toDoubleUp :: MPFloat -> Double
toDoubleUp = mpToDouble MPLow.Up

toDoubleDown :: MPFloat -> Double
toDoubleDown = mpToDouble MPLow.Down

fromIntegerUp :: Precision -> Integer -> MPFloat
fromIntegerUp p i = MPLow.fromIntegerA MPLow.Up (p2mpfrPrec p) i

fromIntegerDown :: Precision -> Integer -> MPFloat
fromIntegerDown p i = MPLow.fromIntegerA MPLow.Down (p2mpfrPrec p) i

type CanBeMPFloat t = ConvertibleExactly t MPFloat
mpFloat :: (CanBeMPFloat t) => t -> MPFloat
mpFloat = convertExactly

instance ConvertibleExactly Integer MPFloat where
    safeConvertExactly n =
        findExact $ map upDown $ standardPrecisions initPrec
        where
        initPrec =
            case getNormLog n of
              NormBits b -> prec (b + 8)
              _ -> prec 8
        upDown p = (fromIntegerDown p n, fromIntegerUp p n)
        findExact [] =
            convError "integer too high to represent exactly" n
        findExact ((nDown, nUp) : rest)
            | nDown == nUp = Right nUp
            | otherwise = findExact rest

instance ConvertibleExactly Int MPFloat where
    safeConvertExactly = safeConvertExactly . integer

fromRationalUp :: Precision -> Rational -> MPFloat
fromRationalUp p x =
    mpFromRationalA MPLow.Up (p2mpfrPrec p) x

fromRationalDown :: Precision -> Rational -> MPFloat
fromRationalDown p x =
    mpFromRationalA MPLow.Down (p2mpfrPrec p) x

instance Convertible MPFloat Double where
  safeConvert x
    | isFinite dbl = Right dbl
    | otherwise = convError "conversion to double: out of bounds" x
    where
    dbl = toDoubleUp x

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
-}