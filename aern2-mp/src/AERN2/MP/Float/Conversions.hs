{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Float.Conversions
    Description :  Conversions and comparisons of arbitrary precision floats
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions and comparisons of arbitrary precision floating point numbers

    Currently, we use hmpfr when compiling with ghc 7.10 and higher
    and haskell-mpfr when compiling with ghc 7.8.
-}

module AERN2.MP.Float.Conversions
  (
   -- * MPFloat to other types (see also instances)
   toDoubleUp, toDoubleDown
   -- * MPFloat constructors (see also instances)
   , CanBeMPFloat, mpFloat
   , fromIntegerUp, fromIntegerDown
   , fromRationalUp, fromRationalDown
   , mpFromRationalA
   )
where

import Numeric.MixedTypes
import qualified Prelude as P

import Data.Ratio

import AERN2.Norm
import AERN2.MP.Precision

import AERN2.MP.Float.Type
import AERN2.MP.Float.Arithmetic

#ifdef HaskellMPFR
import qualified Data.Approximate.MPFRLowLevel as MPLow

mpToDouble :: MPLow.RoundMode -> MPFloat -> Double
mpToDouble = MPLow.toDoubleA

mpToRational :: MPFloat -> Rational
mpToRational x
    | x == 0 = 0.0
    | otherwise = MPLow.toRationalA x

mpFromRationalA :: MPLow.RoundMode -> MPLow.Precision -> Rational -> MPFloat
mpFromRationalA = MPLow.fromRationalA

#endif
#ifdef HMPFR
import qualified Data.Number.MPFR as MPLow

mpToDouble :: MPLow.RoundMode -> MPLow.MPFR -> Double
mpToDouble = MPLow.toDouble

mpToRational :: MPFloat -> Rational
mpToRational x
    | x == 0 = 0.0
    | otherwise = mantissa * 2.0^e
    where
    (mantissa, ePre) = MPLow.decompose x
    e = P.toInteger ePre

mpFromRationalA :: MPLow.RoundMode -> MPLow.Precision -> Rational -> MPFloat
mpFromRationalA dir p q
  | q < 0 =
    MPLow.fromIntegerA dir p (numerator q) `divDir` MPLow.fromIntegerA dir p (denominator q)
  | otherwise =
    MPLow.fromIntegerA dir p (numerator q) `divDir` MPLow.fromIntegerA dirOpp p (denominator q)
  where
  (divDir, dirOpp) =
    case dir of
      MPLow.Down -> (divDown, MPLow.Up)
      MPLow.Up -> (divUp, MPLow.Down)
      _ -> error "in mpFromRationalA"

#endif

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
        findExact $ map upDown $ drop (int 4) standardPrecisions
        where
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
