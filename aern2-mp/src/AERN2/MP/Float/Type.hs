{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Float.Type
    Description :  Arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers with up/down-rounded operations.

    Currently, we use hmpfr when compiling with ghc 7.10 and higher
    and haskell-mpfr when compiling with ghc 7.8.
-}

module AERN2.MP.Float.Type
  (
   -- * MPFloat numbers and their basic operations
   MPFloat, setPrecisionUp, setPrecisionDown
   , toDoubleUp, toDoubleDown
   -- * MPFloat constructors
   , CanBeMPFloat, mpFloat
   , fromIntegerUp, fromIntegerDown
   , fromRationalUp, fromRationalDown
   , piUp, piDown
   -- * MPFloat basic arithmetic
   , addUp, addDown, subUp, subDown
   , mulUp, mulDown, divUp, divDown, recipUp, recipDown
   , distUp, distDown, avgUp, avgDown
   -- * MPFloat selected operations
   , cosUp, cosDown, sinUp, sinDown
   , sqrtUp, sqrtDown, expUp, expDown
   )
where

import Numeric.MixedTypes
import qualified Prelude as P

import Data.Ratio

import AERN2.Norm
import AERN2.MP.Precision

#ifdef HaskellMPFR
import qualified Data.Approximate.MPFRLowLevel as MPLow

{-| Multiple-precision floating-point type based on MPFR via haskell-mpfr. -}
type MPFloat = MPLow.Rounded

mpToDouble :: MPLow.RoundMode -> MPFloat -> Double
mpToDouble = MPLow.toDoubleA

mpToRational :: MPFloat -> Rational
mpToRational x
    | x == zero = 0.0
    | otherwise = MPLow.toRationalA x

mpFromRationalA :: MPLow.RoundMode -> MPLow.Precision -> Rational -> MPFloat
mpFromRationalA = MPLow.fromRationalA

#endif
#ifdef HMPFR
import qualified Data.Number.MPFR as MPLow

{-| Multiple-precision floating-point type based on MPFR via hmpfr. -}
type MPFloat = MPLow.MPFR

mpToDouble :: MPLow.RoundMode -> MPLow.MPFR -> Double
mpToDouble = MPLow.toDouble

mpToRational :: MPFloat -> Rational
mpToRational x
    | x == 0 = 0.0
    | otherwise = mantissa * 2^e
    where
    (mantissa, ePre) = MPLow.decompose x
    e = P.toInteger ePre

mpFromRationalA :: MPLow.RoundMode -> MPLow.Precision -> Rational -> MPFloat
mpFromRationalA MPLow.Up p q =
  MPLow.fromIntegerA MPLow.Up p (numerator q) `divUp` MPLow.fromIntegerA MPLow.Down p (denominator q)
mpFromRationalA MPLow.Down p q =
  MPLow.fromIntegerA MPLow.Down p (numerator q) `divDown` MPLow.fromIntegerA MPLow.Up p (denominator q)
mpFromRationalA _ _ _ =
  error "in mpFromRationalA"

#endif

instance HasPrecision MPFloat where
  getPrecision x = prec (P.toInteger $ MPLow.getPrec x)

instance HasNorm MPFloat where
  getNormLog x
    | x == 0 = NormZero
    | otherwise = NormBits (P.toInteger $ MPLow.getExp x)

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp p = MPLow.set MPLow.Up (p2mpfrPrec p)

setPrecisionDown :: Precision -> MPFloat -> MPFloat
setPrecisionDown p = MPLow.set MPLow.Down (p2mpfrPrec p)

p2mpfrPrec :: Precision -> MPLow.Precision
p2mpfrPrec = P.fromInteger . integer

{- conversions -}

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

{- min, max -}

instance CanMinMaxAsymmetric MPFloat MPFloat

{- common functions -}

instance CanNeg MPFloat where
  negate = unaryUp MPLow.neg

instance CanAbs MPFloat where
  abs x
    | x < MPLow.zero = negate x
    | otherwise = x

addUp, addDown :: MPFloat -> MPFloat -> MPFloat
addUp = binaryUp True MPLow.add
addDown = binaryDown True MPLow.add

subUp, subDown :: MPFloat -> MPFloat -> MPFloat
subUp = binaryUp True MPLow.sub
subDown = binaryDown True MPLow.sub

mulUp, mulDown :: MPFloat -> MPFloat -> MPFloat
mulUp = binaryUp True MPLow.mul
mulDown = binaryDown True MPLow.mul

divUp,divDown :: MPFloat -> MPFloat -> MPFloat
divUp = binaryUp False MPLow.div
divDown = binaryDown False MPLow.div

recipUp :: MPFloat -> MPFloat
recipUp x = divUp (mpFloat 1) x

recipDown :: MPFloat -> MPFloat
recipDown x = divDown (mpFloat 1) x


{- special constants and functions -}

piUp :: Precision -> MPFloat
piUp p =
    MPLow.pi MPLow.Up (p2mpfrPrec p)

piDown :: Precision -> MPFloat
piDown p =
    MPLow.pi MPLow.Down (p2mpfrPrec p)

cosUp :: MPFloat -> MPFloat
cosUp = unaryUp MPLow.cos

cosDown :: MPFloat -> MPFloat
cosDown = unaryDown MPLow.cos

sinUp :: MPFloat -> MPFloat
sinUp = unaryUp MPLow.sin

sinDown :: MPFloat -> MPFloat
sinDown = unaryDown MPLow.sin

sqrtUp :: MPFloat -> MPFloat
sqrtUp = unaryUp MPLow.sqrt

sqrtDown :: MPFloat -> MPFloat
sqrtDown = unaryDown MPLow.sqrt

expUp :: MPFloat -> MPFloat
expUp = unaryUp MPLow.exp

expDown :: MPFloat -> MPFloat
expDown = unaryDown MPLow.exp

-- | Computes an upper bound to the distance @|x - y|@ of @x@ and @y@.
distUp :: MPFloat -> MPFloat -> MPFloat
distUp x y = if x >= y then subUp x y else subUp y x

-- | Computes a lower bound to the distance @|x - y|@ of @x@ and @y@.
distDown :: MPFloat -> MPFloat -> MPFloat
distDown x y = if x >= y then subDown x y else subDown y x

avgUp :: MPFloat -> MPFloat -> MPFloat
avgUp x y = divUp (addUp x y) (mpFloat 2)

avgDown :: MPFloat -> MPFloat -> MPFloat
avgDown x y = divDown (addDown x y) (mpFloat 2)


{- auxiliary functions to automatically determine result precision from operand precisions -}

unaryUp ::
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryUp opRP x = opRP MPLow.Up p x
    where
    p = MPLow.getPrec x

unaryDown ::
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryDown opRP x = opRP MPLow.Down p x
    where
    p = MPLow.getPrec x

binaryUp ::
    Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryUp = binaryApprox True

binaryDown ::
    Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryDown = binaryApprox False

binaryApprox ::
    Bool -> Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryApprox isUp _canBeExact opRP x y =
    withPrec pMax
    where
    pMax = (getPrecision x) `max` (getPrecision y)
    withPrec p
        | isUp = opRP MPLow.Up (p2mpfrPrec p) x y
        | otherwise = opRP MPLow.Down (p2mpfrPrec p) x y
