{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  AERN2.MP.ErrorBound
    Description :  Fixed precision non-negative up-rounded floating-point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Fixed precision non-negative up-rounded floating-point numbers.

    Currently using a fixed-precision MPFloat.
-}
module AERN2.MP.ErrorBound
    (ErrorBound, CanBeErrorBound, errorBound,
     absMP, subMP)
where

import Numeric.MixedTypes
import qualified Prelude as P

import Data.Typeable

import Test.QuickCheck

import Data.Convertible

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.MP.Precision
import AERN2.MP.Accuracy
import qualified AERN2.MP.Float as MPFloat
import AERN2.MP.Float (MPFloat, mpFloat, frequencyElements)
import AERN2.MP.Float.Operators
import AERN2.MP.Dyadic

{- example -}

_example1 :: ErrorBound
_example1 = 2*((errorBound 0.01) + 0.1*(errorBound 0.01)/3)

{- type -}

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
newtype ErrorBound = ErrorBound { er2mp :: MPFloat }
  deriving (P.Eq, P.Ord, Typeable)

instance Show ErrorBound where
    show (ErrorBound d) = show d

errorBoundPrecision :: Precision
errorBoundPrecision = prec 53

instance HasAccuracy ErrorBound where
  getAccuracy (ErrorBound e)
      | eN > 0 =
          bits $ negate $ integerLog2 eN
      | e > 0 && eRecipN > 0 =
          bits $ integerLog2 eRecipN
      | e == 0 = Exact
      | otherwise = NoInformation
      where
      eN = floor $ rational e
      eRecipN = ceiling $ rational $ MPFloat.recipDown e

{- conversions -}

instance ConvertibleExactly ErrorBound MPFloat where
  safeConvertExactly = Right . er2mp

instance ConvertibleExactly ErrorBound Dyadic where
  safeConvertExactly = Right . dyadic . er2mp

instance ConvertibleExactly ErrorBound Rational where
  safeConvertExactly = Right . convertExactly . mpFloat

type CanBeErrorBound t = Convertible t ErrorBound
errorBound :: (CanBeErrorBound t) => t -> ErrorBound
errorBound = convert

instance Convertible Rational ErrorBound where
  safeConvert x
    | x >= 0 = Right $ ErrorBound $ MPFloat.fromRationalUp errorBoundPrecision x
    | otherwise = convError "Trying to construct a negative ErrorBound" x

instance Convertible MPFloat ErrorBound where
  safeConvert x
    | x >= 0 = Right $ ErrorBound $ MPFloat.setPrecisionUp errorBoundPrecision x
    | otherwise = convError "Trying to construct a negative ErrorBound" x

instance Convertible Integer ErrorBound where
  safeConvert x
    | x >= 0 = Right $ ErrorBound $ MPFloat.fromIntegerUp errorBoundPrecision x
    | otherwise = convError "Trying to construct a negative ErrorBound" x

instance Convertible Int ErrorBound where
  safeConvert = safeConvert . integer

{- comparisons -}

instance HasOrderAsymmetric ErrorBound ErrorBound

instance HasOrderAsymmetric ErrorBound MPFloat where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPFloat ErrorBound where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric ErrorBound Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Rational ErrorBound where
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance CanMinMaxAsymmetric ErrorBound ErrorBound

{- converting operations -}

subMP :: MPFloat -> MPFloat -> ErrorBound
a `subMP` b = errorBound $ a -^ b

absMP :: MPFloat -> ErrorBound
absMP = errorBound . abs

{- up-rounded operations -}

instance CanAddAsymmetric ErrorBound ErrorBound where
    add (ErrorBound a) (ErrorBound b) = ErrorBound $ a +^ b

instance CanAddAsymmetric ErrorBound MPFloat where
  type AddType ErrorBound MPFloat = ErrorBound
  add = convertSecondUsing (\ _ f -> convert f) add
instance CanAddAsymmetric MPFloat ErrorBound where
  type AddType MPFloat ErrorBound = ErrorBound
  add = convertFirstUsing (\ f _ -> convert f) add

instance CanMulAsymmetric ErrorBound ErrorBound where
    mul (ErrorBound a) (ErrorBound b) = ErrorBound $ a *^ b

instance CanMulAsymmetric ErrorBound MPFloat where
  type MulType ErrorBound MPFloat = ErrorBound
  mul = convertSecondUsing (\ _ f -> convert f) mul
instance CanMulAsymmetric MPFloat ErrorBound where
  type MulType MPFloat ErrorBound = ErrorBound
  mul = convertFirstUsing (\ f _ -> convert f) mul

instance CanMulAsymmetric ErrorBound Integer where
    type MulType ErrorBound Integer = ErrorBound
    mul (ErrorBound a) i
        | i >= 0 = ErrorBound $ a *^ (MPFloat.fromIntegerUp errorBoundPrecision i)
        | otherwise = error "trying to multiply ErrorBound by a negative integer"
instance CanMulAsymmetric Integer ErrorBound where
    type MulType Integer ErrorBound = ErrorBound
    mul i (ErrorBound b)
        | i >= 0 = ErrorBound $ (MPFloat.fromIntegerUp errorBoundPrecision i) *^ b
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMulAsymmetric ErrorBound Rational where
    type MulType ErrorBound Rational = ErrorBound
    mul (ErrorBound a) r
        | r >= 0.0 = ErrorBound $ a *^ (MPFloat.fromRationalUp errorBoundPrecision r)
        | otherwise = error "trying to multiply ErrorBound by a negative integer"
instance CanMulAsymmetric Rational ErrorBound where
    type MulType Rational ErrorBound = ErrorBound
    mul r (ErrorBound b)
        | r >= 0.0 = ErrorBound $ (MPFloat.fromRationalUp errorBoundPrecision r) *^ b
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanDiv ErrorBound Integer where
    type DivType ErrorBound Integer = ErrorBound
    divide (ErrorBound a) i
        | i > 0 = ErrorBound $ a /^ (MPFloat.fromIntegerUp errorBoundPrecision i)
        | otherwise = error "trying to multiply ErrorBound by a non-positive integer"

instance Arbitrary ErrorBound where
  arbitrary =
    do
    giveSpecialValue <- frequencyElements [(5, False),(1, True)]
    aux giveSpecialValue
    where
      aux giveSpecialValue
        | giveSpecialValue =
            elements (map convert [0.0,0.0,0.0,10.0,1.0,0.5,0.125])
        | otherwise =
          do
          (s :: Integer) <- arbitrary
          let resultR = ((abs s) `mod` (2^35))/(2^32)
          let result = convert resultR
          return result
