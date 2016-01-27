{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Num.ErrorBound 
    (ErrorBound, er2mp, 
     zero, rational2ErrorBound,
     mp2ErrorBound, absMP, subMP,
     getAccuracy) 
where

import AERN2.Num.Operations

import Math.NumberTheory.Logarithms (integerLog2)

import qualified AERN2.Num.Accuracy as A
import qualified AERN2.Num.MPFloat as MP
import AERN2.Num.MPFloat (MPFloat, Precision, prec)

{- example -}

_example1 :: ErrorBound
_example1 = 2*((rational2ErrorBound 0.01) + 0.1*(rational2ErrorBound 0.01)/3)

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
newtype ErrorBound = ErrorBound { er2mp :: MPFloat }

instance Show ErrorBound where
    show (ErrorBound d) = show d

errorBoundPrecision :: Precision
errorBoundPrecision = prec 53

zero :: ErrorBound
zero = ErrorBound $ MP.fromRationalUp errorBoundPrecision 0.0

rational2ErrorBound :: Rational -> ErrorBound
rational2ErrorBound x
    | x >= 0.0 = ErrorBound $ MP.fromRationalUp errorBoundPrecision x
    | otherwise = error $ "Trying to construct a negative ErrorBound: " ++ show x

mp2ErrorBound :: MPFloat -> ErrorBound
mp2ErrorBound x 
    | x >= (MP.fromRationalUp errorBoundPrecision 0.0) = 
        ErrorBound (MP.setPrecisionUp errorBoundPrecision x)
    | otherwise = error $ "Trying to construct a negative ErrorBound: " ++ show x
    
subMP :: MPFloat -> MPFloat -> ErrorBound
a `subMP` b = mp2ErrorBound $ a -^ b 

absMP :: MPFloat -> ErrorBound
absMP = mp2ErrorBound . MP.abs

getAccuracy :: ErrorBound -> A.Accuracy
getAccuracy (ErrorBound e) 
    | e > MP.zero && eRecipN > 0 = 
        A.bits $ toInteger $ integerLog2 eRecipN
    | e == MP.zero = A.Exact
    | otherwise = A.NoInformation
    where
    eRecipN = ceiling $ MP.toRational $ MP.recipDown ppErrorBound e

instance CanAddA (->) ErrorBound ErrorBound where
    addA (ErrorBound a, ErrorBound b) = ErrorBound $ a +^ b

instance CanAddThis ErrorBound ErrorBound
instance CanAddSameType ErrorBound

instance CanMulA (->) ErrorBound ErrorBound where
    mulA (ErrorBound a, ErrorBound b) = ErrorBound $ a *^ b

instance CanMulBy ErrorBound ErrorBound
instance CanMulSameType ErrorBound

instance CanMulA (->) ErrorBound Integer where
    type MulTypeA (->) ErrorBound Integer = ErrorBound
    mulA (ErrorBound a, i)
        | i >= 0 = ErrorBound $ a *^ (MP.fromIntegerUp errorBoundPrecision i)
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMulA (->) Integer ErrorBound where
    type MulTypeA (->) Integer ErrorBound = ErrorBound
    mulA (i, ErrorBound b)
        | i >= 0 = ErrorBound $ (MP.fromIntegerUp errorBoundPrecision i) *^ b
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMulBy ErrorBound Integer

instance CanDivA (->) ErrorBound Integer where
    type DivTypeA (->) ErrorBound Integer = ErrorBound
    divA (ErrorBound a, i)
        | i > 0 = ErrorBound $ a /^ (MP.fromIntegerUp errorBoundPrecision i)
        | otherwise = error "trying to multiply ErrorBound by a non-positive integer"

instance CanMulA (->) ErrorBound Rational where
    type MulTypeA (->) ErrorBound Rational = ErrorBound
    mulA (ErrorBound a, r)
        | r >= 0.0 = ErrorBound $ a *^ (MP.fromRationalUp errorBoundPrecision r)
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMulA (->) Rational ErrorBound where
    type MulTypeA (->) Rational ErrorBound = ErrorBound
    mulA (r, ErrorBound b)
        | r >= 0.0 = ErrorBound $ (MP.fromRationalUp errorBoundPrecision r) *^ b
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMulBy ErrorBound Rational

ppErrorBound :: MP.PrecisionPolicy
ppErrorBound = 
    MP.PrecisionPolicy errorBoundPrecision MP.PrecisionPolicyMode_UseCurrent
    
(+^) :: MPFloat -> MPFloat -> MPFloat
(+^) = MP.addUp ppErrorBound
(-^) :: MPFloat -> MPFloat -> MPFloat
(-^) = MP.subUp ppErrorBound
(*^) :: MPFloat -> MPFloat -> MPFloat
(*^) = MP.mulUp ppErrorBound
(/^) :: MPFloat -> MPFloat -> MPFloat
(/^) = MP.divUp ppErrorBound

