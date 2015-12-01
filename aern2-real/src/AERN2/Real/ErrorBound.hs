{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.ErrorBound 
    (ErrorBound, er2mp, 
     rational2ErrorBound,
     mp2ErrorBound, absMP, subMP,
     accuracyIndex) 
where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational, abs)

import Math.NumberTheory.Logarithms (integerLog2)

import qualified AERN2.Real.MPFloat as MP
import AERN2.Real.MPFloat (MPFloat, Precision, prec)
import AERN2.Real.Operations

{- example -}

_example1 :: ErrorBound
_example1 = 2*((rational2ErrorBound 0.01) + 0.1*(rational2ErrorBound 0.01)/3)

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
newtype ErrorBound = ErrorBound { er2mp :: MPFloat }

instance Show ErrorBound where
    show (ErrorBound d) = show d

errorBoundPrecision :: Precision
errorBoundPrecision = prec 53

rational2ErrorBound :: Rational -> ErrorBound
rational2ErrorBound x
    | x >= 0.0 = ErrorBound $ MP.rationalUp errorBoundPrecision x
    | otherwise = error $ "Trying to construct a negative ErrorBound: " ++ show x

mp2ErrorBound :: MPFloat -> ErrorBound
mp2ErrorBound x 
    | x >= (MP.rationalUp errorBoundPrecision 0.0) = 
        ErrorBound (MP.setPrecisionUp errorBoundPrecision x)
    | otherwise = error $ "Trying to construct a negative ErrorBound: " ++ show x
    
subMP :: MPFloat -> MPFloat -> ErrorBound
a `subMP` b = mp2ErrorBound $ a `MP.subUp` b 

absMP :: MPFloat -> ErrorBound
absMP = mp2ErrorBound . MP.abs

accuracyIndex :: ErrorBound -> Integer
accuracyIndex (ErrorBound e) = 
    toInteger $ integerLog2 $ floor $ MP.toRational $ MP.recipDown e

instance CanAdd ErrorBound ErrorBound where
    type AddType ErrorBound ErrorBound = ErrorBound
    add (ErrorBound a) (ErrorBound b) = ErrorBound $ a `MP.addUp` b

instance CanAddThis ErrorBound ErrorBound
instance CanAddSameType ErrorBound

instance CanMul ErrorBound ErrorBound where
    type MulType ErrorBound ErrorBound = ErrorBound
    mul (ErrorBound a) (ErrorBound b) = ErrorBound $ a `MP.mulUp` b

instance CanMulBy ErrorBound ErrorBound
instance CanMulSameType ErrorBound

instance CanMul ErrorBound Integer where
    type MulType ErrorBound Integer = ErrorBound
    mul (ErrorBound a) i
        | i >= 0 = ErrorBound $ a `MP.mulUp` (MP.integerUp errorBoundPrecision i)
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMul Integer ErrorBound where
    type MulType Integer ErrorBound = ErrorBound
    mul i (ErrorBound b)
        | i >= 0 = ErrorBound $ (MP.integerUp errorBoundPrecision i) `MP.mulUp` b
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMulBy ErrorBound Integer

instance CanDiv ErrorBound Integer where
    type DivType ErrorBound Integer = ErrorBound
    div (ErrorBound a) i
        | i > 0 = ErrorBound $ a `MP.divUp` (MP.integerUp errorBoundPrecision i)
        | otherwise = error "trying to multiply ErrorBound by a non-positive integer"

instance CanMul ErrorBound Rational where
    type MulType ErrorBound Rational = ErrorBound
    mul (ErrorBound a) r
        | r >= 0.0 = ErrorBound $ a `MP.mulUp` (MP.rationalUp errorBoundPrecision r)
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMul Rational ErrorBound where
    type MulType Rational ErrorBound = ErrorBound
    mul r (ErrorBound b)
        | r >= 0.0 = ErrorBound $ (MP.rationalUp errorBoundPrecision r) `MP.mulUp` b
        | otherwise = error "trying to multiply ErrorBound by a negative integer"

instance CanMulBy ErrorBound Rational

