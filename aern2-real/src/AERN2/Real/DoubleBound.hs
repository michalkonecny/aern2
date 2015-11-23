{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.DoubleBound (DoubleBound(..), rational2DoubleBound) where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
import qualified Prelude as P
import Data.Ratio (numerator,denominator)

import AERN2.Real.Double
import AERN2.Real.Operations

{- example -}

_test1 :: DoubleBound
_test1 = 2*((rational2DoubleBound 0.01) + 0.1*(rational2DoubleBound 0.01)/3)

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
newtype DoubleBound = DoubleBound Double

instance Show DoubleBound where
    show (DoubleBound d) = show d

rational2DoubleBound :: Rational -> DoubleBound
rational2DoubleBound a
    | a >= 0.0 = DoubleBound $ _fromRationalUp a
    | otherwise = error "Trying to construct a negative DoubleBound."

_fromRationalUp :: Rational -> Double
_fromRationalUp r =
    (P.fromInteger (numerator r))
    P./
    (P.negate $ P.fromInteger (P.negate $ denominator r)) -- round the denominator downward!

instance CanAdd DoubleBound DoubleBound where
    type AddType DoubleBound DoubleBound = DoubleBound
    add (DoubleBound a) (DoubleBound b) = DoubleBound $ withUpwardsRounding $ a P.+ b

{-
instance CanMul DoubleBound DoubleBound where
    type MulType DoubleBound DoubleBound = DoubleBound
    mul (DoubleBound a) (DoubleBound b) = DoubleBound $ withUpwardsRounding $ a P.* b
-}

instance CanMul DoubleBound Integer where
    type MulType DoubleBound Integer = DoubleBound
    mul (DoubleBound a) i
        | i >= 0 = DoubleBound $ withUpwardsRounding $ a P.* (P.fromInteger i)
        | otherwise = error "trying to multiply DoubleBound by a negative integer"

instance CanMul Integer DoubleBound where
    type MulType Integer DoubleBound = DoubleBound
    mul i (DoubleBound b)
        | i >= 0 = DoubleBound $ withUpwardsRounding $ (P.fromInteger i) P.* b
        | otherwise = error "trying to multiply DoubleBound by a negative integer"

instance CanDiv DoubleBound Integer where
    type DivType DoubleBound Integer = DoubleBound
    div (DoubleBound a) i
        | i > 0 = DoubleBound $ withUpwardsRounding $ a P./ (P.fromInteger i)
        | otherwise = error "trying to multiply DoubleBound by a non-positive integer"

instance CanMul DoubleBound Rational where
    type MulType DoubleBound Rational = DoubleBound
    mul (DoubleBound a) r
        | r >= 0.0 = DoubleBound $ withUpwardsRounding $ a P.* (_fromRationalUp r)
        | otherwise = error "trying to multiply DoubleBound by a negative integer"

instance CanMul Rational DoubleBound where
    type MulType Rational DoubleBound = DoubleBound
    mul r (DoubleBound b)
        | r >= 0.0 = DoubleBound $ withUpwardsRounding $ (_fromRationalUp r) P.* b
        | otherwise = error "trying to multiply DoubleBound by a negative integer"
