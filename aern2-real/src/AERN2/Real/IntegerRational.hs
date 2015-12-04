{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.IntegerRational 
() 
where

{- imports -}

import Prelude hiding
    ((==),(/=),(<),(>),(<=),(>=),
     (+),(*),(/),(-),(^),abs,min,max,
     recip,div,negate,
     fromInteger,fromRational,
     sqrt,cos,sin)
import qualified Prelude as P

import Data.Ratio ((%))

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Real.Operations

{- examples -}

_example1 :: Rational
_example1 = 2 * 3 + (1/2) ^ 2

_example2 :: Integer -- cannot be Int
_example2 = 2 * 3 + 2 ^ 2

instance HasNorm Integer where
    getNormLog n 
        | n == 0 = NormZero
        | otherwise = NormBits $ toInteger $ integerLog2 $ abs n

instance HasNorm Rational where
    getNormLog x 
        | x == 0.0 = NormZero
        | abs x >= 1.0 = NormBits $ toInteger $ integerLog2 $ ceiling $ abs x
        | otherwise = NormBits $ neg $ toInteger $ integerLog2 $ ceiling (1 / (abs x))

{- comparisons -}

instance HasEq Integer Integer where
    equalTo = (P.==)

instance HasOrder Integer Integer where
    lessThan = (P.<)
    leq = (P.<=)

instance HasEq Rational Rational where
    equalTo = (P.==)

instance HasOrder Rational Rational where
    lessThan = (P.<)
    leq = (P.<=)

instance HasEq Integer Rational where
    equalTo a b = (P.fromInteger a) P.== b

instance HasOrder Integer Rational where
    lessThan a b = (P.fromInteger a) P.< b
    leq a b = (P.fromInteger a) P.<= b

instance HasEq Rational Integer where
    equalTo a b = equalTo b a

instance HasOrder Rational Integer where
    lessThan a b = greaterThan b a
    leq a b = geq b a


{- operations on Integers -}

instance CanNeg Integer where
    type NegType Integer = Integer
    neg a = P.negate a
    
instance CanNegSameType Integer

instance CanAbs Integer where
    type AbsType Integer = Integer
    abs a = P.abs a
    
instance CanAbsSameType Integer

instance CanMinMax Integer Integer where
    type MinMaxType Integer Integer = Integer
    min a b = P.min a b
    max a b = P.max a b

instance CanMinMaxThis Integer Integer
instance CanMinMaxSameType Integer

instance CanAdd Integer Integer where
    type AddType Integer Integer = Integer
    add a b = a P.+ b

instance CanAddThis Integer Integer
instance CanAddSameType Integer

instance CanSub Integer Integer -- the default implementation is fine

instance CanSubThis Integer Integer
instance CanSubSameType Integer
    
instance CanMul Integer Integer where
    type MulType Integer Integer = Integer
    mul a b = a P.* b

instance CanMulBy Integer Integer
instance CanMulSameType Integer
    
instance CanPow Integer Integer where
    type PowType Integer Integer = Integer
    pow a b = a P.^ b
    
instance CanRecip Integer where
    type RecipType Integer = Rational
    recip a = 1 % a

instance CanDiv Integer Integer -- the default implementation is fine
    
{- operations on Rationals -}
    
instance CanNeg Rational where
    type NegType Rational = Rational
    neg a = P.negate a

instance CanNegSameType Rational
    
instance CanAbs Rational where
    type AbsType Rational = Rational
    abs a = P.abs a

instance CanAbsSameType Rational

instance CanMinMax Rational Rational where
    type MinMaxType Rational Rational = Rational
    min a b = P.min a b
    max a b = P.max a b

instance CanMinMaxThis Rational Rational
instance CanMinMaxSameType Rational

instance CanAdd Rational Rational where
    type AddType Rational Rational = Rational
    add a b = a P.+ b

instance CanAddThis Rational Rational
instance CanAddSameType Rational

instance CanSub Rational Rational -- the default implementation is fine

instance CanSubThis Rational Rational
instance CanSubSameType Rational
    
instance CanMul Rational Rational where
    type MulType Rational Rational = Rational
    mul a b = a P.* b

instance CanMulBy Rational Rational
instance CanMulSameType Rational
    
instance CanPow Rational Integer where
    type PowType Rational Integer = Rational
    pow a b = a P.^ b
    
instance CanRecip Rational where
    type RecipType Rational = Rational
    recip a = 1 / a

instance CanRecipSameType Rational

instance CanDiv Rational Rational

instance CanDivBy Rational Rational
instance CanDivSameType Rational

{- operations mixing Integer and Rational -}

instance CanAdd Integer Rational where
    type AddType Integer Rational = Rational
    add a b = (P.fromInteger a) P.+ b

instance CanSub Integer Rational

instance CanAdd Rational Integer where
    type AddType Rational Integer = Rational
    add a b = a P.+ (P.fromInteger b)

instance CanAddThis Rational Integer

instance CanSub Rational Integer

instance CanSubThis Rational Integer

instance CanMul Integer Rational where
    type MulType Integer Rational = Rational
    mul a b = (P.fromInteger a) P.* b

instance CanMul Rational Integer where
    type MulType Rational Integer = Rational
    mul a b = a P.* (P.fromInteger b)

instance CanMulBy Rational Integer

instance CanDiv Integer Rational where
    type DivType Integer Rational = Rational
    div a b = (P.fromInteger a) P./ b

instance CanDiv Rational Integer where
    type DivType Rational Integer = Rational
    div a b = a P./ (P.fromInteger b)

instance CanDivBy Rational Integer


