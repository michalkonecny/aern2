{-# LANGUAGE FlexibleInstances #-}

module AERN2.Num.IntegerRational 
() 
where

{- imports -}

import AERN2.Num.Operations -- includes relevant parts of Prelude
import qualified Prelude as P

import Data.Ratio ((%))

{- examples -}

_example1 :: Rational
_example1 = 2 * 3 + (1/2) ^ 2

_example2 :: Integer -- cannot be Int
_example2 = 2 * 3 + 2 ^ 2

{- comparisons -}

instance HasEq Integer Integer where
    equalTo = (P.==)

instance HasOrder Integer Integer where
    lessThan = (P.<)
    leq = (P.<=)

instance HasEq Int Int where
    equalTo = (P.==)

instance HasOrder Int Int where
    lessThan = (P.<)
    leq = (P.<=)

instance HasEq Rational Rational where
    equalTo = (P.==)

instance HasOrder Rational Rational where
    lessThan = (P.<)
    leq = (P.<=)

instance HasEq Int Integer where
    equalTo a b = (fromInt a) P.== b

instance HasOrder Int Integer where
    lessThan a b = (fromInt a) P.< b
    leq a b = (fromInt a) P.<= b

instance HasEq Integer Int where
    equalTo a b = equalTo b a

instance HasOrder Integer Int where
    lessThan a b = greaterThan b a
    leq a b = geq b a


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

instance HasEq Int Rational where
    equalTo a b = (P.fromInteger $ fromInt a) P.== b

instance HasOrder Int Rational where
    lessThan a b = (P.fromInteger $ fromInt a) P.< b
    leq a b = (P.fromInteger $ fromInt a) P.<= b

instance HasEq Rational Int where
    equalTo a b = equalTo b a

instance HasOrder Rational Int where
    lessThan a b = greaterThan b a
    leq a b = geq b a


{- operations on Integers -}

instance CanNeg Integer where
    neg a = P.negate a
    
instance CanNegSameType Integer

instance CanAbs Integer where
    abs a = P.abs a
    
instance CanAbsSameType Integer

instance CanMinMax Integer Integer where

instance CanMinMaxThis Integer Integer
instance CanMinMaxSameType Integer

instance CanAdd Integer Integer where
    add a b = a P.+ b

instance CanAddThis Integer Integer
instance CanAddSameType Integer

instance CanSub Integer Integer -- the default implementation is fine

instance CanSubThis Integer Integer
instance CanSubSameType Integer
    
instance CanMul Integer Integer where
    mul a b = a P.* b

instance CanMulBy Integer Integer
instance CanMulSameType Integer
    
instance CanPow Integer Integer where
    pow a b = a P.^ b
    
instance CanRecip Integer where
    type RecipType Integer = Rational
    recip a = 1 % a

instance CanDiv Integer Integer -- the default implementation is fine
    
{- operations on Rationals -}
    
instance CanNeg Rational where
    neg a = P.negate a

instance CanNegSameType Rational
    
instance CanAbs Rational where
    abs a = P.abs a

instance CanAbsSameType Rational

instance CanMinMax Rational Rational where

instance CanMinMaxThis Rational Rational
instance CanMinMaxSameType Rational

instance CanAdd Rational Rational where
    add a b = a P.+ b

instance CanAddThis Rational Rational
instance CanAddSameType Rational

instance CanSub Rational Rational -- the default implementation is fine

instance CanSubThis Rational Rational
instance CanSubSameType Rational
    
instance CanMul Rational Rational where
    mul a b = a P.* b

instance CanMulBy Rational Rational
instance CanMulSameType Rational
    
instance CanPow Rational Integer where
    pow a b = a P.^ b
    
instance CanRecip Rational where
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


