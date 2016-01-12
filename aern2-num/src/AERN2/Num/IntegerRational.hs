{-# LANGUAGE FlexibleInstances #-}

module AERN2.Num.IntegerRational 
(factorial, (!)) 
where

{- imports -}

import AERN2.Num.Operations  -- includes relevant parts of Prelude
import qualified Prelude as P

import Control.Arrow

import Data.Ratio ((%))
import Data.List (foldl')

{- examples -}

_example1 :: Rational
_example1 = 2 * 3 + (1/2) ^ 2

_example2 :: Integer -- cannot be Int
_example2 = 2 * 3 + 2 ^ 2

{- factorial -}

factorial :: Integer -> Integer
factorial n = foldl' (*) 1 [1..n] 

(!) :: Integer -> Integer
(!) = factorial


{- comparisons -}

instance (ArrowChoice to) => HasEqA to Integer Integer where

instance (ArrowChoice to) => HasOrderA to Integer Integer where

instance (ArrowChoice to) => HasEqA to Int Int where

instance (ArrowChoice to) => HasOrderA to Int Int where

instance (ArrowChoice to) => HasEqA to Rational Rational where

instance (ArrowChoice to) => HasOrderA to Rational Rational where

instance (ArrowChoice to) => HasEqA to Int Integer where
    equalToA = convertFirstA equalToA

instance (ArrowChoice to) => HasOrderA to Int Integer where
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance (ArrowChoice to) => HasEqA to Integer Int where
    equalToA = convertSecondA equalToA

instance (ArrowChoice to) => HasOrderA to Integer Int where
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA


instance (ArrowChoice to) => HasEqA to Integer Rational where
    equalToA = convertFirstA equalToA

instance (ArrowChoice to) => HasOrderA to Integer Rational where
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance (ArrowChoice to) => HasEqA to Rational Integer where
    equalToA = convertSecondA equalToA

instance (ArrowChoice to) => HasOrderA to Rational Integer where
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance (ArrowChoice to) => HasEqA to Int Rational where
    equalToA = convertFirstA equalToA

instance (ArrowChoice to) => HasOrderA to Int Rational where
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance (ArrowChoice to) => HasEqA to Rational Int where
    equalToA = convertSecondA equalToA

instance (ArrowChoice to) => HasOrderA to Rational Int where
    lessThanA = convertSecondA equalToA
    leqA = convertSecondA equalToA


{- operations on Integers -}

instance (ArrowChoice to) => CanNegA to Integer where
    negA = arr P.negate
    
instance (ArrowChoice to) => CanNegSameTypeA to Integer

instance (ArrowChoice to) => CanAbsA to Integer where
    absA = arr P.abs
    
instance (ArrowChoice to) => CanAbsSameTypeA to Integer

instance (ArrowChoice to) => CanMinMaxA to Integer Integer where

instance (ArrowChoice to) => CanMinMaxThisA to Integer Integer
instance (ArrowChoice to) => CanMinMaxSameTypeA to Integer

instance (ArrowChoice to) => CanAddA to Integer Integer where
    addA = arr $ uncurry (P.+)

instance (ArrowChoice to) => CanAddThisA to Integer Integer
instance (ArrowChoice to) => CanAddSameTypeA to Integer

instance (ArrowChoice to) => CanSubA to Integer Integer -- the default implementation is fine

instance (ArrowChoice to) => CanSubThisA to Integer Integer
instance (ArrowChoice to) => CanSubSameTypeA to Integer
    
instance (ArrowChoice to) => CanMulA to Integer Integer where
    mulA = arr $ uncurry (P.*)

instance (ArrowChoice to) => CanMulByA to Integer Integer
instance (ArrowChoice to) => CanMulSameTypeA to Integer
        
instance (ArrowChoice to) => CanPowA to Integer Integer where
    powA = arr $ uncurry (P.^)
instance (ArrowChoice to) => CanPowByA to Integer Integer
    
instance (ArrowChoice to) => RingA to Integer
instance (ArrowChoice to) => CanAddMulScalarA to Integer Integer
    
instance (ArrowChoice to) => CanRecipA to Integer where
    type RecipTypeA to Integer = Rational
    recipA = arr $ \ a -> 1 % a

instance (ArrowChoice to) => CanDivA to Integer Integer -- the default implementation is fine
    
{- operations on Rationals -}
    
instance (ArrowChoice to) => CanNegA to Rational where
    negA = arr P.negate
    
instance (ArrowChoice to) => CanNegSameTypeA to Rational

instance (ArrowChoice to) => CanAbsA to Rational where
    absA = arr P.abs
    
instance (ArrowChoice to) => CanAbsSameTypeA to Rational

instance (ArrowChoice to) => CanMinMaxA to Rational Rational where

instance (ArrowChoice to) => CanMinMaxThisA to Rational Rational
instance (ArrowChoice to) => CanMinMaxSameTypeA to Rational

instance (ArrowChoice to) => CanAddA to Rational Rational where
    addA = arr $ uncurry (P.+)

instance (ArrowChoice to) => CanAddThisA to Rational Rational
instance (ArrowChoice to) => CanAddSameTypeA to Rational

instance (ArrowChoice to) => CanSubA to Rational Rational -- the default implementation is fine

instance (ArrowChoice to) => CanSubThisA to Rational Rational
instance (ArrowChoice to) => CanSubSameTypeA to Rational
    
instance (ArrowChoice to) => CanMulA to Rational Rational where
    mulA = arr $ uncurry (P.*)

instance (ArrowChoice to) => CanMulByA to Rational Rational
instance (ArrowChoice to) => CanMulSameTypeA to Rational
    
instance (ArrowChoice to) => CanPowA to Rational Integer where
    powA = arr $ uncurry (P.^)
instance (ArrowChoice to) => CanPowByA to Rational Integer

    
instance (ArrowChoice to) => CanRecipA to Rational where
    recipA = arr $ \a -> 1 / a

instance (ArrowChoice to) => CanRecipSameTypeA to Rational

instance (ArrowChoice to) => CanDivA to Rational Rational where
    divA = arr $ uncurry (P./)

instance (ArrowChoice to) => CanDivByA to Rational Rational
instance (ArrowChoice to) => CanDivSameTypeA to Rational

instance (ArrowChoice to) => RingA to Rational
instance (ArrowChoice to) => FieldA to Rational
instance (ArrowChoice to) => CanAddMulScalarA to Rational Rational
instance (ArrowChoice to) => CanAddMulDivScalarA to Rational Rational

{- operations mixing Integer and Rational -}

instance (ArrowChoice to) => CanAddA to Integer Rational where
    type AddTypeA to Integer Rational = Rational
    addA = arr $ \ (a, b) -> (P.fromInteger a) P.+ b

instance (ArrowChoice to) => CanSubA to Integer Rational

instance (ArrowChoice to) => CanAddA to Rational Integer where
    type AddTypeA to Rational Integer = Rational
    addA = arr $ \ (a, b) -> a P.+ (P.fromInteger b)

instance (ArrowChoice to) => CanAddThisA to Rational Integer

instance (ArrowChoice to) => CanSubA to Rational Integer

instance (ArrowChoice to) => CanSubThisA to Rational Integer

instance (ArrowChoice to) => CanMulA to Integer Rational where
    type MulTypeA to Integer Rational = Rational
    mulA = arr $ \(a, b) -> (P.fromInteger a) P.* b

instance (ArrowChoice to) => CanMulA to Rational Integer where
    type MulTypeA to Rational Integer = Rational
    mulA = arr $ \(a, b) -> a P.* (P.fromInteger b)

instance (ArrowChoice to) => CanMulByA to Rational Integer

instance (ArrowChoice to) => CanDivA to Integer Rational where
    type DivTypeA to Integer Rational = Rational
    divA = arr $ \(a, b) -> (P.fromInteger a) P./ b

instance (ArrowChoice to) => CanDivA to Rational Integer where
    type DivTypeA to Rational Integer = Rational
    divA = arr $ \(a, b) -> a P./ (P.fromInteger b)

instance (ArrowChoice to) => CanDivByA to Rational Integer

instance (ArrowChoice to) => CanAddMulScalarA to Rational Integer
instance (ArrowChoice to) => CanAddMulDivScalarA to Rational Integer
