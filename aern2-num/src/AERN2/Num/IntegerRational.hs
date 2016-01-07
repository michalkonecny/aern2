{-# LANGUAGE FlexibleInstances #-}

module AERN2.Num.IntegerRational 
() 
where

{- imports -}

import AERN2.Num.Operations  -- includes relevant parts of Prelude
import qualified Prelude as P

import Control.Arrow

import Data.Ratio ((%))

{- examples -}

_example1 :: Rational
_example1 = 2 * 3 + (1/2) ^ 2

_example2 :: Integer -- cannot be Int
_example2 = 2 * 3 + 2 ^ 2

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
    
instance CanNegSameType Integer

instance (ArrowChoice to) => CanAbsA to Integer where
    absA = arr P.abs
    
instance (ArrowChoice to) => CanAbsSameTypeA to Integer

{- TODO 
    Make the following instances arrow-generic.
-}

instance CanMinMax Integer Integer where

instance CanMinMaxThis Integer Integer
instance CanMinMaxSameType Integer

instance CanAddA (->) Integer Integer where
    addA (a, b) = a P.+ b

instance CanAddThis Integer Integer
instance CanAddSameType Integer

instance CanSub Integer Integer -- the default implementation is fine

instance CanSubThis Integer Integer
instance CanSubSameType Integer
    
instance CanMulA (->) Integer Integer where
    mulA = uncurry (P.*)

instance CanMulBy Integer Integer
instance CanMulSameType Integer
        
instance CanPowA (->) Integer Integer where
    powA = uncurry (P.^)
    
instance RingA (->) Integer
instance CanAddMulScalarA (->) Integer Integer
    
instance CanRecipA (->) Integer where
    type RecipTypeA (->) Integer = Rational
    recipA a = 1 % a

instance CanDiv Integer Integer -- the default implementation is fine
    
{- operations on Rationals -}
    
instance (ArrowChoice to) => CanNegA to Rational where
    negA = arr P.negate
    
instance CanNegSameType Rational

instance (ArrowChoice to) => CanAbsA to Rational where
    absA = arr P.abs
    
instance (ArrowChoice to) => CanAbsSameTypeA to Rational

instance CanMinMax Rational Rational where

instance CanMinMaxThis Rational Rational
instance CanMinMaxSameType Rational

instance CanAddA (->) Rational Rational where
    addA = uncurry (P.+)

instance CanAddThis Rational Rational
instance CanAddSameType Rational

instance CanSub Rational Rational -- the default implementation is fine

instance CanSubThis Rational Rational
instance CanSubSameType Rational
    
instance CanMulA (->) Rational Rational where
    mulA = uncurry (P.*)

instance CanMulBy Rational Rational
instance CanMulSameType Rational
    
instance CanPowA (->) Rational Integer where
    powA = uncurry (P.^)
    
instance CanRecipA (->) Rational where
    recipA a = 1 / a

instance CanRecipSameType Rational

instance CanDivA (->) Rational Rational where
    divA = uncurry (P./)

instance CanDivBy Rational Rational
instance CanDivSameType Rational

instance RingA (->) Rational
instance FieldA (->) Rational
instance CanAddMulScalarA (->) Rational Rational
instance CanAddMulDivScalarA (->) Rational Rational

{- operations mixing Integer and Rational -}

instance CanAddA (->) Integer Rational where
    type AddTypeA (->) Integer Rational = Rational
    addA (a, b) = (P.fromInteger a) P.+ b

instance CanSub Integer Rational

instance CanAddA (->) Rational Integer where
    type AddTypeA (->) Rational Integer = Rational
    addA (a, b) = a P.+ (P.fromInteger b)

instance CanAddThis Rational Integer

instance CanSub Rational Integer

instance CanSubThis Rational Integer

instance CanMulA (->) Integer Rational where
    type MulTypeA (->) Integer Rational = Rational
    mulA (a, b) = (P.fromInteger a) P.* b

instance CanMulA (->) Rational Integer where
    type MulTypeA (->) Rational Integer = Rational
    mulA (a, b) = a P.* (P.fromInteger b)

instance CanMulBy Rational Integer

instance CanDivA (->) Integer Rational where
    type DivTypeA (->) Integer Rational = Rational
    divA (a, b) = (P.fromInteger a) P./ b

instance CanDivA (->) Rational Integer where
    type DivTypeA (->) Rational Integer = Rational
    divA (a, b) = a P./ (P.fromInteger b)

instance CanDivBy Rational Integer

instance CanAddMulScalarA (->) Rational Integer
instance CanAddMulDivScalarA (->) Rational Integer
