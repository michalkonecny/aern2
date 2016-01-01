{-# LANGUAGE FlexibleInstances #-}

module AERN2.Num.IntegerRational 
() 
where

{- imports -}

import AERN2.Num.Operations  -- includes relevant parts of Prelude
import qualified Prelude as P

import Data.Ratio ((%))

{- examples -}

_example1 :: Rational
_example1 = 2 * 3 + (1/2) ^ 2

_example2 :: Integer -- cannot be Int
_example2 = 2 * 3 + 2 ^ 2

{- comparisons -}

instance HasEqA (->) Integer Integer where

instance HasOrderA (->) Integer Integer where

instance HasEqA (->) Int Int where

instance HasOrderA (->) Int Int where

instance HasEqA (->) Rational Rational where

instance HasOrderA (->) Rational Rational where

instance HasEqA (->) Int Integer where
    equalToA (a, b) = (fromInt a) P.== b

instance HasOrderA (->) Int Integer where
    lessThanA (a, b) = (fromInt a) P.< b
    leqA (a, b) = (fromInt a) P.<= b

instance HasEqA (->) Integer Int where
    equalToA (a, b) = b == a

instance HasOrderA (->) Integer Int where
    lessThanA (a, b) = greaterThan b a
    leqA (a, b) = geq b a


instance HasEqA (->) Integer Rational where
    equalToA (a, b) = (P.fromInteger a) P.== b

instance HasOrderA (->) Integer Rational where
    lessThanA (a, b) = (P.fromInteger a) P.< b
    leqA (a, b) = (P.fromInteger a) P.<= b

instance HasEqA (->) Rational Integer where
    equalToA (a, b) = equalTo b a

instance HasOrderA (->) Rational Integer where
    lessThanA (a, b) = greaterThan b a
    leqA (a, b) = geq b a

instance HasEqA (->) Int Rational where
    equalToA (a, b) = (P.fromInteger $ fromInt a) P.== b

instance HasOrderA (->) Int Rational where
    lessThanA (a, b) = (P.fromInteger $ fromInt a) P.< b
    leqA (a, b) = (P.fromInteger $ fromInt a) P.<= b

instance HasEqA (->) Rational Int where
    equalToA (a, b) = equalTo b a

instance HasOrderA (->) Rational Int where
    lessThanA (a, b) = greaterThan b a
    leqA (a, b) = geq b a


{- operations on Integers -}

instance CanNegA (->) Integer where
    negA a = P.negate a
    
instance CanNegSameType Integer

instance CanAbsA (->) Integer where
    absA a = P.abs a
    
instance CanAbsSameType Integer

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
    
instance CanRecipA (->) Integer where
    type RecipTypeA (->) Integer = Rational
    recipA a = 1 % a

instance CanDiv Integer Integer -- the default implementation is fine
    
{- operations on Rationals -}
    
instance CanNegA (->) Rational where
    negA a = P.negate a

instance CanNegSameType Rational
    
instance CanAbsA (->) Rational where
    absA a = P.abs a

instance CanAbsSameType Rational

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


