{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, ConstraintKinds, FlexibleContexts, UndecidableInstances #-}
module AERN2.Num.Complex 
(
    Complex(..), complexI,
    complexCR2balls,
    showComplexCR
    ,
    HasComplexA, HasComplex,
    CanBeComplexA, complexA, complexNamedA, complexListA, complexListNamedA, CanBeComplex, complex, complexList
)
where

import AERN2.Num.Operations
import AERN2.Num.CauchyReal
import AERN2.Num.MPBall
import AERN2.Num.Accuracy

data Complex r = r :+ r 

infixr 5 :+

complexI :: (HasIntegers r) => Complex r
complexI = (convert 0) :+ (convert 1)

complexCR2balls :: (Complex CauchyReal) -> Accuracy -> (MPBall, MPBall)
complexCR2balls (r :+ i) a = 
    (cauchyReal2ball r a, cauchyReal2ball i a)

showComplexCR :: Accuracy -> (Complex CauchyReal) -> String
showComplexCR a (r :+ i) = 
    "(" ++ show (cauchyReal2ball r a) ++ ":+" ++ show (cauchyReal2ball i a) ++ ")"

instance
    (ConvertibleA (->) r1 r2)
    => 
    ConvertibleA (->) (Complex r1) (Complex r2) 
    where
    convertA (r :+ i) = (convert r :+ convert i)

type HasComplexA r to = ConvertibleA to (Complex r)
type HasComplex r = HasComplexA r (->)



type CanBeComplexA r to a = ConvertibleA to a (Complex r)
complexA :: (CanBeComplexA r to a) => a `to` (Complex r)
complexA = convertA
complexNamedA :: (CanBeComplexA r to a) => String -> a `to` (Complex r)
complexNamedA = convertNamedA
complexListA :: (CanBeComplexA r to a) => [a] `to` [(Complex r)]
complexListA = convertListA
complexListNamedA :: (CanBeComplexA r to a) => String -> [a] `to` [(Complex r)]
complexListNamedA = convertListNamedA
type CanBeComplex r a = CanBeComplexA r (->) a
complex :: (CanBeComplex r a) => a -> (Complex r)
complex = convert
complexList :: (CanBeComplex r a) => [a] -> [(Complex r)]
complexList = convertList

-- | HasIntegers (Complex r), CanBeComplex Integer
instance (HasIntegers r) => ConvertibleA (->) Integer (Complex r) where
    convertA n =
        (convert n) :+ (convert 0)

-- | HasRationals (Complex r), CanBeComplex Rational
instance (HasRationals r) => ConvertibleA (->) Rational (Complex r) where
    convertA q = (convert q) :+ (convert 0.0)

-- | HasCauchyReals (Complex r), CanBeComplex CauchyReal
instance (HasCauchyReals r, HasIntegers r) => ConvertibleA (->) CauchyReal (Complex r) where
    convertA q = (convert q) :+ (convert 0)

{- Comparison of complex numbers -}

instance (HasEqA (->) r r) => HasEqA (->) (Complex r) (Complex r) where
    type EqCompareTypeA (->) (Complex r) (Complex r) = EqCompareTypeA (->) r r 
    equalToA (r1 :+ i1, r2 :+ i2) = r1 == r2 && i1 == i2

instance 
    (HasOrderA (->) r r, HasEqA (->) r r,
     EqCompareTypeA (->) r r ~ OrderCompareTypeA (->) r r) 
    => 
    HasOrderA (->) (Complex r) (Complex r) 
    where
    type OrderCompareTypeA (->) (Complex r) (Complex r) = OrderCompareTypeA (->) r r 
    lessThanA (r1 :+ i1, r2 :+ i2) = (r1 <= r2 && i1 <= i2) && (r1 /= r2 || i1 /= i2)
    leqA (r1 :+ i1, r2 :+ i2) = (r1 <= r2 && i1 <= i2)


instance 
    (HasIntegers r, HasEqA (->) r r) 
    => 
    HasEqA (->) (Complex r) Integer 
    where
    type EqCompareTypeA (->) (Complex r) Integer = EqCompareTypeA (->) (Complex r) (Complex r)
    equalToA (c, n) = equalTo c nC
        where 
        nC = complex n
        _ = [c, nC] 

instance (HasIntegers r, HasEqA (->) r r) => HasEqA (->) Integer (Complex r) where
    type EqCompareTypeA (->) Integer (Complex r) = EqCompareTypeA (->) (Complex r) (Complex r)
    equalToA (n, c) = equalTo c n

instance
    (HasIntegers r,
     HasOrderA (->) r r, HasEqA (->) r r,
     EqCompareTypeA (->) r r ~ OrderCompareTypeA (->) r r) 
    => 
    HasOrderA (->) (Complex r) Integer 
    where
    type OrderCompareTypeA (->) (Complex r) Integer = OrderCompareTypeA (->) (Complex r) (Complex r)
    lessThanA (c, n) = lessThan c nC
        where 
        nC = complex n
        _ = [c, nC] 
    leqA (c, n) = leq c nC
        where 
        nC = complex n
        _ = [c, nC] 

instance
    (HasIntegers r,
     HasOrderA (->) r r, HasEqA (->) r r,
     EqCompareTypeA (->) r r ~ OrderCompareTypeA (->) r r) 
    => 
    HasOrderA (->) Integer (Complex r) 
    where
    type OrderCompareTypeA (->) Integer (Complex r) = OrderCompareTypeA (->) (Complex r) (Complex r)
    lessThanA (n, c) = lessThan nC c
        where 
        nC = complex n
        _ = [c, nC] 
    leqA (n, c) = leq nC c
        where 
        nC = complex n
        _ = [c, nC] 

{- TODO: 
    * Comparisons between Complex r and Rational.
    * Comparisons between Complex r and CauchyReal.

instance HasEqA (->) (Complex r) Rational where
    equalToA (c, n) = equalTo c (complex n) 

instance HasOrderA (->) (Complex r) Rational where
    lessThanA (c, n) = lessThan c (complex n) 
    leqA (c, n) = leq c (complex n) 

instance HasEqA (->) Rational (Complex r) where
    equalToA (n, c) = equalTo (complex n) c 

instance HasOrderA (->) Rational (Complex r) where
    lessThanA (n, c) = lessThan (complex n) c 
    leqA (n, c) = leq (complex n) c

instance HasEqA (->) (Complex r) CauchyReal where
    equalToA (c, n) = equalTo c (complex n) 

instance HasOrderA (->) (Complex r) CauchyReal where
    lessThanA (c, n) = lessThan c (complex n) 
    leqA (c, n) = leq c (complex n) 

instance HasEqA (->) CauchyReal (Complex r) where
    equalToA (n, c) = equalTo (complex n) c 

instance HasOrderA (->) CauchyReal (Complex r) where
    lessThanA (n, c) = lessThan (complex n) c 
    leqA (n, c) = leq (complex n) c
-}

{- Operations among (Complex r) numbers -}

instance (CanNegSameTypeA (->) r) => CanNegA (->) (Complex r) where
    negA (r :+ i) = (neg r) :+ (neg i)

instance (CanNegSameTypeA (->) r) => CanNegSameType (Complex r)

instance (CanSqrtSameTypeA (->) r, RingA (->) r) => CanAbsA (->) (Complex r) where
    type AbsTypeA (->) (Complex r) = r
    absA (r :+ i) = sqrt (r*r + i*i)

instance (FieldA (->) r) => CanRecipA (->) (Complex r) where
    recipA a = 1/a

instance (FieldA (->) r) => CanRecipSameType (Complex r)

instance (CanAddSameTypeA (->) r) => CanAddA (->) (Complex r) (Complex r) where
    addA (r1 :+ i1, r2 :+ i2) = (r1 + r2) :+ (i1 + i2)

instance (CanAddSameTypeA (->) r) => CanAddThis (Complex r) (Complex r)

instance (CanAddSameTypeA (->) r) => CanAddSameType (Complex r)

instance (CanAddSameTypeA (->) r, CanNegSameTypeA (->) r) => (CanSub (Complex r) (Complex r))  
        
instance (CanAddSameTypeA (->) r, CanNegSameTypeA (->) r) => CanSubThis (Complex r) (Complex r)

instance (CanAddSameTypeA (->) r, CanNegSameTypeA (->) r) => CanSubSameType (Complex r)

instance (RingA (->) r) => CanMulA (->) (Complex r) (Complex r) where
    mulA (r1 :+ i1, r2 :+ i2) =
        (r1 * r2 - i1 * i2) :+ (r1 * i2 + r2 * i1)


instance (RingA (->) r) => CanMulBy (Complex r) (Complex r)

instance (RingA (->) r) => CanMulSameType (Complex r)

instance (FieldA (->) r) => CanDivA (->) (Complex r) (Complex r) where
    divA (r1 :+ i1, r2 :+ i2) =
        ((r1 * r2 + i1 * i2)/d) :+ ((r2 * i1 - r1 * i2)/d)
        where
        d = r2*r2 + i2 * i2
        
instance (FieldA (->) r) => CanDivBy (Complex r) (Complex r)

instance (FieldA (->) r) => CanDivSameType (Complex r)

instance (RingA (->) r, OrderCompareTypeA (->) r r ~ EqCompareTypeA (->) r r) => 
    Ring (Complex r)
instance (FieldA (->) r, OrderCompareTypeA (->) r r ~ EqCompareTypeA (->) r r) => 
    Field (Complex r)

{- (Complex r)-Integer operations -}

instance (CanAddThisA (->) r Integer) => CanAddA (->) Integer (Complex r) where
    type AddTypeA (->) Integer (Complex r) = (Complex r)
    addA (a, r :+ i) = (a + r :+ i) 


instance (CanAddThisA (->) r Integer, CanNegSameTypeA (->) r) => CanSub Integer (Complex r)

instance (CanAddThisA (->) r Integer) => CanAddA (->) (Complex r) Integer where
    type AddTypeA (->) (Complex r) Integer = (Complex r)
    addA (a,b) = add b a 

instance (CanAddThisA (->) r Integer) => CanAddThis (Complex r) Integer

instance (CanAddThisA (->) r Integer) => CanSub (Complex r) Integer

instance (CanAddThisA (->) r Integer) => CanSubThis (Complex r) Integer

instance (CanMulByA (->) r Integer) => CanMulA (->) Integer (Complex r) where
    type MulTypeA (->) Integer (Complex r) = (Complex r)
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance (CanMulByA (->) r Integer) => CanMulA (->) (Complex r) Integer where
    type MulTypeA (->) (Complex r) Integer = (Complex r)
    mulA (a, b) = mul b a 

instance (CanMulByA (->) r Integer) => CanMulBy (Complex r) Integer

instance (FieldA (->) r) => CanDivA (->) Integer (Complex r) where
    type DivTypeA (->) Integer (Complex r) = (Complex r)
    divA (a, b) = aC / b
        where
        aC = complex a
        _ = [aC,b]

instance (CanDivByA (->) r Integer) => CanDivA (->) (Complex r) Integer where
    type DivTypeA (->) (Complex r) Integer = (Complex r)
    divA (r :+ i, a) = r / a :+ i / a 

instance (CanDivByA (->) r Integer) => CanDivBy (Complex r) Integer

{- (Complex r)-Rational operations -}

instance (CanAddThisA (->) r Rational) => CanAddA (->) Rational (Complex r) where
    type AddTypeA (->) Rational (Complex r) = (Complex r)
    addA (a, r :+ i) = (a + r :+ i) 


instance (CanAddThisA (->) r Rational, CanNegSameTypeA (->) r) => CanSub Rational (Complex r)

instance (CanAddThisA (->) r Rational) => CanAddA (->) (Complex r) Rational where
    type AddTypeA (->) (Complex r) Rational = (Complex r)
    addA (a,b) = add b a 

instance (CanAddThisA (->) r Rational) => CanAddThis (Complex r) Rational

instance (CanAddThisA (->) r Rational) => CanSub (Complex r) Rational

instance (CanAddThisA (->) r Rational) => CanSubThis (Complex r) Rational

instance (CanMulByA (->) r Rational) => CanMulA (->) Rational (Complex r) where
    type MulTypeA (->) Rational (Complex r) = (Complex r)
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance (CanMulByA (->) r Rational) => CanMulA (->) (Complex r) Rational where
    type MulTypeA (->) (Complex r) Rational = (Complex r)
    mulA (a, b) = mul b a 

instance (CanMulByA (->) r Rational) => CanMulBy (Complex r) Rational

instance (FieldA (->) r) => CanDivA (->) Rational (Complex r) where
    type DivTypeA (->) Rational (Complex r) = (Complex r)
    divA (a, b) = aC / b
        where
        aC = complex a
        _ = [aC,b]

instance (CanDivByA (->) r Rational) => CanDivA (->) (Complex r) Rational where
    type DivTypeA (->) (Complex r) Rational = (Complex r)
    divA (r :+ i, a) = r / a :+ i / a 

instance (CanDivByA (->) r Rational) => CanDivBy (Complex r) Rational

{- (Complex r)-CauchyReal operations -}

instance (CanAddThisA (->) r CauchyReal) => CanAddA (->) CauchyReal (Complex r) where
    type AddTypeA (->) CauchyReal (Complex r) = (Complex r)
    addA (a, r :+ i) = (a + r :+ i) 


instance (CanAddThisA (->) r CauchyReal, CanNegSameTypeA (->) r) => CanSub CauchyReal (Complex r)

instance (CanAddThisA (->) r CauchyReal) => CanAddA (->) (Complex r) CauchyReal where
    type AddTypeA (->) (Complex r) CauchyReal = (Complex r)
    addA (a,b) = add b a 

instance (CanAddThisA (->) r CauchyReal) => CanAddThis (Complex r) CauchyReal

instance (CanAddThisA (->) r CauchyReal) => CanSub (Complex r) CauchyReal

instance (CanAddThisA (->) r CauchyReal) => CanSubThis (Complex r) CauchyReal

instance (CanMulByA (->) r CauchyReal) => CanMulA (->) CauchyReal (Complex r) where
    type MulTypeA (->) CauchyReal (Complex r) = (Complex r)
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance (CanMulByA (->) r CauchyReal) => CanMulA (->) (Complex r) CauchyReal where
    type MulTypeA (->) (Complex r) CauchyReal = (Complex r)
    mulA (a, b) = mul b a 

instance (CanMulByA (->) r CauchyReal) => CanMulBy (Complex r) CauchyReal

instance (FieldA (->) r, HasCauchyReals r) => CanDivA (->) CauchyReal (Complex r) where
    type DivTypeA (->) CauchyReal (Complex r) = (Complex r)
    divA (a, b) = aC / b
        where
        aC = complex a
        _ = [aC,b]

instance (CanDivByA (->) r CauchyReal) => CanDivA (->) (Complex r) CauchyReal where
    type DivTypeA (->) (Complex r) CauchyReal = (Complex r)
    divA (r :+ i, a) = r / a :+ i / a 

instance (CanDivByA (->) r CauchyReal) => CanDivBy (Complex r) CauchyReal

{- Selected complex functions -}


instance CanSqrtA (->) (Complex r) where
    sqrtA = error "Complex sqrt not implemented yet"

instance CanSqrtSameType (Complex r)

instance 
    (RingA (->) r, CanExpSameTypeA (->) r, CanSineCosineSameTypeA (->) r) 
    => 
    CanExpA (->) (Complex r) where
    expA (r :+ i) =
        (exp r :+ convert 0) * (cos i :+ sin i)

instance
    (RingA (->) r, CanExpSameTypeA (->) r, CanSineCosineSameTypeA (->) r) 
    => 
    CanExpSameType (Complex r)
    
instance CanSineCosineA (->) (Complex r) where
    sinA = error "(Complex r) sin not implemented yet"
    cosA = error "(Complex r) cos not implemented yet"

instance CanSineCosineSameType (Complex r)
