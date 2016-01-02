{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, ConstraintKinds, FlexibleContexts #-}
module AERN2.Num.Complex 
(
    Complex(..), complexI,
    complexCR2balls,
    showComplexCR
    ,
    HasComplexA, HasComplex,
    CanBeComplexA, complexA, complexNamedA, complexListA, complexListNamedA, CanBeComplex, complex, complexList,
    integer2Complex, rational2Complex
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

integer2Complex :: (HasIntegers r) => Integer -> (Complex r)
integer2Complex = convert

-- | HasRationals (Complex r), CanBeComplex Rational
instance (HasRationals r) => ConvertibleA (->) Rational (Complex r) where
    convertA q = (convert q) :+ (convert 0.0)

rational2Complex :: (HasRationals r) => Rational -> (Complex r)
rational2Complex = convert

{- Comparison of complex numbers -}

instance (HasEqA (->) r r) => HasEqA (->) (Complex r) (Complex r) where
    type EqCompareTypeA (->) (Complex r) (Complex r) = EqCompareTypeA (->) r r 
    equalToA (r1 :+ i1, r2 :+ i2) = r1 == r2 && i1 == i2

{- TODO


instance HasOrderA (->) (Complex r) (Complex r) where
    lessThanA (r1 :+ i1, r2 :+ i2) = (r1 <= r2 && i1 <= i2) && (r1 /= r2 || i1 /= i2)
    leqA (r1 :+ i1, r2 :+ i2) = (r1 <= r2 && i1 <= i2)

instance HasEqA (->) (Complex r) Integer where
    equalToA (c, n) = equalTo c (complex n) 

instance HasOrderA (->) (Complex r) Integer where
    lessThanA (c, n) = lessThan c (complex n) 
    leqA (c, n) = leq c (complex n) 

instance HasEqA (->) Integer (Complex r) where
    equalToA (n, c) = equalTo (complex n) c 

instance HasOrderA (->) Integer (Complex r) where
    lessThanA (n, c) = lessThan (complex n) c 
    leqA (n, c) = leq (complex n) c

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

{- Operations among (Complex r) numbers -}

instance CanNegA (->) (Complex r) where
    negA (r :+ i) = (neg r) :+ (neg i)

instance CanNegSameType (Complex r)

instance CanAbsA (->) (Complex r) where
    type AbsTypeA (->) (Complex r) = CauchyReal
    absA (r :+ i) = sqrt (r*r + i*i)

instance CanRecipA (->) (Complex r) where
    recipA a = 1 / a

instance CanRecipSameType (Complex r)

instance CanAddA (->) (Complex r) (Complex r) where
    addA (r1 :+ i1, r2 :+ i2) = (r1 + r2) :+ (i1 + i2)

instance CanAddThis (Complex r) (Complex r)

instance CanAddSameType (Complex r)

instance (CanSub (Complex r) (Complex r))  
        
instance CanSubThis (Complex r) (Complex r)

instance CanSubSameType (Complex r)

instance CanMulA (->) (Complex r) (Complex r) where
    mulA (r1 :+ i1, r2 :+ i2) =
        (r1 * r2 - i1 * i2) :+ (r1 * i2 + r2 * i1)


instance CanMulBy (Complex r) (Complex r)

instance CanMulSameType (Complex r)

instance CanDivA (->) (Complex r) (Complex r) where
    divA (r1 :+ i1, r2 :+ i2) =
        ((r1 * r2 + i1 * i2)/d) :+ ((r2 * i1 - r1 * i2)/d)
        where
        d = r2*r2 + i2 * i2
        
instance CanDivBy (Complex r) (Complex r)

instance CanDivSameType (Complex r)

instance Ring (Complex r)
instance Field (Complex r)

{- (Complex r)-Integer operations -}

instance CanAddA (->) Integer (Complex r) where
    type AddTypeA (->) Integer (Complex r) = (Complex r)
    addA (a, r :+ i) = (a + r :+ i) 

instance CanSub Integer (Complex r)

instance CanAddA (->) (Complex r) Integer where
    type AddTypeA (->) (Complex r) Integer = (Complex r)
    addA (a,b) = add b a 

instance CanAddThis (Complex r) Integer

instance CanSub (Complex r) Integer

instance CanSubThis (Complex r) Integer

instance CanMulA (->) Integer (Complex r) where
    type MulTypeA (->) Integer (Complex r) = (Complex r)
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance CanMulA (->) (Complex r) Integer where
    type MulTypeA (->) (Complex r) Integer = (Complex r)
    mulA (a, b) = mul b a 

instance CanMulBy (Complex r) Integer

instance CanDivA (->) Integer (Complex r) where
    type DivTypeA (->) Integer (Complex r) = (Complex r)
    divA (a, b) = (integer2Complex a) / b

instance CanDivA (->) (Complex r) Integer where
    type DivTypeA (->) (Complex r) Integer = (Complex r)
    divA (r :+ i, a) = r / a :+ i / a 

instance CanDivBy (Complex r) Integer

{- (Complex r)-Rational operations -}

instance CanAddA (->) Rational (Complex r) where
    type AddTypeA (->) Rational (Complex r) = (Complex r)
    addA (a, r :+ i) = (a + r :+ i) 

instance CanSub Rational (Complex r)

instance CanAddA (->) (Complex r) Rational where
    type AddTypeA (->) (Complex r) Rational = (Complex r)
    addA (a,b) = add b a 

instance CanAddThis (Complex r) Rational

instance CanSub (Complex r) Rational

instance CanSubThis (Complex r) Rational

instance CanMulA (->) Rational (Complex r) where
    type MulTypeA (->) Rational (Complex r) = (Complex r)
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance CanMulA (->) (Complex r) Rational where
    type MulTypeA (->) (Complex r) Rational = (Complex r)
    mulA (a,b) = mul b a 

instance CanMulBy (Complex r) Rational

instance CanDivA (->) Rational (Complex r) where
    type DivTypeA (->) Rational (Complex r) = (Complex r)
    divA (a, b) = (rational2Complex a) / b

instance CanDivA (->) (Complex r) Rational where
    type DivTypeA (->) (Complex r) Rational = (Complex r)
    divA (r :+ i, a) = r / a :+ i / a

instance CanDivBy (Complex r) Rational

{- (Complex r)-CauchyReal operations -}

instance CanAddA (->) CauchyReal (Complex r) where
    type AddTypeA (->) CauchyReal (Complex r) = (Complex r)
    addA (a, r :+ i) = (a + r :+ i) 

instance CanSub CauchyReal (Complex r)

instance CanAddA (->) (Complex r) CauchyReal where
    type AddTypeA (->) (Complex r) CauchyReal = (Complex r)
    addA (a,b)= add b a 

instance CanAddThis (Complex r) CauchyReal

instance CanSub (Complex r) CauchyReal

instance CanSubThis (Complex r) CauchyReal

instance CanMulA (->) CauchyReal (Complex r) where
    type MulTypeA (->) CauchyReal (Complex r) = (Complex r)
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance CanMulA (->) (Complex r) CauchyReal where
    type MulTypeA (->) (Complex r) CauchyReal = (Complex r)
    mulA (a,b) = mul b a 

instance CanMulBy (Complex r) CauchyReal

instance CanDivA (->) CauchyReal (Complex r) where
    type DivTypeA (->) CauchyReal (Complex r) = (Complex r)
    divA (a, b) = (cauchyReal2Complex a) / b

instance CanDivA (->) (Complex r) CauchyReal where
    type DivTypeA (->) (Complex r) CauchyReal = (Complex r)
    divA (r :+ i, a) = r / a :+ i / a

instance CanDivBy (Complex r) CauchyReal

instance CanSqrtA (->) (Complex r) where
    sqrtA = error "Complex sqrt not implemented yet"

instance CanSqrtSameType (Complex r)

instance CanExpA (->) (Complex r) where
    expA (r :+ i) =
        (exp r) * (cos i :+ sin i)

instance CanExpSameType (Complex r)

instance CanSineCosineA (->) (Complex r) where
    sinA = error "(Complex r) sin not implemented yet"
    cosA = error "(Complex r) cos not implemented yet"

instance CanSineCosineSameType (Complex r)

-}