{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, ConstraintKinds, FlexibleContexts #-}
module AERN2.Num.Complex 
(
    Complex(..), complexI,
    complex2balls,
    showComplex,
    HasComplexA, HasComplex,
    CanBeComplexA, complexA, complexNamedA, complexListA, complexListNamedA, CanBeComplex, complex, complexList,
    integer2Complex, rational2Complex, cauchyReal2Complex
)
where

import AERN2.Num.Operations
import AERN2.Num.CauchyReal
import AERN2.Num.MPBall
import AERN2.Num.Accuracy

data Complex = CauchyReal :+ CauchyReal 

infixr 5 :+

complexI :: Complex
complexI = (cauchyReal 0) :+ (cauchyReal 1)

complex2balls :: Complex -> Accuracy -> (MPBall, MPBall)
complex2balls (r :+ i) a = 
    (cauchyReal2ball r a, cauchyReal2ball i a)

showComplex :: Accuracy -> Complex -> String
showComplex a (r :+ i) = 
    "(" ++ show (cauchyReal2ball r a) ++ ":+" ++ show (cauchyReal2ball i a) ++ ")"

instance ConvertibleA (->) Complex Complex where convertA = id; convertListA = id

type HasComplexA to = ConvertibleA to Complex
type HasComplex = HasComplexA (->)

type CanBeComplexA to a = ConvertibleA to a Complex
complexA :: (CanBeComplexA to a) => a `to` Complex
complexA = convertA
complexNamedA :: (CanBeComplexA to a) => String -> a `to` Complex
complexNamedA = convertNamedA
complexListA :: (CanBeComplexA to a) => [a] `to` [Complex]
complexListA = convertListA
complexListNamedA :: (CanBeComplexA to a) => String -> [a] `to` [Complex]
complexListNamedA = convertListNamedA
type CanBeComplex a = CanBeComplexA (->) a
complex :: (CanBeComplex a) => a -> Complex
complex = convert
complexList :: (CanBeComplex a) => [a] -> [Complex]
complexList = convertList

-- | HasIntegers Complex, CanBeComplex Integer
instance ConvertibleA (->) Integer Complex where
    convertA n =
        (cauchyReal n) :+ (cauchyReal 0)

integer2Complex :: Integer -> Complex
integer2Complex = convert

-- | HasRationals Complex, CanBeComplex Rational
instance ConvertibleA (->) Rational Complex where
    convertA q = (cauchyReal q) :+ (cauchyReal 0)

rational2Complex :: Rational -> Complex
rational2Complex = convert

instance ConvertibleA (->) CauchyReal Complex where
    convertA r = r :+ (cauchyReal 0)

cauchyReal2Complex :: CauchyReal -> Complex
cauchyReal2Complex = convert

{- Comparison of complex numbers -}

instance HasEqA (->) Complex Complex where
    equalToA (r1 :+ i1, r2 :+ i2) = r1 == r2 && i1 == i2

instance HasOrderA (->) Complex Complex where
    lessThanA (r1 :+ i1, r2 :+ i2) = (r1 <= r2 && i1 <= i2) && (r1 /= r2 || i1 /= i2)
    leqA (r1 :+ i1, r2 :+ i2) = (r1 <= r2 && i1 <= i2)

instance HasEqA (->) Complex Integer where
    equalToA (c, n) = equalTo c (complex n) 

instance HasOrderA (->) Complex Integer where
    lessThanA (c, n) = lessThan c (complex n) 
    leqA (c, n) = leq c (complex n) 

instance HasEqA (->) Integer Complex where
    equalToA (n, c) = equalTo (complex n) c 

instance HasOrderA (->) Integer Complex where
    lessThanA (n, c) = lessThan (complex n) c 
    leqA (n, c) = leq (complex n) c

instance HasEqA (->) Complex Rational where
    equalToA (c, n) = equalTo c (complex n) 

instance HasOrderA (->) Complex Rational where
    lessThanA (c, n) = lessThan c (complex n) 
    leqA (c, n) = leq c (complex n) 

instance HasEqA (->) Rational Complex where
    equalToA (n, c) = equalTo (complex n) c 

instance HasOrderA (->) Rational Complex where
    lessThanA (n, c) = lessThan (complex n) c 
    leqA (n, c) = leq (complex n) c

instance HasEqA (->) Complex CauchyReal where
    equalToA (c, n) = equalTo c (complex n) 

instance HasOrderA (->) Complex CauchyReal where
    lessThanA (c, n) = lessThan c (complex n) 
    leqA (c, n) = leq c (complex n) 

instance HasEqA (->) CauchyReal Complex where
    equalToA (n, c) = equalTo (complex n) c 

instance HasOrderA (->) CauchyReal Complex where
    lessThanA (n, c) = lessThan (complex n) c 
    leqA (n, c) = leq (complex n) c

{- Operations among Complex numbers -}

instance CanNegA (->) Complex where
    negA (r :+ i) = (neg r) :+ (neg i)

instance CanNegSameType Complex

instance CanAbsA (->) Complex where
    type AbsTypeA (->) Complex = CauchyReal
    absA (r :+ i) = sqrt (r*r + i*i)

instance CanRecipA (->) Complex where
    recipA a = 1 / a

instance CanRecipSameType Complex

instance CanAddA (->) Complex Complex where
    addA (r1 :+ i1, r2 :+ i2) = (r1 + r2) :+ (i1 + i2)

instance CanAddThis Complex Complex

instance CanAddSameType Complex

instance (CanSub Complex Complex)  
        
instance CanSubThis Complex Complex

instance CanSubSameType Complex

instance CanMulA (->) Complex Complex where
    mulA (r1 :+ i1, r2 :+ i2) =
        (r1 * r2 - i1 * i2) :+ (r1 * i2 + r2 * i1)


instance CanMulBy Complex Complex

instance CanMulSameType Complex

instance CanDivA (->) Complex Complex where
    divA (r1 :+ i1, r2 :+ i2) =
        ((r1 * r2 + i1 * i2)/d) :+ ((r2 * i1 - r1 * i2)/d)
        where
        d = r2*r2 + i2 * i2
        
instance CanDivBy Complex Complex

instance CanDivSameType Complex

instance Ring Complex
instance Field Complex

{- Complex-Integer operations -}

instance CanAddA (->) Integer Complex where
    type AddTypeA (->) Integer Complex = Complex
    addA (a, r :+ i) = (a + r :+ i) 

instance CanSub Integer Complex

instance CanAddA (->) Complex Integer where
    type AddTypeA (->) Complex Integer = Complex
    addA (a,b) = add b a 

instance CanAddThis Complex Integer

instance CanSub Complex Integer

instance CanSubThis Complex Integer

instance CanMulA (->) Integer Complex where
    type MulTypeA (->) Integer Complex = Complex
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance CanMulA (->) Complex Integer where
    type MulTypeA (->) Complex Integer = Complex
    mulA (a, b) = mul b a 

instance CanMulBy Complex Integer

instance CanDivA (->) Integer Complex where
    type DivTypeA (->) Integer Complex = Complex
    divA (a, b) = (integer2Complex a) / b

instance CanDivA (->) Complex Integer where
    type DivTypeA (->) Complex Integer = Complex
    divA (r :+ i, a) = r / a :+ i / a 

instance CanDivBy Complex Integer

{- Complex-Rational operations -}

instance CanAddA (->) Rational Complex where
    type AddTypeA (->) Rational Complex = Complex
    addA (a, r :+ i) = (a + r :+ i) 

instance CanSub Rational Complex

instance CanAddA (->) Complex Rational where
    type AddTypeA (->) Complex Rational = Complex
    addA (a,b) = add b a 

instance CanAddThis Complex Rational

instance CanSub Complex Rational

instance CanSubThis Complex Rational

instance CanMulA (->) Rational Complex where
    type MulTypeA (->) Rational Complex = Complex
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance CanMulA (->) Complex Rational where
    type MulTypeA (->) Complex Rational = Complex
    mulA (a,b) = mul b a 

instance CanMulBy Complex Rational

instance CanDivA (->) Rational Complex where
    type DivTypeA (->) Rational Complex = Complex
    divA (a, b) = (rational2Complex a) / b

instance CanDivA (->) Complex Rational where
    type DivTypeA (->) Complex Rational = Complex
    divA (r :+ i, a) = r / a :+ i / a

instance CanDivBy Complex Rational

{- Complex-CauchyReal operations -}

instance CanAddA (->) CauchyReal Complex where
    type AddTypeA (->) CauchyReal Complex = Complex
    addA (a, r :+ i) = (a + r :+ i) 

instance CanSub CauchyReal Complex

instance CanAddA (->) Complex CauchyReal where
    type AddTypeA (->) Complex CauchyReal = Complex
    addA (a,b)= add b a 

instance CanAddThis Complex CauchyReal

instance CanSub Complex CauchyReal

instance CanSubThis Complex CauchyReal

instance CanMulA (->) CauchyReal Complex where
    type MulTypeA (->) CauchyReal Complex = Complex
    mulA (a, r :+ i) = (a * r :+ a * i) 

instance CanMulA (->) Complex CauchyReal where
    type MulTypeA (->) Complex CauchyReal = Complex
    mulA (a,b) = mul b a 

instance CanMulBy Complex CauchyReal

instance CanDivA (->) CauchyReal Complex where
    type DivTypeA (->) CauchyReal Complex = Complex
    divA (a, b) = (cauchyReal2Complex a) / b

instance CanDivA (->) Complex CauchyReal where
    type DivTypeA (->) Complex CauchyReal = Complex
    divA (r :+ i, a) = r / a :+ i / a

instance CanDivBy Complex CauchyReal

instance CanSqrtA (->) Complex where
    sqrtA = error "Complex sqrt not implemented yet"

instance CanSqrtSameType Complex

instance CanExpA (->) Complex where
    expA (r :+ i) =
        (exp r) * (cos i :+ sin i)

instance CanExpSameType Complex

instance CanSineCosineA (->) Complex where
    sinA = error "Complex sin not implemented yet"
    cosA = error "Complex cos not implemented yet"

instance CanSineCosineSameType Complex

