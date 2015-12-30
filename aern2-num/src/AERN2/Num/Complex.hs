{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AERN2.Num.Complex 
(
    Complex(..), complexI,
    complex2balls,
    showComplex,
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
complexI = (integer 0) :+ (integer 1)

complex2balls :: Complex -> Accuracy -> (MPBall, MPBall)
complex2balls (r :+ i) a = 
    (cauchyReal2ball r a, cauchyReal2ball i a)

showComplex :: Accuracy -> Complex -> String
showComplex a (r :+ i) = 
    "(" ++ show (cauchyReal2ball r a) ++ ":+" ++ show (cauchyReal2ball i a) ++ ")"

instance HasIntegers Complex where
    integer n =
        (integer n) :+ (integer 0)

integer2Complex :: Integer -> Complex
integer2Complex = integer

instance HasRationals Complex where
    rational q =
        (rational q) :+ (integer 0)

rational2Complex :: Rational -> Complex
rational2Complex = rational

cauchyReal2Complex :: CauchyReal -> Complex
cauchyReal2Complex r = r :+ (integer 0)

{- Operations among Complex numbers -}

instance CanNeg Complex where
    neg (r :+ i) = (neg r) :+ (neg i)

instance CanNegSameType Complex

instance CanAbs Complex where
    type AbsType Complex = CauchyReal
    abs (r :+ i) = sqrt (r*r + i*i)

instance CanRecip Complex where
    recip a = 1 / a

instance CanRecipSameType Complex

instance CanAdd Complex Complex where
    add (r1 :+ i1) (r2 :+ i2) = (r1 + r2) :+ (i1 + i2)

instance CanAddThis Complex Complex

instance CanAddSameType Complex

instance (CanSub Complex Complex)  
        
instance CanSubThis Complex Complex

instance CanSubSameType Complex

instance CanMul Complex Complex where
    mul (r1 :+ i1) (r2 :+ i2) =
        (r1 * r2 - i1 * i2) :+ (r1 * i2 + r2 * i1)

instance CanMulBy Complex Complex

instance CanMulSameType Complex

instance CanDiv Complex Complex where
    div (r1 :+ i1) (r2 :+ i2) =
        ((r1 * r2 + i1 * i2)/d) :+ ((r2 * i1 - r1 * i2)/d)
        where
        d = r2*r2 + i2 * i2
        
instance CanDivBy Complex Complex

instance CanDivSameType Complex

{- Complex-Integer operations -}

instance CanAdd Integer Complex where
    type AddType Integer Complex = Complex
    add a (r :+ i) = (a + r :+ i) 

instance CanSub Integer Complex

instance CanAdd Complex Integer where
    type AddType Complex Integer = Complex
    add = flip add 

instance CanAddThis Complex Integer

instance CanSub Complex Integer

instance CanSubThis Complex Integer

instance CanMul Integer Complex where
    type MulType Integer Complex = Complex
    mul a (r :+ i) = (a * r :+ a * i) 

instance CanMul Complex Integer where
    type MulType Complex Integer = Complex
    mul a b = mul b a 

instance CanMulBy Complex Integer

instance CanDiv Integer Complex where
    type DivType Integer Complex = Complex
    div a b = (integer2Complex a) / b

instance CanDiv Complex Integer where
    type DivType Complex Integer = Complex
    div (r :+ i) a = r / a :+ i / a 

instance CanDivBy Complex Integer

{- Complex-Rational operations -}

instance CanAdd Rational Complex where
    type AddType Rational Complex = Complex
    add a (r :+ i) = (a + r :+ i) 

instance CanSub Rational Complex

instance CanAdd Complex Rational where
    type AddType Complex Rational = Complex
    add = flip add 

instance CanAddThis Complex Rational

instance CanSub Complex Rational

instance CanSubThis Complex Rational

instance CanMul Rational Complex where
    type MulType Rational Complex = Complex
    mul a (r :+ i) = (a * r :+ a * i) 

instance CanMul Complex Rational where
    type MulType Complex Rational = Complex
    mul = flip mul 

instance CanMulBy Complex Rational

instance CanDiv Rational Complex where
    type DivType Rational Complex = Complex
    div a b = (rational2Complex a) / b

instance CanDiv Complex Rational where
    type DivType Complex Rational = Complex
    div (r :+ i) a = r / a :+ i / a

instance CanDivBy Complex Rational

{- Complex-CauchyReal operations -}

instance CanAdd CauchyReal Complex where
    type AddType CauchyReal Complex = Complex
    add a (r :+ i) = (a + r :+ i) 

instance CanSub CauchyReal Complex

instance CanAdd Complex CauchyReal where
    type AddType Complex CauchyReal = Complex
    add = flip add 

instance CanAddThis Complex CauchyReal

instance CanSub Complex CauchyReal

instance CanSubThis Complex CauchyReal

instance CanMul CauchyReal Complex where
    type MulType CauchyReal Complex = Complex
    mul a (r :+ i) = (a * r :+ a * i) 

instance CanMul Complex CauchyReal where
    type MulType Complex CauchyReal = Complex
    mul = flip mul 

instance CanMulBy Complex CauchyReal

instance CanDiv CauchyReal Complex where
    type DivType CauchyReal Complex = Complex
    div a b = (cauchyReal2Complex a) / b

instance CanDiv Complex CauchyReal where
    type DivType Complex CauchyReal = Complex
    div (r :+ i) a = r / a :+ i / a

instance CanDivBy Complex CauchyReal

instance CanExp Complex where
    exp (r :+ i) =
        (exp r) * (cos i :+ sin i)
