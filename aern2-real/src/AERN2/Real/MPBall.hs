{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Real.MPBall
    (MPBall(..), ballAccuracy,
     rational2MPBall, rationals2MPBall,
     piBallUsingPrecision) 
where

import Prelude hiding ((+),(*),(/),(-),abs,recip,fromInteger,fromRational)
--import qualified Prelude as P

import qualified AERN2.Real.ErrorBound as EB
import AERN2.Real.ErrorBound (ErrorBound(..))
import qualified AERN2.Real.MPFloat as MP
import AERN2.Real.MPFloat (MPFloat, Precision)
import AERN2.Real.Operations


data MPBall = MPBall { ball_value :: MPFloat, ball_error :: ErrorBound }

instance Show MPBall
    where
    show (MPBall x e) = "[" ++ show x ++ " ± " ++ show e ++ "]"

rationals2MPBall :: MP.Precision -> (Rational, Rational) -> MPBall 
rationals2MPBall p (x,e) =
    MPBall xUp (xe + eUp)
    where
    (MPBall xUp xe) = rational2MPBall p x
    eUp = EB.rational2ErrorBound e
    
rational2MPBall :: MP.Precision -> Rational -> MPBall
rational2MPBall p x =
    MPBall xUp (xUp `EB.subMP` xDn)
    where
    xUp = MP.rationalUp p x
    xDn = MP.rationalDown p x

ballAccuracy :: 
    MPBall -> Integer
ballAccuracy (MPBall _ e) = 
    EB.accuracyIndex e

ballNonZero :: MPBall -> Bool
ballNonZero (MPBall x e) =
    (MP.abs x) `MP.subDown` (EB.er2mp e) > MP.zero

instance CanNeg MPBall where
    type NegType MPBall = MPBall
    neg (MPBall x1 e1) = MPBall (MP.neg x1) e1

instance CanNegSameType MPBall

instance CanAdd MPBall MPBall where
    type AddType MPBall MPBall = MPBall
    add (MPBall x1 e1) (MPBall x2 e2) =
        MPBall sumUp ((sumUp `EB.subMP` sumDn) + e1 + e2)
        where
        sumUp = MP.addUp x1 x2
        sumDn = MP.addDown x1 x2

instance CanAddThis MPBall MPBall

instance CanAddSameType MPBall

instance
    (CanSub MPBall MPBall)  
    where
    type SubType MPBall MPBall = MPBall
    sub b1 b2 = add b1 (neg b2)
        
instance CanSubThis MPBall MPBall

instance CanSubSameType MPBall

instance CanMul MPBall MPBall where
    type MulType MPBall MPBall = MPBall
    mul (MPBall x1 e1) (MPBall x2 e2) =
        MPBall x12Up (e12 + e1*(EB.absMP x2) + e2*(EB.absMP x1) + e1*e2)
        where
        x12Up = MP.mulUp x1 x2 
        x12Down = MP.mulDown x1 x2
        e12 = EB.mp2ErrorBound $ x12Up `MP.subUp` x12Down

instance CanMulBy MPBall MPBall

instance CanMulSameType MPBall

instance CanDiv MPBall MPBall where
    type DivType MPBall MPBall = MPBall
    div (MPBall x1 e1) b2@(MPBall x2 e2) 
        | ballNonZero b2 =
            MPBall x12Up err
        | otherwise =
            error $ "Division by MPBall that contains 0: " ++ show b2
        where
        x12Up = MP.divUp x1 x2 
        x12Down = MP.divDown x1 x2
        e12 = EB.mp2ErrorBound $ x12Up `MP.subUp` x12Down
        err =
            ((e12 * (EB.mp2ErrorBound (MP.abs x2))) -- e12 * |x2|
             +
             e1
             +
             (EB.mp2ErrorBound (MP.abs x12Up) * e2) -- e2 * |x|
            ) 
            * 
            (EB.mp2ErrorBound $ MP.recipUp (MP.abs x2 `MP.subDown` (EB.er2mp e2))) 
                -- 1/(|x2| - e2) rounded upwards 
{-
A derivation of the above formula for an upper bound on the error:

    * e = 
        * = max ( (x1 ± e1) / (x2 ± e2) - x )
        * = max ( ( x1 ± e1 - (x*(x2 ± e2) ) / (x2 ± e2) )
        * ≤ max ( ( x1 ± e1 - ((x1/x2) ± e12)x2 ± x*e2 ) / (x2 ± e2) )
        * = max ( ( x1 ± e1 - x1 ± e12*x2 ± x*e2 ) / (x2 ± e2) )
        * = max ( ( ± e1 ± e12*x2 ± x*e2 ) / (x2 ± e2) )
        * ≤ (e1 + e12*|x2| + |x|*e2 ) / (|x2| - e2)
        * ≤ (e1 +^ e12*^|x2| +^ |x|*^e2 ) /^ (|x2| -. e2)
-}                


instance CanDivBy MPBall MPBall

instance CanDivSameType MPBall

piBallUsingPrecision :: Precision -> MPBall
piBallUsingPrecision p = MPBall piUp (piUp `EB.subMP` piDown)
    where
    piUp = MP.piUp p 
    piDown = MP.piDown p 

{- 
    TODO: Instances such as: 
        CanDivBy MPBall Integer 
        CanDivBy MPBall Rational
        CanCosine MPBall 
-} 

{- generic methods for computing real functions from MPFR-approximations -}

{-|
    Computes a real function @f@ from correctly rounded MPFR-approximations and a number @lip@ which is a
    Lipschitz constant for @f@, i.e. @|f(x) - f(y)| <= lip * |x - y|@ for all @x@,@y@.
-}
fromApproxWithLipschitz :: 
    (MPFloat -> MPFloat) {-^ @fDown@: a version of @f@ on MPFloat rounding *downwards* -} -> 
    (MPFloat -> MPFloat) {-^ @fUp@: a version of @f@ on MPFloat rounding *upwards* -} ->
    MPFloat {-^ @lip@ a Lipschitz constant for @f@, @lip > 0@ -} -> 
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
fromApproxWithLipschitz fDown fUp lip _x@(MPBall xc xe) = 
    MPBall fxc err
    where
    fxl = fDown xc
    fxu = fUp xc
    (MPBall fxc fxe) = endpoints2Ball fxl fxu 
    err = (EB.mp2ErrorBound lip) * xe  +  fxe

{-|
    Computes a *monotone* real function @f@ from correctly rounded MPFR-approximations.
-}
monotoneFromApprox :: 
    (MPFloat -> MPFloat) {-^ @fDown@: a version of @f@ on MPFloat rounding *downwards* -} -> 
    (MPFloat -> MPFloat) {-^ @fUp@: a version of @f@ on MPFloat rounding *upwards* -} -> 
    (MPBall -> MPBall) {-^ @f@ on MPBall rounding *outwards* -}
monotoneFromApprox fDown fUp x = 
    endpoints2Ball (fDown l) (fUp u)
    where
    (l,u) = ball2endpoints x

endpoints2Ball :: MPFloat -> MPFloat -> MPBall
endpoints2Ball l u =
    MPBall c e
    where
    c = MP.avgUp l u
    e = EB.mp2ErrorBound $ max (MP.distUp c l) (MP.distUp c u)

ball2endpoints :: MPBall -> (MPFloat, MPFloat)
ball2endpoints x = (l,u)
    where
    c    = ball_value x
    r    = er2mp (ball_error x)
    l   = c `MP.subDown` r
    u   = c `MP.addUp` r
    

{- common functions -}

instance CanSqrt MPBall where
        type SqrtType MPBall = MPBall
        sqrt x = monotoneFromApprox MP.sqrtDown MP.sqrtUp x     
        
instance CanSineCosine MPBall where
        type SineCosineType MPBall = MPBall
        sin x = fromApproxWithLipschitz MP.sinDown MP.sinUp (MP.integerUp (MP.prec 53) 1) x
        cos x = fromApproxWithLipschitz MP.cosDown MP.cosUp (MP.integerUp (MP.prec 53) 1) x
