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
     rational2MPBall, rationals2MPBall) 
where

import Prelude hiding ((+),(*),(/),(-),abs,recip,fromInteger,fromRational)
--import qualified Prelude as P

import AERN2.Real.DoubleBound
import qualified AERN2.Real.MPFloat as MP
import AERN2.Real.MPFloat (MPFloat)
import AERN2.Real.Operations


data MPBall = MPBall { ball_value :: MPFloat, ball_error :: DoubleBound }

instance Show MPBall
    where
    show (MPBall x e) = "[" ++ show x ++ " ± " ++ show e ++ "]"

rationals2MPBall :: MP.Precision -> (Rational, Rational) -> MPBall 
rationals2MPBall p (x,e) =
    MPBall xUp (xe + eUp)
    where
    (MPBall xUp xe) = rational2MPBall p x
    eUp = rational2DoubleBound e
    
rational2MPBall :: MP.Precision -> Rational -> MPBall
rational2MPBall p x =
    MPBall xUp (xUp `MP.subDB` xDn)
    where
    xUp = MP.rationalUp p x
    xDn = MP.rationalDown p x

ballAccuracy :: 
    MPBall -> Integer
ballAccuracy (MPBall _ e) = 
    errorIndex e

instance CanNeg MPBall where
    type NegType MPBall = MPBall
    neg (MPBall x1 e1) = MPBall (MP.neg x1) e1

instance CanNegSameType MPBall

instance CanAdd MPBall MPBall where
    type AddType MPBall MPBall = MPBall
    add (MPBall x1 e1) (MPBall x2 e2) =
        MPBall sumUp ((sumUp `MP.subDB` sumDn) + e1 + e2)
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
        MPBall x12Up (e12 + e1*(MP.absDB x2) + e2*(MP.absDB x1) + e1*e2)
        where
        x12Up = MP.mulUp x1 x2 
        x12Down = MP.mulDown x1 x2
        e12 = MP.getDoubleBound $ MP.subUp x12Up x12Down

instance CanMulBy MPBall MPBall

instance CanMulSameType MPBall

{- 
    TODO: Instances such as: 
        CanAddThis MPBall Integer 
        CanAddThis MPBall Rational 
        CanSubThis MPBall Integer 
        CanSubThis MPBall Rational 
        CanMulBy MPBall Integer 
        CanMulBy MPBall Rational 
        CanDivBy MPBall Integer 
        CanDivBy MPBall Rational
        CanCosine MPBall 
-} 
