{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Real.CauchyReal 
(
    CauchyReal,
    showCauchyReal,
    cauchyReal2ball,
    rational2CauchyReal,
    pi
)
where

import Prelude hiding ((+),(*),(/),(-),pi,fromInteger,fromRational)
--import qualified Prelude as P

import AERN2.Real.MPFloat (prec)
import AERN2.Real.MPBall
import AERN2.Real.IntegerRational ()
import AERN2.Real.Operations
--import AERN2.Real.OperationsToBall ()

{-| Invariant: For any @(CauchyReal seq)@ it holds @ball_error (seq i) <= 2^^(-i)@ -}
data CauchyReal = CauchyReal (Integer -> MPBall) 

cauchyReal2ball :: CauchyReal -> Integer -> MPBall
cauchyReal2ball (CauchyReal getBall) i = getBall i

showCauchyReal :: Integer -> CauchyReal -> String
showCauchyReal i r = show (cauchyReal2ball r i)

convergent2Cauchy :: 
    (Integer -> MPBall) -> (Integer -> MPBall)
convergent2Cauchy convergentSeq i =
    aux 2 3
    where
    aux j j'
        | ballAccuracy xj >= i = xj
        | j > maxPrecision = error "convergent2Cauchy: the sequence either converges too slowly or it does not converge"
        | otherwise = aux j' (j+j') -- try precisions following the Fibonacci sequence
        where
        xj = convergentSeq j
        maxPrecision = 1000000

rational2CauchyReal :: Rational -> CauchyReal
rational2CauchyReal q =
    CauchyReal $ convergent2Cauchy $ \ p -> rational2MPBall (prec p) q 

pi :: CauchyReal
pi = CauchyReal piByAccuracy
    
piByAccuracy :: Integer -> MPBall
piByAccuracy =
    convergent2Cauchy (\ p -> piBallUsingPrecision (prec p))

{- operations mixing MPBall and CauchyReal, resulting in an MPBall -}

instance
    CanAdd MPBall CauchyReal 
    where
    type AddType MPBall CauchyReal = MPBall
    add a (CauchyReal b) = add a (b (ballAccuracy a))

instance
    CanAdd CauchyReal  MPBall 
    where
    type AddType CauchyReal MPBall = MPBall
    add (CauchyReal a) b = add (a (ballAccuracy b)) b

instance CanAddThis MPBall CauchyReal

instance
    CanSub MPBall CauchyReal 
    where
    type SubType MPBall CauchyReal = MPBall
    sub a (CauchyReal b) = sub a (b (ballAccuracy a))

instance
    CanSub CauchyReal  MPBall 
    where
    type SubType CauchyReal MPBall = MPBall
    sub (CauchyReal a) b = sub (a (ballAccuracy b)) b

instance CanSubThis MPBall CauchyReal

instance
    CanMul MPBall CauchyReal 
    where
    type MulType MPBall CauchyReal = MPBall
    mul a (CauchyReal b) = mul a (b (ballAccuracy a))

instance
    CanMul CauchyReal  MPBall 
    where
    type MulType CauchyReal MPBall = MPBall
    mul (CauchyReal a) b = mul (a (ballAccuracy b)) b

instance CanMulBy MPBall CauchyReal

instance
    CanDiv MPBall CauchyReal 
    where
    type DivType MPBall CauchyReal = MPBall
    div a (CauchyReal b) = mul a (b (ballAccuracy a))

instance
    CanDiv CauchyReal  MPBall 
    where
    type DivType CauchyReal MPBall = MPBall
    div (CauchyReal a) b = mul (a (ballAccuracy b)) b

instance CanDivBy MPBall CauchyReal


