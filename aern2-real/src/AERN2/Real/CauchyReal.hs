{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Real.CauchyReal where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
--import qualified Prelude as P

import AERN2.Real.MPFloat
import AERN2.Real.MPFloatToBall
import AERN2.Real.Ball
import AERN2.Real.Operations
--import AERN2.Real.OperationsToBall ()

{-| Invariant: For any @(CauchyReal seq)@ it holds @ball_error (seq i) <= 2^^(-i)@ -}
data CauchyReal = CauchyReal (Integer -> MPBall) 

rational2CauchyReal :: Rational -> CauchyReal
rational2CauchyReal q =
    CauchyReal $ convergent2Cauchy $ \ i -> rational2MPBall (Precision i) q 

convergent2Cauchy :: 
    (Integer -> MPBall) -> Integer -> MPBall
convergent2Cauchy convergentSeq i =
    aux 2
    where
    aux j 
        | ballAccuracy xj >= i = xj
        | otherwise = aux (j*2)
        where
        xj = convergentSeq j 

cauchyReal2ball :: CauchyReal -> Integer -> MPBall
cauchyReal2ball (CauchyReal getBall) i = getBall i

showCauchyReal :: Integer -> CauchyReal -> String
showCauchyReal i r = show (cauchyReal2ball r i)

{- operations mixing Ball and CauchyReal -}

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


{- TODO
instance CanSub (Ball a) (CauchyReal a)

instance CanSub (CauchyReal a) (Ball a)

instance CanSubThis (CauchyReal a) (Ball a)

instance CanMul (Ball a) (CauchyReal a) where
    type MulType (Ball a) (CauchyReal a) = (Ball a)
    mul a b = (P.from(Ball a) a) P.* b

instance CanMul (CauchyReal a) (Ball a) where
    type MulType (CauchyReal a) (Ball a) = (Ball a)
    mul a b = a P.* (P.from(Ball a) b)

instance CanMulBy (CauchyReal a) (Ball a)

instance CanDiv (Ball a) (CauchyReal a) where
    type DivType (Ball a) (CauchyReal a) = (Ball a)
    div a b = (P.from(Ball a) a) P./ b

instance CanDiv (CauchyReal a) (Ball a) where
    type DivType (CauchyReal a) (Ball a) = (Ball a)
    div a b = a P./ (P.from(Ball a) b)

instance CanDivBy (CauchyReal a) (Ball a)
-}