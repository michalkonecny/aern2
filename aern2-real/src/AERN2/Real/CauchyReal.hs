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

import Prelude hiding ((+),(*),(/),(-),abs,recip,pi,fromInteger,fromRational,sqrt,sin,cos)
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
        | getAccuracy xj >= i = xj
        | j > maxPrecision = error "convergent2Cauchy: the sequence either converges too slowly or it does not converge"
        | otherwise = aux j' (j+j') -- try precisions following the Fibonacci sequence
        where
        xj = convergentSeq j
        maxPrecision = 1000000

rational2CauchyReal :: Rational -> CauchyReal
rational2CauchyReal q =
    CauchyReal $ convergent2Cauchy $ \ p -> fromRationalP (prec p) q 

pi :: CauchyReal
pi = CauchyReal piByAccuracy
    
piByAccuracy :: Integer -> MPBall
piByAccuracy =
    convergent2Cauchy (\ p -> piBallUsingPrecision (prec p))

{- Operations among CauchyReal's -}

instance CanNeg CauchyReal where
    type NegType CauchyReal = CauchyReal
    neg (CauchyReal getB) = CauchyReal (\i -> neg $ getB i)

instance CanNegSameType CauchyReal

instance CanAbs CauchyReal where
    type AbsType CauchyReal = CauchyReal
    abs (CauchyReal getB) = CauchyReal (\i -> abs $ getB i)

instance CanAbsSameType CauchyReal

{- TODO: preserve fast convergence
instance CanRecip CauchyReal where
    type RecipType CauchyReal = CauchyReal
    recip (CauchyReal getB) = CauchyReal (\i -> recip $ getB i)

instance CanRecipSameType CauchyReal
-}

instance CanAdd CauchyReal CauchyReal where
    type AddType CauchyReal CauchyReal = CauchyReal
    add (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal (\i -> (getB1 (i+1)) + (getB2 (i+1)))

instance CanAddThis CauchyReal CauchyReal

instance CanAddSameType CauchyReal

instance (CanSub CauchyReal CauchyReal)  
        
instance CanSubThis CauchyReal CauchyReal

instance CanSubSameType CauchyReal

{- TODO: preserve fast convergence
instance CanMul CauchyReal CauchyReal where
    type MulType CauchyReal CauchyReal = CauchyReal
    mul (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal (\i -> (getB1 i) * (getB2 i))

instance CanMulBy CauchyReal CauchyReal

instance CanMulSameType CauchyReal

instance CanDiv CauchyReal CauchyReal where
    type DivType CauchyReal CauchyReal = CauchyReal
    div (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal (\i -> (getB1 i) / (getB2 i))

instance CanDivBy CauchyReal CauchyReal

instance CanDivSameType CauchyReal
-}

{- CauchyReal-Integer operations -}

instance CanAdd Integer CauchyReal where
    type AddType Integer CauchyReal = CauchyReal
    add a (CauchyReal getB2) = CauchyReal (\i -> a + (getB2 i))

instance CanSub Integer CauchyReal

instance CanAdd CauchyReal Integer where
    type AddType CauchyReal Integer = CauchyReal
    add (CauchyReal getB1) b = CauchyReal (\i -> (getB1 i) + b)

instance CanAddThis CauchyReal Integer

instance CanSub CauchyReal Integer

instance CanSubThis CauchyReal Integer

{- TODO: preserve fast convergence

instance CanMul Integer CauchyReal where
    type MulType Integer CauchyReal = CauchyReal
    mul a (CauchyReal getB2) = CauchyReal (\i -> a * (getB2 i))

instance CanMul CauchyReal Integer where
    type MulType CauchyReal Integer = CauchyReal
    mul (CauchyReal getB1) b = CauchyReal (\i -> (getB1 i) * b)

instance CanMulBy CauchyReal Integer

instance CanDiv Integer CauchyReal where
    type DivType Integer CauchyReal = CauchyReal
    div a (CauchyReal getB2) = CauchyReal (\i -> a / (getB2 i))

instance CanDiv CauchyReal Integer where
    type DivType CauchyReal Integer = CauchyReal
    div (CauchyReal getB1) b = CauchyReal (\i -> (getB1 i) / b)

instance CanDivBy CauchyReal Integer
-}

instance CanSqrt Integer where
    type SqrtType Integer = CauchyReal
    sqrt x = CauchyReal (convergent2Cauchy $ \p -> sqrt (fromIntegerP (prec p) x))      
        
instance CanSineCosine Integer where
    type SineCosineType Integer = CauchyReal
    sin x = CauchyReal (convergent2Cauchy $ \p -> sin (fromIntegerP (prec p) x))
    cos x = CauchyReal (convergent2Cauchy $ \p -> cos (fromIntegerP (prec p) x))


{- CauchyReal-Rational operations -}

instance CanAdd Rational CauchyReal where
    type AddType Rational CauchyReal = CauchyReal
    add a (CauchyReal getB2) = CauchyReal (\i -> a + (getB2 i))

instance CanSub Rational CauchyReal

instance CanAdd CauchyReal Rational where
    type AddType CauchyReal Rational = CauchyReal
    add (CauchyReal getB1) b = CauchyReal (\i -> (getB1 i) + b)

instance CanAddThis CauchyReal Rational

instance CanSub CauchyReal Rational

instance CanSubThis CauchyReal Rational

{- TODO: preserve fast convergence
instance CanMul Rational CauchyReal where
    type MulType Rational CauchyReal = CauchyReal
    mul a (CauchyReal getB2) = CauchyReal (\i -> a * (getB2 i))

instance CanMul CauchyReal Rational where
    type MulType CauchyReal Rational = CauchyReal
    mul (CauchyReal getB1) b = CauchyReal (\i -> (getB1 i) * b)

instance CanMulBy CauchyReal Rational

instance CanDiv Rational CauchyReal where
    type DivType Rational CauchyReal = CauchyReal
    div a (CauchyReal getB2) = CauchyReal (\i -> a / (getB2 i))

instance CanDiv CauchyReal Rational where
    type DivType CauchyReal Rational = CauchyReal
    div (CauchyReal getB1) b = CauchyReal (\i -> (getB1 i) / b)

instance CanDivBy CauchyReal Rational
-}

instance CanSqrt Rational where
    type SqrtType Rational = CauchyReal
    sqrt x = CauchyReal (convergent2Cauchy $ \p -> sqrt (fromRationalP (prec p) x))      
        
instance CanSineCosine Rational where
    type SineCosineType Rational = CauchyReal
    sin x = CauchyReal (convergent2Cauchy $ \p -> sin (fromRationalP (prec p) x))
    cos x = CauchyReal (convergent2Cauchy $ \p -> cos (fromRationalP (prec p) x))


{- operations mixing MPBall and CauchyReal, resulting in an MPBall -}

instance
    CanAdd MPBall CauchyReal 
    where
    type AddType MPBall CauchyReal = MPBall
    add a (CauchyReal b) = add a (b (getAccuracy a))

instance
    CanAdd CauchyReal  MPBall 
    where
    type AddType CauchyReal MPBall = MPBall
    add (CauchyReal a) b = add (a (getAccuracy b)) b

instance CanAddThis MPBall CauchyReal

instance
    CanSub MPBall CauchyReal 
    where
    type SubType MPBall CauchyReal = MPBall
    sub a (CauchyReal b) = sub a (b (getAccuracy a))

instance
    CanSub CauchyReal  MPBall 
    where
    type SubType CauchyReal MPBall = MPBall
    sub (CauchyReal a) b = sub (a (getAccuracy b)) b

instance CanSubThis MPBall CauchyReal

instance
    CanMul MPBall CauchyReal 
    where
    type MulType MPBall CauchyReal = MPBall
    mul a (CauchyReal b) = mul a (b (getAccuracy a))

instance
    CanMul CauchyReal  MPBall 
    where
    type MulType CauchyReal MPBall = MPBall
    mul (CauchyReal a) b = mul (a (getAccuracy b)) b

instance CanMulBy MPBall CauchyReal

instance
    CanDiv MPBall CauchyReal 
    where
    type DivType MPBall CauchyReal = MPBall
    div a (CauchyReal b) = mul a (b (getAccuracy a))

instance
    CanDiv CauchyReal  MPBall 
    where
    type DivType CauchyReal MPBall = MPBall
    div (CauchyReal a) b = mul (a (getAccuracy b)) b

instance CanDivBy MPBall CauchyReal


