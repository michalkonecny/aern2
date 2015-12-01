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

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Real.MPFloat (prec)
import AERN2.Real.MPBall
import AERN2.Real.IntegerRational ()
import AERN2.Real.Operations
--import AERN2.Real.OperationsToBall ()

import Debug.Trace (trace)
_ = trace

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

{-
Typically ensureAccuracy1 is called with a j such that the result is of
accuracy >= i.  In some cases j needs to be slightly increased.  
For example:

*AERN2.Real.Examples> cauchyReal2ball (10 * pi) 138
ensureAccuracy1: i = 138; j = 141; result accuracy = 137
ensureAccuracy1: i = 138; j = 142; result accuracy = 137
ensureAccuracy1: i = 138; j = 143; result accuracy = 226
[31.41592653589793 ± 5.216071149404186e-69]

*AERN2.Real.Examples> cauchyReal2ball (pi / 10) 56
ensureAccuracy1: i = 56; j = 53; result accuracy = 55
ensureAccuracy1: i = 56; j = 54; result accuracy = 89
[3.141592653589793e-1 ± 1.454028420503369e-27]

-}
ensureAccuracy1 ::
    Integer -> Integer -> (Integer -> MPBall) -> MPBall
ensureAccuracy1 i j getB 
    | getAccuracy result >= i = 
        -- TODO: disable this trace 
        trace (
            "ensureAccuracy1: i = " ++ show i ++ 
            "; j = " ++ show j ++ 
            "; result accuracy = " ++ (show $ getAccuracy result)
        ) $ 
        result
    | otherwise =
        -- TODO: disable this trace 
        trace (
            "ensureAccuracy1: i = " ++ show i ++ 
            "; j = " ++ show j ++ 
            "; result accuracy = " ++ (show $ getAccuracy result)
        ) $ 
        ensureAccuracy1 i (j+1) getB
    where
    result = getB j

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

instance CanMul Integer CauchyReal where
    type MulType Integer CauchyReal = CauchyReal
    mul a (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a * (getB2 j))
            where
            jInit 
                | a == 0 = 0
                | otherwise = i + aNorm
            aNorm = toInteger $ integerLog2 $ abs a 


instance CanMul CauchyReal Integer where
    type MulType CauchyReal Integer = CauchyReal
    mul a b = mul b a 

instance CanMulBy CauchyReal Integer

instance CanDiv Integer CauchyReal where
    type DivType Integer CauchyReal = CauchyReal
    div a (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a / (getB2 j))
            where
            jInit 
                | a == 0 = 0
                | otherwise = i + aNormLog + 2 * bRecipNormLog
            aNormLog = toInteger $ integerLog2 $ abs a
            bRecipNormLog = toInteger $ integerLog2 $ toIntegerUp $ abs $ (1 / getB2 i) 

instance CanDiv CauchyReal Integer where
    type DivType CauchyReal Integer = CauchyReal
    div (CauchyReal getB1) a = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> (getB1 j) / a)
            where
            jInit 
                | a == 0 = 0
                | otherwise = i - aNorm
            aNorm = toInteger $ integerLog2 $ abs a 

instance CanDivBy CauchyReal Integer

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

instance CanMul Rational CauchyReal where
    type MulType Rational CauchyReal = CauchyReal
    mul a (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a * (getB2 j))
            where
            jInit 
                | a == 0.0 = 0
                | otherwise = i + aNorm
            aNorm = toInteger $ integerLog2 $ ceiling $ abs a 

instance CanMul CauchyReal Rational where
    type MulType CauchyReal Rational = CauchyReal
    mul a b = mul b a

instance CanMulBy CauchyReal Rational

instance CanDiv Rational CauchyReal where
    type DivType Rational CauchyReal = CauchyReal
    div a (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a / (getB2 j))
            where
            jInit 
                | a == 0.0 = 0
                | otherwise = i + aNormLog + 2 * bRecipNormLog
            aNormLog = toInteger $ integerLog2 $ ceiling $ abs a
            bRecipNormLog = toInteger $ integerLog2 $ toIntegerUp $ abs $ (1 / getB2 i) 

instance CanDiv CauchyReal Rational where
    type DivType CauchyReal Rational = CauchyReal
    div a b = mul (1/b) a 

instance CanDivBy CauchyReal Rational

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


