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
import AERN2.Real.IntegerRational
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

instance CanRecip CauchyReal where
    type RecipType CauchyReal = CauchyReal
    recip a = 1 / a

instance CanRecipSameType CauchyReal

instance CanAdd CauchyReal CauchyReal where
    type AddType CauchyReal CauchyReal = CauchyReal
    add (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal $
            \i -> ensureAccuracy2 i i i (\j1 j2 -> (getB1 j1) + (getB2 j2))

instance CanAddThis CauchyReal CauchyReal

instance CanAddSameType CauchyReal

instance (CanSub CauchyReal CauchyReal)  
        
instance CanSubThis CauchyReal CauchyReal

instance CanSubSameType CauchyReal

instance CanMul CauchyReal CauchyReal where
    type MulType CauchyReal CauchyReal = CauchyReal
    mul (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal getB
        where
        getB i =
            ensureAccuracy2 i jInit1 jInit2 (\j1 j2 -> (getB1 j1) * (getB2 j2))
            where
            jInit1 = 
                case maybeA2NormLog of
                    Just a2NormLog -> max 0 (i + a2NormLog + 1)
                    Nothing -> 0
            jInit2 = 
                case maybeA1NormLog of
                    Just a1NormLog -> max 0 (i + a1NormLog + 1)
                    Nothing -> 0
            maybeA1NormLog = getBallNormLog (getB1 i)   
            maybeA2NormLog = getBallNormLog (getB2 i)   

instance CanMulBy CauchyReal CauchyReal

instance CanMulSameType CauchyReal

instance CanDiv CauchyReal CauchyReal where
    type DivType CauchyReal CauchyReal = CauchyReal
    div (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal getB
        where
        getB i =
            ensureAccuracy2 i jInit1 jInit2 (\j1 j2 -> (getB1 j1) / (getB2 j2))
            -- TODO: increase j2 to avoid divide by zero errors
            where
            jInit1 = 
                case maybeA2NormLog of
                    Just a2NormLog -> max 0 (i - a2NormLog + 1)
                    Nothing -> 0 -- denominator == 0, we have no chance...
            jInit2 =
                case (maybeA1NormLog, maybeA2NormLog) of
                    (_, Nothing) -> 0 -- denominator == 0, we have no chance... 
                    (Nothing, _) -> 0 -- numerator == 0, it does not matter 
                    (Just a1NormLog, Just a2NormLog) -> 
                        max 0 (i + a1NormLog + 1 - 2 * a2NormLog)
            maybeA1NormLog = getBallNormLog (getB1 i)   
            maybeA2NormLog = getBallNormLog (getB2 i)   

instance CanDivBy CauchyReal CauchyReal

instance CanDivSameType CauchyReal


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

ensureAccuracy2 ::
    Integer -> Integer -> Integer -> (Integer -> Integer -> MPBall) -> MPBall
ensureAccuracy2 i j1 j2 getB 
    | getAccuracy result >= i = 
        -- TODO: disable this trace 
        trace (
            "ensureAccuracy2: i = " ++ show i ++ 
            "; j1 = " ++ show j1 ++ 
            "; j2 = " ++ show j2 ++ 
            "; result accuracy = " ++ (show $ getAccuracy result)
        ) $ 
        result
    | otherwise =
        -- TODO: disable this trace 
        trace (
            "ensureAccuracy2: i = " ++ show i ++ 
            "; j1 = " ++ show j1 ++ 
            "; j2 = " ++ show j2 ++ 
            "; result accuracy = " ++ (show $ getAccuracy result)
        ) $ 
        ensureAccuracy2 i (j1+1)(j2+1) getB
    where
    result = getB j1 j2

{- CauchyReal-Integer operations -}

instance CanAdd Integer CauchyReal where
    type AddType Integer CauchyReal = CauchyReal
    add a (CauchyReal getB2) = 
        CauchyReal $
            \i -> ensureAccuracy1 i i (\j -> a + (getB2 j))
        

instance CanSub Integer CauchyReal

instance CanAdd CauchyReal Integer where
    type AddType CauchyReal Integer = CauchyReal
    add (CauchyReal getB1) b = 
        CauchyReal $
            \i -> ensureAccuracy1 i i (\j -> (getB1 j) + b)

instance CanAddThis CauchyReal Integer

instance CanSub CauchyReal Integer

instance CanSubThis CauchyReal Integer

instance CanMul Integer CauchyReal where
    type MulType Integer CauchyReal = CauchyReal
    mul a1 (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a1 * (getB2 j))
            where
            jInit = 
                case maybeA1NormLog of
                    Just a1NormLog -> max 0 (i + a1NormLog)
                    Nothing -> 0
            maybeA1NormLog = getIntegerNormLog a1


instance CanMul CauchyReal Integer where
    type MulType CauchyReal Integer = CauchyReal
    mul a b = mul b a 

instance CanMulBy CauchyReal Integer

instance CanDiv Integer CauchyReal where
    type DivType Integer CauchyReal = CauchyReal
    div a1 (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a1 / (getB2 j))
            -- TODO: increase j to avoid divide by zero errors
            where
            jInit =
                case (maybeA1NormLog, maybeA2NormLog) of
                    (_, Nothing) -> 0 -- denominator == 0, we have no chance... 
                    (Nothing, _) -> 0 -- numerator == 0, it does not matter 
                    (Just a1NormLog, Just a2NormLog) -> 
                        max 0 (i + a1NormLog - 2 * a2NormLog)
            maybeA1NormLog = getIntegerNormLog a1
            maybeA2NormLog = getBallNormLog (getB2 i)   


instance CanDiv CauchyReal Integer where
    type DivType CauchyReal Integer = CauchyReal
    div (CauchyReal getB1) a2 = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> (getB1 j) / a2)
            where
            jInit = 
                case maybeA2NormLog of
                    Just a2NormLog -> max 0 (i - a2NormLog)
                    Nothing -> 0 -- denominator == 0, we have no chance...
            maybeA2NormLog = getIntegerNormLog a2  

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
    mul a1 (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a1 * (getB2 j))
            where
            jInit = 
                case maybeA1NormLog of
                    Just a1NormLog -> max 0 (i + a1NormLog)
                    Nothing -> 0
            maybeA1NormLog = getRationalNormLog a1

instance CanMul CauchyReal Rational where
    type MulType CauchyReal Rational = CauchyReal
    mul a b = mul b a

instance CanMulBy CauchyReal Rational

instance CanDiv Rational CauchyReal where
    type DivType Rational CauchyReal = CauchyReal
    div a1 (CauchyReal getB2) = 
        CauchyReal getB
        where
        getB i =
            ensureAccuracy1 i jInit (\j -> a1 / (getB2 j))
            -- TODO: increase j to avoid divide by zero errors
            where
            jInit =
                case (maybeA1NormLog, maybeA2NormLog) of
                    (_, Nothing) -> 0 -- denominator == 0, we have no chance... 
                    (Nothing, _) -> 0 -- numerator == 0, it does not matter 
                    (Just a1NormLog, Just a2NormLog) -> 
                        max 0 (i + a1NormLog - 2 * a2NormLog)
            maybeA1NormLog = getRationalNormLog a1
            maybeA2NormLog = getBallNormLog (getB2 i)   

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


