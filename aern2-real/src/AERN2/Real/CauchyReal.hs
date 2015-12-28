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
    integer2CauchyReal, rational2CauchyReal,
    convergent2CauchyReal,
    pickNonZeroReal,
    pi
)
where

--import qualified Prelude as P
import AERN2.Real.Operations
import AERN2.Real.Norm
import AERN2.Real.Accuracy

import AERN2.Real.MPBall
import AERN2.Real.IntegerRational ()

import Debug.Trace (trace)
import Data.List (findIndex)

shouldTrace :: Bool
shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

{-| Invariant: For any @(CauchyReal seq)@ it holds @ball_error (seq i) <= 2^^(-i)@ -}
data CauchyReal = 
    CauchyReal { unCauchyReal :: Accuracy -> MPBall } 

cauchyReal2ball :: CauchyReal -> Accuracy -> MPBall
cauchyReal2ball (CauchyReal getBall) a = getBall a

showCauchyReal :: Accuracy -> CauchyReal -> String
showCauchyReal a r = show (cauchyReal2ball r a)

convergent2CauchyReal :: 
    [MPBall] -> CauchyReal
convergent2CauchyReal convergentSeq =
    CauchyReal sq
    where
    sq i =
        findAccurate convergentSeq
        where
        findAccurate [] =
            error "convergent2CauchyReal: the sequence either converges too slowly or it does not converge"
        findAccurate (b : rest)
            | getAccuracy b >= i = b
            | otherwise = findAccurate rest

seqByPrecision2Cauchy :: 
    (Precision -> MPBall) -> CauchyReal
seqByPrecision2Cauchy seqByPrecision =
    CauchyReal sq    
    where
    sq i =
        findAccurate $ map seqByPrecision $ dropWhile lowPrec standardPrecisions
        where
        lowPrec p = prec2integer p < fromAccuracy i
        findAccurate [] =
            error "seqByPrecision2Cauchy: the sequence either converges too slowly or it does not converge"
        findAccurate (b : rest)
            | getAccuracy b >= i = b
            | otherwise = findAccurate rest


instance HasIntegers CauchyReal where
    integer n =
        seqByPrecision2Cauchy $ \ p -> integer2BallP p n 

integer2CauchyReal :: Integer -> CauchyReal
integer2CauchyReal = integer

instance HasRationals CauchyReal where
    rational q =
        seqByPrecision2Cauchy $ \ p -> rational2BallP p q 

rational2CauchyReal :: Rational -> CauchyReal
rational2CauchyReal = rational

pi :: CauchyReal
pi = seqByPrecision2Cauchy (\ p -> piBallP p)

{- Comparisons of CauchyReals -}

compareTryAccuracies :: [Accuracy]
compareTryAccuracies =
    map bits $ aux 8 13
    where
    aux j j' 
        | j <= maximumCompareAccuracy = j : (aux j' (j+j'))
        | otherwise = []

maximumCompareAccuracy :: Integer
maximumCompareAccuracy = 10000

tryStandardCompareAccuracies :: 
   [Accuracy -> MPBall] -> ([MPBall] -> Maybe t) -> t
tryStandardCompareAccuracies rs rel =
    aux compareTryAccuracies
    where
    aux (a : rest) =
        case rel (map ($ a) rs) of
            Just tv -> tv
            Nothing -> aux rest
    aux [] =
        error "CauchyReal comparison undecided even using maximum standard accuracy"

instance HasOrder CauchyReal CauchyReal where
    type OrderCompareType CauchyReal CauchyReal = Bool
    lessThan (CauchyReal r1) (CauchyReal r2) =
        tryStandardCompareAccuracies [r1,r2] (\[b1,b2] -> b1 `lessThan` b2)
    leq (CauchyReal r1) (CauchyReal r2) = 
        tryStandardCompareAccuracies [r1,r2] (\[b1,b2] -> b1 `leq` b2)

instance HasOrder Integer CauchyReal where
    type OrderCompareType Integer CauchyReal = Bool
    lessThan n r = lessThan ((integer n) :: CauchyReal) r
    leq n r = leq ((integer n) :: CauchyReal) r
    
instance HasOrder CauchyReal Integer where
    type OrderCompareType CauchyReal Integer = Bool
    lessThan r n = lessThan r ((integer n) :: CauchyReal)
    leq r n = leq r ((integer n) :: CauchyReal)
    
instance HasOrder Rational CauchyReal where
    type OrderCompareType Rational CauchyReal = Bool
    lessThan q r = lessThan ((rational q) :: CauchyReal) r
    leq q r = leq ((rational q) :: CauchyReal) r
    
instance HasOrder CauchyReal Rational where
    type OrderCompareType CauchyReal Rational = Bool
    lessThan r q = lessThan r ((rational q) :: CauchyReal)
    leq r q = leq r ((rational q) :: CauchyReal)

pickNonZeroReal :: [(CauchyReal, a)] -> (CauchyReal, a)
pickNonZeroReal rvs =
    tryStandardCompareAccuracies (map unCauchyReal rs) findNonZero
    where 
    rs = map fst rvs
    findNonZero bs =
        fmap (rvs !!) (findIndex id (map isNonZero bs))


{- Operations among CauchyReal's -}

instance CanNeg CauchyReal where
    neg (CauchyReal getB) = CauchyReal (\i -> neg $ getB i)

instance CanNegSameType CauchyReal

instance CanAbs CauchyReal where
    abs (CauchyReal getB) = CauchyReal (\i -> abs $ getB i)

instance CanAbsSameType CauchyReal

instance CanRecip CauchyReal where
    recip a = 1 / a

instance CanRecipSameType CauchyReal

instance CanAdd CauchyReal CauchyReal where
    add (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal $
            \i -> ensureAccuracy2 i i i (\j1 j2 -> (getB1 j1) + (getB2 j2))

instance CanAddThis CauchyReal CauchyReal

instance CanAddSameType CauchyReal

instance (CanSub CauchyReal CauchyReal)  
        
instance CanSubThis CauchyReal CauchyReal

instance CanSubSameType CauchyReal

instance CanMul CauchyReal CauchyReal where
    mul (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal getB
        where
        getB i =
            ensureAccuracy2 i jInit1 jInit2 (\j1 j2 -> (getB1 j1) * (getB2 j2))
            where
            jInit1 = 
                case maybeA2NormLog of
                    NormBits a2NormLog -> max (bits 0) (i + a2NormLog + 1)
                    NormZero -> bits 0
            jInit2 = 
                case maybeA1NormLog of
                    NormBits a1NormLog -> max (bits 0) (i + a1NormLog + 1)
                    NormZero -> bits 0
            maybeA1NormLog = getSeqNormLog i getB1   
            maybeA2NormLog = getSeqNormLog i getB2   

getSeqNormLog :: Accuracy -> (Accuracy -> MPBall) -> NormLog
getSeqNormLog i getB =
    case 1 < getB0 of
        Just True -> getNormLog getB0
        _ -> getNormLog (getB i)
    where
    getB0 = getB (bits 0)

instance CanMulBy CauchyReal CauchyReal

instance CanMulSameType CauchyReal

instance CanDiv CauchyReal CauchyReal where
    div (CauchyReal getB1) (CauchyReal getB2) =
        CauchyReal getB
        where
        getB i =
            ensureAccuracy2 i jInit1 jInit2 (\j1 j2 -> (getB1 j1) / (getB2 j2))
            -- TODO: increase j2 to avoid divide by zero errors
            where
            jInit1 = 
                case maybeA2NormLog of
                    NormBits a2NormLog -> max 0 (i - a2NormLog + 1)
                    NormZero -> bits 0 -- denominator == 0, we have no chance...
            jInit2 =
                case (maybeA1NormLog, maybeA2NormLog) of
                    (_, NormZero) -> bits 0 -- denominator == 0, we have no chance... 
                    (NormZero, _) -> bits 0 -- numerator == 0, it does not matter 
                    (NormBits a1NormLog, NormBits a2NormLog) -> 
                        max 0 (i + a1NormLog + 1 - 2 * a2NormLog)
            maybeA1NormLog = getSeqNormLog i getB1
            maybeA2NormLog = getSeqNormLog i getB2   

instance CanDivBy CauchyReal CauchyReal

instance CanDivSameType CauchyReal

instance CanSqrt CauchyReal where
    sqrt (CauchyReal getB1) = CauchyReal getB
        where
        getB i = 
            ensureAccuracy1 i jInit (\j -> sqrt (getB1 j))
            where
            jInit = 
                case maybeSqrtNormLog of
                    NormBits sqrtNormLog -> max 0 (i - 1 - sqrtNormLog)
                    NormZero -> i
            maybeSqrtNormLog = getSeqNormLog i (\j -> sqrt (getB1 j)) 

instance CanSineCosine CauchyReal where
    sin (CauchyReal getB1) = CauchyReal (\ i -> sin (getB1 i))
    cos (CauchyReal getB1) = CauchyReal (\ i -> cos (getB1 i))

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
    Accuracy -> Accuracy -> (Accuracy -> MPBall) -> MPBall
ensureAccuracy1 i j getB 
    | getAccuracy result >= i = 
        maybeTrace (
            "ensureAccuracy1: i = " ++ show i ++ 
            "; j = " ++ show j ++ 
            "; result accuracy = " ++ (show $ getAccuracy result)
        ) $ 
        result
    | otherwise =
        maybeTrace (
            "ensureAccuracy1: i = " ++ show i ++ 
            "; j = " ++ show j ++ 
            "; result accuracy = " ++ (show $ getAccuracy result)
        ) $ 
        ensureAccuracy1 i (j+1) getB
    where
    result = getB j

ensureAccuracy2 ::
    Accuracy -> Accuracy -> Accuracy -> (Accuracy -> Accuracy -> MPBall) -> MPBall
ensureAccuracy2 i j1 j2 getB 
    | getAccuracy result >= i = 
        maybeTrace (
            "ensureAccuracy2: i = " ++ show i ++ 
            "; j1 = " ++ show j1 ++ 
            "; j2 = " ++ show j2 ++ 
            "; result accuracy = " ++ (show $ getAccuracy result)
        ) $ 
        result
    | otherwise =
        maybeTrace (
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
                    NormBits a1NormLog -> max 0 (i + a1NormLog)
                    NormZero -> bits 0
            maybeA1NormLog = getNormLog a1


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
                    (_, NormZero) -> bits 0 -- denominator == 0, we have no chance... 
                    (NormZero, _) -> bits 0 -- numerator == 0, it does not matter 
                    (NormBits a1NormLog, NormBits a2NormLog) -> 
                        max 0 (i + a1NormLog - 2 * a2NormLog)
            maybeA1NormLog = getNormLog a1
            maybeA2NormLog = getSeqNormLog i getB2   


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
                    NormBits a2NormLog -> max 0 (i - a2NormLog)
                    NormZero -> bits 0 -- denominator == 0, we have no chance...
            maybeA2NormLog = getNormLog a2  

instance CanDivBy CauchyReal Integer

instance CanSqrt Integer where
    type SqrtType Integer = CauchyReal
    sqrt x = seqByPrecision2Cauchy $ \p -> sqrt (integer2BallP p x)      
        
instance CanSineCosine Integer where
    type SineCosineType Integer = CauchyReal
    sin x = seqByPrecision2Cauchy $ \p -> sin (integer2BallP p x)
    cos x = seqByPrecision2Cauchy $ \p -> cos (integer2BallP p x)


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
                    NormBits a1NormLog -> max 0 (i + a1NormLog)
                    NormZero -> bits 0
            maybeA1NormLog = getNormLog a1

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
                    (_, NormZero) -> bits 0 -- denominator == 0, we have no chance... 
                    (NormZero, _) -> bits 0 -- numerator == 0, it does not matter 
                    (NormBits a1NormLog, NormBits a2NormLog) -> 
                        max 0 (i + a1NormLog - 2 * a2NormLog)
            maybeA1NormLog = getNormLog a1
            maybeA2NormLog = getSeqNormLog i getB2   

instance CanDiv CauchyReal Rational where
    type DivType CauchyReal Rational = CauchyReal
    div a b = mul (1/b) a 

instance CanDivBy CauchyReal Rational

instance CanSqrt Rational where
    type SqrtType Rational = CauchyReal
    sqrt x = seqByPrecision2Cauchy $ \p -> sqrt (rational2BallP p x)      
        
instance CanSineCosine Rational where
    type SineCosineType Rational = CauchyReal
    sin x = seqByPrecision2Cauchy $ \p -> sin (rational2BallP p x)
    cos x = seqByPrecision2Cauchy $ \p -> cos (rational2BallP p x)  


{- operations mixing MPBall and CauchyReal, resulting in an MPBall -}

instance
    CanAdd MPBall CauchyReal 
    where
    type AddType MPBall CauchyReal = MPBall
    add a (CauchyReal b) = add a (b (getAccuracyIfExactUsePrec a))

instance
    CanAdd CauchyReal  MPBall 
    where
    type AddType CauchyReal MPBall = MPBall
    add a b = add b a

instance CanAddThis MPBall CauchyReal

instance
    CanSub MPBall CauchyReal 
    where
    type SubType MPBall CauchyReal = MPBall
    sub a (CauchyReal b) = sub a (b (getAccuracyIfExactUsePrec a))

instance
    CanSub CauchyReal  MPBall 
    where
    type SubType CauchyReal MPBall = MPBall
    sub (CauchyReal a) b = sub (a (getAccuracyIfExactUsePrec b)) b

instance CanSubThis MPBall CauchyReal

instance
    CanMul MPBall CauchyReal 
    where
    type MulType MPBall CauchyReal = MPBall
    mul a (CauchyReal b) = mul a (b (getAccuracyIfExactUsePrec a))

instance
    CanMul CauchyReal  MPBall 
    where
    type MulType CauchyReal MPBall = MPBall
    mul a b = mul b a

instance CanMulBy MPBall CauchyReal

instance
    CanDiv MPBall CauchyReal 
    where
    type DivType MPBall CauchyReal = MPBall
    div a (CauchyReal b) = mul a (b (getAccuracyIfExactUsePrec a))

instance
    CanDiv CauchyReal  MPBall 
    where
    type DivType CauchyReal MPBall = MPBall
    div (CauchyReal a) b = mul (a (getAccuracyIfExactUsePrec b)) b

instance CanDivBy MPBall CauchyReal

getAccuracyIfExactUsePrec :: MPBall -> Accuracy
getAccuracyIfExactUsePrec ball =
    case getAccuracy ball of
        Exact -> bits (prec2integer $ getPrecision ball) -- should we also consider the norm of the ball? 
        result -> result
