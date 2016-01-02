{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, TypeOperators, ConstraintKinds #-}
module AERN2.Num.CauchyReal 
(
    CauchyReal,
    showCauchyReal,
    mapCauchyRealUnsafe,
    cauchyReal2ball,
    HasCauchyRealsA, HasCauchyReals,
    CanBeCauchyRealA, cauchyRealA, cauchyRealNamedA, cauchyRealsA, cauchyRealsNamedA, CanBeCauchyReal, cauchyReal, cauchyReals,
    integer2CauchyReal, rational2CauchyReal,
    convergent2CauchyReal,
    compareTryAccuracies,
    ensureAccuracyM2, ensureAccuracyM1,
    pi
)
where

import AERN2.Num.Operations
import AERN2.Num.Norm
import AERN2.Num.Accuracy

import AERN2.Num.MPBall
import AERN2.Num.IntegerRational ()

--import Control.Arrow

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

instance ConvertibleA (->) CauchyReal CauchyReal where convertA = id; convertListA = id


--class
--    (RationalLike a, HasReals a, CanAddMulDivScalar a CauchyReal, CanSqrt a, CanExp a, CanSineCosine a)
--    => 
--    RealLike a

{-| Invariant: For any @(CauchyReal seq)@ it holds @ball_error (seq i) <= 2^^(-i)@ -}
data CauchyReal = 
    CauchyReal { unCauchyReal :: Accuracy -> MPBall } 

cauchyReal2ball :: CauchyReal -> Accuracy -> MPBall
cauchyReal2ball (CauchyReal getBall) a = getBall a

showCauchyReal :: Accuracy -> CauchyReal -> String
showCauchyReal a r = show (cauchyReal2ball r a)

mapCauchyRealUnsafe :: (Accuracy -> MPBall -> MPBall) -> CauchyReal -> CauchyReal
mapCauchyRealUnsafe f (CauchyReal sq) = CauchyReal (\ ac -> f ac (sq ac) ) 

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

type HasCauchyRealsA to = ConvertibleA to CauchyReal
type HasCauchyReals = HasCauchyRealsA (->)

type CanBeCauchyRealA to a = ConvertibleA to a CauchyReal
cauchyRealA :: (CanBeCauchyRealA to a) => a `to` CauchyReal
cauchyRealA = convertA
cauchyRealNamedA :: (CanBeCauchyRealA to a) => String -> a `to` CauchyReal
cauchyRealNamedA = convertNamedA
cauchyRealsA :: (CanBeCauchyRealA to a) => [a] `to` [CauchyReal]
cauchyRealsA = convertListA
cauchyRealsNamedA :: (CanBeCauchyRealA to a) => String -> [a] `to` [CauchyReal]
cauchyRealsNamedA = convertListNamedA
type CanBeCauchyReal a = CanBeCauchyRealA (->) a
cauchyReal :: (CanBeCauchyReal a) => a -> CauchyReal
cauchyReal = convert
cauchyReals :: (CanBeCauchyReal a) => [a] -> [CauchyReal]
cauchyReals = convertList
    

-- | HasIntegers CauchyReal, CanBeCauchyReal Integer
instance ConvertibleA (->) Integer CauchyReal where
    convertA n =
        seqByPrecision2Cauchy $ \ p -> integer2BallP p n 

integer2CauchyReal :: Integer -> CauchyReal
integer2CauchyReal = convert

-- | HasRationals CauchyReal, CanBeCauchyReal Rational
instance ConvertibleA (->) Rational CauchyReal where
    convertA q =
        seqByPrecision2Cauchy $ \ p -> rational2BallP p q 

rational2CauchyReal :: Rational -> CauchyReal
rational2CauchyReal = convert

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
    aux (ac : rest) =
        case rel (map ($ ac) rs) of
            Just tv -> tv
            Nothing -> aux rest
    aux [] =
        error "CauchyReal comparison undecided even using maximum standard accuracy"

instance HasEqA (->) CauchyReal CauchyReal where
    equalToA (CauchyReal r1, CauchyReal r2) =
        tryStandardCompareAccuracies [r1,r2] (\[b1,b2] -> b1 == b2)

instance HasOrderA (->) CauchyReal CauchyReal where
    lessThanA (CauchyReal r1, CauchyReal r2) =
        tryStandardCompareAccuracies [r1,r2] (\[b1,b2] -> b1 `lessThan` b2)
    leqA (CauchyReal r1, CauchyReal r2) = 
        tryStandardCompareAccuracies [r1,r2] (\[b1,b2] -> b1 `leq` b2)

instance HasParallelComparisonsA (->) CauchyReal where
    pickNonZeroA rvs =
        tryStandardCompareAccuracies (map unCauchyReal rs) findNonZero
        where
        rs = map fst rvs
        findNonZero bs =
            aux True $ zip bs rvs
            where
            aux False [] = Nothing
            aux True [] = Just Nothing
            aux allFalse ((b, result) : rest) =
                case b /= 0 of
                    Just True -> Just (Just result)
                    Just False -> aux allFalse rest
                    Nothing -> aux False rest

instance HasEqA (->) Integer CauchyReal where
    equalToA (n, r) = equalTo ((convert n) :: CauchyReal) r

instance HasOrderA (->) Integer CauchyReal where
    lessThanA (n, r) = lessThan ((convert n) :: CauchyReal) r
    leqA (n, r) = leq ((convert n) :: CauchyReal) r
    
instance HasEqA (->) CauchyReal Integer where
    equalToA (r, n) = equalTo r ((convert n) :: CauchyReal)

instance HasOrderA (->) CauchyReal Integer where
    lessThanA (r, n) = lessThan r ((convert n) :: CauchyReal)
    leqA (r, n) = leq r ((convert n) :: CauchyReal)
    
instance HasEqA (->) Rational CauchyReal where
    equalToA (q, r) = equalTo ((convert q) :: CauchyReal) r

instance HasOrderA (->) Rational CauchyReal where
    lessThanA (q, r) = lessThan ((convert q) :: CauchyReal) r
    leqA (q, r) = leq ((convert q) :: CauchyReal) r
    
instance HasEqA (->) CauchyReal Rational where
    equalToA (r, q) = equalTo r ((convert q) :: CauchyReal)

instance HasOrderA (->) CauchyReal Rational where
    lessThanA (r, q) = lessThan r ((convert q) :: CauchyReal)
    leqA (r, q) = leq r ((convert q) :: CauchyReal)


{- Operations among CauchyReal's -}

instance Ring CauchyReal
instance Field CauchyReal
instance CanAddMulScalar CauchyReal CauchyReal
instance CanAddMulDivScalar CauchyReal CauchyReal

instance CanNegA (->) CauchyReal where
    negA (CauchyReal getB) = CauchyReal (\i -> neg $ getB i)

instance CanNegSameType CauchyReal

instance CanAbsA (->) CauchyReal where
    absA (CauchyReal getB) = CauchyReal (\i -> abs $ getB i)

instance CanAbsSameType CauchyReal

instance CanRecipA (->) CauchyReal where
    recipA a = 1 / a

instance CanRecipSameType CauchyReal

instance CanAddA (->) CauchyReal CauchyReal where
    addA (CauchyReal getB1, CauchyReal getB2) =
        CauchyReal $
            \i -> ensureAccuracy2 i i i (\j1 j2 -> (getB1 j1) + (getB2 j2))

instance CanAddThis CauchyReal CauchyReal

instance CanAddSameType CauchyReal

instance (CanSub CauchyReal CauchyReal)  
        
instance CanSubThis CauchyReal CauchyReal

instance CanSubSameType CauchyReal

instance CanMulA (->) CauchyReal CauchyReal where
    mulA (CauchyReal getB1, CauchyReal getB2) =
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

instance CanDivA (->) CauchyReal CauchyReal where
    divA (CauchyReal getB1, CauchyReal getB2) =
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

instance CanSqrtA (->) CauchyReal where
    sqrtA (CauchyReal getB1) = CauchyReal getB
        where
        getB i = 
            ensureAccuracy1 i jInit (\j -> sqrt (getB1 j))
            where
            jInit = 
                case maybeSqrtNormLog of
                    NormBits sqrtNormLog -> max 0 (i - 1 - sqrtNormLog)
                    NormZero -> i
            maybeSqrtNormLog = getSeqNormLog i (\j -> sqrt (getB1 j)) 

instance CanSqrtSameTypeA (->) CauchyReal

instance CanExpA (->) CauchyReal where
    expA (CauchyReal getB1) = CauchyReal getB
        where
        getB i = 
            ensureAccuracy1 i jInit (\j -> exp (getB1 j))
            where
            jInit = 
                case maybeExpNormLog of
                    NormBits expNormLog -> i + expNormLog + 1
                    NormZero -> i -- this should never happen
            maybeExpNormLog = getSeqNormLog i (\j -> exp (getB1 j)) 

instance CanExpSameTypeA (->) CauchyReal

instance CanSineCosineA (->) CauchyReal where
    sinA (CauchyReal getB1) = CauchyReal (\ i -> sin (getB1 i))
    cosA (CauchyReal getB1) = CauchyReal (\ i -> cos (getB1 i))

instance CanSineCosineSameTypeA (->) CauchyReal

{-
Typically ensureAccuracy1 is called with a j such that the result is of
accuracy >= i.  In some cases j needs to be slightly increased.  
For example:

*AERN2.Num.Examples> cauchyReal2ball (10 * pi) 138
ensureAccuracy1: i = 138; j = 141; result accuracy = 137
ensureAccuracy1: i = 138; j = 142; result accuracy = 137
ensureAccuracy1: i = 138; j = 143; result accuracy = 226
[31.41592653589793 ± 5.216071149404186e-69]

*AERN2.Num.Examples> cauchyReal2ball (pi / 10) 56
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

ensureAccuracyM2 ::
    (Monad m) =>
    Accuracy -> Accuracy -> Accuracy -> (Accuracy -> Accuracy -> m MPBall) -> m MPBall
ensureAccuracyM2 i j1 j2 getB =
    do
    result <- getB j1 j2
    if getAccuracy result >= i 
        then return result
        else ensureAccuracyM2 i (j1+1)(j2+1) getB

ensureAccuracyM1 ::
    (Monad m) =>
    Accuracy -> Accuracy -> (Accuracy -> m MPBall) -> m MPBall
ensureAccuracyM1 i j1 getB =
    do
    result <- getB j1
    if getAccuracy result >= i 
        then return result
        else ensureAccuracyM1 i (j1+1) getB



{- CauchyReal-Integer operations -}

instance CanAddMulScalar CauchyReal Integer
instance CanAddMulDivScalar CauchyReal Integer

instance CanAddA (->) Integer CauchyReal where
    type AddTypeA (->) Integer CauchyReal = CauchyReal
    addA (a, CauchyReal getB2) = 
        CauchyReal $
            \i -> ensureAccuracy1 i i (\j -> a + (getB2 j))
        

instance CanSub Integer CauchyReal

instance CanAddA (->) CauchyReal Integer where
    type AddTypeA (->) CauchyReal Integer = CauchyReal
    addA (CauchyReal getB1, b) = 
        CauchyReal $
            \i -> ensureAccuracy1 i i (\j -> (getB1 j) + b)

instance CanAddThis CauchyReal Integer

instance CanSub CauchyReal Integer

instance CanSubThis CauchyReal Integer

instance CanMulA (->) Integer CauchyReal where
    type MulTypeA (->) Integer CauchyReal = CauchyReal
    mulA (a1, CauchyReal getB2) = 
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


instance CanMulA (->) CauchyReal Integer where
    type MulTypeA (->) CauchyReal Integer = CauchyReal
    mulA (a, b) = mul b a 

instance CanMulBy CauchyReal Integer

instance CanDivA (->) Integer CauchyReal where
    type DivTypeA (->) Integer CauchyReal = CauchyReal
    divA (a1, CauchyReal getB2) = 
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


instance CanDivA (->) CauchyReal Integer where
    type DivTypeA (->) CauchyReal Integer = CauchyReal
    divA (CauchyReal getB1, a2) = 
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

instance CanSqrtA (->) Integer where
    type SqrtTypeA (->) Integer = CauchyReal
    sqrtA x = seqByPrecision2Cauchy $ \p -> sqrt (integer2BallP p x)      
        
instance CanExpA (->) Integer where
    type ExpTypeA (->) Integer = CauchyReal
    expA x = seqByPrecision2Cauchy $ \p -> exp (integer2BallP p x)
        
instance CanSineCosineA (->) Integer where
    type SineCosineTypeA (->) Integer = CauchyReal
    sinA x = seqByPrecision2Cauchy $ \p -> sin (integer2BallP p x)
    cosA x = seqByPrecision2Cauchy $ \p -> cos (integer2BallP p x)


{- CauchyReal-Rational operations -}

instance CanAddMulScalar CauchyReal Rational
instance CanAddMulDivScalar CauchyReal Rational

instance CanAddA (->) Rational CauchyReal where
    type AddTypeA (->) Rational CauchyReal = CauchyReal
    addA (a, CauchyReal getB2) = CauchyReal (\i -> a + (getB2 i))

instance CanSub Rational CauchyReal

instance CanAddA (->) CauchyReal Rational where
    type AddTypeA (->) CauchyReal Rational = CauchyReal
    addA (CauchyReal getB1, b) = CauchyReal (\i -> (getB1 i) + b)

instance CanAddThis CauchyReal Rational

instance CanSub CauchyReal Rational

instance CanSubThis CauchyReal Rational

instance CanMulA (->) Rational CauchyReal where
    type MulTypeA (->) Rational CauchyReal = CauchyReal
    mulA (a1, CauchyReal getB2) = 
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

instance CanMulA (->) CauchyReal Rational where
    type MulTypeA (->) CauchyReal Rational = CauchyReal
    mulA (a, b) = mul b a

instance CanMulBy CauchyReal Rational

instance CanDivA (->) Rational CauchyReal where
    type DivTypeA (->) Rational CauchyReal = CauchyReal
    divA (a1, CauchyReal getB2) = 
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

instance CanDivA (->) CauchyReal Rational where
    type DivTypeA (->) CauchyReal Rational = CauchyReal
    divA (a, b) = mul (1/b) a 

instance CanDivBy CauchyReal Rational

instance CanSqrtA (->) Rational where
    type SqrtTypeA (->) Rational = CauchyReal
    sqrtA x = seqByPrecision2Cauchy $ \p -> sqrt (rational2BallP p x)
        
instance CanExpA (->) Rational where
    type ExpTypeA (->) Rational = CauchyReal
    expA x = seqByPrecision2Cauchy $ \p -> exp (rational2BallP p x)
        
instance CanSineCosineA (->) Rational where
    type SineCosineTypeA (->) Rational = CauchyReal
    sinA x = seqByPrecision2Cauchy $ \p -> sin (rational2BallP p x)
    cosA x = seqByPrecision2Cauchy $ \p -> cos (rational2BallP p x)


{- operations mixing MPBall and CauchyReal, resulting in an MPBall -}

instance CanAddMulScalar MPBall CauchyReal
instance CanAddMulDivScalar MPBall CauchyReal

instance
    CanAddA (->) MPBall CauchyReal 
    where
    type AddTypeA (->) MPBall CauchyReal = MPBall
    addA (a, CauchyReal b) = add a (b (getAccuracyIfExactUsePrec a))

instance
    CanAddA (->) CauchyReal  MPBall 
    where
    type AddTypeA (->) CauchyReal MPBall = MPBall
    addA (a, b) = add b a

instance CanAddThis MPBall CauchyReal

instance
    CanSub MPBall CauchyReal 

instance
    CanSub CauchyReal  MPBall 

instance CanSubThis MPBall CauchyReal

instance
    CanMulA (->) MPBall CauchyReal 
    where
    type MulTypeA (->) MPBall CauchyReal = MPBall
    mulA (a, CauchyReal b) = mul a (b (getAccuracyIfExactUsePrec a))

instance
    CanMulA (->) CauchyReal  MPBall 
    where
    type MulTypeA (->) CauchyReal MPBall = MPBall
    mulA (a, b) = mul b a

instance CanMulBy MPBall CauchyReal

instance
    CanDivA (->) MPBall CauchyReal 
    where
    type DivTypeA (->) MPBall CauchyReal = MPBall
    divA (a, CauchyReal b) = mul a (b (getAccuracyIfExactUsePrec a))

instance
    CanDivA (->) CauchyReal  MPBall 
    where
    type DivTypeA (->) CauchyReal MPBall = MPBall
    divA (CauchyReal a, b) = mul (a (getAccuracyIfExactUsePrec b)) b

instance CanDivBy MPBall CauchyReal

getAccuracyIfExactUsePrec :: MPBall -> Accuracy
getAccuracyIfExactUsePrec ball =
    case getAccuracy ball of
        Exact -> bits (prec2integer $ getPrecision ball) -- should we also consider the norm of the ball? 
        result -> result
