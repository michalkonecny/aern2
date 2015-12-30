{-# LANGUAGE CPP, FlexibleInstances #-}
module AERN2.Net.Execution.Direct 
(Interval(..), rati2MPBall, UnaryFnMPBall, UnaryFnCR)
where

import AERN2.Num
import Data.String (fromString)

import AERN2.Net.Spec.Arrow

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

{- Direct evaluation using CauchyReal -}

instance ArrowRational (->) Rational where
    lessA = uncurry (<)
    leqA = uncurry (<=)
    addA = uncurry (+)
    mulA = uncurry (*)
    rationalConstA _name r = const $ r
    rationalOpA _name f = f

instance ArrowRational (->) CauchyReal where
    lessA = uncurry (<)
    leqA = uncurry (<=)
    addA = uncurry (+)
    mulA = uncurry (*)
    rationalConstA _name r = const $ rational r
    rationalOpA _name f = error "rationalOpA not implemented for CauchyReal"

instance ArrowReal (->) CauchyReal where
    pickNonZeroA = pickNonZeroReal
    realConstA _name r = const r
    realOpA _name f = f
    addRealA _name r = (r +)
    mulRealA _name r = (r *) 
    sqrtA = sqrt


{- TODO The Interval type should move somewhere to aern-real -}

data Interval a = Interval a a
    deriving (Show)

rati2MPBall :: Interval Rational -> MPBall
rati2MPBall il@(Interval l r) =
    maybeTrace
    (
        "rati2MPBall: " ++ show il
    ) $
    endpoints2Ball lMP rMP
    where
    lMP = q2MP l
    rMP = q2MP r
    q2MP q =
        case nl of
            NormBits i -> rational2BallP (prec (max 10 (10 - i))) q
            NormZero -> error "rati2MPBall does not work for a singleton interval"
    nl = getNormLog (r - l)

mpBall2rati :: MPBall -> Interval Rational
mpBall2rati b =
    Interval l r
    where
    l = toRationalDown b
    r = toRationalUp b

cri2MPBall :: Interval CauchyReal -> MPBall
cri2MPBall (Interval l r) =
    maybeTrace
    (
        "cri2MPBall: Interval " ++ show lMP ++ " " ++ show rMP
    ) $
    endpoints2Ball lMP rMP
    where
    lMP = cauchyReal2ball l a
    rMP = cauchyReal2ball r a
    a =
        case nl of
            NormBits i -> bits (max 10 (1000 - i))
            NormZero -> error "cri2MPBall does not work for a singleton interval"
    nl = getNormLog ((cauchyReal2ball r a0) - (cauchyReal2ball l a0))
    a0 = bits 10

mpBall2cri :: MPBall -> Interval CauchyReal
mpBall2cri b =
    maybeTrace
    (
        "mpBall2cri: b = " ++ show b
    ) $
    Interval l r
    where
    l = convergent2CauchyReal $ repeat lMP
    r = convergent2CauchyReal $ repeat rMP
    (lMP, rMP) = ball2endpoints b

-- the following instance is currently not used
instance ArrowRationalInterval (->) (Interval CauchyReal) where
    type (IntervalE (Interval CauchyReal)) = CauchyReal
    type (IntervalR (Interval CauchyReal)) = CauchyReal
    getEndpointsA (Interval l r) = (l,r)
    fromEndpointsA (l,r) = Interval l r
    limitIntervalsToRealA sq = convergent2CauchyReal $ map cri2MPBall sq

instance ArrowRationalInterval (->) (Interval Rational) where
    type (IntervalE (Interval Rational)) = Rational
    type (IntervalR (Interval Rational)) = CauchyReal
    getEndpointsA (Interval l r) = (l, r)
    fromEndpointsA (l,r) = (Interval l r)
    limitIntervalsToRealA sq = convergent2CauchyReal $ map rati2MPBall sq

type UnaryFnMPBall = (Interval Rational, MPBall -> MPBall) 

instance ArrowRealUnaryFn (->) UnaryFnMPBall where
    type UFnDom UnaryFnMPBall = Interval Rational
    type UFnR UnaryFnMPBall = CauchyReal
    constUFnA (dom, r) = (dom, \b -> cauchyReal2ball r (getFiniteAccuracy b))
    projUFnA dom = (dom, id)
    getDomainUFnA (dom, _) = dom
    evalAtPointUFnA ((_dom, f), r) = 
        convergent2CauchyReal $ 
            map f $
                map (cauchyReal2ball r) (map bits [1..])
    evalAtUFnDomEA ((_dom, f), r) = 
        convergent2CauchyReal $ 
            map f $ map (flip rational2BallP r) standardPrecisions
    evalOnIntervalUFnA ((_dom, f), ri) =  
        mpBall2rati $ f (rati2MPBall ri)

type UnaryFnCR = (Interval Rational, CauchyReal -> CauchyReal) 

instance ArrowRealUnaryFn (->) UnaryFnCR
    where
    type UFnDom UnaryFnCR = Interval Rational
    type UFnR UnaryFnCR = CauchyReal
    constUFnA (dom, r) = (dom, const r)
    projUFnA dom = (dom, id)
    getDomainUFnA (dom, _) = dom
    evalAtPointUFnA ((_dom, f), r) = f r 
    evalAtUFnDomEA ((_dom, f), r) = f (rational r) 
    evalOnIntervalUFnA ((_dom, f), ri) = 
        error "evalOnIntervalUFnA not implemented for UnaryFnCR"


--{- Direct evaluation using MPBall -}
--
--instance ArrowReal (->) MPBall where
----    piA p = cauchyReal2ball (prec2integer p) pi -- TODO: enable when we have (SizeLimits MPBall)
--    sqrtA = sqrt
--    addA = uncurry (+)
--    mulA = uncurry (*)