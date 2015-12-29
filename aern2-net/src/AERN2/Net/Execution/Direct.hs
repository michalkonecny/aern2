{-# LANGUAGE FlexibleInstances #-}
module AERN2.Net.Execution.Direct where

import AERN2.Real
import Data.String (fromString)

import AERN2.Net.Spec.Arrow


{- Direct evaluation using CauchyReal -}

instance ArrowReal (->) CauchyReal where
    lessA = uncurry (<)
    leqA = uncurry (<=)
    pickNonZeroA = pickNonZeroReal
    realA r _name = const r
    addA = uncurry (+)
    addConstA r _name = (r +) 
    mulA = uncurry (*)
    mulConstA r _name = (r *) 
    sqrtA = sqrt

{- TODO The following should move somewhere to aern-real -}

data Interval a = Interval a a

rati2MPBall :: Interval Rational -> MPBall
rati2MPBall (Interval l r) =
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
    endpoints2Ball lMP rMP
    where
    lMP = cauchyReal2ball l a
    rMP = cauchyReal2ball r a
    a =
        case nl of
            NormBits i -> bits (max 10 (10 - i))
            NormZero -> error "cri2MPBall does not work for a singleton interval"
    nl = getNormLog ((cauchyReal2ball r a0) - (cauchyReal2ball l a0))
    a0 = bits 10

mpBall2cri :: MPBall -> Interval CauchyReal
mpBall2cri b =
    Interval l r
    where
    l = convergent2CauchyReal $ repeat lMP
    r = convergent2CauchyReal $ repeat rMP
    (lMP, rMP) = ball2endpoints b

instance ArrowRealInterval (->) CauchyReal (Interval CauchyReal) where
    getEndpointsA (Interval l r) = (l,r)
    fromEndpointsA (l,r) = Interval l r
    limitIntervalsToRealA sq = convergent2CauchyReal $ map cri2MPBall sq

instance ArrowRealInterval (->) CauchyReal (Interval Rational) where
    getEndpointsA (Interval l r) = (rational l, rational r)
    fromEndpointsA (l,r) = mpBall2rati $ cri2MPBall (Interval l r)
    limitIntervalsToRealA sq = convergent2CauchyReal $ map rati2MPBall sq

type UnaryFnMBall = (Interval CauchyReal, MPBall -> MPBall) 

instance ArrowRealUnaryFn (->) CauchyReal (Interval CauchyReal) UnaryFnMBall
    where
    constUFnA (dom, r) = (dom, \b -> cauchyReal2ball r (getFiniteAccuracy b))
    projUFnA dom = (dom, id)
    getDomainUFnA (dom, _) = dom
    evalAtPointUFnA ((_dom, f), r) = 
        convergent2CauchyReal $ 
            map f $
                map (cauchyReal2ball r) (map bits [1..])
    evalOnIntervalUFnA ((_dom, f), ri) =  
        mpBall2cri $ f (cri2MPBall ri)


type UnaryFnCR = (Interval CauchyReal, CauchyReal -> CauchyReal) 

instance ArrowRealUnaryFn (->) CauchyReal (Interval CauchyReal) UnaryFnCR
    where
    constUFnA (dom, r) = (dom, const r)
    projUFnA dom = (dom, id)
    getDomainUFnA (dom, _) = dom
    evalAtPointUFnA ((_dom, f), r) = f r 
    evalOnIntervalUFnA ((_dom, f), ri) = 
        error "evalOnIntervalUFnA not implemented for UnaryFnCR"


--{- Direct evaluation using MPBall -}
--
--instance ArrowReal (->) MPBall where
----    piA p = cauchyReal2ball (prec2integer p) pi -- TODO: enable when we have (SizeLimits MPBall)
--    sqrtA = sqrt
--    addA = uncurry (+)
--    mulA = uncurry (*)
