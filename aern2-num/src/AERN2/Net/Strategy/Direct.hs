{-# LANGUAGE CPP, FlexibleInstances #-}
module AERN2.Net.Strategy.Direct 
(
    Interval(..), rati2MPBall, UnaryFnMPBall(..), UnaryFnCR(..)
)
where

import AERN2.Num

import AERN2.RealFunction

--import qualified Data.Map as Map

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id


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

_mpBall2rati :: MPBall -> Interval Rational
_mpBall2rati b =
    Interval l r
    where
    l = toRationalDown b
    r = toRationalUp b

_cri2MPBall :: Interval CauchyReal -> MPBall
_cri2MPBall (Interval l r) =
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
    l = convergent2CauchyReal Nothing $ repeat lMP
    r = convergent2CauchyReal Nothing $ repeat rMP
    (lMP, rMP) = ball2endpoints b

{-
-- the following instance is currently not used
instance RationalIntervalA (->) (Interval CauchyReal) where
    type (IntervalE (Interval CauchyReal)) = CauchyReal
    type (IntervalR (Interval CauchyReal)) = CauchyReal
    getEndpointsA (Interval l r) = (l,r)
    fromEndpointsA (l,r) = Interval l r
    limitIntervalsToRealA sq = convergent2CauchyReal Nothing $ map cri2MPBall sq

instance RationalIntervalA (->) (Interval Rational) where
    type (IntervalE (Interval Rational)) = Rational
    type (IntervalR (Interval Rational)) = CauchyReal
    getEndpointsA (Interval l r) = (l, r)
    fromEndpointsA (l,r) = (Interval l r)
    limitIntervalsToRealA sq = convergent2CauchyReal Nothing $ map rati2MPBall sq
-}

{- TODO The following function types should move to aern2-function, when it is created -}

data UnaryFnMPBall =
    UnaryFnMPBall
    {
        ufnB2B_dom :: Interval Rational, 
        ufnB2B_eval :: MPBall -> MPBall
    } 

instance RealUnaryFnA (->) UnaryFnMPBall where
    type UnaryFnDomPoint UnaryFnMPBall = Rational
    type UnaryFnPoint UnaryFnMPBall = CauchyReal
    constUnaryFnA (dom, r) = UnaryFnMPBall dom (\b -> cauchyReal2ball r (getFiniteAccuracy b))
    projUnaryFnA dom = UnaryFnMPBall dom id
    getDomainUnaryFnA = ufnB2B_dom
    evalAtPointUnaryFnA (UnaryFnMPBall _dom f, r) = 
        convergent2CauchyReal Nothing $ 
            map f $
                map (cauchyReal2ball r) (map bits [1..])
    evalAtDomPointUnaryFnA (UnaryFnMPBall _dom f, r) = 
        convergent2CauchyReal Nothing $ 
            map f $ map (flip rational2BallP r) standardPrecisions
    rangeOnIntervalUnaryFnA (UnaryFnMPBall _dom f, ri) =  
        mpBall2cri $ f (rati2MPBall ri) -- TODO: not computing the maximum extension, should do 

data UnaryFnCR =
    UnaryFnCR
    {
        ufnCR2CR_dom :: Interval Rational, 
        ufnCR2CR_eval :: CauchyReal -> CauchyReal
    } 


instance RealUnaryFnA (->) UnaryFnCR
    where
    type UnaryFnDomPoint UnaryFnCR = Rational
    type UnaryFnPoint UnaryFnCR = CauchyReal
    constUnaryFnA (dom, r) = UnaryFnCR dom (const r)
    projUnaryFnA dom = UnaryFnCR dom id
    getDomainUnaryFnA = ufnCR2CR_dom
    evalAtPointUnaryFnA (UnaryFnCR _dom f, r) = f r 
    evalAtDomPointUnaryFnA (UnaryFnCR _dom f, r) = f (cauchyReal r) 
    rangeOnIntervalUnaryFnA (UnaryFnCR _dom _f, _ri) = 
        error "rangeOnIntervalUnaryFnA not implemented for UnaryFnCR"


