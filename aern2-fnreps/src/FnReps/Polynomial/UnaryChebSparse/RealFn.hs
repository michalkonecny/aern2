{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryChebSparse.RealFn 
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryChebSparse.PolyBall

{- examples -}

rf_x :: RealFn
rf_x_wa :: Accuracy -> PolyBall
rf_x@(RealFn rf_x_wa _) = projUnaryFnA (Interval 0.0 (1/3))

{- type definition -} 

data RealFn = 
    RealFn
    {
        rFn_withAccuracy :: Accuracy -> PolyBall,
        rFn_rough :: PolyBall
    }

{- basic function operations -} 

instance
    RealUnaryFnA (->) RealFn
    where
    type UnaryFnIn RealFn = Rational
    type UnaryFnOut RealFn = CauchyReal
    getDomainUnaryFnA =
        arr $ ball_domain . rFn_rough
    constUnaryFnA (dom, value) =
        RealFn withAccuracy (withAccuracy (bits 0))
        where
        withAccuracy acc = constUnaryFnA (dom, cauchyReal2ball value acc)
    projUnaryFnA dom =
        RealFn withAccuracy (withAccuracy (bits 0))
        where
        withAccuracy =
            seqByPrecision2CauchySeq $ \p ->
                runWithPrecisionPolicy (proc () -> projUnaryFnA -< dom) (ppKeepExact p) ()
    evalOnIntervalUnaryFnA =
        error "UnaryChebSparse.RealFn evalOnIntervalUnaryFnA not implemented yet"
--    evalAtInPointUnaryFnA =
--        proc (f, x) ->
--            do
--            xB <- convertA -< x
--            evalAtOutPointUnaryFnA -< (f,xB)
--    evalAtOutPointUnaryFnA =
--        proc (fB, x) ->
--            do
--            let fP = ucsBall_poly fB
--            let (Interval l r) = ucsBall_domain fB
--            let xU = (2*x - r - l)/(r-l)
--            case getAccuracy x of
--                Exact ->
--                    returnA -< evalDirectOnBall fP xU
--                _ ->  
--                    returnA -< evalLipschitzOnBall fP xU

{- pointwise arithmetic -} 


--instance CanNegA (->) PolyBall where
--    negA b = b { ucsBall_poly = neg (ucsBall_poly b) }
--    
--instance CanNegSameType PolyBall
--
--instance CanAddA (->) PolyBall PolyBall where
--    addA = ucsLift2 addAndReduce
--        where
--        addAndReduce maxDegree sqeepThresholdNormLog a b =
--            reduceDegreeAndSweep maxDegree sqeepThresholdNormLog $ a + b
--
--instance CanAddThis PolyBall PolyBall
--instance CanAddSameType PolyBall
--
--instance CanSub PolyBall PolyBall
--instance CanSubThis PolyBall PolyBall
--instance CanSubSameType PolyBall
--        
--instance CanMulA (->) PolyBall PolyBall where
--    mulA = ucsLift2 addAndReduce
--        where
--        addAndReduce maxDegree sqeepThresholdNormLog a b =
--            reduceDegreeAndSweep maxDegree sqeepThresholdNormLog $ a * b
--
--instance CanMulBy PolyBall PolyBall
--instance CanMulSameType PolyBall
--        
--ucsLift2 :: 
--    (Degree -> NormLog -> Poly -> Poly -> Poly)
--    -> 
--    (PolyBall, PolyBall) -> PolyBall
--ucsLift2 polyOpWithSizeLimits (a, b) =
--    PolyBall
--    {
--        ucsBall_poly = polyOpWithSizeLimits maxDegree sqeepThresholdNormLog aPoly bPoly,
--        ucsBall_domain = aDom,
--        ucsBall_maxDegree = maxDegree,
--        ucsBall_sqeepThresholdNormLog = sqeepThresholdNormLog
--    }
--    where
--    maxDegree = max (ucsBall_maxDegree a) (ucsBall_maxDegree b)
--    sqeepThresholdNormLog = min (ucsBall_sqeepThresholdNormLog a) (ucsBall_sqeepThresholdNormLog b)
--    aPoly = ucsBall_poly a
--    bPoly = ucsBall_poly b
--    aDom = ucsBall_domain a
--    _bDom = ucsBall_domain b -- TODO check for domain equality
    
    

