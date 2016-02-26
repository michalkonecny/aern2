{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.RealFn 
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryCheb.PolyBall

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

{- examples -}

rf_x :: RealFn
rf_x = projUnaryFnA (Interval 0.0 (1/3))

rf_x_pi :: CauchyReal
rf_x_pi = evalAtOutPointUnaryFnA (rf_x, pi) 

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
    evalAtInPointUnaryFnA (f, x) =
        evalAtOutPointUnaryFnA (f, convert x)
    evalAtOutPointUnaryFnA (RealFn fR f0, xR) =
        newCRA ([], Nothing, resWithAcc)
        where
        resWithAcc acc =
            keepIncreasingEffort' (evalAtOutPointUnaryFnA (f0,x0)) (zip fAccuracies fSeq0) (zip xAccuracies xSeq0)
            where
            fSeq0 = f0 : (map fR [(acc-1)..])
            fAccuracies = (bits 0) : [(acc-1)..]
            xSeq0@(x0:_) = map (cauchyReal2ball xR) [acc..]
            xAccuracies = [acc..]
            keepIncreasingEffort' resCurrent fSeq@((fA,_f):_) xSeq@((xA,x):_) =
                maybeTrace (
                    "evalAtOutPointUnaryFnA.keepIncreasingEffort:"
                    ++ "\n accuracy for f = " ++ show fA
                    ++ "\n accuracy for x = " ++ show xA
                    ++ "\n x = " ++ show x
                    ++ "\n resCurrent = " ++ show resCurrent
                    ++ "\n getAccuracy resCurrent = " ++ show (getAccuracy resCurrent)
                    ++ "\n requested accuracy = " ++ show acc
                ) $
                keepIncreasingEffort resCurrent fSeq xSeq
            keepIncreasingEffort' _ _ _ = error "keepIncreasingEffort'"
            keepIncreasingEffort resCurrent fSeq@((_,f):fRest@((_,fNext):_)) xSeq@((_,x):xRest@((_,xNext):_)) 
                | accCurrent >= acc = resCurrent
                | accNextX > accCurrent =
                    keepIncreasingEffort' resNextX fSeq xRest
                | accNextF > accCurrent =
                    keepIncreasingEffort' resNextF fRest xSeq
                | otherwise = 
                    keepIncreasingEffort' resNextXNextF fRest xRest
                where
                accCurrent = getAccuracy resCurrent
                resNextX = evalAtOutPointUnaryFnA (f,xNext)
                accNextX = getAccuracy resNextX
                resNextF = evalAtOutPointUnaryFnA (fNext,x)
                accNextF = getAccuracy resNextF
                resNextXNextF = evalAtOutPointUnaryFnA (fNext,xNext)
            keepIncreasingEffort _ _ _ =
                error "internal error in FnReps.Polynomial.UnaryCheb.RealFn RealUnaryFnA.evalAtOutPointUnaryFnA"

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
    
    

