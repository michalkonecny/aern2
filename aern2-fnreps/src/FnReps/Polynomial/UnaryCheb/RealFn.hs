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
rf_x = projUnaryFnA (Interval 0.0 (4/3))

rf_2x :: RealFn
rf_2x = rf_x + rf_x

rf_x2 :: RealFn
rf_x2 = rf_x * rf_x

rf_x_pi :: CauchyReal
rf_x_pi = evalAtOutPointUnaryFnA (rf_x, pi/4)

rf_2x_pi :: CauchyReal
rf_2x_pi = evalAtOutPointUnaryFnA (rf_2x, pi/4)

rf_x2_pi :: CauchyReal
rf_x2_pi = evalAtOutPointUnaryFnA (rf_x2, pi/4)

{- type definition -} 

data RealFn = 
    RealFn
    {
        rFn_withAccuracy :: Accuracy -> PolyBall,
        rFn_rough :: PolyBall
    }

instance Show RealFn where
    show (RealFn withAccuracy _) =
        show $ withAccuracy (bits 100)

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

instance CanNegA (->) RealFn where
    negA = lift1 negA 

instance CanNegSameType RealFn

{-
    TODO
    Recompute when the result is not sufficiently accurate.
    Use the same method as for CauchyReal.
-}

instance CanAddA (->) RealFn RealFn where
    addA = lift2 addA

instance CanAddThis RealFn RealFn
instance CanAddSameType RealFn

instance CanSub RealFn RealFn
instance CanSubThis RealFn RealFn
instance CanSubSameType RealFn
        
instance CanMulA (->) RealFn RealFn where
    mulA = lift2 mulA

instance CanMulBy RealFn RealFn
instance CanMulSameType RealFn

lift1 :: 
    (PolyBall -> PolyBall) -> 
    (RealFn -> RealFn)
lift1 opB (RealFn withAccuracy withAccuracy0) =
    RealFn (\a -> opB $ withAccuracy a) (opB withAccuracy0) 

lift2 :: 
    ((PolyBall, PolyBall) -> PolyBall) -> 
    ((RealFn, RealFn) -> RealFn)
lift2 opB (RealFn withAccuracyA withAccuracy0A, RealFn withAccuracyB withAccuracy0B) =
    RealFn (\a -> opB (withAccuracyA a, withAccuracyB a)) (opB (withAccuracy0A, withAccuracy0B)) 
    
