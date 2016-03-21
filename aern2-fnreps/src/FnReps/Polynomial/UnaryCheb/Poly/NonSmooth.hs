module FnReps.Polynomial.UnaryCheb.Poly.NonSmooth where

import AERN2.RealFunction
import Control.Arrow (arr)

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding (shiftDomainBy)
import FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication (lift1_DCT)
import FnReps.Polynomial.UnaryCheb.Poly.Bernstein

import Debug.Trace (trace)


shouldTrace :: Bool
--shouldTrace = False
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id


_test1 :: Poly
_test1 = sqrtAbs (prec 100) 7 (Interval (-1.0) 2.0)

sqrtAbs :: Precision -> Integer -> Interval Rational -> Poly
sqrtAbs p d (Interval l r) 
    | 0 <= l = x
    | r <= 0 = -x
    | otherwise =
        maybeTrace
        (
            "sqrtAbs: (see http://fooplot.com/plot/4c0k1qo81k)"
            ++ "\n zeroPoint = " ++ show zeroPoint
            ++ "\n targetEps = " ++ showB targetEps
            ++ "\n sqrtPos = " ++ showAP sqrtPos
            ++ "\n sqrtNeg = " ++ showAP sqrtNeg
            ++ "\n sqrtPos^2 = " ++ (showAP $ sqrtPos * sqrtPos)
            ++ "\n sqrtNeg^2 = " ++ (showAP $ sqrtNeg * sqrtNeg)
            ++ "\n sqrtPosSquareE = " ++ showB sqrtPosSquareE
            ++ "\n sqrtNegSquareE = " ++ showB sqrtNegSquareE
            ++ "\n resC = " ++ showAP resC
        ) $ 
        res
    where
    showB = show . getApproximate (bits 30)
    showAP = show . getApproximate (bits 30) . cheb2Power
    
    -- the result polynomial enclosure
    res = Poly $ terms_updateConst pmErrorBound resCTerms
        where 
        pmErrorBound c = endpoints2Ball (c - errorBound) (c + errorBound)
        errorBound = mpBall 0 -- TODO maxDifferenceNeg `max` maxDifferencePos
    resC@(Poly resCTerms) = lift1_DCT (const d) (\b -> sqrt (abs b)) x
    targetEps = ballCentre $ (evalAtDomPointUnaryFnA (resC, zeroPoint)) / 100
    
    -- x over interval [l,r] scaled to the domain [-1,1]
    x :: Poly
    x = 
        setPrecision_poly p $ 
            normaliseCoeffs $ fromList [(0,a0),(1,a1)]
        where
        a1 = rational2BallP p $ (r-l)/2
        a0 = rational2BallP p $ (r+l)/2
    -- the point in [-1,1] that maps to 0 in [l,r] 
    zeroPoint = -(r+l)/(r-l)
    

    -- approximations of sqrt(x) and sqrt(-x) on the two halves around zeroPoint    
    sqrtPos = 
        shiftDomainBy (2*l/(r-l)) $ 
            lift1_DCT (const d) (\b -> (sqrt(max (targetEps/16) b))) (x - l)
    sqrtNeg = 
        shiftDomainBy (2*r/(r-l)) $ 
            lift1_DCT (const d) (\b -> (sqrt(max (targetEps/16) (-b)))) (x - r)
    -- the error of the square of the above approximations    
    Interval _ sqrtPosSquareE =
        abs $ 
            rangeOnIntervalUnaryFnA (sqrtPos * sqrtPos - x, Interval zeroPoint 1.0)
    Interval _ sqrtNegSquareE =
        abs $ 
            rangeOnIntervalUnaryFnA (sqrtNeg * sqrtNeg + x, Interval (-1.0) zeroPoint)

{-
    The following is plotted for d=8, 16 and 32 at:
    http://fooplot.com/plot/omp15b3brz
-}

_sqrtAbsDCT :: Integer -> Poly
_sqrtAbsDCT d = r
    where
    r = lift1_DCT (const d) (\b -> sqrt (abs b)) x 
    x :: Poly
    x = setPrecision_poly (prec 100) $ projUnaryFnA (Interval (-1.0) 1.0) :: Poly

{-
    The following is plotted for d=8, 16, 32 and 64 at:
    http://fooplot.com/plot/demhnuwnms
-}
    
_sqrtAbsBernstein :: Integer -> Poly
_sqrtAbsBernstein d = bernsteinApprox (prec 200) d (\b -> sqrt(abs b))
    
