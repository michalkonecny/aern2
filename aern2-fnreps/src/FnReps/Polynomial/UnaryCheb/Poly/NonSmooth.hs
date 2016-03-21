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


{-
    see http://fooplot.com/plot/d41cy0ouxp
-}
_test1 :: Poly
_test1 = sqrtAbs (prec 100) 7 (Interval (-1.0) 2.0)

_test1_16 :: Poly
_test1_16 = sqrtAbs (prec 200) 15 (Interval (-1.0) 2.0)

_test1_32 :: Poly
_test1_32 = sqrtAbs (prec 300) 31 (Interval (-1.0) 2.0)
    -- throws: Precision must be between 2 and Precision 1000000 (given: p=0).

sqrtAbs :: Precision -> Integer -> Interval Rational -> Poly
sqrtAbs p d (Interval l r) 
    | 0 <= l = x
    | r <= 0 = -x
    | otherwise =
        maybeTrace
        (
            "sqrtAbs:"
            ++ "\n zeroPoint = " ++ show zeroPoint
            ++ "\n targetEps = " ++ showB targetEps
            ++ "\n sqrtPos = " ++ showAP sqrtPos
            ++ "\n sqrtNeg = " ++ showAP sqrtNeg
            ++ "\n sqrtPos^2 = " ++ (showAP $ sqrtPos * sqrtPos)
            ++ "\n sqrtNeg^2 = " ++ (showAP $ sqrtNeg * sqrtNeg)
            ++ "\n sqrtPosSquareE = " ++ showB sqrtPosSquareE
            ++ "\n sqrtNegSquareE = " ++ showB sqrtNegSquareE
            ++ "\n epsilonPos = " ++ showQ epsilonPos
            ++ "\n epsilonPos2 = " ++ showQ epsilonPos2
            ++ "\n sqrtPosZRangeL = " ++ showB sqrtPosZRangeL
            ++ "\n sqrtPosZRangeR = " ++ showB sqrtPosZRangeR
            ++ "\n sqrtPosE = " ++ showB sqrtPosE
            ++ "\n resC = " ++ showAP resC
        ) $ 
        res
    where
    showB = show . getApproximate (bits 30)
    showQ = showB . mpBall
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
    -- sqrt of the above ranges gives an epsilon value used in computing an error bound to sqrtPos and sqrtNeg:
    epsilonPos = toRationalUp $ sqrt sqrtPosSquareE
    epsilonNeg = toRationalUp $ sqrt sqrtNegSquareE
    epsilonPos2 = epsilonPos * epsilonPos
    epsilonNeg2 = epsilonNeg * epsilonNeg
    Interval sqrtPosZRangeL sqrtPosZRangeR =
        rangeOnIntervalUnaryFnA (sqrtPos, Interval zeroPoint (zeroPoint + epsilonPos2))
    Interval sqrtNegZRangeL sqrtNegZRangeR =
        rangeOnIntervalUnaryFnA (sqrtNeg, Interval (zeroPoint - epsilonNeg2) zeroPoint)
    sqrtPosE = foldl1 max [sqrtPosZRangeR, epsilonPos - sqrtPosZRangeL, mpBall (epsilonPos/2)]
    sqrtNegE = foldl1 max [sqrtNegZRangeR, epsilonNeg - sqrtNegZRangeL, mpBall (epsilonNeg/2)]
    

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
    
