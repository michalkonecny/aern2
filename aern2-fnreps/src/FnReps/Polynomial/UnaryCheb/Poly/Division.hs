module FnReps.Polynomial.UnaryCheb.Poly.Division where

import AERN2.RealFunction

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication


divideDCT_poly :: Degree -> Poly -> Poly -> Poly
divideDCT_poly d p@(Poly pTerms) q@(Poly qTerms) = r
    where
    r = normaliseCoeffs $
            Poly $
                terms_updateConst pmErrorBound $ 
                    lift2_DCT (const $ const $ d) (/) pTerms qTerms
    pmErrorBound c = endpoints2Ball (c - errorBound) (c + errorBound) 
    errorBound =
        maxDifference / minQ
        where
        Interval _ maxDifference =
            evalOnIntervalUnaryFnA (p - r * q, polyFixedDomain)
        Interval minQ _ = 
            evalOnIntervalUnaryFnA (q, polyFixedDomain)
    {- 
        error <= (max(p(x)- r(x)*q(x))) / (min|q(x)|)
        
        where r(x) is our approximation of p(x)/q(x). 
    -}


