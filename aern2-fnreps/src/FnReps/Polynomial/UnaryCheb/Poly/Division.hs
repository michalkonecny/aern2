module FnReps.Polynomial.UnaryCheb.Poly.Division where

import AERN2.RealFunction
import Control.Arrow

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding ()
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication

instance (Arrow to) => CanDivA to Poly Poly where
    type DivTypeA to Poly Poly = Poly
    divA = arr aux
        where
        aux (p1@(Poly t1),p2@(Poly t2)) =
            divideDCT_poly d p1 p2
            where
            d = (terms_degree t1) + (terms_degree t2)  

instance (Arrow to) => CanDivA to Integer Poly where
    type DivTypeA to Integer Poly = Poly
    divA = arr aux
        where
        aux (n,a) = (mpBall n) / a

instance (Arrow to) => CanDivA to Rational Poly where
    type DivTypeA to Rational Poly = Poly
    divA = arr aux
        where
        aux (n,a) = (mpBall n) / a

instance (Arrow to) => CanDivA to MPBall Poly where
    type DivTypeA to MPBall Poly = Poly
    divA = arr aux
        where
        aux (n,a) = nP / a
            where
            nP = constUnaryFnA (polyFixedDomain, n) :: Poly

divideDCT_poly :: Degree -> Poly -> Poly -> Poly
divideDCT_poly d p@(Poly pTerms) q@(Poly qTerms) = 
    Poly $ terms_updateConst pmErrorBound rTerms
    where
    r@(Poly rTerms) = 
        normaliseCoeffs $
            Poly $ lift2_DCT (const $ const $ d) (/) pTerms qTerms
    pmErrorBound c = endpoints2Ball (c - errorBound) (c + errorBound) 
    errorBound =
        maxDifference / minQ
        where
        Interval _ maxDifference =
            evalOnIntervalUnaryFnA (p - r * q, polyFixedDomain)
            -- TODO: add abs
        Interval minQ _ = 
            evalOnIntervalUnaryFnA (q, polyFixedDomain)
            -- TODO: add abs
    {- 
        error <= (max(|p(x)- r(x)*q(x))|) / (min|q(x)|)
        
        where r(x) is our approximation of p(x)/q(x). 
    -}

x = projUnaryFnA (Interval (-1.0) 1.0) :: Poly
q = 1 + 100*x*x
p = constUnaryFnA (polyFixedDomain, mpBall 1) :: Poly
r = normaliseCoeffs $ Poly $ lift2_DCT (const $ const $ 40) (/) (poly_terms p) (poly_terms q)
e1 = p - r * q
e1R = evalOnIntervalUnaryFnA (setPrecision_poly (prec 200) e1, polyFixedDomain)
