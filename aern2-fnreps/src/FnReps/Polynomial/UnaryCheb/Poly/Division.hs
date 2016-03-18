module FnReps.Polynomial.UnaryCheb.Poly.Division where

import AERN2.RealFunction
import Control.Arrow

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding ()
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication (lift2_DCT, lift1_DCT)

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
divideDCT_poly d _p@(Poly pTerms) q@(Poly qTerms) 
    | (minQ > 0) == Just True = r
    | otherwise = 
        error "When dividing polynomials, the numerator could not be separated from 0"
        {- TODO: Use Maybe Poly as return type?  
            Then one can avoid checking the range of @q@ twice.
        -} 
    where
    pCTerms = terms_updateConst ballCentre pTerms
    qCTerms = terms_updateConst ballCentre qTerms
    pR = ballRadius $ terms_lookupCoeff pTerms 0
    qR = ballRadius $ terms_lookupCoeff qTerms 0
    pC = Poly pCTerms
    qC = Poly qCTerms
    rC@(Poly rCTerms) = 
        normaliseCoeffs $
            Poly $ lift2_DCT (const $ const $ d) (/) pCTerms qCTerms
    r = Poly $ terms_updateConst pmErrorBound rCTerms
        where 
        pmErrorBound c = endpoints2Ball (c - errorBound) (c + errorBound) 
    errorBound =
        (maxDifferenceC + pR + qR*maxRC) / minQ
    Interval _ maxDifferenceC =
        abs $ rangeOnIntervalUnaryFnA (pC - rC * qC, polyFixedDomain)
    Interval _ maxRC =
        abs $ rangeOnIntervalUnaryFnA (rC, polyFixedDomain)
    Interval minQ _ =
        abs $ rangeOnIntervalUnaryFnA (q, polyFixedDomain)
    {- 
        |r(x) - p(x)/q(x)| <= max(|p(x) - r(x)*q(x)|) / min(|q(x)|)
        
        Assuming q(x) does not change sign, min(|q(x)|) = min |range(q(x))|.
        
        Even if f changes sign, we have max(|f(x)|) = max |range(f(x))|.
        
        With f = p - rq in the above, we reduce the range to centres as follows: 
            range(p(x) - r(x)*q(x)) 
            = range(pC(x) ± pR - r(x)*(qC(x)±qR))
            ⊆ range(pC(x) ± pR - r(x)*qC(x) ± r(x)*qR))
            ⊆ range(pC(x) - r(x)*qC(x)) ± pR ± max(r(x))*qR
    -}

{-
  The following is an aad-hoc test of division.  
  As expected, the degree of the result polynomial affects the error bound.
  For degrees in range 33-64 the error is around 10^-2 and in the range 
  65-128 the error is around 10^-5.
-}

_x :: Poly
_x = setPrecision_poly (prec 100) $ projUnaryFnA (Interval (-1.0) 1.0) :: Poly
_q :: Poly
_q = 1 + 100*_x*_x
_p :: Poly
_p = setPrecision_poly (prec 100) $ constUnaryFnA (polyFixedDomain, mpBall 1) :: Poly
_r :: Poly
_r = normaliseCoeffs $ Poly $ lift2_DCT (const $ const $ 65) (/) (poly_terms _p) (poly_terms _q)
e1 :: Poly
e1 = _p - _r * _q
e1R :: Interval MPBall
e1R = rangeOnIntervalUnaryFnA (setPrecision_poly (prec 200) e1, polyFixedDomain)

_sqrtAbs :: Integer -> Poly
_sqrtAbs d = Poly rTerms
    where
    rTerms = lift1_DCT (const d) (\b -> sqrt (abs b)) xTerms 
    (Poly xTerms) = _x
    
    
    