{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.Poly 
(
    _ucspoly1, _ucspoly1Reduced1, _ucspoly1Reduced2, _ucspolyDirect, _ucspolyDCT,
    Poly(..), fromList, fromListRationalWithPrec,
    ucsFixedDomain,
    normaliseCoeffs,
    Degree,
    reduceDegreeAndSweep,
    module FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding
)
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.SizeReduction
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication (multiplyDirect_terms, multiplyDCT_terms)
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding

_ucspoly1 :: Poly
_ucspoly1 = 
    fromListRationalWithPrec p [(0, 1.0),(1, 1/100),(3, 1.0)]
    where
    p = prec 100

_ucspoly1Reduced1 :: Poly
_ucspoly1Reduced1 =  reduceDegreeAndSweep 3 (NormBits (-2)) _ucspoly1

_ucspoly1Reduced2 :: Poly
_ucspoly1Reduced2 =  reduceDegreeAndSweep 2 (NormZero) _ucspoly1

_ucspolyDirect :: Terms
_ucspolyDirect =
    multiplyDirect_terms _ucsterms1 _ucsterms2
    
_ucspolyDCT :: Terms
_ucspolyDCT =
    multiplyDCT_terms _ucsterms1 _ucsterms2
    
_ucsterms1 :: Terms
_ucsterms1 = terms_fromList [(i,integer2BallP (prec 100) 1) | i <- [0..1000]]

_ucsterms2 :: Terms
_ucsterms2 = terms_fromList [(i,integer2BallP (prec 100) 2) | i <- [0..1000]]

ucsFixedDomain :: Interval Rational
ucsFixedDomain = Interval (-1.0) 1.0

instance HasPrecision Poly where
    getPrecision (Poly terms) =
        foldl1 min $ map getPrecision $ terms_coeffs terms

instance HasAccuracy Poly where
    getAccuracy (Poly terms) =
        getAccuracy $ terms_lookupCoeff terms 0

instance
    (ArrowReal to MPBall) => 
    RealUnaryFnA to Poly
    where
    type UnaryFnIn Poly = Rational
    type UnaryFnOut Poly = MPBall
    getDomainUnaryFnA =
        arr $ const ucsFixedDomain
    constUnaryFnA =
        proc (_dom, value) ->
            returnA -< fromList [(0,value)]
    projUnaryFnA =
        proc (_dom) ->
            do
            a1 <- convertA -< 1
            returnA -< fromList [(1,a1)]
    evalOnIntervalUnaryFnA =
        error "UnaryCheb.Poly evalOnIntervalUnaryFnA not implemented yet"
    evalAtInPointUnaryFnA =
        proc (f, x) ->
            do
            xB <- convertA -< x
            evalAtOutPointUnaryFnA -< (f,xB)
    evalAtOutPointUnaryFnA =
        proc (f, x) ->
            do
            case getAccuracy x of
                Exact ->
                    returnA -< evalDirectOnBall f x
                _ ->  
                    returnA -< evalLipschitzOnBall f x

