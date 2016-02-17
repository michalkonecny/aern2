{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryChebSparse 
(
    _ucspoly1, _ucspoly1Reduced1, _ucspoly1Reduced2, _ucspolyDirect, _ucspolyDCT,
    UnaryChebSparse(..), fromList, fromListRationalWithPrec,
    ucsFixedDomain,
    normaliseCoeffs,
    Degree,
    reduceDegreeAndSweep,
    module FnReps.Polynomial.UnaryChebSparse.EvaluationRootFinding
)
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryChebSparse.Basics
import FnReps.Polynomial.UnaryChebSparse.SizeReduction
import FnReps.Polynomial.UnaryChebSparse.DCTMultiplication (multiplyDirect_terms, multiplyDCT_terms)
import FnReps.Polynomial.UnaryChebSparse.EvaluationRootFinding

_ucspoly1 :: UnaryChebSparse
_ucspoly1 = 
    fromListRationalWithPrec p [(0, 1.0),(1, 1/100),(3, 1.0)]
    where
    p = prec 100

_ucspoly1Reduced1 :: UnaryChebSparse
_ucspoly1Reduced1 =  reduceDegreeAndSweep 3 (NormBits (-2)) _ucspoly1

_ucspoly1Reduced2 :: UnaryChebSparse
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

instance
    (ArrowReal to MPBall) => 
    RealUnaryFnA to UnaryChebSparse
    where
    type UnaryFnIn UnaryChebSparse = Rational
    type UnaryFnOut UnaryChebSparse = MPBall
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
        error "UnaryChebSparse evalOnIntervalUnaryFnA not implemented yet"
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

