module FnReps.Polynomial.UnaryChebSparse 
(
    _ucspoly1, _ucspoly1Reduced1, _ucspoly1Reduced2, _ucspolyDirect, _ucspolyDCT,
    UnaryChebSparse(..), fromList, fromListRationalWithPrec,
    normaliseCoeffs,
    toPowerBase,
    evalDirectOnBall,
    evalDirect,
    Degree,
    reduceDegreeAndSweep
)
where

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


    

