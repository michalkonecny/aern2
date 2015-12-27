module FnReps.Polynomial.UnaryChebSparse 
(
    UnaryChebSparse(..), fromList, fromListRationalWithPrec,
    normaliseCoeffs,
    toPowerBase,
    evalDirectOnBall,
    evalDirect,
    Degree,
    reduceDegreeAndSweep,
    _exampleDirect, _exampleDCT,
)
where

import FnReps.Polynomial.UnaryChebSparse.Basics
import FnReps.Polynomial.UnaryChebSparse.SizeReduction
import FnReps.Polynomial.UnaryChebSparse.DCTMultiplication (multiplyDirect_terms, multiplyDCT_terms)
import FnReps.Polynomial.UnaryChebSparse.EvaluationRootFinding

_example1 :: UnaryChebSparse
_example1 = 
    fromListRationalWithPrec p [(0, 1.0),(1, 1/100),(3, 1.0)]
    where
    p = prec 100

_example1Reduced1 :: UnaryChebSparse
_example1Reduced1 =  reduceDegreeAndSweep 3 (NormBits (-2)) _example1

_example1Reduced2 :: UnaryChebSparse
_example1Reduced2 =  reduceDegreeAndSweep 2 (NormZero) _example1

_exampleDirect :: Terms
_exampleDirect =
    multiplyDirect_terms _example_terms1 _example_terms2
    
_exampleDCT :: Terms
_exampleDCT =
    multiplyDCT_terms _example_terms1 _example_terms2
    
_example_terms1 :: Terms
_example_terms1 = terms_fromList [(i,integer2BallP (prec 100) 1) | i <- [0..1000]]

_example_terms2 :: Terms
_example_terms2 = terms_fromList [(i,integer2BallP (prec 100) 2) | i <- [0..1000]]


    
