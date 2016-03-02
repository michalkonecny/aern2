{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.Poly 
(
    _ucspoly1, _ucspoly1Reduced1, _ucspoly1Reduced2, 
    _ucspoly_MultiplyDCT, _ucspoly_MultiplyDirect, _ucspoly_ones,
    _ucspoly_DivDCT,
    Poly(..), showRawPoly, printRawPoly, fromList, fromListRationalWithPrec,
    ucsFixedDomain,
    normaliseCoeffs,
    Degree,
    reduceDegreeAndSweep,
    module FnReps.Polynomial.UnaryCheb.Poly.Division,
    module FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding,
    module FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power
)
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.SizeReduction
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication (multiplyDirect_terms, multiplyDCT_terms)
import FnReps.Polynomial.UnaryCheb.Poly.Division
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding
import FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power

{- examples -}

_ucspoly1 :: Poly
_ucspoly1 = 
    fromListRationalWithPrec p [(0, 1.0),(1, 1/100),(3, 1.0)]
    where
    p = prec 100

_ucspoly1Reduced1 :: Poly
_ucspoly1Reduced1 =  reduceDegreeAndSweep 3 (NormBits (-2)) _ucspoly1

_ucspoly1Reduced2 :: Poly
_ucspoly1Reduced2 =  reduceDegreeAndSweep 2 (NormZero) _ucspoly1

_ucspoly_MultiplyDCT :: Poly -> Poly -> Poly
_ucspoly_MultiplyDCT (Poly terms1) (Poly terms2) =
    Poly $ multiplyDCT_terms terms1 terms2 

_ucspoly_MultiplyDirect :: Poly -> Poly -> Poly
_ucspoly_MultiplyDirect (Poly terms1) (Poly terms2) =
    Poly $ multiplyDirect_terms terms1 terms2 

_ucspoly_ones :: Precision -> Degree -> Poly
_ucspoly_ones p d = Poly $ terms_fromList [(i,integer2BallP p 1) | i <- [0..d]]

_ucspoly_DivDCT :: Poly -> Poly -> Poly
_ucspoly_DivDCT (Poly terms1) (Poly terms2) =
    Poly $ divideDCT_terms terms1 terms2 


{- RealUnaryFnA instance and related definitions -}

ucsFixedDomain :: Interval Rational
ucsFixedDomain = Interval (-1.0) 1.0

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

