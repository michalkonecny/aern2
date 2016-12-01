{-|
    Module      :  AERN2.Poly.Cheb
    Description :  Chebyshev basis unary sparse polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis unary sparse polynomials
-}

module AERN2.Poly.Cheb
(
  module AERN2.Poly.Cheb.Type
, module AERN2.Poly.Cheb.Eval
, module AERN2.Poly.Cheb.Maximum
, module AERN2.Poly.Cheb.Derivative
, module AERN2.Poly.Cheb.Integration
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import AERN2.Interval
import AERN2.MP

import AERN2.RealFun.Operations
import AERN2.RealFun.SineCosine

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Maximum
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Cheb.Integration


_chPolySineX :: Accuracy -> ChPoly MPBall
_chPolySineX ac =
    sineWithAccuracyGuide ac x
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPolySine10X :: Accuracy -> ChPoly MPBall
_chPolySine10X ac =
    sineWithAccuracyGuide ac (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPolyCosine10X :: Accuracy -> ChPoly MPBall
_chPolyCosine10X ac =
    cosineWithAccuracyGuide ac (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPoly10X :: ChPoly MPBall
_chPoly10X =
    10*x
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)

_chPolySine10XSine20XX :: Accuracy -> ChPoly MPBall
_chPolySine10XSine20XX ac =
    sine(10*x + sine(20*x*x))
    where
    sine = sineWithAccuracyGuide ac
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (-1.0,1.0)


{-

_testSine10X :: ChPoly MPBall
_testSine10X =
    sineWithPrecDegSweep (prec 100) 100 NormZero (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

_testSine10Xe :: ChPoly MPBall
_testSine10Xe =
    sineWithPrecDegSweep (prec 100) 100 NormZero (updateRadius (+ (errorBound 0.1)) (10*x))
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

-}
