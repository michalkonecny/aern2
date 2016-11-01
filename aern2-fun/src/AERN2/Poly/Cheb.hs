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
  module AERN2.Poly.Cheb.Type,
  module AERN2.Poly.Cheb
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import AERN2.RealFun.Operations

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()
import AERN2.Poly.Cheb.Eval ()
-- import AERN2.Poly.Cheb.SineCosine

-- import AERN2.MP.Dyadic
import AERN2.Interval

{- examples -}

_pb_const1 :: PolyBall
_pb_const1 =
    constFn (dom, 1)
    where
    dom = dyadicInterval (0.0,1.0)

_pb_X :: PolyBall
_pb_X =
    varFn sampleFn ()
    where
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)
