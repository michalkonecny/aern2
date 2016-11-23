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
, module AERN2.Poly.Cheb.SineCosine
)
where

-- import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import AERN2.Interval
-- import AERN2.MP.Ball
-- import AERN2.MP.ErrorBound

-- import AERN2.RealFun.Operations

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Maximum
import AERN2.Poly.Cheb.SineCosine
