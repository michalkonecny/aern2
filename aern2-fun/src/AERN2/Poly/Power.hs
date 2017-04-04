{-|
    Module      :  AERN2.Poly.Power
    Description :  Power basis unary sparse polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Power basis unary sparse polynomials
-}

module AERN2.Poly.Power
(
  module AERN2.Poly.Power.Type
, module AERN2.Poly.Power.Roots
, module AERN2.Poly.Power.Eval
, module AERN2.Poly.Power.Maximum
, module AERN2.Poly.Power.SizeReduction
)
where

-- import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import AERN2.Interval
-- import AERN2.MP.Ball
-- import AERN2.MP.ErrorBound

-- import AERN2.RealFun.Operations

import AERN2.Poly.Power.Type
import AERN2.Poly.Power.SizeReduction
import AERN2.Poly.Power.Roots
import AERN2.Poly.Power.Eval
import AERN2.Poly.Power.Maximum
