{-|
    Module      :  AERN2.MP.Ball
    Description :  Arbitrary precision ball arithmetic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision ball arithmetic
-}
module AERN2.MP.Ball
(
  -- * Auxiliary types
  module AERN2.Norm
  , module AERN2.MP.Precision
  , module AERN2.MP.Accuracy
  -- * The Ball type
  , MPBall(..), CanBeMPBall, mpBall, CanBeMPBallP, mpBallP
  , setPrecisionAtLeastAccuracy
  , reducePrecionIfInaccurate
  -- * Ball construction/extraction functions
  , centre, radius
  , centreAndErrorBall
  , endpoints, fromEndpoints
  , integerBounds
  -- , endpointsMP, fromEndpointsMP
  -- * Ball operations (see also instances)
  , intersect
  , piBallP
  -- * Helpers for constructing ball functions
  , byEndpointsMP
  , monotoneFromApprox
  , fromApproxWithLipschitz
)
where

-- import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Accuracy

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions
import AERN2.MP.Ball.Comparisons
import AERN2.MP.Ball.Field ()
import AERN2.MP.Ball.Elementary
