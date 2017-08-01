{-|
    Module      :  AERN2.MP.UseMPFR.Ball
    Description :  Arbitrary precision ball arithmetic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision ball arithmetic
-}
module AERN2.MP.UseMPFR.Ball
(
  -- * Auxiliary types
  module AERN2.Norm
  , module AERN2.MP.Precision
  , module AERN2.MP.Accuracy
  , module AERN2.MP.UseMPFR.ErrorBound
  , module AERN2.MP.Enclosure
  -- * The Ball type
  , MPBall(..), CanBeMPBall, mpBall, CanBeMPBallP, mpBallP
  , reducePrecionIfInaccurate
  -- * Ball construction/extraction functions
  -- , endpointsMP, fromEndpointsMP
  -- * Ball operations (see also instances)
  , piBallP
  -- * Helpers for constructing ball functions
  , byEndpointsMP
  , fromApproxWithLipschitz
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import AERN2.MP.Enclosure

import AERN2.MP.UseMPFR.ErrorBound (ErrorBound, CanBeErrorBound, errorBound)

import AERN2.MP.UseMPFR.Ball.Type
import AERN2.MP.UseMPFR.Ball.Conversions ()
import AERN2.MP.UseMPFR.Ball.Comparisons
import AERN2.MP.UseMPFR.Ball.Field ()
import AERN2.MP.UseMPFR.Ball.Elementary
import AERN2.MP.UseMPFR.Ball.PreludeOps ()

instance Ring MPBall
instance Ring (CN MPBall)
instance Field MPBall
instance Field (CN MPBall)

instance OrderedRing MPBall
instance OrderedRing (CN MPBall)
instance OrderedField MPBall
instance OrderedField (CN MPBall)

instance OrderedCertainlyRing MPBall
instance OrderedCertainlyRing (CN MPBall)
instance OrderedCertainlyField MPBall
instance OrderedCertainlyField (CN MPBall)