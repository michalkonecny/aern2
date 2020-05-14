module AERN2.Poly.Cheb.MaxNaive
(
  maxNaive
)
where

import MixedTypesNumPrelude hiding (maximum, minimum)

import AERN2.MP.Ball

import Data.List

import AERN2.Poly.Cheb
import AERN2.Poly.Basics
import AERN2.Poly.Power hiding (evalDf)
import AERN2.Poly.Conversion
import AERN2.Interval

import qualified Data.Map as Map
import Data.Ratio

maxNaive :: ChPoly MPBall -> Rational -> Rational -> Accuracy -> MPBall
maxNaive f a b bts' =
  mxmm
  where
  bts = min bts' (getAccuracy f)
  dfc@(ChPoly _ _p _acG _) = (derivativeExact  . centre) f
  (_err, dfci) = intify dfc
  q = cheb2Power dfci
  rts =
    findRoots q
    (\i -> getAccuracy (evalF i) >= bts)
    a b
  evalF (Interval c d) =
    evalDf f dfc (fromEndpointsAsIntervals (mpBallP prc c) (mpBallP prc d))
  prc = getPrecision f
  mxmm =
    foldl'
      (\m x -> max m (evalF x))
      (evalF (Interval 1.0 1.0))
      ((Interval (-1.0) (-1.0)) : rts)

  intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
  intify (ChPoly _ p _acG _) =
    (err, pInt)
    where
    termsRational = terms_map (rational . ball_value) (poly_terms p)
    err = termsError * termsDenominator
    termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
    termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
    pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational
