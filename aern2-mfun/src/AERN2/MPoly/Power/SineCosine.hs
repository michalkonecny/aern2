module AERN2.MPoly.Power.SineCosine where

import MixedTypesNumPrelude

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.MPoly.Power.Type
import AERN2.MPoly.Power.Evaluation
import AERN2.MPoly.Power.Ring

import Data.Maybe

import Debug.Trace

import Data.Function ((&))

import qualified Data.Map as Map
import qualified Data.Vector as V

polySine :: PowMPoly MPBall -> PowMPoly MPBall
polySine p =
  resC
  & updateRadius (+ err)
  & setAccuracyGuide (ac `min` getAccuracy resC)
  & reduceDegreeToAccuracy
  & setAccuracyGuide ac
  where
  pC = centre p
  resC =
    updateRadius (+ ((radius p) + (errorBound tErr))) $ makeTerms 1 (pC*pCSquared/!6) pC
  err = (radius p) + (errorBound tErr)
  pCSquared = setAccuracyGuide (2*ac) $ pC*pC
  makeTerms n acc res
    | n == numTerms + 1 = res
    | otherwise =
        let
          sgn
            | odd n     = (mpBall $ -1)
            | otherwise = (mpBall 1)
        in
        makeTerms (n + 1)
        ((acc * pCSquared) /! ((2*(n + 1) + 1) * 2*(n + 1)))
        (res + sgn*acc)
  bnd       = setPrecisionAtLeastAccuracy (2*ac) $ domBound pC
  ac        = getAccuracyGuide p
  (numTerms, tErr)  = computeNumTermsAndError 1
  computeNumTermsAndError n =
    let
      te = tailErr n
    in
    if isJust te && (fromJust te) !<! 0.5^!(fromAccuracy ac) then
      (n, fromJust te)
    else
      computeNumTermsAndError (n + 1)
  tailErr :: Integer -> Maybe MPBall
  tailErr n =
    if alpha*alpha !<! (mpBall 1) then
      Just $ (~!) $ alpha * (alpha^!(2*n + 2)) /! ((1 - alpha*alpha) * (sqrt (12*n + 6)))
    else
      Nothing
    where
    alpha = (exp(1) * bnd)/!(2*n + 1)
