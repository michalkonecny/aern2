{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
#define DEBUG
{-|
    Module      :  AERN2.Poly.Cheb.MinMax
    Description :  Poly pointwise min and max
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Poly pointwise min and max
-}
module AERN2.Poly.Cheb.MinMax where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
import Text.Printf

-- import Control.CollectErrors

-- import AERN2.Normalize

import AERN2.MP
import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.Interval

import AERN2.RealFun.Operations

-- import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Cheb.DCT
import AERN2.Poly.Cheb.MaximumInt (intify)
import AERN2.Poly.Conversion (cheb2Power)
import AERN2.Poly.Power.RootsIntVector (findRootsWithEvaluation)
import AERN2.Poly.Cheb.Ring ()

{- min/max -}

chebMaxDCT ::
  (c ~ MPBall) =>
  ChPoly c -> ChPoly c -> ChPoly c
chebMaxDCT p q =
  case diffCRoots of
    [] -- no roots -> pC and qC are separated (but p and q may still overlap)
      | diffC_onDom !<=! (- pqRB) ->
          q -- enclosure tubes are certainly separated, q >= p
      | pqRB !<=! diffC_onDom ->
          p -- enclosure tubes are certainly separated, p <= q
      | diffC_onDomL !<=! 0 ->
          updateRadius (const pqR) qC -- certainly pC <= qC
      | diffC_onDomL !>=! 0 ->
          updateRadius (const pqR) pC -- certainly pC >= qC
      | otherwise ->
          updateRadius (const (pqR + errorBound diffC_onDom)) pC -- pC and qC very close...
    _ ->
      usingDCT
  where
  acGuide = getAccuracyGuide p `max` getAccuracyGuide q
  precision = getPrecision pC `max` getPrecision qC

  -- separate radius and exact dyadic polynomials:
  pC = centreAsBall p
  qC = centreAsBall q

  pR = radius p
  qR = radius q
  pqR = pR `max` qR
  pqRB = mpBall pqR

  -- enclose roots of pC-qC:
  diff   = p - q
  diffC  = centre diff
  diffC' = derivativeExact diffC
  mapToDomRat (Interval l r) =
    Interval (t l) (t r)
    where 
    t (x :: Rational) = domL + (dyadic 0.5) * (domR - domL) * (x + 1) :: Rational
  mapToDom (Interval l r) = 
    Interval (t l) (t r)
    where 
      t (x :: Dyadic) = domL + (dyadic 0.5) * (domR - domL) * (x + 1) :: Dyadic
  evalDiffOnInterval (Interval l r) =
      evalDf diffC diffC' $
        fromEndpointsAsIntervals (mpBallP precision l) (mpBallP precision r)
  (Interval domL domR) = getDomain diffC
  diffC_onDom =
      evalDiffOnInterval (Interval (rational domL) (rational domR))
  diffC_onDomL =
      evalDirect diff (mpBallP precision domL)

  (_diffCIntErr, diffCInt) = intify diffC
  diffCRoots =
    map
    (\(Interval l r, err) ->
      (centre $ mpBallP (15 + ac2prec acGuide) $ (l + r)/!2, errorBound err)) $
    findRootsWithEvaluation
      (cheb2Power diffCInt)
      (abs . evalDiffOnInterval . mapToDomRat)
      (\v -> (v !<=! (dyadic 0.5)^!(2 + fromAccuracy acGuide)))
      (-1.0) 1.0
  segments :: [(DyadicInterval, ErrorBound)]
  segments =
    reverse $
    aux [] (domL, errorBound 0) (diffCRoots ++ [(domR, errorBound 0)])
    where
    aux is (l, e0) ((x, e1) : []) =
      (Interval l x, max e0 e1) : is
    aux is (l, e0) ((x, e1) : xs) =
      aux ((Interval l x, max e0 e1) : is) (x, e1) xs
    aux _ _ _ = error "internal error in Poly.Cheb.MinMax segments"

  -- bounding the error of a guess:
  boundError resGuess =
    foldl1 max $ map (onSegment . (\(i, e) -> (mapToDom i, e))) segments
    where
    _ = [resGuess, p] -- infer type of resGuess
    onSegment (i@(Interval l r), e)
      | pm !>=! qm = maxDifferenceFrom pC
      | otherwise = maxDifferenceFrom qC
      where
      m  = (dyadic 0.5) * (l + r)
      pm = evalDirect pC (mpBall m)
      qm = evalDirect qC (mpBall m)
      rad = max (pR + e) (qR + e)

      maxDifferenceFrom c =
        (mpBall rad) +
        ((abs (minimumOverDom diff i))  `max` (abs (maximumOverDom diff i)))
        where
        diff = c - resGuess

  initD = 16 -- degree p + degree q

  usingDCT =
    maybeTrace
    (printf "chebMaxDCT: acGuide = %s" (show acGuide)) $
    tryWithDegree NoInformation initD

  tryWithDegree prevAccuracy d =
    maybeTrace
    (printf "chebMaxDCT: tryWithDegree: d = %d" d) $
    maybeTrace
    (printf "chebMaxDCT: tryWithDegree: d = %d; getAccuracy maxCd = %s" d (show $ getAccuracy maxCd)) $
    maybeTrace
    (printf "chebMaxDCT: tryWithDegree: d = %d; dctAccuracy = %s; maxEd = %s"
      d (show dctAccuracy) (show maxEd)) $
    res
    where
    res
      | accurateEnough = updateRadius (+maxEd) maxCd
      | otherwise = tryWithDegree dctAccuracy (2*d)
    maxCd = lift2_DCT (const $ const $ d) max pC qC
    maxEd = errorBound $ boundError maxCd
    dctAccuracy = getAccuracy maxEd
    accurateEnough = dctAccuracy >= acGuide || dctAccuracy <= prevAccuracy -- stop iterating when no improvement

instance CanMinMaxAsymmetric (ChPoly MPBall) (ChPoly MPBall) where
  type MinMaxType  (ChPoly MPBall) (ChPoly MPBall) = (ChPoly MPBall)
  max p q = chebMaxDCT p q
  min p q = negate $ max (- p) (- q)
