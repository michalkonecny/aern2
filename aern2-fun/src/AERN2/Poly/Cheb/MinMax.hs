{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
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

import Control.CollectErrors

import AERN2.Normalize

import AERN2.MP
import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.DCT
import AERN2.Poly.Cheb.MaximumInt (intify)
import AERN2.Poly.Conversion (cheb2Power)
import AERN2.Poly.Power.RootsIntVector (findRootsWithAccuracy)
import AERN2.Poly.Cheb.Ring ()

{- min/max -}

-- isolateIntersections ::
--   -- (PolyCoeffRing c, CanNormalize (ChPoly c)) =>
--   (c~MPBall) =>
--   ChPoly c -> ChPoly c -> [(DyadicInterval, Maybe Ordering, MPBall)]
-- isolateIntersections p q =
--   mergeSameSign $
--     bisect (getDomain p)
--   where
--   acGuide = getAccuracyGuide p `max` getAccuracyGuide q
--   bisect dom
--     | diffMin !>=! 0 = [(dom, Just GT, diffMin)]
--     | diffMax !<=! 0 = [(dom, Just LT, diffMax)]
--     | getAccuracy diffBall >= acGuide = [(dom, Nothing, diffBall)]
--     | otherwise = bisect domL ++ bisect domR
--     where
--     diff = p - q
--     diffMax = maximumOverDom diff dom
--     diffMin = minimumOverDom diff dom
--     diffBall = fromEndpoints diffMin diffMax :: MPBall
--     (domL, domR) = split dom
--   mergeSameSign =
--     map mergeGroup . groupBy sameSign
--     where
--     sameSign (_dom1, Just ord1, _) (_dom2, Just ord2, _) = (ord1 == ord2)
--     sameSign _ _ = False
--     mergeGroup = foldl1 mergeSegments
--     mergeSegments (dom1, Just LT, diff1) (dom2, Just LT, diff2) =
--       (dom1 `union` dom2, Just LT, diff1 `max` diff2)
--     mergeSegments (dom1, Just GT, diff1) (dom2, Just GT, diff2) =
--       (dom1 `union` dom2, Just GT, diff1 `min` diff2)

chebMaxDCT ::
  (c ~ MPBall) =>
  ChPoly c -> ChPoly c -> ChPoly c
chebMaxDCT p q =
  case diffCRoots of
    [] -> undefined -- TODO: p or q, but sometimes need to enlarge the radius
    _ -> undefined -- TODO: use DCT with increasing size until the accuracy is OK
  where
  acGuide = getAccuracyGuide p `max` getAccuracyGuide q

  -- separate radius and exact dyadic polynomials:
  pC = centreAsBall p
  qC = centreAsBall q

  pR = mpBall $ radius p
  qR = mpBall $ radius q

  -- enclose roots of pC-qC:
  diffC = pC - qC
  Interval domL domR = getDomain diffC
  (diffCIntErr, diffCInt) = intify diffC
  diffCRoots = findRootsWithAccuracy (cheb2Power diffCInt) acGuide (rational domL) (rational domR)

  rootSegments = aux (rational domL) diffCRoots
    where
    aux l [] = [(l,rational domR, diffSample l)]
    aux l (Interval rootsL rootsR:rest) =
      (l, rootsL, diffSample l):(rootsL, rootsR, Nothing):(aux rootsR rest)
    diffSample l = Just $ apply diffC (mpBallP (ac2prec acGuide) l)

  -- bounding the error of a guess:
  boundError resGuess =
    foldl1 max $ map onSegment rootSegments
    where
    onSegment (l, r, Nothing) = undefined :: MPBall

-- initD = 16 -- degree p + degree q
--
--
--     r =
--       maybeTrace
--       (printf "chebDivideDCT: acGuide = %s, minQ = %s" (show acGuide) (show minQ)) $
--       tryWithDegree NoInformation initD
--
--     tryWithDegree prevAccuracy d =
--       maybeTrace
--       (printf "chebDivideDCT: tryWithDegree: d = %d" d) $
--       maybeTrace
--       (printf "chebDivideDCT: tryWithDegree: d = %d; getAccuracy rCd = %s" d (show $ getAccuracy rCd)) $
--       maybeTrace
--       (printf "chebDivideDCT: tryWithDegree: d = %d; rCMaxNorm = %s" d (show rCMaxNorm)) $
--       maybeTrace
--       (printf "chebDivideDCT: tryWithDegree: d = %d; maxDifferenceC = %s; dctAccuracy = %s; getAccuracy rEd = %s"
--         d (show maxDifferenceC) (show dctAccuracy) (show $ getAccuracy rEd)) $
--       res
--       where
--       res
--         | accurateEnough = updateRadius (+rEd) rCd
--         | otherwise = tryWithDegree dctAccuracy (2*d)
--       rCd = lift2_DCT (const $ const $ d) (/!) pC qC
--       rEd = errorBound $
--         (maxDifferenceC + pR + qR * rCMaxNorm) /! minQ
--       maxDifferenceC = maxNorm $ pC - rCd * qC
--       rCMaxNorm = maxNorm rCd
--       accurateEnough = dctAccuracy >= acGuide || dctAccuracy <= prevAccuracy -- stop iterating when no improvement
--       dctAccuracy = getAccuracy (errorBound $ maxDifferenceC/!minQ)
--
--     {-
--         |r(x) - p(x)/q(x)| <= max(|p(x) - r(x)*q(x)|) / min(|q(x)|)
--
--         Assuming q(x) does not change sign, min(|q(x)|) = min |range(q(x))|.
--
--         Even if f changes sign, we have max(|f(x)|) = max |range(f(x))|.
--
--         With f = p - rq in the above, we reduce the range to centres as follows:
--             range(p(x) - r(x)*q(x))
--             = range(pC(x) ± pR - r(x)*(qC(x)±qR))
--             ⊆ range(pC(x) ± pR - r(x)*qC(x) ± r(x)*qR))
--             ⊆ range(pC(x) - r(x)*qC(x)) ± pR ± max(r(x))*qR
--     -}
--
--
-- maxNorm ::
--   (r ~ MaximumOverDomType f (Domain f)
--   , r ~ MinimumOverDomType f (Domain f)
--   , CanAbsSameType r
--   , CanMinMaxSameType r
--   , CanMaximiseOverDom f (Domain f)
--   , CanMinimiseOverDom f (Domain f)
--   , HasDomain f)
--   =>
--   f -> r
-- maxNorm f =
--   (abs $ f `maximumOverDom` dom)
--   `max`
--   (abs $ f `minimumOverDom` dom)
--   where
--   dom = getDomain f
--
-- sepFromZero ::
--   (r ~ MaximumOverDomType f (Domain f)
--   , r ~ MinimumOverDomType f (Domain f)
--   , CanNegSameType r
--   , CanMinMaxSameType r
--   , CanMaximiseOverDom f (Domain f)
--   , CanMinimiseOverDom f (Domain f)
--   , HasDomain f)
--   =>
--   f -> r
-- sepFromZero f =
--   (negate $ f `maximumOverDom` dom)
--   `max`
--   (f `minimumOverDom` dom)
--   where
--   dom = getDomain f
--
-- instance CanDiv (ChPoly MPBall) (ChPoly MPBall) where
--   type DivTypeNoCN  (ChPoly MPBall) (ChPoly MPBall) = ChPoly MPBall
--   divideNoCN p q = ((divide p q) ~!)
--   type DivType  (ChPoly MPBall) (ChPoly MPBall) = CN (ChPoly MPBall)
--   divide p q = chebDivideDCT acGuide p q
--     where
--     acGuide = getAccuracyGuide p `max` getAccuracyGuide q
