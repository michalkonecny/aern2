{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
#define DEBUG
{-|
    Module      :  AERN2.PPoly.MinMax
    Description :  PPoly pointwise min and max
    Copyright   :  (c) Michal Konecny, Eike Neumann
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Poly pointwise min and max
-}
module AERN2.PPoly.MinMax where

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
import AERN2.Poly.Ball

import AERN2.PPoly.Type

{- min/max -}

instance CanMinMaxAsymmetric PPoly PPoly where
  type MinMaxType PPoly PPoly = PPoly
  max = ppolyMax
  min a b = negate (max (-a) (-b))

ppolyMax ::
  PPoly -> PPoly -> PPoly
ppolyMax a b =
  PPoly (concat (map pballMax (refine a b)))
        (ppoly_dom a) -- TODO: how to handle polys with different domains?
  where
  pballMax (seg@(Interval domL domR), p@(Ball pC pR), q@(Ball qC qR)) =
    maybeTrace (printf "ppolyMax: seg=%s rootSegments = %s\n" (show seg) (show rootSegments)) $
    mapWithNeighbours maxOnSegment_Root $ map maxOnSegment1 rootSegments
    where
    acGuide = getAccuracyGuide pC `max` getAccuracyGuide qC

    diffC = pC - qC
    (diffCIntErr, diffCInt) = intify diffC
    diffCRoots = findRootsWithAccuracy (cheb2Power diffCInt) acGuide (rational domL) (rational domR)

    rootSegments :: [(DyadicInterval, Maybe MPBall)]
    rootSegments = aux domL diffCRoots
      where
      aux l [] = [(Interval l domR, diffSample l)]
      aux l (Interval rootsL rootsR:rest)
        | l < rootsL_L =
          (Interval l rootsL_L, diffSample l):(Interval rootsL_L rootsR_R, Nothing):(aux rootsR_R rest)
        | otherwise =
          (Interval rootsL_L rootsR_R, Nothing):(aux rootsR_R rest)
        where
        (Interval rootsL_L _) = dyadicInterval $ mpBallP (ac2prec acGuide) rootsL
        (Interval _ rootsR_R) = dyadicInterval $ mpBallP (ac2prec acGuide) rootsR
      diffSample l = Just $ apply diffC (mpBallP (ac2prec acGuide) l)

    maxOnSegment1 seg@(_, Nothing) = Left seg
    maxOnSegment1 (i@(Interval l r), Just diffSample) = Right (fl , (i,f), fr)
      where
      f = if diffSample !>! 0 then p else q
      fl = apply f (mpBall l)
      fr = apply f (mpBall r)
    maxOnSegment_Root (_,Right (_,i_f,_), _) = i_f
    maxOnSegment_Root (mprev, Left (i@(Interval l r), Nothing), mpost) =
      (i, linearinterpolation)
      where
      PPoly [(_,linearinterpolation)] _ =
        -- TODO: increase radius by 2^(-acGuide)
        linearPolygonI [(l, fl), (r, fr)] undefined acGuide
      fl =
        case mprev of
          Just (Right (_,_,v)) -> v
          _ -> ((apply p (mpBall l)) + (apply q (mpBall l)))/!2
      fr =
        case mpost of
          Just (Right (v,_,_)) -> v
          _ -> ((apply p (mpBall r)) + (apply q (mpBall r)))/!2

mapWithNeighbours :: ((Maybe a, a, Maybe a) -> b) -> [a] -> [b]
mapWithNeighbours f list = aux Nothing list
  where
  aux prev [] = []
  aux prev [x] = [f (prev,x,Nothing)]
  aux prev (x:y:rest) = (f (prev,x,Just y)) : aux (Just x) (y:rest)

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
