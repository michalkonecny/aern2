{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
#define DEBUG
{-|
    Module      :  AERN2.Poly.Cheb.Field
    Description :  Poly division and integer power
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Poly division and integer power
-}
module AERN2.Poly.Cheb.Field where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (flip const)
#endif

import Numeric.MixedTypes
import Text.Printf

-- import AERN2.Normalize

import AERN2.TH

import AERN2.MP
import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.RealFun.Operations

-- import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.DCT
import AERN2.Poly.Cheb.Maximum ()
import AERN2.Poly.Cheb.Ring ()

{- division -}

chebDivideDCT ::
  (c ~ MPBall) =>
  Accuracy -> ChPoly c -> ChPoly c -> ChPoly c
chebDivideDCT acGuide p q
    | (minQ > 0) == Just True = r
    | otherwise =
        error "When dividing polynomials, the numerator could not be separated from 0"
        {- TODO: Use Maybe (ChPoly c) as return type?
            Then one can avoid checking the range of @q@ twice.
        -}
    where
    minQ = sepFromZero q

    pC = centreAsBall p
    qC = centreAsBall q

    pR = mpBall $ radius p
    qR = mpBall $ radius q

    initD = degree p + degree q

    r =
      maybeTrace
      (printf "chebDivideDCT: acGuide = %s, minQ = %s" (show acGuide) (show minQ)) $
      tryWithDegree initD

    tryWithDegree d =
      maybeTrace
      (printf "chebDivideDCT: tryWithDegree: d = %d" d) $
      maybeTrace
      (printf "chebDivideDCT: tryWithDegree: d = %d; getAccuracy rCd = %s" d (show $ getAccuracy rCd)) $
      maybeTrace
      (printf "chebDivideDCT: tryWithDegree: d = %d; rCMaxNorm = %s" d (show rCMaxNorm)) $
      maybeTrace
      (printf "chebDivideDCT: tryWithDegree: d = %d; maxDifferenceC = %s; dctAccuracy = %s; getAccuracy rEd = %s"
        d (show maxDifferenceC) (show dctAccuracy) (show $ getAccuracy rEd)) $
      res
      where
      res
        | accurateEnough = updateRadius (+rEd) rCd
        | otherwise = tryWithDegree (2*d)
      rCd = lift2_DCT (const $ const $ d) (/) pC qC
      rEd = errorBound $
        (maxDifferenceC + pR + qR * rCMaxNorm) / minQ
      maxDifferenceC = maxNorm $ pC - rCd * qC
      rCMaxNorm = maxNorm rCd
      accurateEnough = dctAccuracy > acGuide
      dctAccuracy = getAccuracy (errorBound $ maxDifferenceC/minQ)

    {-
        |r(x) - p(x)/q(x)| <= max(|p(x) - r(x)*q(x)|) / min(|q(x)|)

        Assuming q(x) does not change sign, min(|q(x)|) = min |range(q(x))|.

        Even if f changes sign, we have max(|f(x)|) = max |range(f(x))|.

        With f = p - rq in the above, we reduce the range to centres as follows:
            range(p(x) - r(x)*q(x))
            = range(pC(x) ± pR - r(x)*(qC(x)±qR))
            ⊆ range(pC(x) ± pR - r(x)*qC(x) ± r(x)*qR))
            ⊆ range(pC(x) - r(x)*qC(x)) ± pR ± max(r(x))*qR
    -}


maxNorm ::
  (r ~ MaximumOverDomType f (Domain f)
  , r ~ MinimumOverDomType f (Domain f)
  , CanAbsSameType r
  , CanMinMaxSameType r
  , CanMaximiseOverDom f (Domain f)
  , CanMinimiseOverDom f (Domain f)
  , HasDomain f)
  =>
  f -> r
maxNorm f =
  (abs $ f `maximumOverDom` dom)
  `max`
  (abs $ f `minimumOverDom` dom)
  where
  dom = getDomain f

sepFromZero ::
  (r ~ MaximumOverDomType f (Domain f)
  , r ~ MinimumOverDomType f (Domain f)
  , CanNegSameType r
  , CanMinMaxSameType r
  , CanMaximiseOverDom f (Domain f)
  , CanMinimiseOverDom f (Domain f)
  , HasDomain f)
  =>
  f -> r
sepFromZero f =
  (negate $ f `maximumOverDom` dom)
  `max`
  (f `minimumOverDom` dom)
  where
  dom = getDomain f

instance CanDiv (ChPoly MPBall) (ChPoly MPBall) where
  divide p q = chebDivideDCT acGuide p q
    where
    acGuide = getFiniteAccuracy p `min` getFiniteAccuracy q

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
  instance CanDiv $t (ChPoly MPBall) where
    type DivType $t (ChPoly MPBall) = ChPoly MPBall
    divide n p = divide nP p
      where
      _ = [nP,p]
      nP = chPoly (getDomain p,n)
  |]))

-- instance CanDiv MPBall (ChPoly MPBall) where
--   type DivType MPBall (ChPoly MPBall) = ChPoly MPBall
--   divide n p = divide nP p
--     where
--     _ = [nP,p]
--     nP = chPoly (getDomain p,n)
