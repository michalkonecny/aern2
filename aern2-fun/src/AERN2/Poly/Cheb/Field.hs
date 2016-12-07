{-# LANGUAGE TemplateHaskell #-}
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

import Numeric.MixedTypes

-- import AERN2.Normalize

import AERN2.TH

import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.Real

import AERN2.RealFun.Operations

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.DCT
import AERN2.Poly.Cheb.Maximum ()
import AERN2.Poly.Cheb.Ring ()

{- division -}

chebDivideDCT ::
  (c ~ MPBall) =>
  ChPoly c -> ChPoly c -> ChPoly c
chebDivideDCT p q
    | (minQ > 0) == Just True = r
    | otherwise =
        error "When dividing polynomials, the numerator could not be separated from 0"
        {- TODO: Use Maybe (ChPoly c) as return type?
            Then one can avoid checking the range of @q@ twice.
        -}
    where
    minQ = sepFromZero q

    pSize = terms_size $ chPoly_terms p
    qSize = terms_size $ chPoly_terms q
    minPrec = prec (100 + 2*(pSize + qSize)) -- TODO: improve this heuristic
    pWithPrec = raisePrecisionIfBelow minPrec p
    qWithPrec = raisePrecisionIfBelow minPrec q

    pC = centre pWithPrec
    qC = centre qWithPrec

    pR = mpBall $ radius p
    qR = mpBall $ radius q

    d = degree p + degree q
    rC = lift2_DCT (const $ const $ d) (/) pC qC

    divErrorBound = errorBound $
      (maxDifferenceC + pR + qR*maxRC) / minQ
      where
      maxDifferenceC = maxNorm $ pC - rC * qC
      maxRC = maxNorm rC

    r = updateRadius (+divErrorBound) rC

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
  divide = chebDivideDCT

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
