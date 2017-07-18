module AERN2.Local.Integration where

import MixedTypesNumPrelude
import AERN2.RealFun.Operations
import AERN2.Interval
import AERN2.MP.Ball
import AERN2.MP.Dyadic
--import AERN2.MP.Accuracy
import Data.List

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Local.Basics

instance
  (CanSetAccuracyGuide f,
    CanIntegrateOverDom f DyadicInterval,
    c ~ IntegralOverDomType f DyadicInterval,
    HasIntegers c,
    CanAddSameType c,
    CanMul Dyadic c,
    MulType Dyadic c ~ c)
  => (CanIntegrateOverDom (Local f) DyadicInterval)
  where
    type IntegralOverDomType (Local f) DyadicInterval =
      Accuracy -> IntegralOverDomType f DyadicInterval
    integrateOverDom f (Interval l r) ac =
      foldl' (+) (head integrals) (tail integrals)
      --integrateOverDom (f l r ac) dom
      where
      n = max 2 $ fromAccuracy ac
      ac' = ac + (bits . integerLog2 . fromAccuracy) ac + 1
      -- ac' = ac + n + 1
      ps = [l + k*(r - l)/!n | k <- [0 .. n]]
      dyPs = map (centre . mpBallP (prec $ fromAccuracy ac)) ps
      dyIntervals = zip dyPs (tail dyPs)
      integrals = [integrateOverDom (f a b ac') (Interval a b) | (a,b) <- dyIntervals]
