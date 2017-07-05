module AERN2.Local.SineCosine where

import MixedTypesNumPrelude
import AERN2.RealFun.SineCosine
import AERN2.MP
import AERN2.Real
import AERN2.RealFun.Operations
import AERN2.Local.Basics

sineLocal ::
  (HasDomain f, CanApplyApprox f (Domain f)
  , ConvertibleExactly (ApplyApproxType f (Domain f)) MPBall
  , CanNegSameType f, CanAddSameType f, CanMulSameType f
  , CanAddSubMulDivCNBy f Integer, CanAddSubMulDivCNBy f CauchyReal
  , HasAccuracy f, CanSetPrecision f, CanReduceSizeUsingAccuracyGuide f
  , IsBall f
  , Show f)
  => Local f -> Local f
sineLocal f =
  \l r ac -> sineWithAccuracyGuide ac (f l r ac)

cosineLocal ::
  (HasDomain f, CanApplyApprox f (Domain f)
  , ConvertibleExactly (ApplyApproxType f (Domain f)) MPBall
  , CanNegSameType f, CanAddSameType f, CanMulSameType f
  , CanAddSubMulDivCNBy f Integer, CanAddSubMulDivCNBy f CauchyReal
  , HasAccuracy f, CanSetPrecision f, CanReduceSizeUsingAccuracyGuide f
  , IsBall f
  , Show f)
  => Local f -> Local f
cosineLocal f =
  \l r ac -> cosineWithAccuracyGuide ac (f l r ac)
