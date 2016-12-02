{-|
    Module      :  AERN2.Poly.Cheb.Integration
    Description :  Chebyshev basis derivative
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis derivative
-}
module AERN2.Poly.Cheb.Integration where

import Numeric.MixedTypes

import AERN2.Normalize

import AERN2.MP

import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()

instance
  (PolyCoeff c, CanApply (ChPoly c) c, ApplyType (ChPoly c) c ~ c)
  =>
  CanIntegrateOverDom (ChPoly c) DyadicInterval
  where
  type IntegralOverDomType (ChPoly c) DyadicInterval = c
    -- no necessarily convergent, the accuracy is only a guide
  integrateOverDom (cp :: ChPoly c) (Interval l r) =
    (primit `apply` rB) - (primit `apply` lB)
    where
    acGuide = getFiniteAccuracy cp
    primit = primitive_function cp
    lB = setPrecisionAtLeastAccuracy acGuideEval (convertExactly l :: c)
    rB = setPrecisionAtLeastAccuracy acGuideEval (convertExactly r :: c)
    acGuideEval = acGuide + 10
    -- prc = getPrecision cp

primitive_function ::
  (Ring c, CanDivBy c Integer, CanNormalize (ChPoly c))
  =>
  ChPoly c -> ChPoly c
primitive_function (ChPoly dom (Poly terms)) =
  normalize $ ChPoly dom $ Poly $ terms_fromListAddCoeffs $
    concat $ map oneTerm $ terms_toList terms
  where
  oneTerm (n,a)
    | n == 0 = [(1,a)]
    | n == 1 = [(0,a/4), (2,a/4)]
    | otherwise =
      [(n-1, -a/(2*(n-1))),
       (n+1, a/(2*(n+1)))]
