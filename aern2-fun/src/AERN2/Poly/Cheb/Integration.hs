{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.Poly.Cheb.Integration
    Description :  Chebyshev basis integration
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis integration
-}
module AERN2.Poly.Cheb.Integration where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import MixedTypesNumPrelude

import AERN2.Normalize

import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()

instance
  (Ring c, CanDivCNBy c Integer, CanNormalize (ChPoly c)
  , PolyCoeffBall c
  , CanApply (ChPoly c) c, ApplyType (ChPoly c) c ~ c)
  =>
  CanIntegrateOverDom (ChPoly c) DyadicInterval
  where
  type IntegralOverDomType (ChPoly c) DyadicInterval = c
    -- no necessarily convergent, the accuracy is only a guide
  integrateOverDom (cp :: ChPoly c) (Interval l r) =
    maybeTrace (
      "ChPoly integrateOverDom:"
      ++ "\n  (l,r) = " ++ show (l,r)
      ++ "\n  getPrecision cp = " ++ show (getPrecision cp)
      ++ "\n  getAccuracy cp = " ++ show (getAccuracy cp)
      ++ "\n  getAccuracy primit = " ++ show (getAccuracy primit)
      ++ "\n  getAccuracy (primit `apply` lB) = " ++ show (getAccuracy (primit `apply` lB))
      ++ "\n  getAccuracy (primit `apply` rB) = " ++ show (getAccuracy (primit `apply` rB))
    ) $
    (primit `apply` rB) - (primit `apply` lB)
    where
    p = getPrecision cp
    primit = primitive_function cp
    lB = toB l :: c
    rB = toB r :: c
    toB = setPrecision (2*p) . convertExactly
    -- prc = getPrecision cp

primitive_function ::
  (Ring c, CanDivCNBy c Integer, CanNormalize (ChPoly c),
   CanMulBy c Dyadic)
  =>
  ChPoly c -> ChPoly c
primitive_function (ChPoly dom@(Interval l r) (Poly terms) acG _) =
  normalize $
    (dyadic 0.5)*(r - l) *
      ChPoly dom
      (Poly $ terms_fromListAddCoeffs $
        concat $ map oneTerm $ terms_toList terms)
    acG Nothing
  where
  oneTerm (n,a)
    | n == 0 = [(1,a)]
    | n == 1 = [(0,a/!4), (2,a/!4)]
    | otherwise =
      [(n-1, -a/!(2*(n-1))),
       (n+1, a/!(2*(n+1)))]
