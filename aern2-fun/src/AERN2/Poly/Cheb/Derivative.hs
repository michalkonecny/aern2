{-|
    Module      :  AERN2.Poly.Cheb.Derivative
    Description :  Chebyshev basis derivative
    Copyright   :  (c) Eike Neumann, Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis derivative
-}
module AERN2.Poly.Cheb.Derivative where

import Numeric.MixedTypes

import AERN2.Normalize

import Data.List

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Poly.Basics
import AERN2.Interval
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()

derivativeExact :: ChPoly MPBall -> ChPoly MPBall
derivativeExact f@(ChPoly dom (Poly ts) l) =
  {-trace("derivative exact of "++(show f)) $
  trace("accuracy of f: "++(show $ getAccuracy f)) $
  trace("deriavtive of f: "++(show $ aux (getPrecision f) (getPrecision f))) $
  trace("accuracy of derivative: "++(show $ getAccuracy $ aux (getPrecision f) (getPrecision f))) $-}
  aux (getPrecision f) (getPrecision f)
  where
  aux p q =
    let
    try = (derivative . setPrecision p) f
    in
      if getAccuracy try == Exact then
        try
      else
        aux (p + q) p

derivative ::
  (PolyCoeff c,
   CanMulAsymmetric c (ChPoly c),
   MulType c (ChPoly c) ~ ChPoly c,
   CanDivBy c Dyadic)
   => ChPoly c -> ChPoly c
derivative {-(ChPoly dom@(Interval a b) (Poly ts) _)-} =
  derivative'
  {-normalize $
  2/(mpBall (b - a)) * (aux (terms_degree ts) terms_empty)
  where
  aux r dts =
    if r == 0 then
      ChPoly dom (Poly $ terms_updateConst (/2) dts) Nothing
    else
      aux (r - 1)
          (terms_insertWith (+) (r - 1)
           ((terms_lookupCoeff dts (r + 1)) + (2*r*terms_lookupCoeff ts r))
           dts)-}

derivative' ::
  (PolyCoeff c,
  CanMulAsymmetric c (ChPoly c),
  MulType c (ChPoly c) ~ ChPoly c,
  CanDivBy c Dyadic)
  => ChPoly c -> ChPoly c
derivative' (ChPoly dom@(Interval l r) (Poly ts :: Poly c) _)  =
  normalize $
  (((convertExactly 2 :: c) / (r - l)) *
   (foldl' (+)
    zero
    [a*(deriv n) | (n,a) <- terms_toList ts]))
  where
  zero = ChPoly dom (Poly (terms_fromList [(0,convertExactly 0 :: c)])) Nothing
  deriv n =
    ChPoly dom
      (Poly $
        terms_updateConst (/2) $
          terms_fromList [(i, convertExactly (2*n) :: c) | i <- [0 .. n - 1], odd (n - i)])
      Nothing
