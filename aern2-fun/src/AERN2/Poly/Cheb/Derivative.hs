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

import AERN2.MP.Ball

import AERN2.Poly.Basics
import AERN2.Interval
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()

derivative :: ChPoly MPBall -> ChPoly MPBall
derivative (ChPoly dom@(Interval a b) (Poly ts) _) =
  normalize $
  2/(mpBall (b - a)) * (aux (terms_degree ts) terms_empty)
  where
  aux r dts =
    if r == 0 then
      ChPoly dom (Poly $ terms_updateConst (/2) dts) Nothing
    else
      aux (r - 1)
          (terms_insertWith (+) (r - 1)
           ((terms_lookupCoeff dts (r + 1)) + (2*r*terms_lookupCoeff ts r))
           dts)

derivative' :: ChPoly MPBall -> ChPoly MPBall
derivative' (ChPoly dom@(Interval l r) (Poly ts) _)  =
  normalize $
  2/(mpBall (r - l)) * (foldl1 (+) [a*(deriv n) | (n,a) <- terms_toList ts])
  where
  deriv n =
    ChPoly dom
      (Poly $
        terms_updateConst (/2) $
          terms_fromList [(i, mpBall $ 2*n) | i <- [0 .. n - 1], odd (n - i)])
      Nothing
