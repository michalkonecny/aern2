module AERN2.Poly.Cheb.Derivative where

import Numeric.MixedTypes
import AERN2.MP.Ball

import qualified Data.Map as Map

import AERN2.Poly.Basics
import AERN2.Interval
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()

derivative :: ChPoly MPBall -> ChPoly MPBall
derivative (ChPoly dom@(Interval a b) (Poly ts)) =
  2/mpBall (b - a) * aux (terms_degree ts) Map.empty
  where
  aux r dts =
    if r == 0 then
      ChPoly dom (Poly $ terms_updateConst (/2) dts)
    else
      aux (r - 1)
          (Map.insert (r - 1)
           ((terms_lookupCoeff dts (r + 1)) + 2*r*terms_lookupCoeff ts r)
           dts)

derivative' :: ChPoly MPBall -> ChPoly MPBall
derivative' (ChPoly dom@(Interval l r) (Poly ts))  =
  2/mpBall (r - l) * foldl1 (+) [a*(deriv n) | (n,a) <- terms_toList ts]
  where
  deriv n =
    ChPoly dom $
      Poly
      ( terms_updateConst (/2) $
        terms_fromList [(r, mpBall $ 2*n) | r <- [0 .. n - 1], odd (n - r)])
