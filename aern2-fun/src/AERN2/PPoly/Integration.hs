module AERN2.PPoly.Integration where

import Numeric.MixedTypes
import Data.List
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Interval
import AERN2.Poly.Cheb
import AERN2.PPoly.Type

integral :: PPoly -> MPBall -> MPBall -> MPBall
integral (PPoly ps dom) l r =
  foldl' (+)
    (mpBall 0)
    [pieceIntegral i p | (i,p) <- ppoly_pieces f, intersectsLR i]
  where
  lI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) l)
  rI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) r) -- TODO: properly work out required endpoint precision
  unit    = Interval (dyadic $ -1) (dyadic 1)
  f       = PPoly ps unit
  lrInterval = Interval (mpBall lI) (mpBall rI)
  intersectsLR (Interval a b) =
    lrInterval `intersects` Interval (mpBall a) (mpBall b)
    && (b == lI) /= Just True
    && (a == rI) /= Just True
  pieceIntegral (Interval a b) p =
    let
    q  = primitive_function $ centre p
    a' = max a lI
    b' = min b rI
    eps = (mpBall $ radius p)*(b' - a')
    err = fromEndpoints (-eps) eps :: MPBall
    in
    (evalDI q b' - evalDI q a') + err -- TODO: eval direct?
