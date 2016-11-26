module AERN2.PPoly.Eval where

import Numeric.MixedTypes
import AERN2.Poly.Cheb.Type
import qualified AERN2.Poly.Cheb.Eval as ChE (evalDirect, evalDf)
import qualified AERN2.Poly.Power.Eval as PowE (evalDirect, evalDf)
import AERN2.PPoly.Type
import AERN2.MP.Ball
import AERN2.Poly.Power.Type
import AERN2.Interval
import Data.List

evalDirect :: PPoly -> MPBall -> MPBall
evalDirect (PPoly ps _ dom) x =
  foldl1' meet $ map (\(_,f) -> ChE.evalDirect (ChPoly dom f) x) intersectingPieces
  where
  meet :: MPBall -> MPBall -> MPBall
  meet a b =
    let
    (la, ra :: MPBall) = endpoints a
    (lb, rb :: MPBall) = endpoints b
    in
    fromEndpoints (min la lb) (max ra rb)
  xAsInterval = dyadicInterval x
  intersectingPieces =
    filter (\p -> (fst p) `intersects` xAsInterval) ps

evalDf :: PPoly -> [ChPoly MPBall] -> MPBall -> MPBall
evalDf (PPoly ps _ dom) fs' x =
  foldl1' meet $ map (\((_, f), f') -> ChE.evalDf (ChPoly dom f) f' x) intersectingPieces
  where
  meet :: MPBall -> MPBall -> MPBall
  meet a b =
    let
    (la, ra :: MPBall) = endpoints a
    (lb, rb :: MPBall) = endpoints b
    in
    fromEndpoints (min la lb) (max ra rb)
  xAsInterval = dyadicInterval x
  intersectingPieces =
    filter (\p -> fst (fst p) `intersects` xAsInterval) $ zip ps fs'
