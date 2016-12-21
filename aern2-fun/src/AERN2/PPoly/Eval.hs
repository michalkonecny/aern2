module AERN2.PPoly.Eval where

import Numeric.MixedTypes
import AERN2.Poly.Cheb as Cheb hiding (evalDf)
import qualified AERN2.Poly.Cheb.Eval as ChE (evalDirect, evalDf, evalLDf)
import AERN2.PPoly.Type
import AERN2.MP.Ball
import AERN2.Poly.Ball
import AERN2.Interval
import Data.List

evalDirect :: PPoly -> MPBall -> MPBall
evalDirect (PPoly ps _) x =
  foldl1' meet $
  map (\(_,f) -> (ballLift1TR ChE.evalDirect) f x) intersectingPieces
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

evalDirectWithAccuracy :: Accuracy -> PPoly -> MPBall -> MPBall
evalDirectWithAccuracy bts (PPoly ps _) x =
  foldl1' meet $
  map (\(_,f) -> (ballLift1TR $ Cheb.evalDirectWithAccuracy bts) f x) intersectingPieces
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
evalDf (PPoly ps _) fs' x =
  foldl1' meet $
  map (\((_, f), f') -> (ballLift1TR (\g -> ChE.evalDf g f')) f x) intersectingPieces
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

evalLDf :: PPoly -> [ChPoly MPBall] -> MPBall -> MPBall
evalLDf (PPoly ps _) fs' x =
  foldl1' meet $
  map (\((_, f), f') -> (ballLift1TR (\g -> ChE.evalLDf g f')) f x) intersectingPieces
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

evalDI :: PPoly -> MPBall -> MPBall
evalDI f@(PPoly ps _) x =
  evalDf f dfs x
  where
  dfs = map ((ballLift1R Cheb.derivative) . snd) ps
