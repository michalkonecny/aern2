module AERN2.PPoly.Eval where

import Numeric.MixedTypes
import Numeric.CatchingExceptions
import qualified AERN2.Poly.Cheb as Cheb hiding (evalDf)
import AERN2.Poly.Cheb (ChPoly)
import qualified AERN2.Poly.Cheb.Eval as ChE (evalDirect, evalDf, evalLDf)
import AERN2.PPoly.Type
import AERN2.MP.Ball
import AERN2.Poly.Ball
import AERN2.Interval
import Data.List

import AERN2.RealFun.Operations

import Debug.Trace

evalDirect :: PPoly -> MPBall -> MPBall
evalDirect (PPoly ps dom) x =
  foldl1' meet $
  map (\(_,f) -> (ballLift1TR ChE.evalDirect) f xI) intersectingPieces
  where
  xI = (Cheb.fromDomToUnitInterval dom x)
  meet :: MPBall -> MPBall -> MPBall
  meet a b =
    let
    (la, ra :: MPBall) = endpoints a
    (lb, rb :: MPBall) = endpoints b
    in
    fromEndpoints (min la lb) (max ra rb)
  xAsInterval = dyadicInterval xI
  intersectingPieces =
    filter (\p -> (fst p) `intersects` xAsInterval) ps

evalDirectWithAccuracy :: Accuracy -> PPoly -> MPBall -> MPBall
evalDirectWithAccuracy bts (PPoly ps dom) x =
  foldl1' meet $
  map (\(_,f) -> (ballLift1TR $ Cheb.evalDirectWithAccuracy bts) f xI) intersectingPieces
  where
  xI = (Cheb.fromDomToUnitInterval dom x)
  meet :: MPBall -> MPBall -> MPBall
  meet a b =
    let
    (la, ra :: MPBall) = endpoints a
    (lb, rb :: MPBall) = endpoints b
    in
    fromEndpoints (min la lb) (max ra rb)
  xAsInterval = dyadicInterval xI
  intersectingPieces =
    filter (\p -> (fst p) `intersects` xAsInterval) ps

evalDf :: PPoly -> [ChPoly MPBall] -> MPBall -> MPBall
evalDf (PPoly ps dom) fs' x =
  foldl1' meet $
  map (\((_, f), f') -> (ballLift1TR (\g -> ChE.evalDf g f')) f xI) intersectingPieces
  where
  xI = (Cheb.fromDomToUnitInterval dom x)
  meet :: MPBall -> MPBall -> MPBall
  meet a b =
    let
    (la, ra :: MPBall) = endpoints a
    (lb, rb :: MPBall) = endpoints b
    in
    fromEndpoints (min la lb) (max ra rb)
  xAsInterval = dyadicInterval xI
  intersectingPieces =
    filter (\p -> fst (fst p) `intersects` xAsInterval) $ zip ps fs'

evalLDf :: PPoly -> [ChPoly MPBall] -> MPBall -> MPBall
evalLDf (PPoly ps dom) fs' x =
  foldl1' meet $
  map (\((_, f), f') -> (ballLift1TR (\g -> ChE.evalLDf g f')) f xI) intersectingPieces
  where
  xI = (Cheb.fromDomToUnitInterval dom x)
  meet :: MPBall -> MPBall -> MPBall
  meet a b =
    let
    (la, ra :: MPBall) = endpoints a
    (lb, rb :: MPBall) = endpoints b
    in
    fromEndpoints (min la lb) (max ra rb)
  xAsInterval = dyadicInterval (Cheb.fromDomToUnitInterval dom xI)
  intersectingPieces =
    filter (\p -> fst (fst p) `intersects` xAsInterval) $ zip ps fs'

evalDI :: PPoly -> MPBall -> MPBall
evalDI f@(PPoly ps dom) x =
  evalDf f dfs x
  where
  (Interval l r) = dom
  c = 1/(0.5*(r - l))
  dfs = map ((c *) . (ballLift1R Cheb.derivative) . snd) ps

instance
  CanApply PPoly MPBall where
  type ApplyType PPoly MPBall = MPBall
  apply p x =
    case getAccuracy x of
      Exact -> evalDirect p x
      _ -> evalDI p x

instance
  CanApply PPoly (CatchingNumExceptions MPBall) where
  type ApplyType PPoly (CatchingNumExceptions MPBall) = CatchingNumExceptions MPBall
  apply p x =
    -- TODO: check x is in the domain
    apply p <$> x

instance CanApplyApprox PPoly DyadicInterval where
  type ApplyApproxType PPoly DyadicInterval = DyadicInterval
  applyApprox p di =
    dyadicInterval (fromEndpoints lB uB :: MPBall)
    where
    (Interval lB uB) = sampledRange di 5 p :: Interval MPBall MPBall
