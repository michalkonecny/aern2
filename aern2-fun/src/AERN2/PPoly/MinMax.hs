{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
#define DEBUG
{-|
    Module      :  AERN2.PPoly.MinMax
    Description :  PPoly pointwise min and max
    Copyright   :  (c) Michal Konecny, Eike Neumann
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Poly pointwise min and max
-}
module AERN2.PPoly.MinMax where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude

import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.Poly.Cheb
import AERN2.Poly.Cheb.MaximumInt (intify)
import AERN2.Poly.Conversion (cheb2Power)
import AERN2.Poly.Power.RootsIntVector (findRootsWithEvaluation)
import AERN2.Poly.Cheb.Ring ()
import AERN2.Poly.Ball

import AERN2.PPoly.Type

{- min/max -}

instance CanMinMaxAsymmetric PPoly PPoly where
  type MinMaxType PPoly PPoly = PPoly
  max = ppolyMax
  min a b = negate (max (-a) (-b))

ppolyMax ::
  PPoly -> PPoly -> PPoly
ppolyMax a b =
  PPoly (concat (map pballMax (refine a b)))
        (ppoly_dom a) -- TODO: how to handle polys with different domains?
  where
  (Interval dl dr) = ppoly_dom a
  pballMax ((Interval domL domR), p@(Ball pC _pR), q@(Ball qC _qR)) =
    polys
    where

    fromUnitIntervalToDom :: MPBall -> MPBall
    fromUnitIntervalToDom x = dl + 0.5*(x + 1)*(dr - dl)

    acGuide = getAccuracyGuide pC `max` getAccuracyGuide qC
    precision = getPrecision pC `max` getPrecision qC

    diffC  = centre $ p - q
    diffC' = derivativeExact diffC
    evalOnInterval (Interval l r) =
      evalDf diffC diffC' $
        fromUnitIntervalToDom $
        fromEndpoints (mpBallP precision l) (mpBallP precision r)
    (_diffCIntErr, diffCInt) = intify diffC
    diffCRoots =
      map
      (\(Interval l r, err) ->
        (centre $ mpBallP (ac2prec acGuide) $ (l + r)/!2, errorBound err)) $
      findRootsWithEvaluation
        (cheb2Power diffCInt)
        (abs . evalOnInterval)
        (\v -> getAccuracy v >= acGuide)
        (rational domL) (rational domR)
    intervals :: [(DyadicInterval, ErrorBound)]
    intervals = aux [] (domL, errorBound 0) (diffCRoots ++ [(domR, errorBound 0)])
      where
      aux is (l, e0) ((x, e1) : []) =
        is ++ [(Interval l x, max e0 e1)]
      aux is (l, e0) ((x, e1) : xs) =
        aux (is ++ [(Interval l x, max e0 e1)]) (x, e1) xs
    biggest :: (DyadicInterval, ErrorBound) -> (DyadicInterval, PolyBall)
    biggest (i@(Interval l r), e) =
      if (pm >= qm) == Just True then
        (i, p')
      else
        (i, q')
      where
      p' = updateRadius (+ e) p
      q' = updateRadius (+ e) q
      m  = (dyadic 0.5) * (l + r)
      pm = apply pC (fromUnitIntervalToDom $ mpBall m)
      qm = apply qC (fromUnitIntervalToDom $ mpBall m)
    polys :: [(DyadicInterval, PolyBall)]
    polys =
      map biggest intervals
