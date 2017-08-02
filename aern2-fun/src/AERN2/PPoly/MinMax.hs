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
import AERN2.Poly.Power.RootsInt (findRoots)
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
  pballMax ((Interval domL domR), p@(Ball pC pR), q@(Ball qC qR)) =
    polys
    where
    acGuide = getAccuracyGuide pC `max` getAccuracyGuide qC
    realAc  = getAccuracy p `min` getAccuracy q `min` acGuide
    precision = getPrecision pC `max` getPrecision qC

    pe =
      pC + (fromEndpoints (mpBall $ -dyadic pR) (mpBall $ dyadic pR) :: MPBall)
    qe =
      qC + (fromEndpoints (mpBall $ -dyadic qR) (mpBall $ dyadic qR) :: MPBall)

    diff   = pe  - qe
    diffC  = centre diff
    diffC' = derivative diffC
    evalOnInterval (Interval l r) =
      evalDf diff diffC' $
        fromEndpoints (mpBallP precision l) (mpBallP precision r)
    (_diffCIntErr, diffCInt) = intify diffC
    diffCRoots =
      map (\(Interval l r) -> centre $ mpBallP (ac2prec acGuide) $ (l + r)/!2) $
      findRoots (cheb2Power diffCInt)
        (\i -> getAccuracy (evalOnInterval i) >= realAc)
        (rational domL) (rational domR)
    intervals :: [DyadicInterval]
    intervals = aux [] domL (diffCRoots ++ [domR])
      where
      aux is l (x:[])  = is ++ [Interval l x]
      aux is l (x:xs) = aux (is ++ [Interval l x]) x xs
    biggest :: DyadicInterval -> (DyadicInterval, PolyBall)
    biggest i@(Interval l r) =
      if (pm >= qm) == Just True then
        (i, p)
      else
        (i, q)
      where
      m  = (dyadic 0.5) * (l + r)
      pm = apply pC (mpBall m)
      qm = apply qC (mpBall m)
    polys :: [(DyadicInterval, PolyBall)]
    polys =
      map biggest intervals
