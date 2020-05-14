{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.Poly.Cheb.Elementary
    Description :  Poly point-wise elementary functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    ChPoly point-wise elementary functions
-}
module AERN2.Poly.Cheb.Elementary where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
-- import Text.Printf

-- import AERN2.Normalize

import AERN2.MP

-- import AERN2.Real

-- import AERN2.RealFun.Operations
import AERN2.RealFun.SineCosine

-- import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
-- import AERN2.Poly.Cheb.DCT
import AERN2.Poly.Cheb.Maximum ()
import AERN2.Poly.Cheb.Ring ()

instance CanSinCos (ChPoly MPBall) where
  type SinCosType (ChPoly MPBall) = ChPoly MPBall
  sin p = sineWithAccuracyGuide (chPoly_acGuide p) p
  cos p = cosineWithAccuracyGuide (chPoly_acGuide p) p
