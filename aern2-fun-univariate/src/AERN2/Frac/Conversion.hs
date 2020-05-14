{-# LANGUAGE CPP #-}
#define DEBUG
module AERN2.Frac.Conversion where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude

import Text.Printf

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.MP

import AERN2.Frac.Type
import AERN2.Poly.Cheb as ChPoly
import AERN2.PPoly.Type as PPoly
import AERN2.PPoly.Division

toPPoly :: Frac MPBall -> PPoly
toPPoly (Frac p q _) =
  -- maybeTrace (printf "Frac toPPoly: ac p = %s; ac q = %s; acG = %s; acGmul = %s; "
  --   (show $ getAccuracy p) (show $ getAccuracy q) (show $ acG) (show $ acGmul)) $
  -- maybeTrace (printf "Frac toPPoly: ac inv = %s " (show $ getAccuracy inv)) $
  -- maybeTrace (printf "Frac toPPoly: ac res = %s " (show $ getAccuracy res)) $
  -- res
  (PPoly.fromPoly p) * (inverse . PPoly.fromPoly) q
  -- where
  -- res = setAccuracyGuide acG $ (setAccuracyGuide acGmul $ PPoly.fromPoly p) * (setAccuracyGuide acGmul inv)
  -- inv = (inverse . PPoly.fromPoly) q
  -- acG = (getAccuracyGuide p) `min` (getAccuracyGuide q)
  -- acGmul = acG * (integer $ integerLog2 $ max 2 $ ChPoly.degree p)
