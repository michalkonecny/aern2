{-# LANGUAGE CPP #-}
-- #define DEBUG
module AERN2.Poly.Cheb.MaximumInt
(
maximum,
maximumWithAccuracy,
maximumOptimised,
maximumOptimisedWithAccuracy,
minimum,
minimumOptimised,
minimumOptimisedWithAccuracy
, maximumOptimisedWithAccuracyAndBounds
, intify
) where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude hiding (maximum, minimum)

--import Text.Printf

import AERN2.MP.Ball
-- import AERN2.MP.Dyadic
import Data.Ratio
import qualified Data.Map as Map

-- import AERN2.Poly.Basics (terms_updateConst)

import qualified AERN2.Poly.Power as Pow hiding (genericMaximum)
import qualified AERN2.Poly.Power.MaximumIntAlt as Pow

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Conversion
import AERN2.Interval

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _acG _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational

chPolyBoundsError :: ChPolyBounds c
chPolyBoundsError = error "ChPolyBounds undefined in internal MaximumInt functions"

maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum (ChPoly dom poly acG _) l r  =
   Pow.genericMaximum (evalDf f $ reduceToEvalDirectAccuracy df (bits 0))
    (Map.fromList [(0, (evalDirect df, (err , cheb2Power dfInt)))])
    (getFiniteAccuracy f)
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
   where
   f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly acG chPolyBoundsError
   df@(ChPoly _ dfp _ _) = derivativeExact (centre f) --makeExactCentre $ derivative f
   termsRational = terms_map (rational . ball_value) (poly_terms dfp)
   err = termsError * termsDenominator
   termsError = ball_error $ terms_lookupCoeff (poly_terms dfp) 0
   termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
   dfInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational

maximumWithAccuracy :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximumWithAccuracy acc (ChPoly dom poly acG _) l r  =
  Pow.genericMaximum (evalDf f $ reduceToEvalDirectAccuracy df (bits 0))
   (Map.fromList [(0, (evalDirect df, (err , cheb2Power dfInt)))])
   (min (getFiniteAccuracy f) acc)
   (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
  where
  f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly acG chPolyBoundsError
  df@(ChPoly _ dfp _ _) = derivativeExact (centre f) --makeExactCentre $ derivative f
  termsRational = terms_map (rational . ball_value) (poly_terms dfp)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms dfp) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  dfInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational


maximumOptimisedWithAccuracy
  :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimisedWithAccuracy acc (ChPoly dom@(Interval dR dL) poly acG _) l r initialDegree steps =
  --trace("f = "++(show f)) $
    {-trace("maximum optimised... ")$
    trace("f: "++(show f))$
    trace("df: "++(show fc'))$
    trace("dfs: "++(show dfs))$-}
    Pow.genericMaximum
      (evalDf f (c*reduceToEvalDirectAccuracy fc' (bits 0))) dfsWithEval
      (min (getFiniteAccuracy f) acc)
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) l))
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) r))
  where
  c = 1/!(0.5*(dR - dL))
  f   = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly acG chPolyBoundsError
  fc' = ({-makeExactCentre .-} derivativeExact . centre) f
  maxKey = max 0 (ceiling ((degree f - initialDegree) /! steps))
  ch2Power :: (ErrorBound, Poly Integer) -> (ErrorBound, Pow.PowPoly Integer)
  ch2Power (e, p) = (e, cheb2Power p)
  dfsWithEval =
    Map.fromList
    [(k,(evalDirect df :: MPBall -> MPBall, ch2Power $ intify df)) | (k,df) <- dfs]
  dfs = [(k, reduceDegree (initialDegree + steps*k) fc') | k <- [0..maxKey + 1]]

maximumOptimisedWithAccuracyAndBounds
  :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall -> MPBall -> MPBall
maximumOptimisedWithAccuracyAndBounds acc (ChPoly dom poly acG _) l r initialDegree steps lower upper =
  --trace("f = "++(show f)) $
    {-trace("maximum optimised... ")$
    trace("f: "++(show f))$
    trace("df: "++(show fc'))$
    trace("dfs: "++(show dfs))$-}
    Pow.genericMaximumWithBounds
      (evalDf f (reduceToEvalDirectAccuracy fc' (bits 0))) dfsWithEval
      (min (getFiniteAccuracy f) acc)
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) l))
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) r))
      lower
      upper
  where
  f   = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly acG chPolyBoundsError
  fc' = (derivativeExact . centre) f
  maxKey = max 0 (ceiling ((degree f - initialDegree) /! steps))
  ch2Power :: (ErrorBound, Poly Integer) -> (ErrorBound, Pow.PowPoly Integer)
  ch2Power (e, p) = (e, cheb2Power p)
  dfsWithEval =
    Map.fromList
    [(k,(evalDirect df :: MPBall -> MPBall, ch2Power $ intify df)) | (k,df) <- dfs]
  dfs = [(k, reduceDegree (initialDegree + steps*k) fc') | k <- [0..maxKey + 1]]

maximumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f =
  maximumOptimisedWithAccuracy (getFiniteAccuracy f) f

minimum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r)

minimumOptimisedWithAccuracy :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimisedWithAccuracy acc f l r iDeg steps = -(maximumOptimisedWithAccuracy acc (-f) l r iDeg steps)

minimumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f = minimumOptimisedWithAccuracy (getFiniteAccuracy f) f
