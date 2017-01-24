{-# LANGUAGE CPP #-}
#define DEBUG
module AERN2.Poly.Cheb.MaximumInt
(
maximum,
maximumOptimised,
maximumOptimisedWithAccuracy,
minimum,
minimumOptimised,
minimumOptimisedWithAccuracy
) where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (flip const)
#endif

import Numeric.MixedTypes hiding (maximum, minimum)

--import Text.Printf

import AERN2.MP.Ball
import AERN2.MP.Dyadic
import Data.Ratio
import qualified Data.Map as Map

-- import AERN2.Poly.Basics (terms_updateConst)

import qualified AERN2.Poly.Power as Pow hiding (genericMaximum)
import qualified AERN2.Poly.Power.MaximumInt as Pow

import AERN2.RealFun.Operations

import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Conversion
import AERN2.Interval

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational

maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum (ChPoly dom poly _) l r  =
   Pow.genericMaximum (evalDf f df)
    (Map.fromList [(0, (evalDirect dfc, (err , cheb2Power dfInt)))])
    (getFiniteAccuracy f)
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
   where
   f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly Nothing
   df@(ChPoly _ dfp _) = makeExactCentre $ derivative f
   termsRational = terms_map (rational . ball_value) (poly_terms dfp)
   err = termsError * termsDenominator
   termsError = ball_error $ terms_lookupCoeff (poly_terms dfp) 0
   termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
   dfInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational
   dfc = derivative $ centre f

maximumOptimisedWithAccuracy
  :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimisedWithAccuracy acc (ChPoly dom poly _) l r initialDegree steps =
  --trace("f = "++(show f)) $
    {-trace("maximum optimised... ")$
    trace("f: "++(show f))$
    trace("df: "++(show fc'))$
    trace("dfs: "++(show dfs))$-}
    Pow.genericMaximum
      (evalDf f (reduceToEvalDirectAccuracy fc' (bits 0))) dfsWithEval
      (min (getFiniteAccuracy f) acc)
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) l))
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) r))
  where
  f   = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly Nothing
  fc' = ({-makeExactCentre .-} derivativeExact . centre) f
  maxKey = max 0 (ceiling ((degree f - initialDegree) / steps))
  ch2Power :: (ErrorBound, Poly Integer) -> (ErrorBound, Pow.PowPoly Integer)
  ch2Power (e, p) = (e, cheb2Power p)
  dfsWithEval =
    Map.fromList
    [(k,(evalDirect df, ch2Power $ intify df)) | (k,df) <- dfs]
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

instance CanMinimiseOverDom (ChPoly MPBall) DyadicInterval where
  type MinimumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  minimumOverDom f (Interval l r) =
    minimumOptimised f (mpBall l) (mpBall r) 5 5
    {-res
    where
    (_, Just res) = last $ iterateUntilAccurate ac withPrec
    ac = getFiniteAccuracy f
    withPrec p =
      maybeTrace (printf "ChPoly: MinimumOverDomType: withPrec: p = %s; ac = %s"
        (show p) (show $ getAccuracy resP)) $
      Just resP
      where
      resP = minimumOptimised (setPrecision p f) (mpBall l) (mpBall r) 5 5-}

instance CanMaximiseOverDom (ChPoly MPBall) DyadicInterval where
  type MaximumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  maximumOverDom f (Interval l r) =
    maximumOptimised f (mpBall l) (mpBall r) 5 5
    {-res
    where
    (_, Just res) = last $ iterateUntilAccurate ac withPrec
    ac = getFiniteAccuracy f
    withPrec p =
      maybeTrace (printf "ChPoly: MaximumOverDomType: withPrec: p = %s; ac = %s"
        (show p) (show $ getAccuracy resP)) $
      Just resP
      where
      resP = maximumOptimised (setPrecision p f) (mpBall l) (mpBall r) 5 5-}
