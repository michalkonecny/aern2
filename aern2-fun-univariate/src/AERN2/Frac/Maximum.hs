module AERN2.Frac.Maximum
(
  maximumOptimisedWithAccuracy
  , maximumOptimised
  , maximum
  , minimum
  , minimumOptimised
  , minimumOptimisedWithAccuracy
)
where

import MixedTypesNumPrelude hiding (maximum, minimum)

import qualified Data.Map as Map

import AERN2.MP.Accuracy
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Interval
import AERN2.Frac.Type
import AERN2.Frac.Eval
import AERN2.Frac.Ring()
import AERN2.Poly.Cheb (derivativeExact, reduceDegree, chPoly_dom,
      ChPoly(..),
      fromDomToUnitInterval, reduceToEvalDirectAccuracy)
import qualified AERN2.Poly.Cheb as Cheb
import AERN2.Poly.Conversion
import AERN2.Poly.Basics
import Data.Ratio
import qualified AERN2.Poly.Power as Pow
import qualified AERN2.Poly.Power.MaximumInt as PM

-- import Debug.Trace

maximum :: Frac MPBall -> MPBall -> MPBall -> MPBall
maximum f l r = maximumOptimised f l r (degree f - 1) 1

minimum :: Frac MPBall -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r )

maximumOptimised :: Frac MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f l r =
  maximumOptimisedWithAccuracy (getFiniteAccuracy f) f l r

minimumOptimised :: Frac MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f l r iDeg steps =
  -(maximumOptimised (-f) l r iDeg steps)

minimumOptimisedWithAccuracy :: Accuracy -> Frac MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimisedWithAccuracy acc f l r iDeg steps =
  -(maximumOptimisedWithAccuracy acc (-f) l r iDeg steps)

maximumOptimisedWithAccuracy :: Accuracy -> Frac MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimisedWithAccuracy acc f@(Frac p q _) l r iDeg steps =
  --trace("dfs size: "++(show $ length dfs)) $
  --trace("dfs: "++(show dfs)) $
  PM.genericMaximum evalf dfsWithEval (min (getFiniteAccuracy f) acc) lI rI
  where
  evalf = evalDf f (reduceToEvalDirectAccuracy p (bits 0))
                   (reduceToEvalDirectAccuracy q (bits 1))
  dom = chPoly_dom p
  unit = Interval (dyadic $ -1) (dyadic 1)
  pI = makeExactCentre $ p { chPoly_dom = unit }
  qI = makeExactCentre $ q { chPoly_dom = unit }
  lI = fromDomToUnitInterval dom l
  rI = fromDomToUnitInterval dom r -- TODO: set precision
  pc = centre pI
  qc = centre qI
  df = (derivativeExact pc) * qc - pc * (derivativeExact qc)
  maxKey = max 0 (ceiling ((Cheb.degree df - iDeg) /! steps))
  dfs = [(k, reduceDegree (iDeg + steps*k) df) | k <- [0..maxKey + 1]]
  dfsWithEval =
    Map.fromList
    [(k,(Cheb.evalDirect dfk :: MPBall -> MPBall, (ch2Power . intify) dfk)) | (k, dfk) <- dfs]

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _ _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational

ch2Power :: (ErrorBound, Poly Integer) -> (ErrorBound, Pow.PowPoly Integer)
ch2Power (e, p) = (e, cheb2Power p)
