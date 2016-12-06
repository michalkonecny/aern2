module AERN2.Poly.Cheb.Maximum
(
maximum,
maximumOptimised,
maximumOptimisedWithAccuracy,
minimum,
minimumOptimised,
minimumOptimisedWithAccuracy
) where


import Numeric.MixedTypes hiding (maximum, minimum)
import AERN2.MP.Ball
-- import AERN2.MP.Dyadic
import qualified Data.Map as Map

import qualified AERN2.Poly.Power as Pow

import AERN2.RealFun.Operations

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Conversion
import AERN2.Interval

import Debug.Trace

{-shouldTrace :: Bool
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id-}

maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum (ChPoly dom poly) l r  =
   Pow.genericMaximum (evalDf f df)
    (Map.fromList [(0, (evalDirect dfc, cheb2Power $ chPoly_poly dfc))]) 
    (getAccuracy f)
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
   where
   f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly
   df = makeExactCentre $ derivative f
   dfc = derivative $ centre f

maximumOptimisedWithAccuracy
  :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimisedWithAccuracy acc (ChPoly dom poly) l r initialDegree steps =
  Pow.genericMaximum (evalDf f f') dfsWithEval
    acc
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
  where
  f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly
  f' = makeExactCentre $ derivative f
  fc' = makeExactCentre $ derivative $ centre f
  maxKey = ceiling $ (degree f - initialDegree) / steps
  dfsWithEval =
    Map.fromList
    [(k,(evalDirect df, (cheb2Power . chPoly_poly) df)) | (k,df) <- dfs]
  dfs = [(k, reduceDegree (initialDegree + steps*k) fc') | k <- [0..maxKey]]

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
    minimumOptimised (setPrecision prc f) lB rB 5 5
    where
    prc = 3*(getPrecision f)
    lB = raisePrecisionIfBelow prc $ mpBall l
    rB = raisePrecisionIfBelow prc $ mpBall r

instance CanMaximiseOverDom (ChPoly MPBall) DyadicInterval where
  type MaximumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  maximumOverDom f (Interval l r) =
    maximumOptimised (setPrecision prc f) lB rB 5 5
    where
    prc = 3*(getPrecision f)
    lB = raisePrecisionIfBelow prc $ mpBall l
    rB = raisePrecisionIfBelow prc $ mpBall r
