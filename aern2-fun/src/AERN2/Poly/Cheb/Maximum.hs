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

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Conversion
import AERN2.Interval

import Debug.Trace

shouldTrace :: Bool
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum (ChPoly dom poly) l r  =
   Pow.genericMaximum (evalDf f f')
    (Map.fromList [(0, (evalDirect f', powDerivative 100 50))]) -- TODO maybe reduce initial precision
    (getAccuracy f)
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
   where
   f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly
   f' = makeExactCentre $ derivative f
   fAccuracy = getAccuracy f'
   fc' = centre f'
   powDerivative n m =
     maybeTrace (
      "fAccuracy "++(show fAccuracy)
     ) $
     let
        tryF' = (cheb2Power . chPoly_poly)
                (setPrecision (prec n) fc')
     in
     maybeTrace (
      "getAccuracy "++(show $ getAccuracy tryF')++"\n"++
      "tryF':" ++(show tryF')
     ) $
     if getAccuracy tryF' >= fAccuracy - 1 then
       tryF'
     else
       powDerivative (n + m) n

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
  dfsWithEval = Map.fromList [(k,(evalDirect df, (cheb2Power . chPoly_poly . centre) df)) | (k,df) <- dfs] -- TODO maybe reduce initial precision
  dfs = [(k, reduceDegree (initialDegree + steps*k) fc') | k <- [0..maxKey]]

maximumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f =
  maximumOptimisedWithAccuracy (getAccuracy f) f

minimum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r)

minimumOptimisedWithAccuracy :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimisedWithAccuracy acc f l r iDeg steps = -(maximumOptimisedWithAccuracy acc (-f) l r iDeg steps)

minimumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f = minimumOptimisedWithAccuracy (getAccuracy f) f
