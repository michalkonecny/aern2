module AERN2.Poly.Cheb.Maximum
(
maximum,
maximumOptimised
) where


import Numeric.MixedTypes hiding (maximum)
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

maximumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised (ChPoly dom poly) l r initialDegree steps =
  Pow.genericMaximum (evalDf f f') dfsWithEval
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
  where
  f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly
  f' = makeExactCentre $ derivative f
  maxKey = ceiling $ (degree f - initialDegree) / steps
  dfsWithEval = Map.fromList [(k,(evalDirect df, (cheb2Power . chPoly_poly . centre) df)) | (k,df) <- dfs] -- TODO maybe reduce initial precision
  dfs = [(k, reduceDegreeAndSweep (initialDegree + steps*k) NormZero f') | k <- [0..maxKey]]

{-minimumI :: ChPoly MPBall -> MPBall
minimumI f = -maximumI (-f)-}
