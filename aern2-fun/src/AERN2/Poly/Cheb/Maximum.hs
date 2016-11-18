module AERN2.Poly.Cheb.Maximum where


import Numeric.MixedTypes
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import qualified Data.Map as Map

import AERN2.Poly.Cheb.Type
import qualified AERN2.Poly.Power.Type as Pow
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Conversion
import qualified AERN2.Poly.Power.Maximum as PMax
import AERN2.Interval

maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum (ChPoly dom poly) l r  =
   PMax.genericMaximum (evalDf f f') (Map.fromList [(0, f')])
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
   where
   f  = ChPoly (Interval (dyadic $ -1) (dyadic 1)) poly
   f' = (Pow.derivative . cheb2Power . chPoly_poly) f

maximumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f l r initialDegree steps =
  PMax.maximumOptimised (cheb2Power $ chPoly_poly f) l r initialDegree steps  -- TODO: evaluate in Chebyshev basis - this requires a good evaluation function

{-minimumI :: ChPoly MPBall -> MPBall
minimumI f = -maximumI (-f)-}
