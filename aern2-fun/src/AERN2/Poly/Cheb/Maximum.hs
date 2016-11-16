module AERN2.Poly.Cheb.Maximum where


import Numeric.MixedTypes
import AERN2.MP.Ball

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Conversion
import qualified AERN2.Poly.Power.Maximum as PMax (genericMaximum, maximum, maximumOptimised)


maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum f l r  =
   PMax.maximum (cheb2Power $ chPoly_poly f) l r -- TODO: evaluate in Chebyshev basis - this requires a good evaluation function

maximumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f l r initialDegree steps =
  PMax.maximumOptimised (cheb2Power $ chPoly_poly f) l r initialDegree steps

{-minimumI :: ChPoly MPBall -> MPBall
minimumI f = -maximumI (-f)-}
