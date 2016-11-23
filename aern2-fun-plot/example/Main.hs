import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Dyadic
import AERN2.MP.Ball as MPBall

import AERN2.Interval
import AERN2.RealFun.Operations
import qualified AERN2.Poly.Cheb as ChPoly

import AERN2.RealFun.PlotService as Plot

main :: IO ()
main = Plot.startServer fns shouldLog (int 4000)
  where
  shouldLog = True

fns :: Plot.Functions
fns = fnsCP

fnsCP :: Plot.Functions
fnsCP = map chPolyFn [("sin(6x)", sine (6*x)), ("x^2", x*x), ("x-x", xP - xP)]
  where
  sine = ChPoly.sineWithDegSweep 10 NormZero
  chPolyFn (name, cp) =
    Plot.Function
    { function_name = name
    , function_dom = getDomain cp
    , function_getBounds = applyViaMPBall cp
    }
  x :: ChPoly.PolyBall
  x = varFn sampleFn ()
  sampleFn = constFn (dom, 1)
  dom = dyadicInterval (-1.0,1.0)
  xP :: ChPoly.PolyBall
  xP = varFn sampleFnP ()
  sampleFnP = constFn (domP, 1)
  domP = dyadicInterval (0.0,1.0)
  applyViaMPBall cp di =
    Interval (v-(mpBall (dyadic e))) (v+(mpBall (dyadic e)))
    where
    cpC = centreAsBall cp
    e = radius cp
    v :: MPBall
    v = apply cpC (mpBall di)
