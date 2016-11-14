import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Dyadic
import AERN2.MP.Ball as MPBall

import AERN2.Interval
import AERN2.RealFun.Operations
import AERN2.Poly.Cheb

import AERN2.RealFun.PlotService as Plot

main :: IO ()
main = Plot.startServer fns shouldLog (int 4000)
  where
  shouldLog = True

fns :: Plot.Functions
fns = map polyBallFn [("x^2", x*x), ("x-x", xP - xP)]
  where
  polyBallFn (name, pb) =
    Plot.Function
    { function_name = name
    , function_dom = getDomain pb
    , function_getBounds = applyViaMPBall pb
    }
  x :: PolyBall
  x = varFn sampleFn ()
  xP :: PolyBall
  xP = varFn sampleFnP ()
  sampleFn = constFn (dom, 1)
  sampleFnP = constFn (domP, 1)
  dom = dyadicInterval (-1.0,1.0)
  domP = dyadicInterval (0.0,1.0)
  applyViaMPBall (Ball chpoly e) di =
    Interval (v-(mpBall (dyadic e))) (v+(mpBall (dyadic e)))
    where
    v :: MPBall
    v = apply chpoly (mpBall di)
