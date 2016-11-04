import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.Interval
import AERN2.RealFun.Operations
import AERN2.Poly.Cheb

import qualified AERN2.RealFun.PlotService as Plot

main = Plot.startServer fns (int 3000)

fns :: Plot.Functions PolyBall
fns = Plot.Functions [("x^2", x*x)] getDomain applyViaMPBall
  where
  x :: PolyBall
  x = varFn sampleFn ()
  sampleFn = constFn (dom, 1)
  dom = dyadicInterval (0.0,1.0)
  applyViaMPBall (Ball chpoly e) di =
    Interval (v-(mpBall (dyadic e))) (v+(mpBall (dyadic e)))
    where
    v :: MPBall
    v = apply chpoly (mpBall di)
