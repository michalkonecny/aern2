import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Dyadic
import AERN2.MP

import AERN2.Interval
import AERN2.RealFun.Operations
import AERN2.RealFun.SineCosine
import AERN2.Poly.Cheb (ChPoly)

import AERN2.RealFun.PlotService as Plot

main :: IO ()
main = Plot.startServer fns shouldLog (int 4000)
  where
  shouldLog = True

fns :: Plot.Functions
fns = fnsCP

fnsCP :: Plot.Functions
fnsCP = map chPolyFn
  [
    ("sin(6x)", sine (6*x))
  , ("cos(6x)", cosine (6*x))
  , ("x^2", x*x)
  , ("x-x", xP - xP)
  ]
  where
  sine = sineWithAccuracyGuide (bits 5)
  cosine = cosineWithAccuracyGuide (bits 5)
  chPolyFn (name, cp) =
    Plot.Function
    { function_name = name
    , function_dom = getDomain cp
    , function_getBounds = applyViaMPBall cp
    }
  x :: ChPoly MPBall
  x = varFn sampleFn ()
  sampleFn = constFn (dom, 1)
  dom = dyadicInterval (-1.0,1.0)
  xP :: ChPoly MPBall
  xP = varFn sampleFnP ()
  sampleFnP = constFn (domP, 1)
  domP = dyadicInterval (0.0,1.0)
  applyViaMPBall cp di =
    Interval (v-e) (v+e)
    where
    cpC = centreAsBall cp
    e = mpBall $ dyadic $ radius cp
    v :: MPBall
    v = apply cpC (mpBall di)
