import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Dyadic
import AERN2.MP

import AERN2.Interval
import AERN2.RealFun.Operations
import AERN2.RealFun.SineCosine
import qualified AERN2.Poly.Cheb as ChPoly
import AERN2.Poly.Cheb (ChPoly)

import AERN2.RealFun.PlotService as Plot

main :: IO ()
main = Plot.startServer fns shouldLog (int 4000)
  where
  shouldLog = True

fns :: Plot.Functions
fns = fnsPoly ++ fnsFun

fnsFun :: Plot.Functions
fnsFun = map funFn
  [
    ("Fun 0", unitDom, \x -> x-x)
  , ("Fun 1/(10*x^2+1)", unitDom, \x -> 1/(10*x^2+1))
  -- , ("Fun (1+sin(6x))/2", unitDom, \x -> (1+sin (6*x))/2)
  -- , ("Fun (1+cos(6x))/2", unitDom, \x -> (1+cos (6*x))/2)
  ]

unitDom :: DyadicInterval
unitDom = dyadicInterval (-1.0,1.0)
unitDomP :: DyadicInterval
unitDomP = dyadicInterval (0.0,1.0)

fnsPoly :: Plot.Functions
fnsPoly = map chPolyFn
  [
    ("Poly 0", x - x)
  , ("Poly 1/(10*x^2+1)", ChPoly.reduceDegree 8 $ ChPoly.chebDivideDCT (bits 4) (x-x+1) (10*x*x+1))
  , ("Poly 1/(10*x^2+1)", ChPoly.reduceDegree 12 $ ChPoly.chebDivideDCT (bits 4) (x-x+1) (10*x*x+1))
  , ("Poly 1/(10*x^2+1)", ChPoly.reduceDegree 16 $ ChPoly.chebDivideDCT (bits 4) (x-x+1) (10*x*x+1))
  -- , ("Poly (1+sin[ac=3](6x))/2", (1+sine (6*x))/2)
  -- , ("Poly (1+cos[ac=3](6x))/2", (1+cosine (6*x))/2)
  ]
  where
  -- sine = sineWithAccuracyGuide (bits 3)
  -- cosine = cosineWithAccuracyGuide (bits 3)
  x :: ChPoly MPBall
  x = varFn sampleFn ()
  sampleFn = constFn (unitDom, 1)
  xP :: ChPoly MPBall
  xP = varFn sampleFnP ()
  sampleFnP = constFn (unitDomP, 1)

funFn :: (String, DyadicInterval, MPBall -> MPBall) -> Plot.Function
funFn (name, dom, b2b) =
  Plot.Function
  { function_name = name
  , function_dom = dom
  , function_getBounds = \x -> let r = b2b (mpBall x) in Interval r r
  }

chPolyFn :: (String, ChPoly MPBall) -> Plot.Function
chPolyFn (name, cp) =
  Plot.Function
  { function_name = name
  , function_dom = getDomain cp
  , function_getBounds = applyViaMPBall
  }
  where
  applyViaMPBall di =
    Interval (v-e) (v+e)
    where
    cpC = centreAsBall cp
    e = mpBall $ dyadic $ radius cp
    v :: MPBall
    v = apply cpC (mpBall di)
