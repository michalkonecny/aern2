{-# LANGUAGE CPP #-}
#define DEBUG

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (flip const)
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P

import System.Environment

import AERN2.MP.Dyadic
import AERN2.MP

import AERN2.Interval (Interval(..), DyadicInterval, dyadicInterval)
import qualified AERN2.Interval as Interval
import AERN2.RealFun.Operations
import AERN2.RealFun.SineCosine
import qualified AERN2.Poly.Cheb as ChPoly
import AERN2.Poly.Cheb (ChPoly)
import qualified AERN2.PPoly as PPoly
import AERN2.PPoly (PPoly)

import AERN2.RealFun.PlotService as Plot

main :: IO ()
main =
  do
  args <- getArgs
  Plot.startServer (fns args) shouldLog (int 4000)
  where
  shouldLog = True

fns :: [String] -> Plot.Functions
fns ["rungeFun"] = fnsRungeFun
fns ["rungePoly"] = fnsRungePoly
fns ["rungePPoly"] = fnsRungePPoly
fns ["sine"] = fnsSine
fns ["square"] = fnsSquare
fns _ = error "Please specify what to plot."

unitDom :: DyadicInterval
unitDom = dyadicInterval (-1.0,1.0)
unitDomP :: DyadicInterval
unitDomP = dyadicInterval (0.0,1.0)

fnsRungeFun :: Plot.Functions
fnsRungeFun =
  (map (funRoughFn 3)
  [ ("Fun 1/(10*x^2+1)", unitDom, runge10)])
  ++
  (map (funRoughFn 5)
  [ ("Fun 1/(10*x^2+1)", unitDom, runge10)])
  ++
  (map (funRoughFn 7)
  [ ("Fun 1/(10*x^2+1)", unitDom, runge10)])
  ++
  (map funFn
  [ ("Fun 1/(10*x^2+1)", unitDom, runge10)])
  where
  runge10 :: MPBall -> MPBall
  runge10 x = 1/(max 1 (10*x^2+1))

fnsRungePoly :: Plot.Functions
fnsRungePoly =
  map chPolyFn
  [ ("Poly(8) 1/(10*x^2+1)", ChPoly.reduceDegree 8 runge_4bits)
  , ("Poly(12) 1/(10*x^2+1)", ChPoly.reduceDegree 12 runge_4bits)
  , ("Poly(16) 1/(10*x^2+1)", ChPoly.reduceDegree 16 runge_4bits)
  ]
  ++
  (map funFn
  [ ("Fun 1/(10*x^2+1)", unitDom, \x -> 1/(10*x^2+1))])
  where
  runge_4bits = ChPoly.chebDivideDCT (bits 4) (xU-xU+1) (10*xU*xU+1)
  xU :: ChPoly MPBall
  xU = varFn sampleFn ()
  sampleFn = constFn (unitDom, 1)

fnsRungePPoly :: Plot.Functions
fnsRungePPoly =
  map ppolyFn
  [ ("PPoly 1/(10*x^2+1)", runge_bits 1)
  ]
  ++
  (map funFn
  [ ("Fun 1/(10*x^2+1)", unitDom, \x -> 1/(10*x^2+1))])
  where
  runge_bits b =
    PPoly.inverseWithAccuracy (bits b) (PPoly.fromPoly $ setPrecision (prec (12)) $ 10*xU*xU+1)
  xU :: ChPoly MPBall
  xU = varFn sampleFn ()
  sampleFn = constFn (unitDom, 1)

fnsSine :: Plot.Functions
fnsSine =
  map chPolyFn
  [ ("Poly (1+sin[ac=3](6x))/2", (1+sine (6*xU))/2)
  , ("Poly (1+cos[ac=3](6x))/2", (1+cosine (6*xU))/2)
  ]
  ++
  (map funFn
  [ ("Fun (1+sin(6x))/2", unitDom, \x -> (1+sin (6*x))/2)
  , ("Fun (1+cos(6x))/2", unitDom, \x -> (1+cos (6*x))/2)
  ])
  where
  sine = sineWithAccuracyGuide (bits 3)
  cosine = cosineWithAccuracyGuide (bits 3)
  xU :: ChPoly MPBall
  xU = varFn sampleFn ()
  sampleFn = constFn (unitDom, 1)

fnsSquare :: Plot.Functions
fnsSquare =
  map chPolyFn
  [ ("Poly x^2", ChPoly.reduceDegree 1 $ xP*xP) ]
  ++
  (map funFn
  [
    ("Fun x^2", unitDom, \x -> x^2)
  ])
  where
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

funRoughFn :: Integer -> (String, DyadicInterval, MPBall -> MPBall) -> Plot.Function
funRoughFn depth (name, dom, b2b) =
  -- maybeTrace
  -- ("funRoughFn: partitionValues = " ++ show partitionValues) $
  Plot.Function
  { function_name = name
  , function_dom = dom
  , function_getBounds = getBounds
  }
  where
  getBounds x =
    -- maybeTrace
    -- ("funRoughFn: getBounds: x = " ++ show x ++ "; e = " ++ show e) $
    Interval l r
    where
    (l,r) = endpoints e
    e = foldl1 fromEndpoints $ map snd $ getSegments x
  getSegments x = filter (Interval.intersects x . fst) partitionValues
  partitionValues = map (\s -> (s,b2b $ mpBall s)) partition
  partition = bisectDepth depth dom

bisectDepth ::
  Integer -> DyadicInterval -> [DyadicInterval]
bisectDepth d ival
  | d == 0 = [ival]
  | otherwise =
    (bisectDepth (d-1) l) ++ (bisectDepth (d-1) r)
  where
  (l,r) = Interval.split ival

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


ppolyFn :: (String, PPoly) -> Plot.Function
ppolyFn (name, pp) =
  Plot.Function
  { function_name = name
  , function_dom = getDomain pp
  , function_getBounds = applyViaMPBall
  }
  where
  applyViaMPBall di =
    maybeTrace ("ppolyFn: applyViaMPBall: di = " ++ show di ++ "; v = " ++ show v) $
    Interval l r
    where
    v :: MPBall
    v = PPoly.evalDirect pp (mpBall di)
    (l,r) = endpoints v
