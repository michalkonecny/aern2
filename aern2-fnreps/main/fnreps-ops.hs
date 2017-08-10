{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
#define DEBUG
module Main where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P
import Text.Printf

import qualified Data.Map as Map

-- import Control.Applicative (liftA2)

import System.Environment

import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall

import AERN2.Real

import AERN2.Interval

import AERN2.RealFun.Operations
import AERN2.RealFun.UnaryBallFun
import AERN2.RealFun.UnaryBallDFun
import AERN2.RealFun.UnaryModFun
-- import AERN2.Poly.Basics

import qualified AERN2.PPoly as PPoly
import AERN2.PPoly (PPoly)

import AERN2.Poly.Cheb (ChPoly)

import qualified AERN2.Frac as Frac
import AERN2.Frac (Frac)

import qualified AERN2.Local as Local
import qualified AERN2.Local.Poly as LPoly
import qualified AERN2.Local.PPoly as LPPoly
import qualified AERN2.Local.Frac as LFrac

type FracMB = Frac MPBall

type LPolyMB = LPoly.LocalPoly MPBall
type LPPolyMB = LPPoly.LocalPPoly
type LFracMB = LFrac.LocalFrac MPBall

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, result) <- processArgs args
    putStrLn $ computationDescription
    putStrLn $ "result = " ++ show result
    putStrLn $ "accuracy: " ++ show (getAccuracy result)
    putStrLn $ "precision = " ++ show (getPrecision result)

processArgs ::
    [String] ->
    IO (String, MPBall)
processArgs (operationCode : functionCode : representationCode : effortArgs) =
    return (computationDescription, result)
    where
    computationDescription =
        "computing " ++ operationCode ++ "  " ++ fnDescription

    (result, fnDescription) =
      case (representationCode, operationCode) of
        ("fun", "max") ->  eval functions maxModFun id x_MF
        ("fun", "integrate") -> eval functions integrateModFun id x_MF
        ("ball", "max") -> eval functions maxBallFun id x_BF
        ("ball", "integrate") -> eval functions integrateBallFun id x_BF
        ("dball", "max") -> eval functions maxDBallFun id x_DBF
        ("dball", "integrate") -> eval functions integrateDBallFun id x_DBF
        ("poly", "max") -> eval functions maxPB id (x_PB accuracy)
        ("poly", "integrate") -> eval functions integratePB id (x_PB accuracy)
        ("ppoly", "max") -> eval functions maxPP PPoly.fromPoly (x_PB accuracy)
        ("ppoly", "integrate") -> eval functions integratePP PPoly.fromPoly (x_PB accuracy)
        ("frac", "max") -> eval functions maxFR Frac.fromPoly (x_PB accuracy)
        ("frac", "integrate") -> eval functions integrateFR Frac.fromPoly (x_PB accuracy)
        ("lpoly", "max") -> eval functions maxLP id x_LP
        ("lpoly", "integrate") -> eval functions integrateLP id x_LP
        ("lppoly", "max") -> eval functions maxLPP LPPoly.fromPoly x_LP
        ("lppoly", "integrate") -> eval functions integrateLPP LPPoly.fromPoly x_LP
        ("lfrac", "max") -> eval functions maxLF LFrac.fromPoly x_LP
        ("lfrac", "integrate") -> eval functions integrateLF LFrac.fromPoly x_LP
        _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    eval ::
      (Signature1 f1, Signature2 f2)
      =>
      Map.Map String (String, (f1 -> f2) -> (f1 -> f2)) ->
      (f2 -> Accuracy -> MPBall) ->
      (f1 -> f2) ->
      f1 ->
      (MPBall, String)
    eval fns functional tr12 x  =
      case Map.lookup functionCode fns of
        Just (fnDescription2, fn_x) ->
          (functional (fn_x tr12 x) accuracy, fnDescription2)
        _ -> error $ "unknown function: " ++ functionCode

    accuracy = bits $ (read accuracyS :: Int)
    [accuracyS] = effortArgs

    maxPB :: ChPoly MPBall -> Accuracy -> MPBall
    maxPB f _ = f `maximumOverDom` (getDomain f)

    integratePB :: ChPoly MPBall -> Accuracy -> MPBall
    integratePB f _ =
      maybeTrace ("integratePB: accuracy f = " ++ show (getAccuracy f)) $
      f `integrateOverDom` (getDomain f)

    maxLP :: LPolyMB -> Accuracy -> MPBall
    maxLP lf ac = Local.maximum lf (mpBall (-1)) (mpBall 1) ac

    maxLPP :: LPPolyMB -> Accuracy -> MPBall
    maxLPP lf ac = Local.maximum lf (mpBall (-1)) (mpBall 1) ac

    maxLF :: LFracMB -> Accuracy -> MPBall
    maxLF lf ac = Local.maximum lf (mpBall (-1)) (mpBall 1) ac

    integrateLP lf = lf `integrateOverDom` unaryIntervalDom
    integrateLPP lf = lf `integrateOverDom` unaryIntervalDom
    integrateLF lf = lf `integrateOverDom` unaryIntervalDom

    maxPP :: PPoly -> Accuracy -> MPBall
    maxPP f _ = f `maximumOverDomPP` (getDomain f)
      where
      maximumOverDomPP f2 (Interval l r) =
        PPoly.maximum f2 lB rB
        where
        lB = setPrecision prc $ mpBall l
        rB = setPrecision prc $ mpBall r
        prc = getPrecision f2

    integratePP :: PPoly -> Accuracy -> MPBall
    integratePP f _ = f `integrateOverDomPP` (getDomain f)
      where
      integrateOverDomPP ff (Interval l r) =
        PPoly.integral ff (mpBall l) (mpBall r)

    maxFR :: FracMB -> Accuracy -> MPBall
    maxFR f _ = f `maximumOverDomFR` (getDomain f)
      where
      maximumOverDomFR f2 (Interval l r) =
        Frac.maximumOptimisedWithAccuracy accuracy (setPrc f2) lB rB 5 5
        where
        lB = setPrc $ mpBall l
        rB = setPrc $ mpBall r
        setPrc :: (CanSetPrecision a) => a -> a
        setPrc =
          setPrecisionAtLeastAccuracy (accuracy) . setPrecision prc
        prc = getPrecision f2

    integrateFR :: FracMB -> Accuracy -> MPBall
    integrateFR f _ = f `integrateOverDomFR` (getDomain f)
      where
      integrateOverDomFR ff (Interval l r) =
        Frac.integral ff (mpBall l) (mpBall r)

    maxBallFun :: UnaryBallFun -> Accuracy -> MPBall
    maxBallFun fn ac =
        m ? AccuracySG ac ac
        where
        m = fn `maximumOverDom` getDomain fn

    maxModFun :: UnaryModFun -> Accuracy -> MPBall
    maxModFun fn ac =
        m ? AccuracySG ac ac
        where
        m = fn `maximumOverDom` getDomain fn

    maxDBallFun :: UnaryBallDFun -> Accuracy -> MPBall
    maxDBallFun (UnaryBallDFun [f, f']) ac =
        m ? AccuracySG ac ac
        where
        m = fn `maximumOverDom` getDomain f
        fn = UnaryBallDFun [f,f']
    maxDBallFun _ _ = error "maxDBallFun: invalid UnaryBallDFun"

    integrateBallFun :: UnaryBallFun -> Accuracy -> MPBall
    integrateBallFun fn ac =
        r ? AccuracySG ac ac
        where
        r = (~!) $ fn `integrateOverDom` (getDomain fn)

    integrateModFun :: UnaryModFun -> Accuracy -> MPBall
    integrateModFun fn ac =
        r ? AccuracySG ac ac
        where
        r = (~!) $ fn `integrateOverDom` (getDomain fn)

    integrateDBallFun :: UnaryBallDFun -> Accuracy -> MPBall
    integrateDBallFun (UnaryBallDFun [f, f']) ac =
        r ? AccuracySG ac ac
        where
        r = (~!) $ fn `integrateOverDom` (getDomain f)
        dom = getDomain f
        fn = UnaryBallDFun [f,f']
    integrateDBallFun _ _ = error "integrateDBallFun: invalid UnaryBallDFun"
processArgs _ =
    error "expecting arguments: <operationCode> <functionCode> <representationCode> <effort parameters...>"

x_MF :: UnaryModFun
x_MF = varFn unaryIntervalDom ()

x_BF :: UnaryBallFun
x_BF = varFn unaryIntervalDom ()

x_DBF :: UnaryBallDFun
x_DBF = varFn unaryIntervalDom ()

x_PB :: Accuracy -> ChPoly MPBall
x_PB acG =
  setAccuracyGuide acG $ varFn (unaryIntervalDom, bits 10) ()

x_LP :: LPoly.LocalPoly MPBall
x_LP = LPoly.variable

unaryIntervalDom :: DyadicInterval
unaryIntervalDom = dyadicInterval (-1,1)

functions ::
  forall f1 f2. (Signature1 f1, Signature2 f2) =>
  Map.Map String (String, (f1 -> f2) -> f1 -> f2)
functions =
    Map.fromList
    [
      ("sine+cos", (sinecos_Name, sinecos_x))
    , ("sine+cospi", (sinecospi_Name, sinecospi_x))
    , ("sinesine", (sinesine_Name, sinesine_x))
    , ("sinesine+cos", (sinesineCos_Name, sinesineCos_x))
    , ("runge", (runge_Name, runge_x))
    , ("rungeSC", (rungeSC_Name, rungeSC_x))
    , ("fracSin", (fracSin_Name, fracSin_x))
    , ("fracSinSC", (fracSinSC_Name, fracSinSC_x))
    -- -- , ("hat", (hat_Name, hat_x))
    , ("bumpy", (bumpy_Name, bumpy_x))
    , ("bumpy2", (bumpy2_Name, bumpy2_x))
    ]


type Signature1 f =
  ( HasAccuracy f, HasAccuracyGuide f
  , CanSetAccuracyGuide f
  , CanSinCosSameType f
  , CanMulBy f Integer
  , CanMulBy f CauchyReal
  , CanAddSameType f
  , CanAddThis f Integer
  , CanAddThis f CauchyReal
  , CanPowCNBy f Integer)

type Signature2 f =
  ( HasAccuracy f, HasAccuracyGuide f
  , CanSetAccuracyGuide f
  , CanMulSameType f
  , CanMinMaxSameType f
  , CanDivCNSameType f, CanRecipCNSameType f)

-----------------------------------
-----------------------------------

sinecos_Name :: String
sinecos_Name = "sin(10x)+cos(20x) over [-1,1]"

sinecos_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
sinecos_x tr12 x = tr12 $ sin(10*x)+cos(20*x)

-----------------------------------
-----------------------------------

sinecospi_Name :: String
sinecospi_Name = "sin(10x)+cos(7pi*x) over [-1,1]"

sinecospi_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
sinecospi_x tr12 x = tr12 $ sin(10*x)+cos(7*pi*x)

-----------------------------------
-----------------------------------

sinesine_Name :: String
sinesine_Name = "sin(10x+sin(7pi*x^2)) over [-1,1]"

sinesine_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
sinesine_x tr12 xPre =
  -- maybeTrace (printf "sin(20*x^!2): acG = %s; ac = %s" (show $ getAccuracyGuide sin20x2) (show $ getAccuracy sin20x2)) $
  -- maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  -- res
  tr12 $ sin(10*x + sin(7*pi*x^!2))
  where
  -- res = tr12 $ sin(10*x + sin20x2)
  -- sin20x2 = sin(20*x^!2)
  x = adjustAccuracyGuide (\a -> max 45 (a+15)) xPre

-----------------------------------
-----------------------------------

sinesineCos_Name :: String
sinesineCos_Name = "sin(10x+sin(7pi*x^2)) + cos(3pi*x) over [-1,1]"

sinesineCos_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
sinesineCos_x tr12 xPre =
  tr12 $ sin(10*x + sin(7*pi*x^!2)) + cos(3*pi*x)
  where
  x = adjustAccuracyGuide (\a -> max 45 (a+15)) xPre

-----------------------------------
-----------------------------------

runge_Name :: String
runge_Name = "1/(100x^2+1) over [-1,1]"

runge_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
runge_x tr12 x =
  -- maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  -- res
  1/!(tr12 $ 100*x^!2+1)
  -- where
  -- res = 1/!(tr12 $ 100*x^!2+1)

-----------------------------------
-----------------------------------

rungeSC_Name :: String
rungeSC_Name = "(sin(10x)+cos(7pi*x))/(100x^2+1) over [-1,1]"

rungeSC_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
rungeSC_x tr12 x =
  maybeTrace (printf "numer: acG = %s; ac = %s" (show $ getAccuracyGuide numer) (show $ getAccuracy numer)) $
  maybeTrace (printf "inv: acG = %s; ac = %s" (show $ getAccuracyGuide inv) (show $ getAccuracy inv)) $
  maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  res
  -- (tr12 $ sin (10*xA) + cos(7*pi*xA))/!(tr12 $ 100*x^!2+1)
  where
  res = numer*inv
  inv = 1/!(tr12 $ 100*x^!2+1)
  numer = tr12 $ sin (10*xA) + cos(7*pi*xA)
  xA = adjustAccuracyGuide (\a -> 4*a+15) x
  -- tr12R = tr12 . setAccuracyGuide (getAccuracyGuide x)

-----------------------------------
-----------------------------------

fracSin_Name :: String
fracSin_Name = "1/(10(sin(7x))^2+1) over [-1,1]"

fracSin_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
fracSin_x tr12 x =
  1/!(tr12R $ 10*(sin(7*xA)^!2)+1)
  where
  xA = adjustAccuracyGuide (\a -> 2*a+20) x
  tr12R = tr12 . setAccuracyGuide (getAccuracyGuide x)

-----------------------------------
-----------------------------------

fracSinSC_Name :: String
fracSinSC_Name = "(sin(10x)+cos(7pi*x))/(10(sin(7x))^2+1) over [-1,1]"

fracSinSC_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
fracSinSC_x tr12 x =
  maybeTrace (printf "numer: acG = %s; ac = %s" (show $ getAccuracyGuide numer) (show $ getAccuracy numer)) $
  maybeTrace (printf "denom: acG = %s; ac = %s" (show $ getAccuracyGuide denom) (show $ getAccuracy denom)) $
  maybeTrace (printf "inv: acG = %s; ac = %s" (show $ getAccuracyGuide inv) (show $ getAccuracy inv)) $
  maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  res
  -- (tr12R $ sin (10*xA1) + cos(7*pi*xA1))/!(tr12R $ 10*(sin(7*xA2)^!2)+1)
  where
  res = (setACprod $ tr12 numer) * (setACprod inv)
  inv = 1/! (tr12 $ setACdiv $ denom)
  numer = sin (10*xA1) + cos(7*pi*xA1)
  denom = 10*(sin(7*xA2)^!2)+1

  -- xA1 = adjustAccuracyGuide (\a -> 4*a+30) x
  -- xA2 = adjustAccuracyGuide (\a -> 2*a+30) x
  -- tr12R = tr12 . setAccuracyGuide (getAccuracyGuide x + 2)

  ac = getAccuracyGuide x
  xA1 = setAccuracyGuide (8*ac+30) x
  xA2 = setAccuracyGuide (2*ac+30) x
  setACdiv = setAccuracyGuide (ac + 2)
  setACprod = setAccuracyGuide (2*ac + 2)

-----------------------------------
-----------------------------------

hat_Name :: String
hat_Name = "1-|x+1/3| over [-1,1]"

-- hat_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
-- hat_x tr12 x =
--   tr12 $ 1 - (abs (x+1/3))

-----------------------------------
-----------------------------------
bumpy_Name :: String
bumpy_Name = "max(sin(10x),cos(11*x)) over [-1,1]"

bumpy_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
bumpy_x tr12 x =
  maybeTrace (printf "maxL: acG = %s; ac = %s" (show $ getAccuracyGuide maxL) (show $ getAccuracy maxL)) $
  maybeTrace (printf "maxR: acG = %s; ac = %s" (show $ getAccuracyGuide maxR) (show $ getAccuracy maxR)) $
  max (setACmax maxL) (setACmax maxR)
  where
  maxL = tr12 $ sin (10*xA1)
  maxR = tr12 $ cos (11*xA1)

  ac = getAccuracyGuide x
  xA1 = setAccuracyGuide (10*ac+10) x
  setACmax = setAccuracyGuide (ac)

bumpy2_Name :: String
bumpy2_Name = "max((x^2)/2,(sin(10x)+cos(7pi*x))/(10(sin(7x))^2+1) over [-1,1]"

bumpy2_x :: forall f1 f2.(Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
bumpy2_x tr12 x =
  maybeTrace (printf "numer: acG = %s; ac = %s" (show $ getAccuracyGuide numer) (show $ getAccuracy numer)) $
  maybeTrace (printf "denom: acG = %s; ac = %s" (show $ getAccuracyGuide denom) (show $ getAccuracy denom)) $
  maybeTrace (printf "inv: acG = %s; ac = %s" (show $ getAccuracyGuide inv) (show $ getAccuracy inv)) $
  maybeTrace (printf "frac: acG = %s; ac = %s" (show $ getAccuracyGuide frac) (show $ getAccuracy frac)) $
  max (setACmax maxL) (setACmax maxR)
  where
  maxL = tr12 $ (x^!2)*(real 0.5)
  maxR = frac
  frac = (setACprod $ tr12 $ numer) * (setACprod inv)
  inv = 1/!(tr12 $ setACdiv denom)
  numer = sin (10*xA1) + cos(7*pi*xA1)
  denom = 10*(sin(7*xA2)^!2)+1

  ac = getAccuracyGuide x
  xA1 = setAccuracyGuide (8*ac+10) x
  xA2 = setAccuracyGuide (2*ac+30) x
  setACdiv = setAccuracyGuide (ac + 2)
  setACprod = setAccuracyGuide (2*ac + 2)
  setACmax = setAccuracyGuide (ac)
