{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
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

    result =
      case (representationCode, operationCode) of
        ("fun", "max") -> maxModFun (fn_x id x_MF) accuracy
        ("fun", "integrate") -> integrateModFun (fn_x id x_MF) accuracy
        ("ball", "max") -> maxBallFun (fn_x id x_BF) accuracy
        ("ball", "integrate") -> integrateBallFun (fn_x id x_BF) accuracy
        ("dball", "max") -> maxDBallFun (fn_x id x_DBF) accuracy
        ("dball", "integrate") -> integrateDBallFun (fn_x id x_DBF) accuracy
        ("poly", "max") -> maxPB $ fn_x id (x_PB accuracy)
        ("poly", "integrate") -> integratePB $ fn_x id (x_PB accuracy)

        ("ppoly", "max") -> maxPP $ fn_x PPoly.fromPoly (x_PB accuracy)
        ("ppoly", "integrate") -> integratePP $ fn_x PPoly.fromPoly (x_PB accuracy)
        ("frac", "max") -> maxFR $ fn_x Frac.fromPoly (x_PB accuracy)
        ("frac", "integrate") -> integrateFR $ fn_x Frac.fromPoly (x_PB accuracy)

        ("lpoly", "max") -> maxLP (fn_x id x_LP) accuracy
        ("lpoly", "integrate") -> integrateLP (fn_x id x_LP) accuracy
        ("lppoly", "max") -> maxLPP (fn_x LPPoly.fromPoly x_LP) accuracy
        ("lppoly", "integrate") -> integrateLPP (fn_x LPPoly.fromPoly x_LP) accuracy
        ("lfrac", "max") -> maxLF (fn_x LFrac.fromPoly x_LP) accuracy
        ("lfrac", "integrate") -> integrateLF (fn_x LFrac.fromPoly x_LP) accuracy
        _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    (Just (fnDescription, fn_x)) = Map.lookup functionCode functions

    accuracy = bits $ (read accuracyS :: Int)
    [accuracyS] = effortArgs

    maxPB :: ChPoly MPBall -> MPBall
    maxPB f = f `maximumOverDom` (getDomain f)

    integratePB :: ChPoly MPBall -> MPBall
    integratePB f =
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

    maxPP :: PPoly -> MPBall
    maxPP f = f `maximumOverDomPP` (getDomain f)
      where
      maximumOverDomPP f2 (Interval l r) =
        PPoly.maximum f2 lB rB
        where
        lB = setPrecision prc $ mpBall l
        rB = setPrecision prc $ mpBall r
        prc = getPrecision f2

    integratePP :: PPoly -> MPBall
    integratePP f = f `integrateOverDomPP` (getDomain f)
      where
      integrateOverDomPP ff (Interval l r) =
        PPoly.integral ff (mpBall l) (mpBall r)

    maxFR :: FracMB -> MPBall
    maxFR f = f `maximumOverDomFR` (getDomain f)
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

    integrateFR :: FracMB -> MPBall
    integrateFR f = f `integrateOverDomFR` (getDomain f)
      where
      integrateOverDomFR ff (Interval l r) =
        Frac.integral ff (mpBall l) (mpBall r)

    maxBallFun :: UnaryBallFun -> Accuracy -> MPBall
    maxBallFun fn ac =
        m ? accuracySG ac
        where
        m = fn `maximumOverDom` getDomain fn

    maxModFun :: UnaryModFun -> Accuracy -> MPBall
    maxModFun fn ac =
        m ? accuracySG ac
        where
        m = fn `maximumOverDom` getDomain fn

    maxDBallFun :: UnaryBallDFun -> Accuracy -> MPBall
    maxDBallFun (UnaryBallDFun [f, f']) ac =
        m ? accuracySG ac
        where
        m = fn `maximumOverDom` getDomain f
        fn = UnaryBallDFun [f,f']
    maxDBallFun _ _ = error "maxDBallFun: invalid UnaryBallDFun"

    integrateBallFun :: UnaryBallFun -> Accuracy -> MPBall
    integrateBallFun fn ac =
        r ? accuracySG ac
        where
        r = (~!) $ fn `integrateOverDom` (getDomain fn)

    integrateModFun :: UnaryModFun -> Accuracy -> MPBall
    integrateModFun fn ac =
        r ? accuracySG ac
        where
        r = (~!) $ fn `integrateOverDom` (getDomain fn)

    integrateDBallFun :: UnaryBallDFun -> Accuracy -> MPBall
    integrateDBallFun (UnaryBallDFun [f, f']) ac =
        r ? accuracySG ac
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
  Map.Map String (String, forall f1 f2. (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2)
functions =
    Map.fromList
    [
      ("sine+cos", (sinecos_Name, sinecos_x))
    , ("sinesine", (sinesine_Name, sinesine_x))
    , ("sinesine+cos", (sinesineCos_Name, sinesineCos_x))
    , ("runge", (runge_Name, runge_x))
    , ("rungeSC", (rungeSC_Name, rungeSC_x))
    , ("fracSin", (fracSin_Name, fracSin_x))
    , ("fracSinSC", (fracSinSC_Name, fracSinSC_x))
    -- -- , ("hat", (hat_Name, hat_x))
    -- -- , ("bumpy", (bumpy_Name, bumpy_x))
    ]


type Signature1 f =
  ( HasAccuracy f, HasAccuracyGuide f
  , CanSinCosSameType f
  , CanMulBy f Integer
  , CanSetAccuracyGuide f
  , CanAddSameType f
  , CanAddThis f Integer
  , CanPowCNBy f Integer)

type Signature2 f =
  ( HasAccuracy f, HasAccuracyGuide f
  , CanMulSameType f
  , CanDivCNSameType f, CanRecipCNSameType f)

-----------------------------------
-----------------------------------

sinecos_Name :: String
sinecos_Name = "sin(10x)+cos(20x) over [-1,1]"

sinecos_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
sinecos_x tr12 x = tr12 $ sin(10*x)+cos(20*x)

-----------------------------------
-----------------------------------

sinesine_Name :: String
sinesine_Name = "sin(10x+sin(20x^2)) over [-1,1]"

sinesine_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
sinesine_x tr12 xPre =
  -- maybeTrace (printf "sin(20*x^!2): acG = %s; ac = %s" (show $ getAccuracyGuide sin20x2) (show $ getAccuracy sin20x2)) $
  -- maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  -- res
  tr12 $ sin(10*x + sin(20*x^!2))
  where
  -- res = tr12 $ sin(10*x + sin20x2)
  -- sin20x2 = sin(20*x^!2)
  x = adjustAccuracyGuide (\a -> max 45 (a+15)) xPre

-----------------------------------
-----------------------------------

sinesineCos_Name :: String
sinesineCos_Name = "sin(10x+sin(20x^2)) + cos(10x) over [-1,1]"

sinesineCos_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
sinesineCos_x tr12 xPre =
  tr12 $ sin(10*x + sin(20*x^!2)) + cos(10*x)
  where
  x = adjustAccuracyGuide (\a -> max 45 (a+15)) xPre

-----------------------------------
-----------------------------------

runge_Name :: String
runge_Name = "1/(100x^2+1) over [-1,1]"

runge_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
runge_x tr12 x =
  -- maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  -- res
  1/!(tr12 $ 100*x^!2+1)
  where
  res = 1/!(tr12 $ 100*x^!2+1)

-----------------------------------
-----------------------------------

rungeSC_Name :: String
rungeSC_Name = "(sin(10x)+cos(20x))/(100x^2+1) over [-1,1]"

rungeSC_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
rungeSC_x tr12 x =
  maybeTrace (printf "numer: acG = %s; ac = %s" (show $ getAccuracyGuide numer) (show $ getAccuracy numer)) $
  maybeTrace (printf "inv: acG = %s; ac = %s" (show $ getAccuracyGuide inv) (show $ getAccuracy inv)) $
  maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  res
  -- (tr12 $ sin (10*xA) + cos(20*xA))/!(tr12 $ 100*x^!2+1)
  where
  res = numer*inv
  inv = 1/!(tr12 $ 100*x^!2+1)
  numer = tr12 $ sin (10*xA) + cos(20*xA)
  xA = adjustAccuracyGuide (\a -> 4*a+15) x
  -- tr12R = tr12 . setAccuracyGuide (getAccuracyGuide x)

-----------------------------------
-----------------------------------

fracSin_Name :: String
fracSin_Name = "1/(10(sin(7x))^2+1) over [-1,1]"

fracSin_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
fracSin_x tr12 x =
  1/!(tr12R $ 10*(sin(7*xA)^!2)+1)
  where
  xA = adjustAccuracyGuide (\a -> 2*a+20) x
  tr12R = tr12 . setAccuracyGuide (getAccuracyGuide x)

-----------------------------------
-----------------------------------

fracSinSC_Name :: String
fracSinSC_Name = "(sin(10x)+cos(20x))/(10(sin(7x))^2+1) over [-1,1]"

fracSinSC_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
fracSinSC_x tr12 x =
  maybeTrace (printf "numer: acG = %s; ac = %s" (show $ getAccuracyGuide numer) (show $ getAccuracy numer)) $
  maybeTrace (printf "denom: acG = %s; ac = %s" (show $ getAccuracyGuide denom) (show $ getAccuracy denom)) $
  maybeTrace (printf "inv: acG = %s; ac = %s" (show $ getAccuracyGuide inv) (show $ getAccuracy inv)) $
  maybeTrace (printf "res: acG = %s; ac = %s" (show $ getAccuracyGuide res) (show $ getAccuracy res)) $
  res
  -- (tr12R $ sin (10*xA1) + cos(20*xA1))/!(tr12R $ 10*(sin(7*xA2)^!2)+1)
  where
  res = (tr12 $ numer) * inv
  inv = 1/! (tr12R $ denom)
  numer = sin (10*xA1) + cos(20*xA1)
  denom = 10*(sin(7*xA2)^!2)+1
  xA1 = adjustAccuracyGuide (\a -> 4*a+30) x
  xA2 = adjustAccuracyGuide (\a -> 2*a+30) x
  tr12R = tr12 . setAccuracyGuide (getAccuracyGuide x + 2)

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
bumpy_Name = "max(sin(10x),cos(11x)) over [-1,1]"

-- bumpy_x :: (Signature1 f1, Signature2 f2) => (f1 -> f2) -> f1 -> f2
-- bumpy_x tr12 x =
--     tr12 $ max (sin (10*x)) (cos (11*x))
