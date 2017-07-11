{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
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
-- import Text.Printf

import qualified Data.Map as Map

-- import Control.Applicative (liftA2)

import System.Environment

import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall

import AERN2.Real

import AERN2.Interval

import AERN2.RealFun.Operations
import AERN2.RealFun.SineCosine
import AERN2.RealFun.UnaryBallFun
import AERN2.RealFun.UnaryBallDFun
import AERN2.RealFun.UnaryModFun
-- import AERN2.Poly.Basics

import qualified AERN2.PPoly as PPoly
import AERN2.PPoly (PPoly)

import AERN2.Poly.Cheb (ChPoly, chPolyMPBall)
import qualified AERN2.Poly.Cheb as ChPoly

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
    -- putStrLn $ "result ~ " ++ showB result
    putStrLn $ "accuracy: " ++ show (getAccuracy result)
    putStrLn $ "precision = " ++ show (getPrecision result)
    where
    -- showB = show . getApproximate (bits 30)
--    showAP = show . getApproximate (bits 50) . cheb2Power

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
        ("fun", "max") -> maxModFun fnModFun accuracy
        ("ball", "max") -> maxBallFun fnB2B accuracy
        ("dball", "max") -> maxDBallFun fnB2B dfnB2B accuracy
        ("fun", "integrate") -> integrateModFun fnModFun accuracy
        ("ball", "integrate") -> integrateBallFun fnB2B accuracy
        ("dball", "integrate") -> integrateDBallFun fnB2B dfnB2B accuracy
        ("poly", "max") -> maxPB $ fnPB accuracy
        ("poly", "integrate") -> integratePB $ fnPB accuracy
        ("ppoly", "max") -> maxPP $ fnPP accuracy
        ("ppoly", "integrate") -> integratePP $ fnPP accuracy
        ("frac", "max") -> maxFR $ fnFR accuracy
        ("frac", "integrate") -> integrateFR $ fnFR accuracy
        ("lpoly", "max") -> maxLP fnLP accuracy
        ("lpoly", "integrate") -> integrateLP fnLP accuracy
        ("lppoly", "max") -> maxLPP fnLPP accuracy
        ("lppoly", "integrate") -> integrateLPP fnLPP accuracy
        ("lfrac", "max") -> maxLF fnLF accuracy
        ("lfrac", "integrate") -> integrateLF fnLF accuracy
        _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    (Just (fnDescription, fnPB, fnModFun, fnB2B, dfnB2B, fnPP, fnFR, fnLP, fnLPP, fnLF)) = Map.lookup functionCode functions

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

    maxDBallFun :: UnaryBallFun -> UnaryBallFun -> Accuracy -> MPBall
    maxDBallFun f f' ac =
        m ? accuracySG ac
        where
        m = fn `maximumOverDom` getDomain f
        fn = UnaryBallDFun [f,f']

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

    integrateDBallFun :: UnaryBallFun -> UnaryBallFun -> Accuracy -> MPBall
    integrateDBallFun f f' ac =
        r ? accuracySG ac
        where
        r = (~!) $ fn `integrateOverDom` (getDomain f)
        dom = getDomain f
        fn = UnaryBallDFun [f,f']
processArgs _ =
    error "expecting arguments: <operationCode> <functionCode> <representationCode> <effort parameters...>"

functions ::
  Map.Map String
  (String, Accuracy -> ChPoly MPBall, UnaryModFun, UnaryBallFun, UnaryBallFun,
   Accuracy -> PPoly, Accuracy -> FracMB,
   LPolyMB, LPPolyMB, LFracMB)
functions =
    Map.fromList
    [
      ("sine+cos", (sinecos_Name, sinecos_PB, sinecos_ModFun, sinecos_B2B, sinecosDeriv_B2B, sinecos_PP, sinecos_FR, sinecos_LP, sinecos_LPP, sinecos_LF))
    , ("sinesine", (sinesine_Name, sinesine_PB, sinesine_ModFun, sinesine_B2B, sinesineDeriv_B2B, sinesine_PP, sinesine_FR, sinesine_LP, sinesine_LPP, sinesine_LF))
    , ("sinesine+sin", (sinesineSin_Name, sinesineSin_PB, sinesineSin_ModFun, sinesineSin_B2B, sinesineSinDeriv_B2B, sinesineSin_PP, sinesineSin_FR, sinesineSin_LP, sinesineSin_LPP, sinesineSin_LF))
    , ("sinesine+cos", (sinesineCos_Name, sinesineCos_PB, sinesineCos_ModFun, sinesineCos_B2B, sinesineCosDeriv_B2B, sinesineCos_PP, sinesineCos_FR, sinesineCos_LP, sinesineCos_LPP, sinesineCos_LF))
    , ("runge", (runge_Name, runge_PB, runge_ModFun, runge_B2B, rungeDeriv_B2B, runge_PP, runge_FR, runge_LP, runge_LPP, runge_LF))
    , ("rungeX", (rungeX_Name, rungeX_PB, rungeX_ModFun, rungeX_B2B, rungeXDeriv_B2B, rungeX_PP, rungeX_FR, rungeX_LP, rungeX_LPP, rungeX_LF))
    , ("rungeSC", (rungeSC_Name, rungeSC_PB, rungeSC_ModFun, rungeSC_B2B, rungeSCDeriv_B2B, rungeSC_PP, rungeSC_FR, rungeSC_LP, rungeSC_LPP, rungeSC_LF))
    , ("fracSin", (fracSin_Name, fracSin_PB, fracSin_ModFun, fracSin_B2B, fracSinDeriv_B2B, fracSin_PP, fracSin_FR, fracSin_LP, fracSin_LPP, fracSin_LF))
    , ("fracSinX", (fracSinX_Name, fracSinX_PB, fracSinX_ModFun, fracSinX_B2B, fracSinXDeriv_B2B, fracSinX_PP, fracSinX_FR, fracSinX_LP, fracSinX_LPP, fracSinX_LF))
    , ("fracSinSC", (fracSinSC_Name, fracSinSC_PB, fracSinSC_ModFun, fracSinSC_B2B, fracSinSCDeriv_B2B, fracSinSC_PP, fracSinSC_FR, fracSinSC_LP, fracSinSC_LPP, fracSinSC_LF))
    -- , ("hat", (hat_Name, hat_PB, hat_B2B, hatDeriv_B2B, hat_PP, hat_FR))
    -- , ("bumpy", (bumpy_Name, bumpy_PB, bumpy_B2B, bumpyDeriv_B2B, bumpy_PP, bumpy_FR))
    ]

-- data Operator = OpMax | OpIntegrate
-- type FnPP = Operator -> Precision -> Degree -> Rational -> Integer -> Accuracy -> MPBall

sinecos_Name :: String
sinecos_Name = "sin(10x)+cos(20x) over [-1,1]"

sinecos_PB :: Accuracy -> ChPoly MPBall
sinecos_PB acGuide =
  sine(10*x)+cosine(20*x)
  where
  sine = sineWithAccuracyGuide acGuide
  cosine = cosineWithAccuracyGuide acGuide
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

sinecos_ModFun :: UnaryModFun
sinecos_ModFun =
  sin(10*x)+cos(20*x)
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

sinecos_B2B :: UnaryBallFun
sinecos_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    sin(10*x)+cos(20*x)

sinecosDeriv_B2B :: UnaryBallFun
sinecosDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    10*cos(10*x)-20*sin(20*x)

sinecos_PP :: Accuracy -> PPoly
sinecos_PP =
  error $ "Not (yet) supporting PPoly for: " ++ sinecos_Name

sinecos_FR :: Accuracy -> FracMB
sinecos_FR =
  error $ "Not (yet) supporting Frac for: " ++ sinecos_Name

sinecos_LP :: LPolyMB
sinecos_LP =
  sine(10*x)+cosine(20*x)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB
  cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

sinecos_LPP :: LPPolyMB
sinecos_LPP =
  error $ "Not (yet) supporting LPPoly for: " ++ sinecos_Name

sinecos_LF :: LFracMB
sinecos_LF =
  error $ "Not (yet) supporting LFrac for: " ++ sinecos_Name
  -- -- sine(10*x)+cosine(20*x)
  -- LFrac.fromPoly $ sine(10*x)+cosine(20*x)
  -- where
  -- -- x = LFrac.fromPoly $ LPoly.variable
  -- x = LPoly.variable
  -- sine = Local.sineLocal
  -- cosine = Local.cosineLocal

-----------------------------------
-----------------------------------

sinesine_Name :: String
sinesine_Name = "sin(10x+sin(20x^2)) over [-1,1]"

sinesine_PB :: Accuracy -> ChPoly MPBall
sinesine_PB acGuide =
  sine2(10*x + sine1(20*x*x))
  where
  sine1 = sineWithAccuracyGuide (acGuide+2)
  sine2 = sineWithAccuracyGuide acGuide
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

sinesine_ModFun :: UnaryModFun
sinesine_ModFun =
  sin(10*x + sin(20*x*x))
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

sinesine_B2B :: UnaryBallFun
sinesine_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    sin(10*x + sin(20*x*x))

sinesineDeriv_B2B :: UnaryBallFun
sinesineDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (10+40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))

sinesine_PP :: Accuracy -> PPoly
sinesine_PP acGuide =
  error $ "Not (yet) supporting PPoly for: " ++ sinesine_Name
    -- PPolyBench.sinesineMax deg deg rangeAcc p

sinesine_FR :: Accuracy -> FracMB
sinesine_FR acGuide =
  error $ "Not (yet) supporting Frac for: " ++ sinesine_Name

sinesine_LP :: LPolyMB
sinesine_LP =
  sine(10*x + sine(20*x*x))
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

sinesine_LPP :: LPPolyMB
sinesine_LPP =
  error $ "Not (yet) supporting LPPoly for: " ++ sinesine_Name

sinesine_LF :: LFracMB
sinesine_LF =
  error $ "Not (yet) supporting LFrac for: " ++ sinesine_Name
  -- sine(10*x + sine(20*x*x))
  -- where
  -- x = LFrac.fromPoly $ LPoly.variable
  -- sine = Local.sineLocal

-----------------------------------
-----------------------------------

sinesineCos_Name :: String
sinesineCos_Name = "sin(10x+sin(20x^2)) + cos(10x) over [-1,1]"
-- sinesineCos_Name = "sin(10x+sin(20x^2)) + sin(10x) over [-1,1]"

sinesineCos_PB :: Accuracy -> ChPoly MPBall
sinesineCos_PB acGuide =
  sine2(10*x + sine1(20*x*x))
    + cosine2(10*x)
    -- + sine2(10*x)
  where
  sine1 = sineWithAccuracyGuide (acGuide+2)
  sine2 = sineWithAccuracyGuide acGuide
  cosine2 = cosineWithAccuracyGuide acGuide
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

sinesineCos_ModFun :: UnaryModFun
sinesineCos_ModFun =
  sin(10*x + sin(20*x*x)) + cos(10*x)
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

sinesineCos_B2B :: UnaryBallFun
sinesineCos_B2B =
    UnaryBallFun unaryIntervalDom $ \x ->
        sin(10*x + sin(20*x*x))
           + cos(10*x)
            -- + sin(10*x)

sinesineCosDeriv_B2B :: UnaryBallFun
sinesineCosDeriv_B2B =
    UnaryBallFun unaryIntervalDom $ \x ->
      (10+40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))
         - 10*sin(10*x)
          -- + 10*cos(10*x)

sinesineCos_PP :: Accuracy -> PPoly
sinesineCos_PP acGuide =
  error $ "Not (yet) supporting PPoly for: " ++ sinesineCos_Name

sinesineCos_FR :: Accuracy -> FracMB
sinesineCos_FR acGuide =
  error $ "Not (yet) supporting Frac for: " ++ sinesineCos_Name

sinesineCos_LP :: LPolyMB
sinesineCos_LP =
  sine(10*x + sine(20*x*x)) + cosine(10*x)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB
  cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

sinesineCos_LPP :: LPPolyMB
sinesineCos_LPP =
  error $ "Not (yet) supporting LPPoly for: " ++ sinesineCos_Name

sinesineCos_LF :: LFracMB
sinesineCos_LF =
  error $ "Not (yet) supporting LFrac for: " ++ sinesineCos_Name
  -- sine(10*x + sine(20*x*x)) + cosine(10*x)
  -- where
  -- x = LFrac.fromPoly $ LPoly.variable
  -- sine = Local.sineLocal
  -- cosine = Local.cosineLocal

-----------------------------------
-----------------------------------

sinesineSin_Name :: String
sinesineSin_Name = "sin(10x+sin(20x^2)) + sin(10x) over [-1,1]"

sinesineSin_PB :: Accuracy -> ChPoly MPBall
sinesineSin_PB acGuide =
  sine2(10*x + sine1(20*x*x))
    + sine2(10*x)
  where
  sine1 = sineWithAccuracyGuide (acGuide+2)
  sine2 = sineWithAccuracyGuide acGuide
  -- cosine2 = cosineWithAccuracyGuide acGuide
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

sinesineSin_ModFun :: UnaryModFun
sinesineSin_ModFun =
  sin(10*x + sin(20*x*x)) + sin(10*x)
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

sinesineSin_B2B :: UnaryBallFun
sinesineSin_B2B =
    UnaryBallFun unaryIntervalDom $ \x ->
        sin(10*x + sin(20*x*x))
            + sin(10*x)

sinesineSinDeriv_B2B :: UnaryBallFun
sinesineSinDeriv_B2B =
    UnaryBallFun unaryIntervalDom $ \x ->
      (10+40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))
          + 10*cos(10*x)

sinesineSin_PP :: Accuracy -> PPoly
sinesineSin_PP acGuide =
  error $ "Not (yet) supporting PPoly for: " ++ sinesineSin_Name

sinesineSin_FR :: Accuracy -> FracMB
sinesineSin_FR acGuide =
  error $ "Not (yet) supporting Frac for: " ++ sinesineSin_Name

sinesineSin_LP :: LPolyMB
sinesineSin_LP =
  sine(10*x + sine(20*x*x)) + sine(10*x)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

sinesineSin_LPP :: LPPolyMB
sinesineSin_LPP =
  error $ "Not (yet) supporting LPPoly for: " ++ sinesineSin_Name

sinesineSin_LF :: LFracMB
sinesineSin_LF =
  error $ "Not (yet) supporting LFrac for: " ++ sinesineSin_Name
  -- sine(10*x + sine(20*x*x)) + cosine(10*x)
  -- where
  -- x = LFrac.fromPoly $ LPoly.variable
  -- sine = Local.sineLocal
  -- cosine = Local.cosineLocal

-----------------------------------
-----------------------------------

runge_Name :: String
runge_Name = "1/(100x^2+1) over [-1,1]"

runge_PB :: Accuracy -> ChPoly MPBall
runge_PB acGuide =
  (~!) $ ChPoly.chebDivideDCT acGuide (x-x+1) denom
  where
  denom = 100*(x*x)+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

runge_ModFun :: UnaryModFun
runge_ModFun =
  1/!(100*x*x+1)
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

runge_B2B :: UnaryBallFun
runge_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->    1/(100*x^2+1)

rungeDeriv_B2B :: UnaryBallFun
rungeDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (-200*x)/((100*x^2+1)^2)

runge_PP :: Accuracy -> PPoly
runge_PP acGuide =
  maybeTrace ("runge_PP: getAccuracy inv = " ++ show (getAccuracy inv)) $
  inv
  where
  inv = setPrc2 $ PPoly.inverse $ PPoly.fromPoly $ 100*x*x+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc2 :: (CanSetPrecision t) => t -> t
  setPrc2 = setPrecisionAtLeastAccuracy (10*acGuide)
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

runge_FR :: Accuracy -> FracMB
runge_FR acGuide =
  inv
  where
  inv = setPrec (1 /! (Frac.fromPoly $ 100*x*x+1))
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrec = setPrecisionAtLeastAccuracy (4*acGuide)

runge_LP :: LPolyMB
runge_LP =
  1/!(100*x*x+1)
  where
  x = LPoly.variable

runge_LPP :: LPPolyMB
runge_LPP =
  1/!(100*x*x+1)
  where
  x = LPPoly.fromPoly $ LPoly.variable

runge_LF :: LFracMB
runge_LF =
  1/!(100*x*x+1)
  where
  x = LFrac.fromPoly $ LPoly.variable

-----------------------------------
-----------------------------------

rungeX_Name :: String
rungeX_Name = "x/(100x^2+1) over [-1,1]"

rungeX_PB :: Accuracy -> ChPoly MPBall
rungeX_PB acGuide =
  (~!) $ ChPoly.chebDivideDCT acGuide x denom
  where
  denom = 100*(x*x)+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

rungeX_ModFun :: UnaryModFun
rungeX_ModFun =
  x/!(100*x*x+1)
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

rungeX_B2B :: UnaryBallFun
rungeX_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    x/(100*x^2+1)

rungeXDeriv_B2B :: UnaryBallFun
rungeXDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (1-100*x^2)/((100*x^2+1)^2)

rungeX_PP :: Accuracy -> PPoly
rungeX_PP acGuide =
  maybeTrace ("rungeX_PP: getAccuracy inv = " ++ show (getAccuracy inv)) $
  x * inv
  where
  inv = setPrc2 $ PPoly.inverse $ PPoly.fromPoly $ 100*x*x+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc2 :: (CanSetPrecision t) => t -> t
  setPrc2 = setPrecisionAtLeastAccuracy (10*acGuide)
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

rungeX_FR :: Accuracy -> FracMB
rungeX_FR acGuide =
  inv
  where
  inv = setPrec $ (Frac.fromPoly x) /! (Frac.fromPoly $ 100*x*x+1)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrec = setPrecisionAtLeastAccuracy (4*acGuide)

rungeX_LP :: LPolyMB
rungeX_LP =
  x/!(100*x*x+1)
  where
  x = LPoly.variable

rungeX_LPP :: LPPolyMB
rungeX_LPP =
  x/!(100*x*x+1)
  where
  x = LPPoly.fromPoly $ LPoly.variable

rungeX_LF :: LFracMB
rungeX_LF =
  x/!(100*x*x+1)
  where
  x = LFrac.fromPoly $ LPoly.variable

-----------------------------------
-----------------------------------

rungeSC_Name :: String
rungeSC_Name = "(sin(10x)+cos(20x))/(100x^2+1) over [-1,1]"

rungeSC_PB :: Accuracy -> ChPoly MPBall
rungeSC_PB acGuide =
  maybeTrace ("rungeSC_PB: getAccuracy num = " ++ show (getAccuracy num)) $
  maybeTrace ("rungeSC_PB: getPrecision num = " ++ show (getPrecision num)) $
  maybeTrace ("rungeSC_PB: getAccuracy denom = " ++ show (getAccuracy denom)) $
  maybeTrace ("rungeSC_PB: getPrecision denom = " ++ show (getPrecision denom)) $
  maybeTrace ("rungeSC_PB: getAccuracy res = " ++ show (getAccuracy res)) $
  maybeTrace ("rungeSC_PB: getPrecision res = " ++ show (getPrecision res)) $
  res
  where
  res = (~!) $ ChPoly.chebDivideDCT acGuide num denom
  num = sine (10*x) + cosine (20*x)
  denom = 100*(x*x)+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (16*acGuide+16)
  sine = sineWithAccuracyGuide acGuide
  cosine = cosineWithAccuracyGuide acGuide

rungeSC_ModFun :: UnaryModFun
rungeSC_ModFun =
  (sin (10*x) + cos(20*x))/!(100*x*x+1)
  where
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

rungeSC_B2B :: UnaryBallFun
rungeSC_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (sin (10*x) + cos(20*x))/(100*x^2+1)

rungeSCDeriv_B2B :: UnaryBallFun
rungeSCDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    ((10*cos(10*x) - 20*sin(20*x))
     /
     (100*x^2+1))
    -
    ((sin (10*x) + cos(20*x))*(200*x)
     /
     ((100*x^2+1)^2))

rungeSC_PP :: Accuracy -> PPoly
rungeSC_PP acGuide =
  maybeTrace ("rungeSC_PP: getAccuracy num = " ++ show (getAccuracy num)) $
  maybeTrace ("rungeSC_PP: getPrecision num = " ++ show (getPrecision num)) $
  maybeTrace ("rungeSC_PP: getAccuracy inv = " ++ show (getAccuracy inv)) $
  maybeTrace ("rungeSC_PP: getPrecision inv = " ++ show (getPrecision inv)) $
  num * inv
  where
  num = PPoly.fromPoly $ sine (10*x) + cosine (20*x)
  inv = setPrc2 $ PPoly.inverse $ PPoly.fromPoly $ 100*x*x+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc2 :: (CanSetPrecision t) => t -> t
  setPrc2 = setPrecisionAtLeastAccuracy (64*acGuide)
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (8*acGuide)
  sine = sineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)
  cosine = cosineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)

rungeSC_FR :: Accuracy -> FracMB
rungeSC_FR acGuide =
  maybeTrace ("rungeSC_FR: getAccuracy num = " ++ show (getAccuracy num)) $
  maybeTrace ("rungeSC_FR: getPrecision num = " ++ show (getPrecision num)) $
  maybeTrace ("rungeSC_FR: getAccuracy denom = " ++ show (getAccuracy denom)) $
  maybeTrace ("rungeSC_FR: getPrecision denom = " ++ show (getPrecision denom)) $
  res
  where
  res = (setPrc2 num) /! denom
  num = Frac.fromPoly $ sine (10*x) + cosine (20*x)
  denom = Frac.fromPoly $ 100*x*x+1
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  x = setPrc1 xPre
  sine = sineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)
  cosine = cosineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)
  -- sine = sineWithAccuracyGuide ((fromAccuracy acGuide + 1)*(acGuide) + 25)
  -- cosine = cosineWithAccuracyGuide ((fromAccuracy acGuide + 1)*(acGuide) + 25)
  setPrc2 :: (CanSetPrecision t) => t -> t
  setPrc2 = setPrecisionAtLeastAccuracy (64*acGuide)
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (8*acGuide)

rungeSC_LP :: LPolyMB
rungeSC_LP =
  (sine(10*x)+cosine(20*x))/!(100*x*x+1)
  where
    x = LPoly.variable
    sine = Local.sineLocal :: LPolyMB -> LPolyMB
    cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

rungeSC_LPP :: LPPolyMB
rungeSC_LPP =
  (LPPoly.fromPoly $ sine(10*x)+cosine(20*x))/!(LPPoly.fromPoly $ 100*x*x+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB
  cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

rungeSC_LF :: LFracMB
rungeSC_LF =
  (LFrac.fromPoly $ sine(10*x)+cosine(20*x))/!(LFrac.fromPoly $ 100*x*x+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB
  cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

-----------------------------------
-----------------------------------

fracSin_Name :: String
fracSin_Name = "1/(10(sin(7x))^2+1) over [-1,1]"

fracSin_PB :: Accuracy -> ChPoly MPBall
fracSin_PB acGuide =
  (~!) $ ChPoly.chebDivideDCT acGuide (x-x+1) denom
  where
  denom = (10*(sine7x*sine7x)+1)
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

fracSin_ModFun :: UnaryModFun
fracSin_ModFun =
  1/!(10*(sin7x*sin7x)+1)
  where
  sin7x = sin (7*x)
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

fracSin_B2B :: UnaryBallFun
fracSin_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    1/(10*(sin (7*x))^2+1)

fracSinDeriv_B2B :: UnaryBallFun
fracSinDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (-140*sin(7*x)*cos(7*x))/((10*(sin (7*x))^2+1)^2)

fracSin_PP :: Accuracy -> PPoly
fracSin_PP acGuide =
  maybeTrace ("fracSin_PP: getAccuracy sine7x = " ++ show (getAccuracy sine7x)) $
  maybeTrace ("fracSin_PP: getAccuracy inv = " ++ show (getAccuracy inv)) $
  inv
  where
  inv = setPrc2 $ PPoly.inverse $ PPoly.fromPoly $ (10*(sine7x*sine7x)+1)
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc2 :: (CanSetPrecision t) => t -> t
  setPrc2 = setPrecisionAtLeastAccuracy (10*acGuide)
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

fracSin_FR :: Accuracy -> FracMB
fracSin_FR acGuide =
  inv
  where
  inv = 1 /! (Frac.fromPoly $ (10*(sine7x*sine7x)+1))
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

fracSin_LP :: LPolyMB
fracSin_LP =
  1/!(10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

fracSin_LPP :: LPPolyMB
fracSin_LPP =
  1/!(LPPoly.fromPoly $ 10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

fracSin_LF :: LFracMB
fracSin_LF =
  1/!(LFrac.fromPoly $ 10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

-----------------------------------
-----------------------------------

fracSinX_Name :: String
fracSinX_Name = "x/(10(sin(7x))^2+1) over [-1,1]"

fracSinX_PB :: Accuracy -> ChPoly MPBall
fracSinX_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ fracSinX_Name
    -- let sx = setMaxDegree d2 $ sin (7*x) in x/(10*sx*sx+1)
    -- where
    -- x =
    --     setMaxDegree d1 $
    --     setPrecision p $
    --     projUnaryFnA (unaryIntervalDom

fracSinX_ModFun :: UnaryModFun
fracSinX_ModFun =
  x/!(10*(sin7x*sin7x)+1)
  where
  sin7x = sin (7*x)
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

fracSinX_B2B :: UnaryBallFun
fracSinX_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    x/(10*(sin (7*x))^2+1)

fracSinXDeriv_B2B :: UnaryBallFun
fracSinXDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (-140*sin(7*x)*cos(7*x)*x)/((10*(sin (7*x))^2+1)^2)
    +
    (1/(10*(sin (7*x))^2+1))
fracSinX_PP :: Accuracy -> PPoly
fracSinX_PP acGuide =
  maybeTrace ("fracSinX_PP: getAccuracy sine7x = " ++ show (getAccuracy sine7x)) $
  maybeTrace ("fracSinX_PP: getAccuracy inv = " ++ show (getAccuracy inv)) $
  x * inv
  where
  inv = setPrc2 $ PPoly.inverse $ PPoly.fromPoly $ (10*(sine7x*sine7x)+1)
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc2 :: (CanSetPrecision t) => t -> t
  setPrc2 = setPrecisionAtLeastAccuracy (10*acGuide)
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

fracSinX_FR :: Accuracy -> FracMB
fracSinX_FR acGuide =
  inv
  where
  inv = (Frac.fromPoly x) /! (Frac.fromPoly $ (10*(sine7x*sine7x)+1))
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

fracSinX_LP :: LPolyMB
fracSinX_LP =
  x/!(10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

fracSinX_LPP :: LPPolyMB
fracSinX_LPP =
  (LPPoly.fromPoly $ x)/!(LPPoly.fromPoly $ 10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

fracSinX_LF :: LFracMB
fracSinX_LF =
  (LFrac.fromPoly $ x)/!(LFrac.fromPoly $ 10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB

-----------------------------------
-----------------------------------

fracSinSC_Name :: String
fracSinSC_Name = "(sin(10x)+cos(20x))/(10(sin(7x))^2+1) over [-1,1]"

fracSinSC_PB :: Accuracy -> ChPoly MPBall
fracSinSC_PB acGuide =
  (~!) $ ChPoly.chebDivideDCT acGuide num denom
  where
  num = sine2(10*x) + cosine(20*x)
  denom = (10*(sine7x*sine7x)+1)
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)
  sine2 = sineWithAccuracyGuide (acGuide)
  cosine = cosineWithAccuracyGuide (acGuide)

fracSinSC_ModFun :: UnaryModFun
fracSinSC_ModFun =
  (sin(10*x)+cos(20*x))/!(10*(sin7x*sin7x)+1)
  where
  sin7x = sin (7*x)
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

fracSinSC_B2B :: UnaryBallFun
fracSinSC_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (sin(10*x)+cos(20*x))/(10*(sin (7*x))^2+1)

fracSinSCDeriv_B2B :: UnaryBallFun
fracSinSCDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    ((10*cos(10*x)-20*sin(20*x))
     /
     (10*(sin (7*x))^2+1))
    -
    ((sin(10*x)+cos(20*x))*(140*sin(7*x)*cos(7*x))
     /
     ((10*(sin (7*x))^2+1)^2))

fracSinSC_PP :: Accuracy -> PPoly
fracSinSC_PP acGuide =
  maybeTrace ("fracSinSC_PP: getAccuracy sine7x = " ++ show (getAccuracy sine7x)) $
  maybeTrace ("fracSinSC_PP: getAccuracy num = " ++ show (getAccuracy num)) $
  maybeTrace ("fracSinSC_PP: getAccuracy inv = " ++ show (getAccuracy inv)) $
  fracSinPP
  where
  fracSinPP = num * inv
  inv = PPoly.inverse denom
  num = PPoly.fromPoly $ sine2(10*x) + cosine(20*x)
  denom = PPoly.fromPoly $ (10*(sine7x*sine7x)+1)
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (2*acGuide + 10)
  sine2 = sineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)
  cosine = cosineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  -- x = setPrc1 xPre
  -- setPrc2 :: (CanSetPrecision t) => t -> t
  -- setPrc2 = setPrecisionAtLeastAccuracy (10*acGuide)
  -- setPrc1 :: (CanSetPrecision t) => t -> t
  -- setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

fracSinSC_FR :: Accuracy -> FracMB
fracSinSC_FR acGuide =
  maybeTrace ("fracSinSC_FR: getAccuracy sine7x = " ++ show (getAccuracy sine7x)) $
  maybeTrace ("fracSinSC_FR: getAccuracy num = " ++ show (getAccuracy num)) $
  maybeTrace ("fracSinSC_FR: getAccuracy denom = " ++ show (getAccuracy denom)) $
  maybeTrace ("fracSinSC_FR: getAccuracy fracSinSC = " ++ show (getAccuracy fracSinSC)) $
  maybeTrace ("fracSinSC_FR: getPrecision fracSinSC = " ++ show (getPrecision fracSinSC)) $
  fracSinSC
  where
  fracSinSC = num /! denom
  num = Frac.fromPoly $ sine2(10*x) + cosine(20*x)
  denom = Frac.fromPoly $ (10*(sine7x*sine7x)+1)
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (2*acGuide + 10)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  sine2 = sineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)
  cosine = cosineWithAccuracyGuide ((fromAccuracy acGuide `div` 4 + 1)*(acGuide) + 25)

fracSinSC_LP :: LPolyMB
fracSinSC_LP =
  (sine(10*x)+cosine(20*x))/!(10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB
  cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

fracSinSC_LPP :: LPPolyMB
fracSinSC_LPP =
  (LPPoly.fromPoly $ sine(10*x)+cosine(20*x))/!(LPPoly.fromPoly $ 10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB
  cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

fracSinSC_LF :: LFracMB
fracSinSC_LF =
  (LFrac.fromPoly $ sine(10*x)+cosine(20*x))/!(LFrac.fromPoly $ 10*(sine (7*x))*(sine (7*x))+1)
  where
  x = LPoly.variable
  sine = Local.sineLocal :: LPolyMB -> LPolyMB
  cosine = Local.cosineLocal :: LPolyMB -> LPolyMB

-----------------------------------
-----------------------------------

hat_Name :: String
hat_Name = "1-|x+1/3| over [-1,1]"

hat_PB :: Accuracy -> ChPoly MPBall
hat_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ hat_Name
  -- 1 - (PolyBall (absXshifted p d) (Interval (-1.0) (1.0)) d NormZero)

hat_B2B :: UnaryBallFun
hat_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    cn $ 1 - (abs (x+1/3))

hatDeriv_B2B :: UnaryBallFun
hatDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \xCN ->
    cn $
      case ((~!) xCN) > -1/!3 of
         Just True -> mpBall 1
         Just False -> mpBall (-1)
         _ -> fromEndpoints (mpBall $ -1) (mpBall 1)

hat_PP :: Accuracy -> PPoly
hat_PP acGuide =
  error $ "Not (yet) supporting PPoly for: " ++ hat_Name

hat_FR :: Accuracy -> FracMB
hat_FR acGuide =
  error $ "Not (yet) supporting Frac for: " ++ hat_Name

bumpy_Name :: String
bumpy_Name = "max(sin(10x),cos(11x)) over [-1,1]"

bumpy_PB :: Accuracy -> ChPoly MPBall
bumpy_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ bumpy_Name
    -- PolyBall (maxViaAbs sin10x cos11x) (Interval (-1.0) (1.0)) d1 NormZero
    -- where
    -- maxViaAbs f g = ((absViaCompose (f - g)) + f + g)/2
    -- absViaCompose f =
    --     (absX p d' (unaryIntervalDom) `comp` (reduceDegreeAndSweep d' NormZero f)
    -- d' = toIntegerUp $ sqrt (mpBall d1)
    -- comp = compose d1 NormZero
    -- ChPoly sin10x _ _ _ = sin (10*x)
    -- ChPoly cos11x _ _ _ = cos (11*x)
    -- x :: PolyBall
    -- x =
    --     setMaxDegree d1 $
    --     setPrecision p $
    --     projUnaryFnA (unaryIntervalDom

bumpy_B2B :: UnaryBallFun
bumpy_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    cn $ max (sin (10*x)) (cos (11*x))

bumpyDeriv_B2B :: UnaryBallFun
bumpyDeriv_B2B =
  error $ "DFun currently not supported for " ++ bumpy_Name

bumpy_PP :: Accuracy -> PPoly
bumpy_PP acGuide =
  error $ "Not yet supporting PPoly for: " ++ bumpy_Name

bumpy_FR :: Accuracy -> FracMB
bumpy_FR acGuide =
  error $ "Not yet supporting Frac for: " ++ bumpy_Name


unaryIntervalDom :: DyadicInterval
unaryIntervalDom = dyadicInterval (-1,1)
