{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
module Main where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import Numeric.MixedTypes
import Numeric.CatchingExceptions
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

type FracMB = Frac MPBall

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
        _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    (Just (fnDescription, fnPB, fnModFun, fnB2B, dfnB2B, fnPP, fnFR)) = Map.lookup functionCode functions

    accuracy = bits $ (read accuracyS :: Int)
    [accuracyS] = effortArgs

    maxPB :: ChPoly MPBall -> MPBall
    maxPB f = f `maximumOverDom` (getDomain f)

    integratePB :: ChPoly MPBall -> MPBall
    integratePB f =
      maybeTrace ("integratePB: accuracy f = " ++ show (getAccuracy f)) $
      f `integrateOverDom` (getDomain f)

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
        qaMakeQuery m ac
        where
        m = fn `maximumOverDom` getDomain fn

    maxModFun :: UnaryModFun -> Accuracy -> MPBall
    maxModFun fn ac =
        qaMakeQuery m ac
        where
        m = fn `maximumOverDom` getDomain fn

    maxDBallFun :: UnaryBallFun -> UnaryBallFun -> Accuracy -> MPBall
    maxDBallFun f f' ac =
        qaMakeQuery m ac
        where
        m = fn `maximumOverDom` getDomain f
        fn = UnaryBallDFun [f,f']

    integrateBallFun :: UnaryBallFun -> Accuracy -> MPBall
    integrateBallFun fn ac =
        qaMakeQuery r ac
        where
        r = fn `integrateOverDom` (getDomain fn)

    integrateModFun :: UnaryModFun -> Accuracy -> MPBall
    integrateModFun fn ac =
        qaMakeQuery r ac
        where
        r = fn `integrateOverDom` (getDomain fn)

    integrateDBallFun :: UnaryBallFun -> UnaryBallFun -> Accuracy -> MPBall
    integrateDBallFun f f' ac =
        qaMakeQuery r ac
        where
        r = fn `integrateOverDom` (getDomain f)
        dom = getDomain f
        fn = UnaryBallDFun [f,f']
processArgs _ =
    error "expecting arguments: <operationCode> <functionCode> <representationCode> <effort parameters...>"

functions :: Map.Map String (String, Accuracy -> ChPoly MPBall, UnaryModFun, UnaryBallFun, UnaryBallFun, Accuracy -> PPoly, Accuracy -> FracMB)
functions =
    Map.fromList
    [
      ("sine+cos", (sinecos_Name, sinecos_PB, sinecos_ModFun, sinecos_B2B, sinecosDeriv_B2B, sinecos_PP, sinecos_FR))
    , ("sinesine", (sinesine_Name, sinesine_PB, sinesine_ModFun, sinesine_B2B, sinesineDeriv_B2B, sinesine_PP, sinesine_FR))
    , ("sinesine+cos", (sinesineCos_Name, sinesineCos_PB, sinesineCos_ModFun, sinesineCos_B2B, sinesineCosDeriv_B2B, sinesineCos_PP, sinesineCos_FR))
    , ("runge", (runge_Name, runge_PB, runge_ModFun, runge_B2B, rungeDeriv_B2B, runge_PP, runge_FR))
    , ("rungeX", (rungeX_Name, rungeX_PB, rungeX_ModFun, rungeX_B2B, rungeXDeriv_B2B, rungeX_PP, rungeX_FR))
    , ("fracSin", (fracSin_Name, fracSin_PB, fracSin_ModFun, fracSin_B2B, fracSinDeriv_B2B, fracSin_PP, fracSin_FR))
    , ("fracSinCos", (fracSinCos_Name, fracSinCos_PB, fracSinCos_ModFun, fracSinCos_B2B, fracSinCosDeriv_B2B, fracSinCos_PP, fracSinCos_FR))
    , ("fracSinX", (fracSinX_Name, fracSinX_PB, fracSinX_ModFun, fracSinX_B2B, fracSinXDeriv_B2B, fracSinX_PP, fracSinX_FR))
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
    (10-40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))

sinesine_PP :: Accuracy -> PPoly
sinesine_PP acGuide =
  error $ "Not (yet) supporting PPoly for: " ++ sinesine_Name
    -- PPolyBench.sinesineMax deg deg rangeAcc p

sinesine_FR :: Accuracy -> FracMB
sinesine_FR acGuide =
  error $ "Not (yet) supporting Frac for: " ++ sinesine_Name

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
      (10-40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))
         - 10*sin(10*x)
          -- + 10*cos(10*x)

sinesineCos_PP :: Accuracy -> PPoly
sinesineCos_PP acGuide =
  error $ "Not (yet) supporting PPoly for: " ++ sinesineCos_Name

sinesineCos_FR :: Accuracy -> FracMB
sinesineCos_FR acGuide =
  error $ "Not (yet) supporting Frac for: " ++ sinesineCos_Name

runge_Name :: String
runge_Name = "1/(100x^2+1) over [-1,1]"

runge_PB :: Accuracy -> ChPoly MPBall
runge_PB acGuide =
  ChPoly.chebDivideDCT acGuide (x-x+1) denom
  where
  denom = 100*(x*x)+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

runge_ModFun :: UnaryModFun
runge_ModFun =
  1/(100*x*x+1)
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
  inv = 1 / (Frac.fromPoly $ 100*x*x+1)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

rungeX_Name :: String
rungeX_Name = "x/(100x^2+1) over [-1,1]"

rungeX_PB :: Accuracy -> ChPoly MPBall
rungeX_PB acGuide =
  ChPoly.chebDivideDCT acGuide x denom
  where
  denom = 100*(x*x)+1
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

rungeX_ModFun :: UnaryModFun
rungeX_ModFun =
  x/(100*x*x+1)
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
  inv = (Frac.fromPoly x) / (Frac.fromPoly $ 100*x*x+1)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

fracSin_Name :: String
fracSin_Name = "1/(10(sin(7x))^2+1) over [-1,1]"

fracSin_PB :: Accuracy -> ChPoly MPBall
fracSin_PB acGuide =
  ChPoly.chebDivideDCT acGuide (x-x+1) denom
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
  1/(10*(sin7x*sin7x)+1)
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
  inv = 1 / (Frac.fromPoly $ (10*(sine7x*sine7x)+1))
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

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
  x/(10*(sin7x*sin7x)+1)
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
  inv = (Frac.fromPoly x) / (Frac.fromPoly $ (10*(sine7x*sine7x)+1))
  sine7x = sine1 (7*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()


fracSinCos_Name :: String
fracSinCos_Name = "1/(10(sin(7x)+cos(5x))^2+1) over [-1,1]"

fracSinCos_PB :: Accuracy -> ChPoly MPBall
fracSinCos_PB acGuide =
  ChPoly.chebDivideDCT acGuide (x-x+1) denom
  where
  denom = (10*(sinecos*sinecos)+1)
  sinecos = sine7x + cos5x
  sine7x = sine1 (7*x)
  cos5x = cos1 (5*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  cos1 = cosineWithAccuracyGuide (acGuide + 10)
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

fracSinCos_ModFun :: UnaryModFun
fracSinCos_ModFun =
  1/(10*(sincos*sincos)+1)
  where
  sincos = sin(7*x) + cos(5*x)
  x = varFn (unaryModFun (unaryIntervalDom, 0)) ()

fracSinCos_B2B :: UnaryBallFun
fracSinCos_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    1/(10*(sin (7*x) + cos(5*x))^2+1)

fracSinCosDeriv_B2B :: UnaryBallFun
fracSinCosDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    (-20*(sin(7*x)+cos(5*x))*(7*cos(7*x)-5*sin(5*x)))
    /((10*(sin (7*x)+cos(5*x))^2+1)^2)

fracSinCos_PP :: Accuracy -> PPoly
fracSinCos_PP acGuide =
  maybeTrace ("fracSin_PP: getAccuracy sine7x = " ++ show (getAccuracy sine7x)) $
  maybeTrace ("fracSin_PP: getAccuracy inv = " ++ show (getAccuracy inv)) $
  inv
  where
  inv = setPrc2 $ PPoly.inverse $ PPoly.fromPoly $ (10*(sinecos*sinecos)+1)
  sinecos = sine7x + cos5x
  sine7x = sine1 (7*x)
  cos5x = cos1 (5*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  cos1 = cosineWithAccuracyGuide (acGuide + 10)
  x = setPrc1 xPre
  xPre = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()
  setPrc2 :: (CanSetPrecision t) => t -> t
  setPrc2 = setPrecisionAtLeastAccuracy (10*acGuide)
  setPrc1 :: (CanSetPrecision t) => t -> t
  setPrc1 = setPrecisionAtLeastAccuracy (3*acGuide)

fracSinCos_FR :: Accuracy -> FracMB
fracSinCos_FR acGuide =
  inv
  where
  inv = 1 / (Frac.fromPoly $ (10*(sinecos*sinecos)+1))
  sinecos = sine7x + cos5x
  sine7x = sine1 (7*x)
  cos5x = cos1 (5*x)
  sine1 = sineWithAccuracyGuide (acGuide + 10)
  cos1 = cosineWithAccuracyGuide (acGuide + 10)
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()



hat_Name :: String
hat_Name = "1-|x+1/3| over [-1,1]"

hat_PB :: Accuracy -> ChPoly MPBall
hat_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ hat_Name
  -- 1 - (PolyBall (absXshifted p d) (Interval (-1.0) (1.0)) d NormZero)

hat_B2B :: UnaryBallFun
hat_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    1 - (abs (x+1/3))

hatDeriv_B2B :: UnaryBallFun
hatDeriv_B2B =
  UnaryBallFun unaryIntervalDom $ \x ->
    catchingNumExceptions $
      case x > -1/3 of
         Just (Just True) -> mpBall 1
         Just (Just False) -> mpBall (-1)
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
    max (sin (10*x)) (cos (11*x))

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
