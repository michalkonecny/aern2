{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
module Main where

import Numeric.MixedTypes
import Numeric.CatchingExceptions
-- import qualified Prelude as P
-- import Text.Printf

import qualified Data.Map as Map

-- import Control.Applicative (liftA2)

import System.Environment

import AERN2.Norm
import AERN2.MP.Accuracy
import AERN2.MP.Precision
import AERN2.MP.Ball (MPBall, mpBall, IsBall(..),IsInterval(..))
-- import qualified AERN2.MP.Ball as MPBall

import AERN2.Real

import AERN2.Interval

import AERN2.RealFun.Operations
import AERN2.RealFun.UnaryFun
import AERN2.RealFun.UnaryDFun
import AERN2.Poly.Cheb (ChPoly, chPolyMPBall)
import qualified AERN2.Poly.Cheb as ChPoly
import AERN2.Poly.Basics

-- import FnReps.Polynomial.UnaryCheb.Poly (compose,absX,absXshifted, reduceDegreeAndSweep)
-- import qualified FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Benchmarks as PPolyBench

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, result) <- processArgs args
    putStrLn $ computationDescription
    putStrLn $ "result = " ++ show result
    -- putStrLn $ "result ~ " ++ showB result
    putStrLn $ "accuracy: " ++ (show $ getAccuracy result)
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
            ("fun", "max") -> maxFun fnB2B accuracy
            ("dfun", "max") -> maxDFun fnB2B dfnB2B accuracy
            ("fun", "integrate") -> integrateFun fnB2B accuracy
            ("dfun", "integrate") -> integrateDFun fnB2B dfnB2B accuracy
            ("poly", "max") -> maxPB $ fnPB accuracy
            -- ("poly", "integrate") -> integratePB $ fnPB p maxDeg1 maxDeg2
            -- ("ppoly", "max") -> fnPP OpMax pp_prec pp_maxDeg pp_divThreshold pp_divIts pp_rangeAcc
            -- ("ppoly", "integrate") -> fnPP OpIntegrate pp_prec pp_maxDeg pp_divThreshold pp_divIts pp_rangeAcc
            _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    (Just (fnDescription, fnPB, fnB2B, dfnB2B, fnPP)) = Map.lookup functionCode functions

    maxDeg1, maxDeg2 :: Degree
    maxDeg1 = read maxDeg1S
    maxDeg2 = read maxDeg2S
    p = prec $ read precS
    [precS, maxDeg1S, maxDeg2S] = effortArgs

    accuracy = bits $ (read accuracyS :: Int)
    [accuracyS] = effortArgs

    pp_prec = prec $ read pp_precS
    pp_maxDeg :: Degree
    pp_maxDeg = read pp_maxDegS
    pp_divThreshold = toRational ((read pp_divThresholdS) :: Double)
    pp_divIts :: Integer
    pp_divIts = read pp_divItsS
    pp_rangeAcc = bits $ (read pp_rangeBitsS :: Int)
    [pp_precS, pp_maxDegS, pp_divThresholdS, pp_divItsS, pp_rangeBitsS] = effortArgs

    -- integratePB :: (ChPoly MPBall) -> MPBall
    -- integratePB b =
    --     integrateUnaryFnA (b, mpBall domL, mpBall domR)
    --     where
    --     Interval domL domR = ball_domain b

    maxPB :: (ChPoly MPBall) -> MPBall
    maxPB f =
      updateRadius (+ (radius f)) resC
      where
      resC = ChPoly.maximumOptimised (setPrecision prc fC) lB rB 5 5
      fC = centreAsBall f
      (Interval l r) = getDomain f
      prc = (getPrecision f)
      lB = raisePrecisionIfBelow prc $ mpBall l
      rB = raisePrecisionIfBelow prc $ mpBall r

    maxFun :: UnaryFun -> Accuracy -> MPBall
    maxFun fn ac =
        qaMakeQuery m ac
        where
        Interval _ m = fn `apply` domain
        domain = getDomain fn

    maxDFun :: UnaryFun -> UnaryFun -> Accuracy -> MPBall
    maxDFun f f' ac =
        qaMakeQuery m ac
        where
        Interval _ m = fn `apply` domain
        domain = getDomain f
        fn = UnaryDFun [f,f']

    integrateFun :: UnaryFun -> Accuracy -> MPBall
    integrateFun fn ac =
        qaMakeQuery r ac
        where
        r = integrate fn dom
        dom = getDomain fn

    integrateDFun :: UnaryFun -> UnaryFun -> Accuracy -> MPBall
    integrateDFun f f' ac =
        qaMakeQuery r ac
        where
        r = integrate fn dom
        dom = getDomain f
        fn = UnaryDFun [f,f']
processArgs _ =
    error "expecting arguments: <operationCode> <functionCode> <representationCode> <effort parameters...>"

functions :: Map.Map String (String, Accuracy -> ChPoly MPBall, UnaryFun, UnaryFun, FnPP)
functions =
    Map.fromList
    [
        ("sine+cos", (sinecos_Name, sinecos_PB, sinecos_B2B, sinecosDeriv_B2B, sinecos_PP)),
        ("sinesine", (sinesine_Name, sinesine_PB, sinesine_B2B, sinesineDeriv_B2B, sinesine_PP)),
        ("sinesine+cos", (sinesineCos_Name, sinesineCos_PB, sinesineCos_B2B, sinesineCosDeriv_B2B, sinesineCos_PP)),
        ("runge", (runge_Name, runge_PB, runge_B2B, rungeDeriv_B2B, runge_PP)),
        ("rungeX", (rungeX_Name, rungeX_PB, rungeX_B2B, rungeXDeriv_B2B, rungeX_PP)),
        ("fracSin", (fracSin_Name, fracSin_PB, fracSin_B2B, fracSinDeriv_B2B, fracSin_PP)),
        ("fracSinX", (fracSinX_Name, fracSinX_PB, fracSinX_B2B, fracSinXDeriv_B2B, fracSinX_PP)),
        ("hat", (hat_Name, hat_PB, hat_B2B, hatDeriv_B2B, hat_PP)),
        ("bumpy", (bumpy_Name, bumpy_PB, bumpy_B2B, bumpyDeriv_B2B, bumpy_PP))
    ]

data Operator = OpMax | OpIntegrate
type FnPP = Operator -> Precision -> Degree -> Rational -> Integer -> Accuracy -> MPBall

sinecos_Name :: String
sinecos_Name = "sin(10x)+cos(20x) over [-1,1]"

sinecos_PB :: Accuracy -> ChPoly MPBall
sinecos_PB acGuide =
  sine(10*x)+cosine(20*x)
  where
  sine = ChPoly.sineWithAccuracyGuide acGuide
  cosine = ChPoly.cosineWithAccuracyGuide acGuide
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

sinecos_B2B :: UnaryFun
sinecos_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    sin(10*x)+cos(20*x)

sinecosDeriv_B2B :: UnaryFun
sinecosDeriv_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    10*cos(10*x)-20*sin(20*x)

sinecos_PP :: FnPP
sinecos_PP =
  error $ "Not (yet) supporting PPoly for: " ++ sinecos_Name

sinesine_Name :: String
sinesine_Name = "sin(10x+sin(20x^2)) over [-1,1]"

sinesine_PB :: Accuracy -> ChPoly MPBall
sinesine_PB acGuide =
  sine2(10*x + sine1(20*x*x))
  where
  sine1 = ChPoly.sineWithAccuracyGuide (acGuide+2)
  sine2 = ChPoly.sineWithAccuracyGuide acGuide
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

sinesine_B2B :: UnaryFun
sinesine_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    sin(10*x + sin(20*x*x))

sinesineDeriv_B2B :: UnaryFun
sinesineDeriv_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    (10-40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))

sinesine_PP :: FnPP
sinesine_PP OpMax p deg _divThresholdAcc _divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ sinesine_Name
    -- PPolyBench.sinesineMax deg deg rangeAcc p
sinesine_PP OpIntegrate p deg _divThresholdAcc _divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ sinesine_Name
    -- PPolyBench.sinesineIntegral deg deg rangeAcc p

sinesineCos_Name :: String
sinesineCos_Name = "sin(10x+sin(20x^2)) + cos(10x) over [-1,1]"
-- sinesineCos_Name = "sin(10x+sin(20x^2)) + sin(10x) over [-1,1]"

sinesineCos_PB :: Accuracy -> ChPoly MPBall
sinesineCos_PB acGuide =
  sine2(10*x + sine1(20*x*x))
    + cosine2(10*x)
    -- + sine2(10*x)
  where
  sine1 = ChPoly.sineWithAccuracyGuide (acGuide+2)
  sine2 = ChPoly.sineWithAccuracyGuide acGuide
  cosine2 = ChPoly.cosineWithAccuracyGuide acGuide
  x = varFn (chPolyMPBall (unaryIntervalDom, 0)) ()

sinesineCos_B2B :: UnaryFun
sinesineCos_B2B =
    UnaryFun unaryIntervalDom $ \x ->
        sin(10*x + sin(20*x*x))
           + cos(10*x)
            -- + sin(10*x)

sinesineCosDeriv_B2B :: UnaryFun
sinesineCosDeriv_B2B =
    UnaryFun unaryIntervalDom $ \x ->
      (10-40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))
         - 10*sin(10*x)
          -- + 10*cos(10*x)

sinesineCos_PP :: FnPP
sinesineCos_PP OpMax p deg _divThresholdAcc _divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ sinesineCos_Name
  -- PPolyBench.sinesineCosMax deg deg rangeAcc p
sinesineCos_PP OpIntegrate p deg _divThresholdAcc _divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ sinesineCos_Name
  -- PPolyBench.sinesineCosIntegral deg deg rangeAcc p

runge_Name :: String
runge_Name = "1/(100x^2+1) over [-1,1]"

runge_PB :: Accuracy -> ChPoly MPBall
runge_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ runge_Name
    -- 1/(100*x*x+1)
    -- where
    -- x =
    --     setMaxDegree d1 $
    --     setPrecision p $
    --     projUnaryFnA (unaryIntervalDom

runge_B2B :: UnaryFun
runge_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    1/(100*x^2+1)

rungeDeriv_B2B :: UnaryFun
rungeDeriv_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    (-200*x)/((100*x^2+1)^2)

runge_PP :: FnPP
runge_PP OpMax p _deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ runge_Name
    -- PPolyBench.rungeMax divThresholdAcc divIterations p rangeAcc
runge_PP OpIntegrate p _deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ runge_Name
    -- PPolyBench.rungeIntegral divThresholdAcc divIterations p rangeAcc

rungeX_Name :: String
rungeX_Name = "x/(100x^2+1) over [-1,1]"

rungeX_PB :: Accuracy -> ChPoly MPBall
rungeX_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ rungeX_Name
    -- x/(100*x*x+1)
    -- where
    -- x =
    --     setMaxDegree d1 $
    --     setPrecision p $
    --     projUnaryFnA (unaryIntervalDom

rungeX_B2B :: UnaryFun
rungeX_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    x/(100*x^2+1)

rungeXDeriv_B2B :: UnaryFun
rungeXDeriv_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    (1-100*x^2)/((100*x^2+1)^2)

rungeX_PP :: FnPP
rungeX_PP OpMax p _deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ rungeX_Name
  -- PPolyBench.rungeXMax divThresholdAcc divIterations p rangeAcc
rungeX_PP OpIntegrate p _deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ rungeX_Name
  -- PPolyBench.rungeXIntegral divThresholdAcc divIterations p rangeAcc

fracSin_Name :: String
fracSin_Name = "1/(10(sin(7x))^2+1) over [-1,1]"

fracSin_PB :: Accuracy -> ChPoly MPBall
fracSin_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ fracSin_Name
    -- let sx = setMaxDegree d2 $ sin (7*x) in 1/(10*sx*sx+1)
    -- where
    -- x =
    --     setMaxDegree d1 $
    --     setPrecision p $
    --     projUnaryFnA (unaryIntervalDom

fracSin_B2B :: UnaryFun
fracSin_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    1/(10*(sin (7*x))^2+1)

fracSinDeriv_B2B :: UnaryFun
fracSinDeriv_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    (-140*sin(7*x)*cos(7*x))/((10*(sin (7*x))^2+1)^2)

fracSin_PP :: FnPP
fracSin_PP OpMax p deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ fracSin_Name
  -- PPolyBench.fracSinMax deg divThresholdAcc divIterations p rangeAcc
fracSin_PP OpIntegrate p deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ fracSin_Name
  -- PPolyBench.fracSinIntegral deg divThresholdAcc divIterations p rangeAcc

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

fracSinX_B2B :: UnaryFun
fracSinX_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    x/(10*(sin (7*x))^2+1)

fracSinXDeriv_B2B :: UnaryFun
fracSinXDeriv_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    (-140*sin(7*x)*cos(7*x)*x)/((10*(sin (7*x))^2+1)^2)
    +
    (1/(10*(sin (7*x))^2+1))
fracSinX_PP :: FnPP
fracSinX_PP OpMax p deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ fracSinX_Name
  -- PPolyBench.fracSinXMax deg divThresholdAcc divIterations p rangeAcc
fracSinX_PP OpIntegrate p deg divThresholdAcc divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ fracSinX_Name
--    PPolyBench.fracSinXIntegral deg divThresholdAcc divIterations p rangeAcc

hat_Name :: String
hat_Name = "1-|x+1/3| over [-1,1]"

hat_PB :: Accuracy -> ChPoly MPBall
hat_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ hat_Name
  -- 1 - (ChPoly MPBall (absXshifted p d) (Interval (-1.0) (1.0)) d NormZero)

hat_B2B :: UnaryFun
hat_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    1 - (abs (x+1/3))

hatDeriv_B2B :: UnaryFun
hatDeriv_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    catchingNumExceptions $
      case x > -1/3 of
         Just (Just True) -> mpBall 1
         Just (Just False) -> mpBall (-1)
         _ -> fromEndpoints (mpBall $ -1) (mpBall 1)

hat_PP :: FnPP
hat_PP OpMax p _deg _divThresholdAcc _divIterations rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ hat_Name
  -- PPolyBench.hatMax p rangeAcc
hat_PP OpIntegrate p _deg _divThresholdAcc _divIterations _rangeAcc =
  error $ "Not (yet) supporting PPoly for: " ++ hat_Name
  -- PPolyBench.hatIntegral p

bumpy_Name :: String
bumpy_Name = "max(sin(10x),cos(11x)) over [-1,1]"

bumpy_PB :: Accuracy -> ChPoly MPBall
bumpy_PB acGuide =
  error $ "Not (yet) supporting Poly for: " ++ bumpy_Name
    -- ChPoly MPBall (maxViaAbs sin10x cos11x) (Interval (-1.0) (1.0)) d1 NormZero
    -- where
    -- maxViaAbs f g = ((absViaCompose (f - g)) + f + g)/2
    -- absViaCompose f =
    --     (absX p d' (unaryIntervalDom) `comp` (reduceDegreeAndSweep d' NormZero f)
    -- d' = toIntegerUp $ sqrt (mpBall d1)
    -- comp = compose d1 NormZero
    -- ChPoly sin10x _ _ _ = sin (10*x)
    -- ChPoly cos11x _ _ _ = cos (11*x)
    -- x :: ChPoly MPBall
    -- x =
    --     setMaxDegree d1 $
    --     setPrecision p $
    --     projUnaryFnA (unaryIntervalDom

bumpy_B2B :: UnaryFun
bumpy_B2B =
  UnaryFun unaryIntervalDom $ \x ->
    max (sin (10*x)) (cos (11*x))

bumpyDeriv_B2B :: UnaryFun
bumpyDeriv_B2B =
  error $ "DFun currently not supported for " ++ bumpy_Name

bumpy_PP :: FnPP
bumpy_PP =
  error $ "Not yet supporting PPoly for: " ++ bumpy_Name

unaryIntervalDom :: DyadicInterval
unaryIntervalDom = dyadicInterval (-1,1)
