module Main where

import AERN2.Num

import qualified Data.Map as Map

import System.Environment

import AERN2.RealFunction
import AERN2.Net
import FnReps.Polynomial.UnaryCheb.PolyBall
import FnReps.Polynomial.UnaryCheb.Poly (compose,absX,absXshifted, reduceDegreeAndSweep)
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Benchmarks
--import FnReps.Polynomial.UnaryCheb.RealFn

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, result) <- processArgs args 
    putStrLn $ computationDescription
    putStrLn $ "result = " ++ show result
    putStrLn $ "result ~ " ++ showB result
    putStrLn $ "accuracy: " ++ (show $ getAccuracy result)
    putStrLn $ "precision = " ++ show (getPrecision result)
    where
    showB = show . getApproximate (bits 30)
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
            ("fun", "integrate") -> integrateFun fnB2B accuracy 
            ("dfun", "integrate") -> integrateDFun fnB2B dfnB2B accuracy 
            ("poly", "max") -> maxPB $ fnPB p maxDeg 
            ("poly", "integrate") -> integratePB $ fnPB p maxDeg
            ("ppoly", "max") -> fnPP OpMax pp_prec pp_maxDeg pp_divThreshold pp_divIts pp_rangeAcc
            ("ppoly", "integrate") -> fnPP OpIntegrate pp_prec pp_maxDeg pp_divThreshold pp_divIts pp_rangeAcc
            _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    (Just (fnDescription, fnPB, fnB2B, dfnB2B, fnPP)) = Map.lookup functionCode functions
    maxDeg = read maxDegS
    p = prec $ read precS
    rangeBits = bits $ read rangeBitsS
    [precS, maxDegS, rangeBitsS] = effortArgs
    accuracy = bits $ read accuracyS
    [accuracyS] = effortArgs
    pp_prec = prec $ read pp_precS
    pp_maxDeg = read pp_maxDegS
    pp_divThreshold = bits $ read pp_divThresholdS
    pp_divIts = read pp_divItsS
    pp_rangeAcc = bits $ read pp_rangeBitsS
    [pp_precS, pp_maxDegS, pp_divThresholdS, pp_divItsS, pp_rangeBitsS] = effortArgs

    integratePB :: PolyBall -> MPBall
    integratePB b =
        integrateUnaryFnA (b, mpBall domL, mpBall domR) 
        where
        Interval domL domR = ball_domain b
    maxPB :: PolyBall -> MPBall
    maxPB b =
        m
        where
--        Interval _ m = rangeOnIntervalUnaryFnA (b, domain) 
        Interval _ m = rangePB rangeBits (b, domain) 
        domain = ball_domain b

    maxFun :: UnaryFnMPBall -> Accuracy -> MPBall
    maxFun fn ac =
        cauchyReal2ball m ac
        where
        Interval _ m = rangeOnIntervalUnaryFnA (fn, domain) 
        domain = ufnB2B_dom fn

    integrateFun :: UnaryFnMPBall -> Accuracy -> MPBall
    integrateFun fn ac =
        cauchyReal2ball r ac
        where
        r = integrateUnaryFnA (fn, cauchyReal domL, cauchyReal domR) 
        Interval domL domR = ufnB2B_dom fn

integrateDFun :: UnaryFnMPBall -> UnaryFnMPBall -> Accuracy -> MPBall
integrateDFun fn@(UnaryFnMPBall _dom f) _dfn@(UnaryFnMPBall _ df) acG =
    withAccuracy lG rG acG
    where
    Interval lG rG = ufnB2B_dom fn
    withAccuracy l r ac =
        ifExceptionDie "integrateUnaryFnA for an UnaryFnMPBall" $
            integr l r ac 
    integr l r ac 
        | getAccuracy value >= ac =
            value 
        | otherwise = 
            (integr l m (ac+1))
            +
            (integr m r (ac+1))
        where
        m = (l+r)/2
        mB = z + m
        value = (f mB)*(r-l)+errB
        errB = ((deriv - deriv)/2)*(((r-l)/2)^2)/2
        deriv = df lr
        lr = endpoints2Ball lB rB
        lB = z + l
        rB = z + r
        z = setPrecisionMatchAccuracy (ac + 100) $ mpBall 0
    

functions :: Map.Map String (String, Precision -> Degree -> PolyBall, UnaryFnMPBall, UnaryFnMPBall, FnPP)
functions =
    Map.fromList
    [
        ("sinesine", (sinenested_Name, sinenested_PB, sinenested_B2B, sinenestedDeriv_B2B, sinenested_PP)),
        ("sinesinecos", (sinenestedcos_Name, sinenestedcos_PB, sinenestedcos_B2B, sinenestedcosDeriv_B2B, sinenestedcos_PP)),
        ("sine+cos", (sinecos_Name, sinecos_PB, sinecos_B2B, sinecosDeriv_B2B, sinecos_PP)),
        ("runge", (runge_Name, runge_PB, runge_B2B, rungeDeriv_B2B, runge_PP)),
        ("xrunge", (xrunge_Name, xrunge_PB, xrunge_B2B, xrungeDeriv_B2B, xrunge_PP)),
        ("runge-periodic", (rungePeriodic_Name, rungePeriodic_PB, rungePeriodic_B2B, rungePeriodicDeriv_B2B, rungePeriodic_PP)),
        ("abs", (abs13_Name, abs13_PB, abs13_B2B, abs13Deriv_B2B, abs13_PP)),
        ("bumpy", (bumpy_Name, bumpy_PB, bumpy_B2B, bumpyDeriv_B2B, bumpy_PP))
    ]

data Operator = OpMax | OpIntegrate
type FnPP = Operator -> Precision -> Degree -> Accuracy -> Integer -> Accuracy -> MPBall

sinenested_Name :: String
sinenested_Name = "sin(10x+sin(20x^2)) over [-1,1]"

sinenested_PB :: Precision -> Degree -> PolyBall
sinenested_PB p d =
    sin(10*x + sin(20*x*x))
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

sinenested_B2B :: UnaryFnMPBall
sinenested_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ sin(10*x + sin(20*x*x))

sinenestedDeriv_B2B :: UnaryFnMPBall
sinenestedDeriv_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ (10-40*x*cos(20*x*x))*cos(10*x + sin(20*x*x))

sinenested_PP :: FnPP
sinenested_PP = error $ "Not supporting PPoly for: " ++ sinenested_Name

sinenestedcos_Name :: String
sinenestedcos_Name = "sin(10x+sin(20x^2)) + cos(10x) over [-1,1]"

sinenestedcos_PB :: Precision -> Degree -> PolyBall
sinenestedcos_PB p d =
    sin(10*x + sin(20*x*x)) + cos(10*x)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

sinenestedcos_B2B :: UnaryFnMPBall
sinenestedcos_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ sin(10*x + sin(20*x*x)) + cos(10*x)

sinenestedcosDeriv_B2B :: UnaryFnMPBall
sinenestedcosDeriv_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ (10-40*x*cos(20*x*x))*cos(10*x + sin(20*x*x)) - 10*sin(10*x)

sinenestedcos_PP :: FnPP
sinenestedcos_PP = error $ "Not supporting PPoly for: " ++ sinenestedcos_Name

sinecos_Name :: String
sinecos_Name = "sin(10x)+cos(20x) over [-1,1]"

sinecos_PB :: Precision -> Degree -> PolyBall
sinecos_PB p d =
    sin(10*x)+cos(20*x)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

sinecos_B2B :: UnaryFnMPBall
sinecos_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ sin(10*x)+cos(20*x)

sinecosDeriv_B2B :: UnaryFnMPBall
sinecosDeriv_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ 10*cos(10*x)-20*sin(20*x)

sinecos_PP :: FnPP
sinecos_PP = error $ "Not supporting PPoly for: " ++ sinecos_Name

runge_Name :: String
runge_Name = "1/(100x^2+1) over [-1,1]"

runge_PB :: Precision -> Degree -> PolyBall
runge_PB p d =
    1/(100*x*x+1)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

runge_B2B :: UnaryFnMPBall
runge_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> 1/(catchingExceptions $ 100*x^2+1)

rungeDeriv_B2B :: UnaryFnMPBall
rungeDeriv_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> (catchingExceptions $ -200*x)/(catchingExceptions $ (100*x^2+1)^2)

runge_PP :: FnPP
runge_PP OpMax prec _deg divThresholdAcc divIterations rangeAcc =
    rungeMax (2.0 ^^ (- (fromAccuracy divThresholdAcc))) divIterations prec rangeAcc
runge_PP OpIntegrate prec _deg divThresholdAcc divIterations rangeAcc =
    rungeIntegral (2.0 ^^ (- (fromAccuracy divThresholdAcc))) divIterations prec rangeAcc

xrunge_Name :: String
xrunge_Name = "x/(100x^2+1) over [-1,1]"

xrunge_PB :: Precision -> Degree -> PolyBall
xrunge_PB p d =
    x/(100*x*x+1)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

xrunge_B2B :: UnaryFnMPBall
xrunge_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> (catchingExceptions x)/(catchingExceptions $ 100*x^2+1)

xrungeDeriv_B2B :: UnaryFnMPBall
xrungeDeriv_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> (catchingExceptions $ 1-100*x^2)/(catchingExceptions $ (100*x^2+1)^2)

xrunge_PP :: FnPP
xrunge_PP OpMax prec _deg divThresholdAcc divIterations rangeAcc =
    error "xrunge_PP OpMax not implemented yet" -- TODO
    -- rungeMax (2.0 ^^ (- (fromAccuracy divThresholdAcc))) divIterations prec rangeAcc
xrunge_PP OpIntegrate prec _deg divThresholdAcc divIterations rangeAcc =
    error "xrunge_PP OpIntegrate not implemented yet" -- TODO
--    rungeIntegral (2.0 ^^ (- (fromAccuracy divThresholdAcc))) divIterations prec rangeAcc

rungePeriodic_Name :: String
rungePeriodic_Name = "1/(10(sin(7x))^2+1) over [-1,1]"

rungePeriodic_PB :: Precision -> Degree -> PolyBall
rungePeriodic_PB p d =
    let sx = sin (7*x) in 1/(10*sx*sx+1)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

rungePeriodic_B2B :: UnaryFnMPBall
rungePeriodic_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> 1/(catchingExceptions $ 10*(sin (7*x))^2+1)

rungePeriodicDeriv_B2B :: UnaryFnMPBall
rungePeriodicDeriv_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> (catchingExceptions $ -140*sin(7*x)*cos(7*x))/(catchingExceptions $ (10*(sin (7*x))^2+1)^2)

rungePeriodic_PP :: FnPP
rungePeriodic_PP OpMax prec deg divThresholdAcc divIterations rangeAcc =
    fracSinMax deg (2.0 ^^ (- (fromAccuracy divThresholdAcc))) divIterations prec rangeAcc
rungePeriodic_PP OpIntegrate prec deg divThresholdAcc divIterations rangeAcc =
    fracSinIntegral deg (2.0 ^^ (- (fromAccuracy divThresholdAcc))) divIterations prec rangeAcc

abs13_Name :: String
abs13_Name = "1-|x+1/3| over [-1,1]"

abs13_PB :: Precision -> Degree -> PolyBall
abs13_PB p d =
    1 - (PolyBall (absXshifted p d) (Interval (-1.0) (1.0)) d NormZero)   

abs13_B2B :: UnaryFnMPBall
abs13_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ 1 - (abs (x+1/3))

abs13Deriv_B2B :: UnaryFnMPBall
abs13Deriv_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x ->
        case x > -1/3 of
             Just True -> catchingExceptions $ mpBall 1
             Just False -> catchingExceptions $ mpBall (-1)
             _ -> catchingExceptions $ endpoints2Ball (mpBall $ -1) (mpBall 1)

abs13_PP :: FnPP
abs13_PP = error $ "Not yet supporting PPoly for: " ++ abs13_Name

bumpy_Name :: String
bumpy_Name = "max(sin(10x),cos(11x)) over [-1,1]"

bumpy_PB :: Precision -> Degree -> PolyBall
bumpy_PB p d =
    PolyBall (maxViaAbs sin10x cos11x) (Interval (-1.0) (1.0)) d NormZero
    where
    maxViaAbs f g = ((absViaCompose (f - g)) + f + g)/2
    absViaCompose f = 
        (absX p d' (Interval (-1.0) 1.0)) `comp` (reduceDegreeAndSweep d' NormZero f)
    d' = toIntegerUp $ sqrt (mpBall d)
    comp = compose d NormZero
    PolyBall sin10x _ _ _ = sin (10*x)
    PolyBall cos11x _ _ _ = cos (11*x) 
    x :: PolyBall
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

bumpy_B2B :: UnaryFnMPBall
bumpy_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ max (sin (10*x)) (cos (11*x))

bumpyDeriv_B2B :: UnaryFnMPBall
bumpyDeriv_B2B =
    error $ "DFun currently not supported for " ++ bumpy_Name

bumpy_PP :: FnPP
bumpy_PP = error $ "Not yet supporting PPoly for: " ++ bumpy_Name
