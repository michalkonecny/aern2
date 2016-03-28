module Main where

import AERN2.Num

import qualified Data.Map as Map

import System.Environment

import AERN2.RealFunction
import AERN2.Net
import FnReps.Polynomial.UnaryCheb.PolyBall
--import FnReps.Polynomial.UnaryCheb.RealFn

main =
    do
    args <- getArgs
    (computationDescription, result) <- processArgs args 
    putStrLn $ computationDescription
    putStrLn $ "result = " ++ show result
    putStrLn $ "result ~ " ++ showB result
    putStrLn $ "accuracy: " ++ (show $ getAccuracy result)
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
            ("poly", "max") -> maxPB $ fnPB p maxDeg 
            ("poly", "integrate") -> integratePB $ fnPB p maxDeg 
    (Just (fnDescription, fnPB, fnB2B)) = Map.lookup functionCode functions
    maxDeg = read maxDegS
    p = prec $ read precS
    [precS, maxDegS] = effortArgs
    accuracy = bits $ read accuracyS
    [accuracyS] = effortArgs

    integratePB :: PolyBall -> MPBall
    integratePB b =
        integrateUnaryFnA (b, mpBall domL, mpBall domR) 
        where
        Interval domL domR = ball_domain b
    maxPB :: PolyBall -> MPBall
    maxPB b =
        m
        where
        Interval _ m = rangeOnIntervalUnaryFnA (b, domain) 
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

functions :: Map.Map String (String, Precision -> Degree -> PolyBall, UnaryFnMPBall)
functions =
    Map.fromList
    [
        ("sinesine", (analyticFn1_Name, analyticFn1_PB, analyticFn1_B2B))
    ]

analyticFn1_Name :: String
analyticFn1_Name = "sin(10x+sin(20x^2)) over [-1,1]"

analyticFn1_PB :: Precision -> Degree -> PolyBall
analyticFn1_PB p d =
    sin(10*x + sin(20*x*x))
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

analyticFn1_B2B :: UnaryFnMPBall
analyticFn1_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> sin(10*x + sin(20*x*x))

main2 :: IO ()
main2 =
    do
    {- A plot of the following enclosures of 1/(10x^2+1) is at: 
        http://fooplot.com/plot/2yt7q2sevi
    -}
    printWithName "1/(10x^2+1) degree 10" $ divT10 10
    printWithName "1/(10x^2+1) degree 14" $ divT10 14
    printWithName "1/(10x^2+1) degree 18" $ divT10 18
    
    printWithName "1/(100x^2+1) degree 30" $ divT100 30
    printWithName "1/(100x^2+1) degree 40" $ divT100 40
    printWithName "1/(100x^2+1) degree 50" $ divT100 50

    printWithName "∫_(-1)^(1) 1/(10x^2+1)dx degree 18" $ integrateUnaryFnA (divT10 18, mpBall (-1), mpBall 1)
    
    {- A plot of the following enclosure is at:
        http://fooplot.com/plot/d1gdgvg3sf -}
    printWithName "1/(1/(10x^2+1)) degree 18" $ 1 / (divT10 18)
    where
    divT10 d = divT d 10
    divT100 d = divT d 100
    divT d n = 1 / (setMaxDegree d ((n * x*x) + 1))
    x = projUnaryFnA (Interval (-1.0) 1.0) :: PolyBall
    
    printWithName name value =
        do
        putStrLn $ banner ++ "\n" ++ name ++ ":\n" ++ show value
        putStrLn $ "\nAPPROX:\n" ++ show (getApproximate (bits 30) value)
        where
        banner = replicate (int 100) '='

