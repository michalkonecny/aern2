module Main where

import AERN2.Num

import qualified Data.Map as Map

import System.Environment

import AERN2.RealFunction
import AERN2.Net
import FnReps.Polynomial.UnaryCheb.PolyBall
import FnReps.Polynomial.UnaryCheb.Poly (absXshifted)
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
            ("poly", "max") -> maxPB $ fnPB p maxDeg 
            ("poly", "integrate") -> integratePB $ fnPB p maxDeg
            _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    (Just (fnDescription, fnPB, fnB2B)) = Map.lookup functionCode functions
    maxDeg = read maxDegS
    p = prec $ read precS
    rangeBits = bits $ read rangeBitsS
    [precS, maxDegS, rangeBitsS] = effortArgs
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

functions :: Map.Map String (String, Precision -> Degree -> PolyBall, UnaryFnMPBall)
functions =
    Map.fromList
    [
        ("sinesine", (analyticFn1_Name, analyticFn1_PB, analyticFn1_B2B)),
        ("sine+cos", (analyticFn2_Name, analyticFn2_PB, analyticFn2_B2B)),
        ("fraction", (nearsingulatityFn1_Name, nearsingulatityFn1_PB, nearsingulatityFn1_B2B)),
        ("fraction-periodic", (nearsingulatityPerFn1_Name, nearsingulatityPerFn1_PB, nearsingulatityPerFn1_B2B)),
        ("abs", (nonsmoothFn1_Name, nonsmoothFn1_PB, nonsmoothFn1_B2B))
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
    \x -> catchingExceptions $ sin(10*x + sin(20*x*x))

analyticFn2_Name :: String
analyticFn2_Name = "sin(10x)+cos(20x) over [-1,1]"

analyticFn2_PB :: Precision -> Degree -> PolyBall
analyticFn2_PB p d =
    sin(10*x)+cos(20*x)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

analyticFn2_B2B :: UnaryFnMPBall
analyticFn2_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ sin(10*x)+cos(20*x)

nearsingulatityFn1_Name :: String
nearsingulatityFn1_Name = "1/(100x^2+1) over [-1,1]"

nearsingulatityFn1_PB :: Precision -> Degree -> PolyBall
nearsingulatityFn1_PB p d =
    1/(100*x*x+1)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

nearsingulatityFn1_B2B :: UnaryFnMPBall
nearsingulatityFn1_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> 1/(catchingExceptions $ 100*x^2+1)

nearsingulatityPerFn1_Name :: String
nearsingulatityPerFn1_Name = "1/(10(sin(7x))^2+1) over [-1,1]"

nearsingulatityPerFn1_PB :: Precision -> Degree -> PolyBall
nearsingulatityPerFn1_PB p d =
    let sx = sin (7*x) in 1/(10*sx*sx+1)
    where
    x = 
        setMaxDegree d $
        setPrecision p $
        projUnaryFnA (Interval (-1.0) 1.0)

nearsingulatityPerFn1_B2B :: UnaryFnMPBall
nearsingulatityPerFn1_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> 1/(catchingExceptions $ 10*(sin (7*x))^2+1)


nonsmoothFn1_Name :: String
nonsmoothFn1_Name = "|x+1/3| over [-1,1]"

nonsmoothFn1_PB :: Precision -> Degree -> PolyBall
nonsmoothFn1_PB p d =
    PolyBall (absXshifted p d) (Interval (-1.0) (1.0)) defaultMaxDegree defaultSweepThresholdNormLog   

nonsmoothFn1_B2B :: UnaryFnMPBall
nonsmoothFn1_B2B =
    UnaryFnMPBall (Interval (-1.0) 1.0) $
    \x -> catchingExceptions $ abs (x+1/3)

