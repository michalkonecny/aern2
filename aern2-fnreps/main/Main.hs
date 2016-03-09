module Main where

import AERN2.Num
import AERN2.RealFunction
import FnReps.Polynomial.UnaryCheb.PolyBall
--import FnReps.Polynomial.UnaryCheb.RealFn

main :: IO ()
main =
    do
    printWithName "1/(10x^2+1) degree 10" divT10d10
    printWithName "1/(10x^2+1) degree 10, APPROX:" $ getApproximate (bits 30) divT10d10
    printWithName "1/(10x^2+1) degree 20" divT10d20
    printWithName "1/(10x^2+1) degree 20, APPROX:" $ getApproximate (bits 30) divT10d20
--    putStrLn $ show $ getApproximate (bits 100) $ 1 / (10 * x^2 + 1)
    where
    divT10d10 = divT 10 10
    divT10d20 = divT 20 10
    divT d n = 1 / (setMaxDegree d ((n * x*x) + 1))
    x = projUnaryFnA (Interval (-1.0) 1.0) :: PolyBall
    
    printWithName name value =
        putStrLn $ banner ++ "\n" ++ name ++ ":\n" ++ show value
        where
        banner = replicate (int 100) '='

