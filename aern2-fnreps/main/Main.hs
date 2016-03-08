module Main where

import AERN2.Num
import AERN2.RealFunction
import FnReps.Polynomial.UnaryCheb.PolyBall
--import FnReps.Polynomial.UnaryCheb.RealFn

main :: IO ()
main =
    do
    putStrLn $ show $ divT10
    putStrLn $ show $ getApproximate (bits 30) $ divT10
--    putStrLn $ show $ getApproximate (bits 100) $ 1 / (10 * x^2 + 1)
    where
    divT10 = divT 20 10
    divT d n = 1 / (setMaxDegree d ((n * x*x) + 1))
    x = projUnaryFnA (Interval (-1.0) 1.0) :: PolyBall

