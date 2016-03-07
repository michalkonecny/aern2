module Main where

import AERN2.Num
import AERN2.RealFunction
import FnReps.Polynomial.UnaryCheb.PolyBall
--import FnReps.Polynomial.UnaryCheb.RealFn

main :: IO ()
main =
    do
    putStrLn $ show $ 1 / (setMaxDegree 20 ((10 * x*x) + 1))
    putStrLn $ show $ getApproximate (bits 30) $ 1 / (setMaxDegree 20 ((10 * x*x) + 1))
--    putStrLn $ show $ getApproximate (bits 100) $ 1 / (10 * x^2 + 1)
    where
    x = projUnaryFnA (Interval (-1.0) 1.0) :: PolyBall

