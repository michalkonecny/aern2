module Main where

import AERN2.Num
import AERN2.RealFunction
import FnReps.Polynomial.UnaryCheb.PolyBall
--import FnReps.Polynomial.UnaryCheb.RealFn

main :: IO ()
main =
    do
    {- A plot of the following enclosures of 1/(10x^2+1) 
        is at http://fooplot.com/plot/2yt7q2sevi
    -}
    printWithName "1/(10x^2+1) degree 10" $ divT10 10
    printWithName "1/(10x^2+1) degree 14" $ divT10 14
    printWithName "1/(10x^2+1) degree 18" $ divT10 18
    
    printWithName "1/(100x^2+1) degree 30" $ divT100 30
    printWithName "1/(100x^2+1) degree 40" $ divT100 40
    printWithName "1/(100x^2+1) degree 50" $ divT100 50
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

