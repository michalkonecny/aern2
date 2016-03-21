module FnReps.Polynomial.UnaryCheb.Poly.Bernstein 
(bernsteinApprox, binomialcoeffs)
where

import AERN2.RealFunction
--import Control.Arrow (arr)

import FnReps.Polynomial.UnaryCheb.Poly.Basics
import FnReps.Polynomial.UnaryCheb.Poly.EvaluationRootFinding ()
import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication ()

import Debug.Trace (trace)


shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

{-
    Bernstein approximation is defined over [0,1]:
    
    B(f,n) = sum_{i=0}^n (f(i/n) * binomialcoeff(n,i)* x^i*(1-x)^(n-i))
    
    As need an approximation over [-1,1], we use:
    
    f : [-1,1] -> R
    f ~ B(f.(\x->(2x-1)), n) . (\x -> (x+1)/2)
        = sum_{i=0}^n (f((2i/n)-1) * binomialcoeff(n,i)* ((x+1)/2)^i*((1-x)/2)^(n-i))
-}
bernsteinApprox :: Precision -> Integer -> (MPBall -> MPBall) -> Poly
bernsteinApprox p d f =
    maybeTrace (
        "bernsteinApprox:"
        ++ "\n  n = " ++ show n
        ++ "\n  x = " ++ show x
        ++ "\n  x+1 = " ++ show (x+1)
        ++ "\n  (x+1)/2 = " ++ show ((x+1)/2)
        ++ "\n  powers1 = " ++ show (take (int (n+1)) powers1)
        ++ "\n  powers2 = " ++ show (take (int (n+1)) powers1)
        ++ "\n  binomial coeffs = " ++ show (take (int (n+1)) (binomialcoeffs !!! n))
        ++ "\n  f samples: = " ++ show [fi i | i <- [0..n]]
    )$
    foldl1 (+) $
        zipWith (*) 
            (binomialcoeffs !!! n) 
            [(fi i) * (powers1 !!! i) * (powers2 !!! (n - i)) | i <- [0..n]]
    where
    fi i = f $ setPrecision p $ mpBall ((2*i/n) - 1)
    n = d
    powers1 = iterate (* ((x+1)/2)) poly1
    powers2 = iterate (* ((1-x)/2)) poly1
    x = setPrecision_poly p $ projUnaryFnA polyFixedDomain :: Poly
    poly1 = setPrecision_poly p $ constUnaryFnA (polyFixedDomain, mpBall 1) :: Poly

binomialcoeffs :: [[Integer]]
binomialcoeffs =
    iterate makeRow (1 : repeat 0)
    where
    makeRow prevRow = zipWith (+) prevRow (0:prevRow)
