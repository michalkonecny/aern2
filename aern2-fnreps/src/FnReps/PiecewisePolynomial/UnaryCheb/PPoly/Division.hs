module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Division
(
linSpline,
xInverse,
_test1,
_test2,
_testA
)
where

import AERN2.Num
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Evaluation
import qualified FnReps.Polynomial.UnaryCheb.Poly as Poly
import Data.List as List

linSpline :: Integer -> PPoly
linSpline n = linearPolygon $ zipWith (\x y -> (x, mpBall y)) mirroredXs mirroredYs
              where
              (mirroredXs, mirroredYs) = ((map (negA) $ reverse basicXs) ++ basicXs, (reverse basicYs) ++ basicYs) 
              (basicXs,basicYs) = aux n [] []
              aux 0 xs ys = (0.5^n:xs, 2.0^n:ys)
              aux k xs ys = aux (k - 1) (0.5^(n - k )*2.0/3.0:0.5^(n - k):xs) ( 2.0^(n - k)*3.0/2.0:2.0^(n - k):ys)
              
xInverse :: Integer -> Integer -> PPoly
xInverse n k = it spline k
               where
               spline = linSpline n
               x = Poly.fromList [(0,mpBall 0), (1, mpBall 1)]      
               it f 0 = f
               it f i = it (2*f - x*f*f) (i - 1)
               
_test1 n k = err
             where
             Interval _ err = abs $ range (bits 100) ((xInverse n k)*x - one) $ Interval (mpBall $ (0.5^n)) (mpBall 1)
             one = fromPoly $ Poly.fromList [(0,mpBall 1)]
             x = Poly.fromList [(0,mpBall 0), (1, mpBall 1)]
             
_test2 n k m = err
             where  
             err = foldl' max (mpBall 0) [let x = mpBall $ 0.5^(n - 1) + (1 - 0.5^(n - 1))*i/m in abs $ (eval (xInverse (n + 1) k) x) - 1/x | i <- [0..m]]
             
_testA n k = (i, {-getApproximate (bits 53) $ -}Poly.cheb2Power p)
             where
             PPoly pcs = xInverse n k           
             (i, p) = head $ drop (convert $ 2*n + 1) pcs    