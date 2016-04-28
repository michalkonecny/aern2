module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Division
(
linSpline,
xInverse,
xInverseRaw,
xInverseIRRAM,
xInverseP,
xInverseRawP,
xInverseSIRRAM
{-_test1,
_test1A,
_test1B,
_test2,
_testA-}
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
              --aux 0 xs ys = (0.5^n:xs, 2.0^n:ys)
              --aux k xs ys = aux (k - 1) (0.5^(n - k )*2.0/3.0:0.5^(n - k):xs) ( 2.0^(n - k)*3.0/2.0:2.0^(n - k):ys)
              aux 0 xs ys = (0.5^n:xs, 1.0:ys)
              aux k xs ys = aux (k - 1) (0.5^(n - k )*2.0/3.0:0.5^(n - k):xs) ( 0.5^(k)*3.0/2.0:0.5^(k):ys)
              
xInverseRaw :: Integer -> Integer -> PPoly
xInverseRaw n k = 2^n * (it spline k)
               where
               spline = setPrecision_ppoly (prec $ 200*n*k + 100*n*n + 100*k*k + 53) $ dropAllErrors $ linSpline n
               h = PPoly [(Interval (-1.0) (-(0.5^n)),2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)]), (Interval (-(0.5^n)) (0.5^n), Poly.fromList [(0, mpBall 1)]),  (Interval (0.5^n) 1.0, 2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)])]   
               --x = Poly.fromList [(0,mpBall 0), (1, mpBall 1)]
               it f 0 = f
               it f i = it ({-setPrecision_ppoly (prec q) $-} dropAllErrors (2*f - h*f*f)) (i - 1)
               dropAllErrors (PPoly pcs) = PPoly $ map dae pcs
               dae (i, p) = (i, Poly.polyCentre p)

xInverseRawP :: Integer -> Integer -> Integer -> PPoly
xInverseRawP n k pr = 2^n * (it spline k)
                      where
                      spline = setPrecision_ppoly (prec $ pr) $ dropAllErrors $ linSpline n
                      h = PPoly [(Interval (-1.0) (-(0.5^n)),2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)]), (Interval (-(0.5^n)) (0.5^n), Poly.fromList [(0, mpBall 1)]),  (Interval (0.5^n) 1.0, 2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)])]   
                      --x = Poly.fromList [(0,mpBall 0), (1, mpBall 1)]
                      it f 0 = f
                      it f i = it ({-setPrecision_ppoly (prec q) $-} dropAllErrors (2*f - h*f*f)) (i - 1)
                      dropAllErrors (PPoly pcs) = PPoly $ map dae pcs
                      dae (i, p) = (i, Poly.polyCentre p)

xInverseRawS :: Integer -> Integer -> Integer -> PPoly
xInverseRawS n k pr = 2^n * (it spline k)
                      where
                      spline = setPrecision_ppoly (prec $ pr) $ dropAllErrors $ linSpline n
                      h = PPoly [(Interval (-1.0) (-(0.5^n)),2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)]), (Interval (-(0.5^n)) (0.5^n), Poly.fromList [(0, mpBall 1)]),  (Interval (0.5^n) 1.0, 2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)])]   
                      --x = Poly.fromList [(0,mpBall 0), (1, mpBall 1)]
                      it f 0 = f
                      it (PPoly xs) i = it (dropAllErrors $ PPoly $ (map (\(a,p) -> (a, itP p)) (take (int $ k - i + 1) xs)) ++ drop (int $ k - i + 1) xs) (i - 1)
                      itP f = 2*f - 2^n*(Poly.fromList [(0,mpBall 0), (1,mpBall 1)])*f*f -- TODO use h?
                      dropAllErrors (PPoly pcs) = PPoly $ map dae pcs
                      dae (i, p) = (i, Poly.polyCentre p)

xInverseS :: Integer -> Integer -> Integer -> (MPBall, PPoly)
xInverseS n k pr = (err, addToErrorTerm err xInv)
               where
               xInv = xInverseRawS n k pr
               err = let Interval _ errp = abs $ range (bits $ pr) ((xInv)*x - one) $ Interval (mpBall $ (0.5^n)) (mpBall 1) in 
                       2^n*errp
               one = fromPoly $ Poly.fromList [(0,mpBall 1)]
               x = fromPoly $ Poly.fromList [(0,mpBall 0), (1, mpBall 1)]

xInverseP :: Integer -> Integer -> Integer -> (MPBall, PPoly)
xInverseP n k pr = (err, addToErrorTerm err xInv)
               where
               xInv = xInverseRawP n k pr
               err = let Interval _ errp = abs $ range (bits $ pr) ((xInv)*x - one) $ Interval (mpBall $ (0.5^n)) (mpBall 1) in 
                       2^n*errp
               one = fromPoly $ Poly.fromList [(0,mpBall 1)]
               x = fromPoly $ Poly.fromList [(0,mpBall 0), (1, mpBall 1)]

xInverseIRRAM :: Integer -> Integer -> (MPBall, PPoly)
xInverseIRRAM n k = aux 100
                    where
                    aux pr = let (err,res) = xInverseP n k pr in
                                if getAccuracy err > bits (2*n*k) then
                                    (err, res)
                                else
                                    aux (2*pr) 

xInverseSIRRAM :: Integer -> Integer -> (MPBall, PPoly)
xInverseSIRRAM n k = aux 100
                    where
                    aux pr = let (err,res) = xInverseS n k pr in
                                if getAccuracy err > bits (2*n*k) then
                                    (err, res)
                                else
                                    aux (2*pr) 

xInverse :: Integer -> Integer -> (MPBall,PPoly)
xInverse n k = (err, addToErrorTerm err xInv)
               where
               xInv = xInverseRaw n k
               err = getErrAccu 1 (mpBall 10^(n + k))
               getErrAccu 0 e = e
               getErrAccu i e = getErrAccu (i - 1) (min e $ tryPrec 100*(n + k - i + 1))
               tryPrec p = 
                    let Interval _ errp = abs $ range (bits $ 200*n*k + 100*n*n + 100*k*k + 53) ({-setPrecision_ppoly (prec p) $-} (xInv)*x - one) $ Interval (mpBall $ (0.5^n)) (mpBall 1) in 
                    2^n*errp
               one = fromPoly $ Poly.fromList [(0,mpBall 1)]
               x = fromPoly $ Poly.fromList [(0,mpBall 0), (1, mpBall 1)]
               
               
               
{-_test1 n k p = 2^n*err
             where
             Interval _ err = abs $ range (bits p) (setPrecision_ppoly (prec p) $ (xInverse n k)*x - one) $ Interval (mpBall $ (0.5^n)) (mpBall 1)
             one = fromPoly $ Poly.fromList [(0,mpBall 1)]
             x = Poly.fromList [(0,mpBall 0), (1, mpBall 1)]

_test1A n k = 2^n*err
             where
             Interval _ err = abs $ range (bits $ 200*n) (setPrecision_ppoly (prec $ 200*n) $ (xInverse n k)*x - one) $ Interval (mpBall $ (0.5^n)) (mpBall 1)
             one = fromPoly $ Poly.fromList [(0,mpBall 1)]
             x = Poly.fromList [(0,mpBall 0), (1, mpBall 1)]

_test1B n k = err
              where
              m = 1000
              err = foldl' max (mpBall 0) [let x = mpBall $ 0.5^(n) + (1 - 0.5^(n))*i/m in abs $ (eval (xInverse n k) x) - 1/x | i <- [0..m]]
              xInv = {-setPrecision_ppoly (prec $ 200*n) $ -}(xInverse n k)
             
_test2 n k m = err
             where  
             err = foldl' max (mpBall 0) [let x = mpBall $ 0.5^(n) + (1 - 0.5^(n))*i/m in abs $ (eval (xInverse (n) k) x) - 1/x | i <- [0..m]]
             
_testA n k = (i, {-getApproximate (bits 53) $ Poly.cheb2Power-} Poly.showRawPoly $ p)
             where
             PPoly pcs = xInverse n k           
             (i, p) = head $ drop (convert $ 2*n + 1) pcs -}