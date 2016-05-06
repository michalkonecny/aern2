module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Division
(
initialApproximation,
inverse,
inverseWithInitP,
inverseWithInitRawP,
xInverse
)
where

import AERN2.Num
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Evaluation
import qualified FnReps.Polynomial.UnaryCheb.Poly as Poly

initialApproximation :: PPoly -> PPoly
initialApproximation f = linearPolygon ((fst $ head nodes) : (map snd nodes)) smallestOverlap
  where
  nodes = refineUntilAccurate ((-1.0, 1/(eval f (mpBall $ -1))), (1.0, 1/(eval f (mpBall 1))))
  smallestOverlap = foldl1 min $ map (\((l,_), (r,_)) -> overlap (l,r)) nodes
  pieceAccurate ((l,fl), (r,fr)) = 
    let Interval _ err = abs $ range' (bits 100) (f*(lineSegment ((l,fl), (r,fr))) - 1)   -- TODO bits?
                               (Interval (mpBall l) (mpBall r))
    in
      err < 0.5
  refineUntilAccurate :: ((Rational,MPBall), (Rational,MPBall)) -> [((Rational, MPBall), (Rational, MPBall))]
  refineUntilAccurate p = 
    if pieceAccurate p == Just True then
      [p]
    else let refined = refinePiece p in
      (refineUntilAccurate $ refined !!! 0) ++ (refineUntilAccurate $ refined !!! 1) -- TODO memory efficient?
  refinePiece :: ((Rational,MPBall), (Rational,MPBall)) -> [((Rational, MPBall), (Rational, MPBall))]
  refinePiece ((l,fl), (r,fr)) = 
    let m  = (l + r)/2 
        fm = 1/(eval f (mpBall m))
    in 
    [((l,fl), (m, fm)), ((m, fm), (r, fr))]
  overlap (l,r) = (r - l)/2
  lineSegment ((l,fl), (r, fr)) = 
    if l /= -1.0 && r /= 1.0 then
      linearPolygon [(-1.0, fl), (l, fl), (r, fr), (1.0, fr)] $ overlap  (l,r)
    else if l /= -1.0 && r == 1.0 then
      linearPolygon [(-1.0, fl), (l, fl), (r, fr)] $ overlap  (l,r)
    else if l == -1.0 && r /= 1.0 then
      linearPolygon [(l, fl), (r, fr), (1.0, fr)] $ overlap  (l,r)
    else
      linearPolygon [(l,fl), (r,fr)] $ overlap  (l,r)

inverseWithInitRawP :: PPoly -> PPoly -> Integer -> Integer -> PPoly
inverseWithInitRawP f f0 its pr = aux (setPrecision_ppoly (prec pr) f0) its
  where
  aux fn k = 
    if k == 0 then
      fn
    else
      aux (dropAllErrors $ 2*fn - f*fn*fn) (k - 1)    
  
inverseWithInitP :: PPoly -> PPoly -> Integer -> Integer -> (MPBall, PPoly)
inverseWithInitP f f0 its pr = (err, raw)
  where
  raw = inverseWithInitRawP f f0 its pr
  Interval _ err = abs $ range' (bits pr) (raw * f - 1) (Interval (mpBall $ -1) (mpBall 1))

inverseWithInit :: PPoly -> PPoly -> Integer -> PPoly
inverseWithInit f f0 its = addToErrorTerm e $ dropAllErrors r
  where
  (e,r) = aux 100
  aux pr = let (err,res) = inverseWithInitP f f0 its pr in
              if getAccuracy err > bits (2*its) then
                  (err, res)
              else
                  aux (2*pr) 

inverse :: PPoly -> Integer -> PPoly
inverse f = inverseWithInit f (initialApproximation f)

linSpline :: Integer -> PPoly
linSpline n = linearPolygon (zipWith (\x y -> (x, mpBall y)) mirroredXs mirroredYs) (0.25*0.5^n) 
              where
              (mirroredXs, mirroredYs) = ((map (negA) $ reverse basicXs) ++ basicXs, (reverse basicYs) ++ basicYs) 
              (basicXs,basicYs) = aux n [] []
              aux 0 xs ys = (0.5^n:xs, 1.0:ys)
              aux k xs ys = aux (k - 1) (0.5^(n - k )*2.0/3.0:0.5^(n - k):xs) ( 0.5^(k)*3.0/2.0:0.5^(k):ys)

xInverseRawP :: Integer -> Integer -> Integer -> PPoly
xInverseRawP n k pr = 2^n * (it spline k)
                      where
                      spline = setPrecision_ppoly (prec $ pr) $ dropAllErrors $ linSpline n
                      h = PPoly [(Interval (-1.0) (-(0.5^n)),2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)]), 
                                 (Interval (-(0.5^n)) (0.5^n), Poly.fromList [(0, mpBall 1)]),  
                                 (Interval (0.5^n) 1.0, 2^n*Poly.fromList [(0,mpBall 0), (1, mpBall 1)])]
                                 ((0.5)^n)
                      it f 0 = f
                      it f i = it (dropAllErrors (2*f - h*f*f)) (i - 1)

xInverseP :: Integer -> Integer -> Integer -> (MPBall, PPoly)
xInverseP n k pr = (err, addToErrorTerm err xInv)
               where
               xInv = xInverseRawP n k pr
               err = let Interval _ errp = abs $ range' (bits $ pr) ((xInv)*x - 1) $ Interval (mpBall $ 0.5^n) (mpBall 1) in 
                       2^n*errp
               x = fromPoly $ Poly.fromList [(0,mpBall 0), (1, mpBall 1)]

xInverseIRRAM :: Integer -> Integer -> (MPBall, PPoly)
xInverseIRRAM n k = aux 100
                    where
                    aux pr = let (err,res) = xInverseP n k pr in
                                if getAccuracy err > bits (2*n*k) then
                                    (err, res)
                                else
                                    aux (2*pr) 
                                    
xInverse :: Integer -> Integer -> PPoly
xInverse n k = addToErrorTerm err xInv
               where
               (err, xInv) = xInverseIRRAM n k
               