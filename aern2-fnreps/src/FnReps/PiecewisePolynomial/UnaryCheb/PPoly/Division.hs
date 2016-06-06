module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Division
(
inverse,
xInverse
)
where

import qualified Prelude as Prelude

import AERN2.Num
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Evaluation
import qualified FnReps.Polynomial.UnaryCheb.Poly as Poly

import Data.Set (Set)
import qualified Data.Set as Set

data LineSegment = LineSegment (Rational, MPBall) (Rational, MPBall) deriving Eq

lsFst :: LineSegment -> (Rational, MPBall)
lsFst (LineSegment a _) = a

lsSnd :: LineSegment -> (Rational, MPBall)
lsSnd (LineSegment _ b) = b

instance Prelude.Ord LineSegment where
  (<=) (LineSegment (a,_) _) (LineSegment (b,_) _) = a <= b 

initialApproximation :: Rational -> Accuracy -> PPoly -> PPoly
initialApproximation threshold acc f = 
  linearPolygon ((lsFst $ head nodes) : (map lsSnd nodes)) smallestOverlap
  where
  nodes = Set.toList $ refineUntilAccurate $ LineSegment (-1.0, 1/(eval f (mpBall $ -1))) (1.0, 1/(eval f (mpBall 1)))
  smallestOverlap = foldl1 min $ map (\(LineSegment (l,_) (r,_)) -> overlap (l,r)) nodes
  pieceAccurate (LineSegment (l,fl) (r,fr)) =
    let Interval _ err = abs $ range' acc (f*(lineSegment ((l,fl), (r,fr))) - 1) (Interval (mpBall l) (mpBall r))
    in
      err < threshold
  refineUntilAccurate :: LineSegment -> Set LineSegment
  refineUntilAccurate p = 
    if pieceAccurate p == Just True then
      Set.singleton p
    else let refined = refinePiece p in
      (refineUntilAccurate $ Set.elemAt (int 0) refined) `Set.union` (refineUntilAccurate $ Set.elemAt (int 1) refined)
  refinePiece :: LineSegment -> Set LineSegment
  refinePiece (LineSegment (l,fl) (r,fr)) = 
    let m  = (l + r)/2 
        fm = 1/(eval f (mpBall m))
    in 
      Set.fromList [LineSegment (l,fl) (m, fm), LineSegment (m, fm) (r, fr)]
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

               
inverseWithInitP :: PPoly -> PPoly -> Integer -> Precision -> PPoly
inverseWithInitP f f0 its pr = 
  aux (setPrecision pr f0) its
  where
  aux fn k = 
    if k == 0 then
      fn
    else
      aux (reduceDegree (2*fn - f*fn*fn)) (k - 1)    

inverseWithInit :: PPoly -> PPoly -> Integer -> PPoly
inverseWithInit f f0 its = 
  aux 100
  where
  aux pr = 
    let
      try = inverseWithInitP f f0 its (prec pr) 
      err = radius try
    in
      if getNormLog err < getNormLog 1
      && getAccuracy err > (normLog2Accuracy $ getNormLog err) + 1 then
        try
      else
        aux $ pr + pr `Prelude.div` 2

reduceDegree :: PPoly -> PPoly 
reduceDegree f = aux 10
  where
  err0 = radius f
  aux d = 
    let 
      reduced = reduceDegreeAndSweep d NormZero $ f
      err = radius reduced
      justTrue (Just True) = True
      justTrue _ = False
    in
      if justTrue (err < 1.5*err0) then
        reduced
      else
        aux (d + 10)

inverse :: PPoly -> Rational -> Integer -> Accuracy -> PPoly
inverse f threshold its acc = 
  addToErrorTerm (mpBall err) $ inverseWithInit f (initialApproximation threshold acc f) its
  where
  err = aux its threshold 
  aux k e = if k == 0 then e else aux (k - 1) (e*e)  
  
{- 1/x : -}

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
                      spline = setPrecision (prec $ pr) $ dropAllErrors $ linSpline n
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
               