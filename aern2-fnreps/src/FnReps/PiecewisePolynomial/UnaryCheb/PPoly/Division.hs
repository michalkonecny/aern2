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

import Debug.Trace(trace)

shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

data LineSegment = LineSegment (Rational, MPBall) (Rational, MPBall) deriving Eq

lsFst :: LineSegment -> (Rational, MPBall)
lsFst (LineSegment a _) = a

lsSnd :: LineSegment -> (Rational, MPBall)
lsSnd (LineSegment _ b) = b

instance Prelude.Ord LineSegment where
  (<=) (LineSegment (a,_) _) (LineSegment (b,_) _) = a <= b 
  
inverse :: PPoly -> Rational -> Integer -> Accuracy -> PPoly
inverse f threshold its acc = 
  PPoly (zipWith (\ie (i,p) -> (i, Poly.polyAddToRadius p (finalErr ie))) iE ps) ov
  where
  Interval _ b = abs $ range (bits 100) f (Interval (mpBall $ -1) (mpBall 1))
  PPoly ps ov = inverseWithInit f iA b its
  (iA, iE) = initialApproximation threshold acc f b
  finalErr e = aux its e
  aux k e = if k == 0 then e else aux (k - 1) (e*e)        
        
inverseWithInit :: PPoly -> PPoly -> MPBall -> Integer -> PPoly
inverseWithInit f f0 bf its = 
  aux f0 its
  where
  aux fn k = 
    if k == 0 then
      fn
    else
      maybeTrace (
        "iterating...\n"++
        "radius before iteration: "++(show $ radius fn)++"\n"++
        "radius after: "++(show $ radius next)++"\n"
      ) $
      aux (reduceDegree next) (k - 1) 
      where
      next = 2*fn - (mwb f (mwb fn fn (mpBall 2) (mpBall 2)) bf (mpBall 4)) 
      mwb g h bg bh = PPoly [(i, mwbPoly p q bg bh) | (i,p,q) <- refine g h] $ min (ppoly_overlap g) (ppoly_overlap h)
      mwbPoly g h bg bh = 
        let
          rh = Poly.polyRadius h
          rg = Poly.polyRadius g
        in 
        Poly.polyAddToRadius ((Poly.polyCentre g) * (Poly.polyCentre h)) $ (bg*rh + bh*rg + rh*rg) 
      
reduceDegree :: PPoly -> PPoly 
reduceDegree f@(PPoly ps ov) = 
  maybeTrace (
    "reducing degree \n"++
    "original degree: "++(show $ Poly.poly_degree $ (snd . head) ps) ++ "\n"++
    "new degree:" ++ (show $ Poly.poly_degree $ (snd. head . ppoly_pieces) result) ++"\n"++
    "original radius: "++(show $ radius f) ++ "\n"++
    "new radius: "++(show $ radius result)
  ) $
  result
  where
  result = PPoly (map (aux $ (Poly.poly_degree $ (snd . head) ps) `Prelude.div` 2) ps) ov
  aux d (i, p) = 
    let 
      reduced = Poly.reduceDegreeAndSweep d NormZero $ p
      err0 = Poly.polyRadius p
      err = Poly.polyRadius reduced
      justTrue (Just True) = True
      justTrue _ = False
    in
      if justTrue (err < 100*err0) then
        maybeTrace(
        "reducing degree\n"++
        "original degree: "++ (show (Poly.poly_degree p)) ++"\n"++
        "final degree "++(show d)++ "\n"++
        "error: "++(show err) ++"\n"++
        "target error:" ++(show err0)
        ) $
        (i, reduced)
      else
        aux (d + 10) (i,p)    
      
initialApproximation :: Rational -> Accuracy -> PPoly -> MPBall -> (PPoly, [MPBall])
initialApproximation threshold acc f bf = 
  maybeTrace (
  --"f: "++(show f) ++"\n"++
  --"f reduced: "++(show fReduced) ++ "\n"++
  "accuracy of f: "++(show $ getAccuracy f) ++"\n"++
  "accuracy of f reduced: "++(show $ getAccuracy fReduced) ++ "\n"++
  "number of nodes: "++(show $ length nodes) ++ "\n"++
  "first node: " ++(show $ lsFst $ head nodes)
  ) $
  result
  where
  result = (linearPolygon ((lsFst $ head nodes) : (map lsSnd nodes)) smallestOverlap, nodeErrs) 
  
  fReduced = reduceF 5
  reduceF d = 
    let 
      try = reduceDegreeAndSweep d NormZero f
    in
      if getAccuracy try > (normLog2Accuracy $ getNormLog threshold) + 1 then
        try
      else
        reduceF (d + 5)
        
  mwb g h bg bh = PPoly [(i, mwbPoly p q bg bh) | (i,p,q) <- refine g h] $ min (ppoly_overlap g) (ppoly_overlap h)
  mwbPoly g h bg bh = 
    let
      rh = Poly.polyRadius h
      rg = Poly.polyRadius g
    in 
    Poly.polyAddToRadius ((Poly.polyCentre g) * (Poly.polyCentre h)) $ (bg*rh + bh*rg + rh*rg) 
  
  nodesNErrs = Set.toList $ refineUntilAccurate $ LineSegment (-1.0, 1/(eval f (mpBall $ -1))) (1.0, 1/(eval f (mpBall 1)))
  nodes = map fst nodesNErrs
  nodeErrs = map snd nodesNErrs
  smallestOverlap = foldl1 min $ map (\(LineSegment (l,_) (r,_)) -> overlap (l,r)) nodes
  pieceError (LineSegment (l,fl) (r,fr)) =
    let Interval _ err = abs $ range' acc ((mwb fReduced (lineSegment ((l,fl), (r,fr))) bf (mpBall 2)) - 1) (Interval (mpBall l) (mpBall r))
    in
      err
  refineUntilAccurate :: LineSegment -> Set (LineSegment, MPBall)
  refineUntilAccurate p = 
    let
      err = pieceError p 
    in
    if (err < threshold) == Just True then
      Set.singleton (p, err)
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
