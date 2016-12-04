module AERN2.PPoly.Division

where

import Numeric.MixedTypes hiding (maximum, minimum)
import qualified Prelude

import AERN2.MP.Dyadic
import AERN2.Poly.Ball as PolyBall
import AERN2.MP.Ball

import AERN2.PPoly.Type as PPoly
import AERN2.PPoly.Eval
import AERN2.PPoly.Maximum

import AERN2.Interval

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

inverse :: PPoly -> PPoly -- TODO: allow negative f; maximum optimised for PPoly
inverse f@(PPoly _ (Interval l r)) =
  iterateInverse f if0 bf
  where
  bf = abs $ AERN2.PPoly.Maximum.maximum f (mpBall l) (mpBall r)
  if0 = initialApproximation f (1/(1 + 2*bf)) bf

iterateInverse :: PPoly -> PPoly -> MPBall -> PPoly
iterateInverse f if0 bf =
  aux if0
  where
  aux ifn =
    let
      next = newton ifn
    in
      if getAccuracy next <= getAccuracy ifn then
        ifn
      else
        aux next

  newtonPiece :: PolyBall -> PolyBall -> PolyBall
  newtonPiece pg pf =
    let
    cg = centreAsBall pg
    rg = radius pg
    in
    updateRadius (+ (errorBound bf)*rg*rg) $
    2*cg
    - PolyBall.multiplyWithBounds pf bf
      (PolyBall.multiplyWithBounds cg (mpBall 2) cg (mpBall 2)) (mpBall 4)
  newton :: PPoly -> PPoly
  newton g =
    PPoly
      [(i, newtonPiece pg pf) | (i,pg,pf) <- refine g f]
      (ppoly_dom f)

{- -}

data LineSegment = LineSegment (Dyadic, MPBall) (Dyadic, MPBall) deriving (Prelude.Eq)

lsFst :: LineSegment -> (Dyadic, MPBall)
lsFst (LineSegment a _) = a

lsSnd :: LineSegment -> (Dyadic, MPBall)
lsSnd (LineSegment _ b) = b

instance Prelude.Ord LineSegment where
  (<=) (LineSegment (a,_) _) (LineSegment (b,_) _) = a <= b

{- -}

initialApproximation :: PPoly -> MPBall -> MPBall -> PPoly
initialApproximation f@(PPoly _ dom@(Interval l r)) threshold bf =
  result
  where
  PPoly linps _ = linearPolygon ((lsFst $ head nodes) : (map lsSnd nodes)) dom
  result =
    PPoly
      (zipWith
        (\(i,p) e -> (i, updateRadius (+ (errorBound e)) p))
        linps
        errs)
      dom

  nodesNErrs =
    Set.toList $
    refineUntilAccurate $
    LineSegment (l, 1/(evalDirect f (mpBall l)))
                (r, 1/(evalDirect f (mpBall r)))
  nodes = map fst nodesNErrs
  errs = map snd nodesNErrs
  pieceError (LineSegment (a, fa) (b, fb)) =
    let
      p      = lineSegment ((a,fa), (b,fb))
      pf     = PPoly.multiplyWithBounds p (mpBall 2) f bf
      maxErr = maximum (pf - 1) (mpBall a) (mpBall b)
      minErr = minimum (pf - 1) (mpBall a) (mpBall b)
      absErr = max (abs maxErr) (abs minErr)
      minf   = minimum f (mpBall a) (mpBall b) -- TODO requires f > 0
    in
      trace (
      "multiplication result: "++(show pf)++"\n"++
      "abs error: "++(show absErr)++"\n"++
      "target error: "++(show threshold) ++ "\n"++
      "minf: "++(show minf)
      )
      absErr/minf
    {-let
      Interval _ err  = abs $ range' acc ((mwb fReduced (lineSegment ((l,fl), (r,fr))) bf (mpBall 2)) - 1) (Interval (mpBall l) (mpBall r))
      Interval minf _ = abs $ range' acc fReduced (Interval (mpBall l) (mpBall r))
    in
      err/minf-}
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
  refinePiece (LineSegment (a,fa) (b,fb)) =
    let
      m  = 0.5*(a + b)
      fm = 1/(evalDirect f (mpBall m))
    in
      trace (
      "refining "++(show a) ++ " " ++(show b) ++"\n"++
      "with value "++(show fm)
      )
      Set.fromList [LineSegment (a,fa) (m, fm), LineSegment (m, fm) (b, fb)]
  lineSegment ((a,fa), (b, fb)) =
    if a /=  l && b /= r then
      linearPolygon [(l, fa), (a, fa), (b, fb), (r, fb)] dom
    else if a /= l && b == r then
      linearPolygon [(l, fa), (a, fa), (b, fb)] dom
    else if a == l && b /= r then
      linearPolygon [(a, fa), (b, fb), (r, fb)] dom
    else
      linearPolygon [(a,fa), (b,fb)] dom
