module AERN2.PPoly.Division
(inverse, inverseWithAccuracy, initialApproximation)
where

import Numeric.MixedTypes hiding (maximum, minimum)
import qualified Prelude
import Data.List

import AERN2.Poly.Cheb (reduceDegree, degree)

import AERN2.MP.Dyadic
import AERN2.Poly.Ball as PolyBall
import AERN2.MP.Ball
import AERN2.Normalize

import AERN2.PPoly.Type as PPoly
import AERN2.PPoly.Eval
import AERN2.PPoly.Maximum

import AERN2.Interval

import Data.Set (Set)
import qualified Data.Set as Set

--import Debug.Trace

inverseWithAccuracy :: Accuracy -> PPoly -> PPoly
inverseWithAccuracy cutoff f@(PPoly _ (Interval l r)) =
  updateRadius (+ radius f) $
  iterateInverse (min (getAccuracy f) cutoff) fc (setPrecision (getPrecision f) if0)
  where
  fc = centre f
  fRed0 = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 (bits 4)) fc
  fRed1 = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 thresholdAccuracy) fc
  bf   = abs $ AERN2.PPoly.Maximum.maximumOptimisedWithAccuracy fRed0 (mpBall l) (mpBall r) 5 5 (bits 4)
  threshold = 1/(1 + 2*bf)
  if0 = initialApproximation fRed1 thresholdAccuracy
  thresholdAccuracy = 4 + getAccuracy ((fromEndpoints (mpBall 0) (threshold)) :: MPBall)

inverse :: PPoly -> PPoly -- TODO: allow negative f
inverse f =
  inverseWithAccuracy (getFiniteAccuracy f) f
  {-iterateInverse f (setPrecision (getPrecision f) if0)
  where
  fRed0 = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 (bits 4)) f
  fRed1 = f--(liftCheb2PPoly $ reduceDegreeToAccuracy 5 thresholdAccuracy) f
  bf   = abs $ AERN2.PPoly.Maximum.maximumOptimisedWithAccuracy fRed0 (mpBall l) (mpBall r) 5 5 (bits 4)
  threshold = 1/(1 + 4*bf)
  if0 = initialApproximation fRed1 thresholdAccuracy
  thresholdAccuracy = 2 + getAccuracy ((fromEndpoints (mpBall 0) (threshold)) :: MPBall)-}

iterateInverse :: Accuracy -> PPoly -> PPoly -> PPoly
iterateInverse cutoff f if0 =
  --aux (newton if0)
  PPoly
    [let
     bfp = maximumOptimisedWithAccuracy fRed (mpBall l) (mpBall r) 5 5 (bits 4)
     in
     (i, aux' pg pf bfp) | (i@(Interval l r),pg,pf) <- refine if0 f]
    (ppoly_dom f)
  where
  fRed = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 (bits 4)) f
  aux' ipn pf bfp =
    let
      next = {-reduce $-} newtonPiece ipn pf bfp
      nextAccuracy = getAccuracy next
    in
      if nextAccuracy >= cutoff then
        next
      else if nextAccuracy <= getAccuracy ipn then
        ipn
      else
        aux' next pf bfp
  {-aux ifn =
    let
      next = newton ifn
    in
      {-trace (
      "next iterate degree "++(show $ (degree . centre . snd . head . ppoly_pieces) next)++"\n"++
      "accuracy: "++(show $ getAccuracy next)
      ) $-}
      if getAccuracy next <= getAccuracy ifn then
        ifn
      else
        aux next-}
  reduce :: PolyBall -> PolyBall
  reduce p =
    aux' 10
    where
    aux' d =
      let
        red = setPrecision (getPrecision p) $ normalize $ Ball ((ballLift1R $ reduceDegree d) p) (errorBound 0)
      in
        {-trace (
        "reducing..."++(show d)++"\n"++
        "reduced accuracy: "++(show $ getAccuracy red)++"\n"++
        "original accuracy: "++(show $ getAccuracy p)
        ) $-}
        if d >= (ballLift1R degree) p
        || getAccuracy red >= max (bits 3) (getAccuracy p - (bits 4))
        then
          red
        else
          aux' (d + 10)
  newtonError :: PolyBall -> MPBall -> ErrorBound
  newtonError pg bfp =
    let
    rg = radius pg
    in
    errorBound bfp*rg*rg
  newtonPiece :: PolyBall -> PolyBall -> MPBall -> PolyBall
  newtonPiece pg pf bfp =
    let
      cg = setPrecision (getPrecision f) $ centreAsBall pg
      ni = normalize $
            Ball (((ballLift1R $ reduceDegreeToAccuracy 5 (getAccuracy ne + 1)))
              ((2 - cg*pf)*cg))
              (errorBound 0)
      ne = newtonError pg bfp
    in
      updateRadius (+ne) ni
    {-let
    ne = newtonError pg bfp
    newtonAux p q =
      let
        cg  = setPrecision p $ centreAsBall pg
        pf' = setPrecision p pf
        ni  = normalize $
              Ball (((ballLift1R $ reduceDegreeToAccuracy 5 (getAccuracy ne + 1)))
                ((2 - cg*pf')*cg))
                (errorBound 0)
      in
      {-trace("accuracy of f: "++(show $ getAccuracy f)) $
      trace("accuracy of cg: "++(show $ getAccuracy cg)) $
      trace("precision "++(show p)++"\naccuracy of multiplication result "++(show $ getAccuracy ni))$-}
      if getAccuracy ni > 2 + getAccuracy ne then
        updateRadius (+ne) ni
      else if p > 4*q then
        trace("increasing precision failed.") $
        updateRadius (+ne) ni
      else
        trace("trying to increase precision...") $
        newtonAux (p + q) q
    in
    newtonAux (getPrecision pf) (getPrecision pg)-}

    --updateRadius (+ newtonError pg bfp) $
    --  (2 - cg*pf)*cg
    {-2*cg
    - PolyBall.multiplyWithBounds pf bf
      (PolyBall.multiplyWithBounds cg (mpBall 2) cg (mpBall 2)) (mpBall 4)-}
  {-newton :: PPoly -> PPoly
  newton g =
    PPoly
      [(i, {-reduce $-} newtonPiece pg pf) | (i,pg,pf) <- refine g f]
      (ppoly_dom f)-}

{- -}

data LineSegment = LineSegment (Dyadic, MPBall) (Dyadic, MPBall) deriving (Prelude.Eq)

lsFst :: LineSegment -> (Dyadic, MPBall)
lsFst (LineSegment a _) = a

lsSnd :: LineSegment -> (Dyadic, MPBall)
lsSnd (LineSegment _ b) = b

instance Prelude.Ord LineSegment where
  (<=) (LineSegment (a,_) _) (LineSegment (b,_) _) = a <= b

{- -}

initialApproximation :: PPoly -> Accuracy -> PPoly
initialApproximation f@(PPoly _ dom@(Interval l r)) thresholdAccuracy {-bf-} =
  result
  where
  fRed = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 (bits 4)) f
  --thresholdAccuracy = 2 + getAccuracy ((fromEndpoints (mpBall 0) threshold) :: MPBall)
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
    LineSegment (l, mpBall $ centre $ 1/(evalDirect f (mpBall l)))
                (r, mpBall $ centre $ 1/(evalDirect f (mpBall r)))
  nodes = map fst nodesNErrs
  errs = map snd nodesNErrs
  {-minf =
      minimumOptimised f (mpBall l) (mpBall r) 5 5 -- TODO requires f > 0-}
  sampledError (LineSegment (a,fa) (b,fb)) =
    let
      p      = lineSegment ((a,fa), (b,fb))
      errs   =
        [let
          x = (a + k*(setPrecision (getPrecision f) $ mpBall b - a)/16)
         in
          abs $ 1/(evalDirect f x) - evalDirect p x
          | k <- [1..4] ]
      absErr = foldl' max (mpBall 0) errs
      --minf   = minimumOptimised f (mpBall a) (mpBall b) 5 5 -- TODO requires f > 0
    in
      absErr
  bf = maximumOptimisedWithAccuracy fRed (mpBall l) (mpBall r) 5 5 (bits 4)
  pieceThreshold (LineSegment (a, fa) (b,fb)) =
    1/(1 + 4*bf)
    {-let
    bfp = maximumOptimisedWithAccuracy fRed (mpBall a) (mpBall b) 5 5 (bits 4)
    in
    1/(1 + 2*bfp)-}
  pieceError (LineSegment (a, fa) (b, fb)) =
    let
      p      = lineSegment ((a, fa), (b,fb))
      pf     = p*f --PPoly.multiplyWithBounds p (mpBall 2) f bf
      maxErr = maximumOptimisedWithAccuracy (pf - 1) (mpBall a) (mpBall b) 5 5 thresholdAccuracy
      minErr = minimumOptimisedWithAccuracy (pf - 1) (mpBall a) (mpBall b) 5 5 thresholdAccuracy
      absErr = max (abs maxErr) (abs minErr)
      --minf   = minimumOptimised f (mpBall a) (mpBall b) 5 5 -- TODO requires f > 0
      {-maxErr = maximumOptimised (pf - 1) (mpBall a) (mpBall b) 5 5
      minErr = minimumOptimised (pf - 1) (mpBall a) (mpBall b) 5 5
      absErr = max (abs maxErr) (abs minErr)-}
      minf   = minimumOptimisedWithAccuracy fRed (mpBall a) (mpBall b) 5 5 thresholdAccuracy
    in
      absErr/minf

  refineUntilAccurate :: LineSegment -> Set (LineSegment, MPBall)
  refineUntilAccurate p@(LineSegment (a,_) (b, _)) =
    let
      serr = sampledError p
      err  = pieceError p
      threshold = pieceThreshold p
    in
    --trace("piece: "++(show a) ++ " " ++ (show b)) $
    --trace("error: "++(show err)) $
    --trace("sampled error: "++(show serr)) $
    --trace("threshold: "++(show threshold)) $
    if (serr < threshold) == Just True
    && (err < threshold)  == Just True then
      Set.singleton (p, err)
    else let refined = refinePiece p in
      (refineUntilAccurate $ Set.elemAt (int 0) refined) `Set.union` (refineUntilAccurate $ Set.elemAt (int 1) refined)
  refinePiece :: LineSegment -> Set LineSegment
  refinePiece (LineSegment (a,fa) (b,fb)) =
    let
      m  = 0.5*(a + b)
      fm = 1/(mpBall $ centre $ evalDirect f (mpBall m))
    in
      {-trace (
      "refining "++(show a) ++ " " ++(show b) ++"\n"++
      "with value "++(show fm)
      )-}
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


reduceDegreeToAccuracy d cutoffAccuracy g =
  let
    try = reduceDegree d g
  in
    {-trace("try: "++(show try)) $
    trace("degree: "++(show d)) $
    trace("accuracy: "++(show $ getAccuracy try)) $
    trace("target: "++(show $ min (getAccuracy g) cutoffAccuracy)) $-}
    if getAccuracy try >= min (getAccuracy g) cutoffAccuracy then
      try
    else
      --trace("trying with higher degree") $
      reduceDegreeToAccuracy (d + 5) cutoffAccuracy g
