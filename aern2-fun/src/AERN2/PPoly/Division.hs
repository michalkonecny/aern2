{-# LANGUAGE CPP #-}
-- #define DEBUG
module AERN2.PPoly.Division
(inverse, inverseWithAccuracy, initialApproximation)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import MixedTypesNumPrelude hiding (maximum, minimum)
import qualified Prelude

import Control.CollectErrors

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

import Math.NumberTheory.Logarithms (integerLog2)

-- import Debug.Trace
-- import Data.Maybe

instance CanDiv PPoly PPoly where -- TODO: support negative denominator
  type DivTypeNoCN PPoly PPoly = PPoly
  divideNoCN p q =
    p * inverse q
  type DivType PPoly PPoly = CN PPoly
  divide p q =
    cn $ p * inverse q -- TODO: detect divide by zero

instance CanDiv Integer PPoly where -- TODO: support negative denominator
  type DivTypeNoCN Integer PPoly = PPoly
  divideNoCN n q =
    n * inverse q
  type DivType Integer PPoly = CN PPoly
  divide n q =
    cn $ n * inverse q -- TODO: detect divide by zero

inverseWithAccuracy :: Accuracy -> PPoly -> PPoly
inverseWithAccuracy cutoff' f@(PPoly _ (Interval l r)) =
  maybeTrace("num its: "++(show numIts)) $
  maybeTrace("initial bits: "++(show bts)) $
  maybeTrace("cutoff: "++(show cutoff  )) $
  maybeTrace("getAccuracy f: "++(show $ getAccuracy f)) $
  maybeTrace("getAccuracy if0: "++(show $ getAccuracy if0)) $
  maybeTrace("getAccuracy fcInv: "++(show $ getAccuracy fcInv)) $
  updateRadius (+ radius f) fcInv
  where
  cutoff = min (getFiniteAccuracy f) cutoff'
  numIts = ((integer . integerLog2 . (`max` 1) . ceiling . (/! 10) . fromAccuracy) cutoff)
  fcInv = iterateInverse cutoff numIts fc (setPrecision (getPrecision f) if0)
  bts   = max (2 + (integer . integerLog2 . snd . integerBounds) bf) 0 -- (fromAccuracy cutoff) `Prelude.div` (2^!numIts)
  fc    =
          setAccuracyGuide ((2^!numIts)*cutoff) $
            centre f
  fRed0 = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 (bits 1)) fc
  fRed1 = fc--(liftCheb2PPoly $ reduceDegreeToAccuracy 5 (2*thresholdAccuracy)) fc
  bf    = abs $ AERN2.PPoly.Maximum.maximumOptimisedWithAccuracy fRed0 (mpBall l) (mpBall r) 5 5 (bits 4)
  threshold = (mpBall $ (dyadic 0.5)^!bts)/!(centreAsBall bf) -- 1/((2^bts)*(1 + (centreAsBall bf)))
  if0 =
    setAccuracyGuide ((2^!numIts)*cutoff) $
    initialApproximation fRed1 bts thresholdAccuracy
  thresholdAccuracy = 2 + 2*getAccuracy ((fromEndpoints (-threshold) (threshold)) :: MPBall)

inverse :: PPoly -> PPoly -- TODO: allow negative f
inverse f =
  inverseWithAccuracy (getAccuracyGuide f) f
  -- inverseWithAccuracy (getFiniteAccuracy f) f
  {-iterateInverse f (setPrecision (getPrecision f) if0)
  where
  fRed0 = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 (bits 4)) f
  fRed1 = f--(liftCheb2PPoly $ reduceDegreeToAccuracy 5 thresholdAccuracy) f
  bf   = abs $ AERN2.PPoly.Maximum.maximumOptimisedWithAccuracy fRed0 (mpBall l) (mpBall r) 5 5 (bits 4)
  threshold = 1/(1 + 4*bf)
  if0 = initialApproximation fRed1 thresholdAccuracy
  thresholdAccuracy = 2 + getAccuracy ((fromEndpoints (mpBall 0) (threshold)) :: MPBall)-}

iterateInverse :: Accuracy -> Integer -> PPoly -> PPoly -> PPoly
iterateInverse cutoff n f if0 =
  --aux (newton if0)
  PPoly
    [let
     bfp = maximumOptimisedWithAccuracy fRed (mpBall $ fromUnitIntervalToDom l) (mpBall $ fromUnitIntervalToDom r) 5 5 (bits 4)
     in
     (i, aux'' i pg (getAccuracy pg) pf bfp n) | (i@(Interval l r),pg,pf) <- refine if0 f]
    (ppoly_dom f)
  where
  fromUnitIntervalToDom x = (dyadic 0.5)*((r - l)*x + (r + l)) where (Interval l r) = ppoly_dom f
  fRed = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 (bits 4)) f
  aux'' i ipn ipnAc pf bfp k =
    -- maybeTrace ("i = " ++ show i) $
    -- maybeTrace ("ipnAc = " ++ show ipnAc) $
    -- maybeTrace ("ipnAc' = " ++ show ipnAc') $
    if (ipnAc' <= ipnAc) || ipnAc >= cutoff then
      ipn
    else
      aux'' i ipn' ipnAc' pf bfp (k - 1)
    where
    ipn' = newtonPiece ipn pf bfp
    ipnAc' = getAccuracy ipn'
  -- aux' ipn pf bfp =
  --   let
  --     next = {-reduce $-} newtonPiece ipn pf bfp
  --     nextAccuracy = getAccuracy next
  --   in
  --     if nextAccuracy >= cutoff then
  --       next
  --     else if nextAccuracy <= getAccuracy ipn then
  --       ipn
  --     else
  --       aux' next pf bfp
  -- aux ifn =
  --   let
  --     next = newton ifn
  --   in
  --     {-trace (
  --     "next iterate degree "++(show $ (degree . centre . snd . head . ppoly_pieces) next)++"\n"++
  --     "accuracy: "++(show $ getAccuracy next)
  --     ) $-}
  --     if getAccuracy next <= getAccuracy ifn then
  --       ifn
  --     else
  --       aux next
  -- reduce :: PolyBall -> PolyBall
  -- reduce p =
  --   aux' (((ballLift1R degree) p) `Prelude.div` 2)
  --   where
  --   aux' d =
  --     let
  --       red = setPrecision (getPrecision p) $ normalize $ Ball ((ballLift1R $ reduceDegree d) p) (errorBound 0)
  --       redRad = radius red
  --       pRad   = radius p
  --     in
  --       {-trace (
  --       "reducing..."++(show d)++"\n"++
  --       "reduced accuracy: "++(show $ getAccuracy red)++"\n"++
  --       "original accuracy: "++(show $ getAccuracy p)
  --       ) $-}
  --       if d >= (ballLift1R degree) p
  --       {-- || getAccuracy red >= max (bits 5) (getAccuracy p - (bits 2))-}
  --         || redRad <= 4*pRad
  --       then
  --         red
  --       else
  --         aux' (d + 10)
  newtonError :: PolyBall -> MPBall -> ErrorBound
  newtonError pg bfp =
    let
    rg = radius pg
    in
    errorBound bfp*rg*rg
  newtonPiece :: PolyBall -> PolyBall -> MPBall -> PolyBall
  newtonPiece pg pf bfp =
    let
      cg =
        setPrecision (getPrecision f) $
        setAccuracyGuide (getAccuracyGuide f) $
          centreAsBall pg
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

initialApproximation :: PPoly -> Integer -> Accuracy -> PPoly
initialApproximation f@(PPoly _ dom@(Interval l r)) bts thresholdAccuracy {-bf-} =
  result
  where
  fromUnitIntervalToDom x = (dyadic 0.5)*((r - l)*x + (r + l))
  fRed = (liftCheb2PPoly $ reduceDegreeToAccuracy 5 thresholdAccuracy) f
  --thresholdAccuracy = 2 + getAccuracy ((fromEndpoints (mpBall 0) threshold) :: MPBall)
  PPoly linps _ = linearPolygonI ((lsFst $ head nodes) : (map lsSnd nodes)) dom thresholdAccuracy
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
    LineSegment (dyadic $ -1, mpBall $ centre $ 1/!(evalDirect f (mpBall l)))
                (dyadic 1, mpBall $ centre $ 1/!(evalDirect f (mpBall r)))
  nodes = map fst nodesNErrs
  errs = map snd nodesNErrs
  {-minf =
      minimumOptimised f (mpBall l) (mpBall r) 5 5 -- TODO requires f > 0-}
  {-sampledError (LineSegment (a,fa) (b,fb)) =
    let
      p      = lineSegment ((a,fa), (b,fb))
      errs   =
        [let
          x = (a + k*(setPrecision (getPrecision f) $ mpBall b - a)/16)
         in
          abs $ 1/(evalDirect f x) - evalDirect p x
          | k <- [1..16] ]
      absErr = foldl' max (mpBall 0) errs
      --minf   = minimumOptimised f (mpBall a) (mpBall b) 5 5 -- TODO requires f > 0
    in
      absErr-}
  bf = maximumOptimisedWithAccuracy fRed (mpBall l) (mpBall r) 5 5 (bits 1)
  pieceThreshold (LineSegment (a, fa) (b,fb)) =
    mpBall $ (dyadic 0.5)^!bts--1/((2^bts)*(1 + bf))
    {-let
    bfp = maximumOptimisedWithAccuracy fRed (mpBall a) (mpBall b) 5 5 (bits 4)
    in
    1/(1 + 2*bfp)-}
  pieceError (LineSegment (aI, fa) (bI, fb)) =
    let
      a      = fromUnitIntervalToDom aI
      b      = fromUnitIntervalToDom bI
      p      = lineSegment ((aI, fa), (bI,fb))
      pf     = p*f --PPoly.multiplyWithBounds p (mpBall 2) f bf
      maxErr = maximumOptimisedWithAccuracy (pf - 1) (mpBall a) (mpBall b) 5 5 thresholdAccuracy
      minErr = minimumOptimisedWithAccuracy (pf - 1) (mpBall a) (mpBall b) 5 5 thresholdAccuracy
      absErr = max (abs maxErr) (abs minErr)
      --minf   = minimumOptimised f (mpBall a) (mpBall b) 5 5 -- TODO requires f > 0
      {-maxErr = maximumOptimised (pf - 1) (mpBall a) (mpBall b) 5 5
      minErr = minimumOptimised (pf - 1) (mpBall a) (mpBall b) 5 5
      absErr = max (abs maxErr) (abs minErr)-}
      minf   = minimumOptimisedWithAccuracy f (mpBall a) (mpBall b) 5 5 thresholdAccuracy
    in
        absErr/!minf

  refineUntilAccurate :: LineSegment -> Set (LineSegment, MPBall)
  refineUntilAccurate p@(LineSegment (a,_) (b, _)) =
    let
      --serr = sampledError p
      err  = pieceError p
      threshold = pieceThreshold p
    in
    --trace("piece: "++(show a) ++ " " ++ (show b)) $
    --trace("error: "++(show err)) $
    --trace("sampled error: "++(show serr)) $
    --trace("threshold: "++(show threshold)) $
    if {-}(serr < threshold) == Just True
    && -}(err < threshold)  == Just True then
      Set.singleton (p, err)
    else let refined = refinePiece p in
      (refineUntilAccurate $ Set.elemAt (int 0) refined) `Set.union` (refineUntilAccurate $ Set.elemAt (int 1) refined)
  refinePiece :: LineSegment -> Set LineSegment
  refinePiece (LineSegment (a,fa) (b,fb)) =
    let
      m  = (dyadic 0.5)*(a + b)
      fm = 1/!((mpBall . centre . evalDirect f . mpBall . fromUnitIntervalToDom) m)
    in
      Set.fromList [LineSegment (a,fa) (m, fm), LineSegment (m, fm) (b, fb)]
  lineSegment ((a,fa), (b, fb)) =
    if a /= -1 && b /= 1 then
      linearPolygonI [(dyadic $ -1, fa), (a, fa), (b, fb), (dyadic 1, fb)] dom thresholdAccuracy
    else if a /= -1 && b == 1 then
      linearPolygonI [(dyadic $ -1, fa), (a, fa), (b, fb)] dom thresholdAccuracy
    else if a == -1 && b /= 1 then
      linearPolygonI [(a, fa), (b, fb), (dyadic 1, fb)] dom thresholdAccuracy
    else
      linearPolygonI [(a,fa), (b,fb)] dom thresholdAccuracy

{- -}
{-initialApproximation' :: PPoly -> PPoly -- for now approximation up to error 1/(2|P(x)|)
initialApproximation' f@(PPoly _ dom@(Interval l r)) =
  undefined
  where
  bf = maximumOptimisedWithAccuracy f (mpBall l) (mpBall r) 5 5 (bits 3) -- TODO: reduce f?
  lip = 10000 -- TODO: compute real lipschitz constant
  logbf = integer $ integerLog2 $ snd $ integerBounds bf

  lineSegment ((a,fa), (b, fb)) =
    if a /=  l && b /= r then
      linearPolygon [(l, fa), (a, fa), (b, fb), (r, fb)] dom
    else if a /= l && b == r then
      linearPolygon [(l, fa), (a, fa), (b, fb)] dom
    else if a == l && b /= r then
      linearPolygon [(a, fa), (b, fb), (r, fb)] dom
    else
      linearPolygon [(a,fa), (b,fb)] dom-}


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


instance
  (CanDiv PPoly b
  , CanEnsureCE es b
  , CanEnsureCE es (DivType PPoly b)
  , CanEnsureCE es (DivTypeNoCN PPoly b)
  , SuitableForCE es)
  =>
  CanDiv PPoly (CollectErrors es  b)
  where
  type DivType PPoly (CollectErrors es  b) =
    EnsureCE es (DivType PPoly b)
  divide = lift2TLCE divide
  type DivTypeNoCN PPoly (CollectErrors es  b) =
    EnsureCE es (DivTypeNoCN PPoly b)
  divideNoCN = lift2TLCE divideNoCN

instance
  (CanDiv a PPoly
  , CanEnsureCE es a
  , CanEnsureCE es (DivType a PPoly)
  , CanEnsureCE es (DivTypeNoCN a PPoly)
  , SuitableForCE es)
  =>
  CanDiv (CollectErrors es a) PPoly
  where
  type DivType (CollectErrors es  a) PPoly =
    EnsureCE es (DivType a PPoly)
  divide = lift2TCE divide
  type DivTypeNoCN (CollectErrors es  a) PPoly =
    EnsureCE es (DivTypeNoCN a PPoly)
  divideNoCN = lift2TCE divideNoCN
