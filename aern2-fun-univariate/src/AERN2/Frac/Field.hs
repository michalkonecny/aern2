module AERN2.Frac.Field
where

import MixedTypesNumPrelude

import Control.CollectErrors

import AERN2.MP.Accuracy
import AERN2.MP.Ball
import AERN2.Interval
import AERN2.Frac.Type
import AERN2.Poly.Cheb as Cheb

import AERN2.RealFun.Operations

inverseWithLowerBound ::
  (Field a) => (ChPoly a) -> a -> Frac a
inverseWithLowerBound p@(ChPoly _ _ acG _) m =
  Frac one p (1/!m)
  where
  dom = getDomain p
  one = constFn (dom,acG) 1

instance CanDiv (Frac MPBall) (Frac MPBall) where
  type DivTypeNoCN (Frac MPBall) (Frac MPBall) = Frac MPBall
  type DivType (Frac MPBall) (Frac MPBall) = CN (Frac MPBall)
  divideNoCN f1 f2 = (~!) (divide f1 f2)
  divide (Frac p0 q0 _) (Frac p1 q1 _)
    | m !>! 0 =
        pure $ Frac (p0*q1) q0p1 (1/!m)
    | otherwise =
        noValueCN [(ErrorPotential, DivByZero)]
    where
    (Interval l r) = chPoly_dom p0
    q0p1 = q0*p1
    m =
      Cheb.minimumOptimisedWithAccuracy (bits 4) q0p1 (mpBall l) (mpBall r) 5 5

instance CanDiv Integer (Frac MPBall) where
  type DivTypeNoCN Integer (Frac MPBall) = Frac MPBall
  divideNoCN n f = divideNoCN nFR f
    where
    nFR = fromPoly $ constFn (dom, acG) n :: Frac MPBall
    dom = getDomain f
    acG = getAccuracyGuide f
  type DivType Integer (Frac MPBall) = CN (Frac MPBall)
  divide n f = divide nFR f
    where
    nFR = fromPoly $ constFn (dom, acG) n :: Frac MPBall
    dom = getDomain f
    acG = getAccuracyGuide f


instance
  (CanDiv (Frac c) b
  , CanEnsureCE es b
  , CanEnsureCE es (DivType (Frac c) b)
  , CanEnsureCE es (DivTypeNoCN (Frac c) b)
  , CanBeErrors es)
  =>
  CanDiv (Frac c) (CollectErrors es  b)
  where
  type DivType (Frac c) (CollectErrors es  b) =
    EnsureCE es (DivType (Frac c) b)
  divide = lift2TLCE divide
  type DivTypeNoCN (Frac c) (CollectErrors es  b) =
    EnsureCE es (DivTypeNoCN (Frac c) b)
  divideNoCN = lift2TLCE divideNoCN

instance
  (CanDiv a (Frac c)
  , CanEnsureCE es a
  , CanEnsureCE es (DivType a (Frac c))
  , CanEnsureCE es (DivTypeNoCN a (Frac c))
  , CanBeErrors es)
  =>
  CanDiv (CollectErrors es a) (Frac c)
  where
  type DivType (CollectErrors es  a) (Frac c) =
    EnsureCE es (DivType a (Frac c))
  divide = lift2TCE divide
  type DivTypeNoCN (CollectErrors es  a) (Frac c) =
    EnsureCE es (DivTypeNoCN a (Frac c))
  divideNoCN = lift2TCE divideNoCN
