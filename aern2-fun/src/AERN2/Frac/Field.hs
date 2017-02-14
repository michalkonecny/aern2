module AERN2.Frac.Field
where

import Numeric.MixedTypes
import AERN2.MP.Accuracy
import AERN2.MP.Ball
import AERN2.Interval
import AERN2.Frac.Type
import AERN2.Poly.Cheb as Cheb

import AERN2.RealFun.Operations

inverseWithLowerBound ::
  (Field a) => (ChPoly a) -> a -> Frac a
inverseWithLowerBound p m =
  Frac one p (1/m)
  where
  dom = getDomain p
  one = chPoly (dom,1)

instance CanDiv (Frac MPBall) (Frac MPBall) where
  type DivType (Frac MPBall) (Frac MPBall) = Frac MPBall
  divide (Frac p0 q0 _) (Frac p1 q1 _) =
    Frac (p0*q1) q0p1 (1/m)
    where
    (Interval l r) = chPoly_dom p0
    q0p1 = q0*p1
    m =
      Cheb.minimumOptimisedWithAccuracy (bits 4) q0p1 (mpBall l) (mpBall r) 5 5

instance CanDiv Integer (Frac MPBall) where
  type DivType Integer (Frac MPBall) = Frac MPBall
  divide n f = divide nFR f
    where
    nFR = fromPoly $ chPoly (dom,n) :: Frac MPBall
    dom = getDomain f
