module AERN2.Frac.Field
where

import Numeric.MixedTypes
import AERN2.MP.Accuracy
import AERN2.MP.Ball
import AERN2.Interval
import AERN2.Frac.Type
import AERN2.Poly.Cheb as Cheb
import AERN2.Poly.Basics as Poly

inverseWithLowerBound ::
  (Field a) => (ChPoly a) -> a -> Frac a
inverseWithLowerBound p m =
  Frac one p (1/m)
  where
  dom = chPoly_dom p
  one = ChPoly dom (Poly $ Poly.terms_fromList [(0, convertExactly 1)]) Nothing

instance CanDiv (Frac MPBall) (Frac MPBall) where
  type DivType (Frac MPBall) (Frac MPBall) = Frac MPBall
  divide (Frac p0 q0 _) (Frac p1 q1 _) =
    Frac (p0*q1) q0p1 (1/m)
    where
    (Interval l r) = chPoly_dom p0
    q0p1 = q0*p1
    m =
      Cheb.minimumOptimisedWithAccuracy (bits 1) q0p1 (mpBall l) (mpBall r) 5 5
