module AERN2.Frac.Eval
(
    evalDirect
  , evalDI
  , evalDf
  , evalLip
)
where

import MixedTypesNumPrelude
import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.RealFun.Operations

import AERN2.Poly.Cheb (ChPoly)
import qualified AERN2.Poly.Cheb as Cheb

import AERN2.Frac.Type

instance
  (CanApply (ChPoly c) t, CanDivSameType (ApplyType (ChPoly c) t))
  =>
  CanApply (Frac c) t
  where
  type ApplyType (Frac c) t = ApplyType (ChPoly c) t
  apply (Frac p q _) t = (apply p t) /! (apply q t)
    -- TODO: replace with a specific instance with c~MPBall using evalDI?

instance
  (CanApplyApprox (ChPoly c) t, CanDivSameType (ApplyApproxType (ChPoly c) t))
  =>
  CanApplyApprox (Frac c) t
  where
  type ApplyApproxType (Frac c) t = ApplyApproxType (ChPoly c) t
  applyApprox (Frac p q _) t = (applyApprox p t) /! (applyApprox q t)

evalDirect ::
  (Field t, CanAddSubMulDivCNBy t Dyadic, CanDivCNBy t Integer,
  CanAddSubMulBy t c, Ring c)
  => Frac c -> t -> t
evalDirect (Frac p q _) x =
  (Cheb.evalDirect p x) /! (Cheb.evalDirect q x)

evalDI :: Frac MPBall -> MPBall -> MPBall
evalDI f@(Frac p q _) =
  evalDf f (Cheb.derivative p) (Cheb.derivative q)

evalDf :: Frac MPBall -> ChPoly MPBall -> ChPoly MPBall -> MPBall -> MPBall
evalDf f dp dq x =
  evalLip f (abs $ Cheb.evalDirect dp x) (abs $ Cheb.evalDirect dq x) x

evalLip :: Frac MPBall -> MPBall -> MPBall -> MPBall -> MPBall
evalLip f@(Frac _ _ m) lp lq x =
  fc + (fromEndpoints (-errB) (errB) :: MPBall)
  where
  absFc = abs fc
  fc    = evalDirect f c
  c     = centreAsBall x
  eps   = mpBall $ radius x
  errB  = m*(lp + lq*absFc)*eps
