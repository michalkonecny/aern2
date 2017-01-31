module AERN2.Frac.Eval
(
    evalDirect
  , evalDI
  , evalDf
  , evalLip
)
where

import Numeric.MixedTypes
import AERN2.MP.Dyadic
import AERN2.MP.Ball
import AERN2.Frac.Type
import AERN2.Poly.Cheb (ChPoly)
import qualified AERN2.Poly.Cheb as Cheb

evalDirect ::
  (Field t, CanAddSubMulDivBy t Dyadic, CanDivBy t Integer,
  CanAddSubMulBy t c, Ring c)
  => Frac c -> t -> t
evalDirect (Frac p q _) x =
  (Cheb.evalDirect p x) / (Cheb.evalDirect q x)

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
