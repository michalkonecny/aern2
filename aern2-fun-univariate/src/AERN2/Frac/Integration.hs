module AERN2.Frac.Integration where

import AERN2.MP.Ball

import AERN2.Frac.Type
import AERN2.Frac.Conversion
import qualified AERN2.PPoly.Integration as PPoly
import AERN2.RealFun.Operations
import AERN2.Interval

integral :: Frac MPBall -> MPBall -> MPBall -> MPBall
integral f =
  PPoly.integral (toPPoly f)


instance CanIntegrateOverDom (Frac MPBall) DyadicInterval where
  type IntegralOverDomType (Frac MPBall) DyadicInterval = MPBall
  integrateOverDom f (Interval l r) =
    integral f (mpBall l) (mpBall r)
