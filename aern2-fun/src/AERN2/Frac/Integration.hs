module AERN2.Frac.Integration where

import AERN2.MP.Ball

import AERN2.Frac.Type
import AERN2.Frac.Conversion
import AERN2.PPoly.Integration as PPoly

integral :: Frac MPBall -> MPBall -> MPBall -> MPBall
integral f =
  PPoly.integral (toPPoly f)
