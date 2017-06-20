module AERN2.Frac.Conversion where

import MixedTypesNumPrelude
import AERN2.MP.Ball

import AERN2.Frac.Type
import AERN2.PPoly.Type as PPoly
import AERN2.PPoly.Division

toPPoly :: Frac MPBall -> PPoly
toPPoly (Frac p q _) =
  (PPoly.fromPoly p) * (inverse . PPoly.fromPoly) q
