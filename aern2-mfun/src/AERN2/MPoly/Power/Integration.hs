module AERN2.MPoly.Power.Integration where

import MixedTypesNumPrelude
import AERN2.Util.Vector ((!), (!.), vlength)
import AERN2.MP.Dyadic

import qualified AERN2.Util.Vector as V
import AERN2.MPoly.Type (MPoly)
import qualified AERN2.MPoly.Type as MPoly
import AERN2.MPoly.Power.Type
import AERN2.Box (Box)
import qualified AERN2.Box as Box
import AERN2.Interval

primitiveInVariable ::
  (MPolyCoeff a)
  => PowMPoly a -> Integer -> PowMPoly a
primitiveInVariable (PowMPoly dom p acG) j =
  sweepAndNormalise $ PowMPoly dom p' acG
  where
  p' = (MPoly.fromList . map monomialPrimitive . MPoly.toList) p
  monomialPrimitive (i, a) =
    (i', a /! (1 + i !. j))
    where
    i' = V.generate (V.length i) (\k -> if k == j then (i ! k) + 1 else (i ! k))

integrateVariable ::
  (MPolyCoeff a) =>
  PowMPoly a -> Integer -> a -> a -> PowMPoly a
integrateVariable (PowMPoly dom p acG) j l r =
  sweepAndNormalise $ PowMPoly dom' p' acG
  where
  dom' = Box.remove j dom
  p' = (MPoly.fromListWith (+) . map monomialIntegral . MPoly.toList) p
  monomialIntegral (i, a) =
    (i', (a /! k) *(r^!k - l^!k))
    where
    k  = 1 + i !.j
    i' =
      V.generate (int $ V.length i - 1)
                 (\s -> if s < j then (i ! s) else (i !. (s + 1)))

integrateOverBox ::
  (MPolyCoeff a) =>
  PowMPoly a -> Box Dyadic -> a
integrateOverBox p b =
  if dim p == 0 then
    coef p (V.empty)
  else
    integrateOverBox
    (integrateVariable p 0 (convertExactly l) (convertExactly r))
    (Box.remove 0 b)
    where
    (Interval l r) = b !. 0
