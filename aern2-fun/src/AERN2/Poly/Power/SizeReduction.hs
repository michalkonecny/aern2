module AERN2.Poly.Power.SizeReduction where

import AERN2.Poly.Basics
import AERN2.Poly.Power.Type
import MixedTypesNumPrelude
import qualified Data.Map as Map
import AERN2.MP.Ball hiding (iterateUntilAccurate)

reduceDegree :: PowPoly MPBall -> MPBall -> MPBall -> Integer -> PowPoly MPBall
reduceDegree (PowPoly (Poly ts)) l r n =
  PowPoly $ Poly $
    Map.updateMin (\a -> Just $ a + errBall) $
    Map.filterWithKey (\k _ -> k <= n) ts
  where
  m = max (abs l) (abs r)
  errBall = hullMPBall (-err) err
  err = Map.foldlWithKey' (\s k c -> s + (abs c) * (m^!k)) (mpBall 0)
          $ Map.filterWithKey (\k _ -> k > n) ts

reduceDegreeI :: PowPoly MPBall -> Integer -> PowPoly MPBall
reduceDegreeI p =
  reduceDegree p (mpBall $ -1) (mpBall 1)
