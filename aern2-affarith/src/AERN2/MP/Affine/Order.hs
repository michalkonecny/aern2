module AERN2.MP.Affine.Order
  (
  )
where

import AERN2.MP.Affine.Conversions (mpAffineFromBall, mpAffineWithSample)
import AERN2.MP.Affine.Ring ()
import AERN2.MP.Affine.Type (MPAffine, ErrorTermId (..))
import AERN2.MP (Kleenean, mpBall)
import MixedTypesNumPrelude
import Data.Hashable (Hashable(hash))
-- import qualified Prelude as P

instance HasOrderAsymmetric MPAffine MPAffine where
  type OrderCompareType MPAffine MPAffine = Kleenean
  lessThan aff1 aff2 = mpBall (aff1 - aff2) < 0
  leq aff1 aff2 = mpBall (aff1 - aff2) <= 0

instance HasOrderAsymmetric MPAffine Integer where
  type OrderCompareType MPAffine Integer = Kleenean
  lessThan aff1 n = mpBall aff1 < n
  leq aff1 n = mpBall aff1 <= n

instance HasOrderAsymmetric Integer MPAffine where
  type OrderCompareType Integer MPAffine = Kleenean
  lessThan n aff2 = n < mpBall aff2
  leq n aff2 = n <= mpBall aff2

instance HasOrderAsymmetric MPAffine Int where
  type OrderCompareType MPAffine Int = Kleenean
  lessThan aff1 n = mpBall aff1 < n
  leq aff1 n = mpBall aff1 <= n

instance HasOrderAsymmetric Int MPAffine where
  type OrderCompareType Int MPAffine = Kleenean
  lessThan n aff2 = n < mpBall aff2
  leq n aff2 = n <= mpBall aff2

instance HasOrderAsymmetric MPAffine Rational where
  type OrderCompareType MPAffine Rational = Kleenean
  lessThan aff1 q = mpBall aff1 < q
  leq aff1 q = mpBall aff1 <= q

instance HasOrderAsymmetric Rational MPAffine where
  type OrderCompareType Rational MPAffine = Kleenean
  lessThan q aff2 = q < mpBall aff2
  leq q aff2 = q <= mpBall aff2

instance CanTestPosNeg MPAffine where

instance CanAbs MPAffine where
  abs aff 
    | aff !>=! 0 = aff
    | aff !<! 0 = -aff
    | otherwise = mpAffineFromBall aff newTermId (abs (mpBall aff))
    where
      newTermId = ErrorTermId (hash ("abs", aff))

instance CanMinMaxAsymmetric MPAffine MPAffine where
  min aff1 aff2
    | aff1 !<=! aff2 = aff1
    | aff2 !<=! aff1 = aff2
    | otherwise = mpAffineFromBall aff1 newTermId (min (mpBall aff1) (mpBall aff2))
    where
      newTermId = ErrorTermId (hash ("min", aff1, aff2))
  max aff1 aff2 = negate $ min (-aff1) (-aff2)

instance CanMinMaxAsymmetric MPAffine Integer where
  min aff n = min aff (mpAffineWithSample aff n)
  max aff n = max aff (mpAffineWithSample aff n) 

instance CanMinMaxAsymmetric Integer MPAffine where
  type MinMaxType Integer MPAffine = MPAffine
  min n aff = min (mpAffineWithSample aff n) aff
  max n aff = max (mpAffineWithSample aff n) aff
