{-# OPTIONS_GHC -Wno-orphans #-}

module AERN2.AffArith.Order
  (
  )
where

import AERN2.AffArith.Conversions ()
import AERN2.AffArith.Ring ()
import AERN2.AffArith.Type (MPAffine)
import AERN2.MP (Kleenean, mpBall)
import MixedTypesNumPrelude
-- import qualified Prelude as P

instance HasOrderAsymmetric MPAffine MPAffine where
  type OrderCompareType MPAffine MPAffine = Kleenean
  lessThan aff1 aff2 = mpBall (aff1 - aff2) <= 0
  leq aff1 aff2 = mpBall (aff1 - aff2) < 0

instance HasOrderAsymmetric MPAffine Integer where
  type OrderCompareType MPAffine Integer = Kleenean
  lessThan aff1 n = mpBall aff1 <= n
  leq aff1 n = mpBall aff1 < n

instance HasOrderAsymmetric Integer MPAffine where
  type OrderCompareType Integer MPAffine = Kleenean
  lessThan n aff2 = n <= mpBall aff2
  leq n aff2 = n < mpBall aff2

instance HasOrderAsymmetric MPAffine Int where
  type OrderCompareType MPAffine Int = Kleenean
  lessThan aff1 n = mpBall aff1 <= n
  leq aff1 n = mpBall aff1 < n

instance HasOrderAsymmetric Int MPAffine where
  type OrderCompareType Int MPAffine = Kleenean
  lessThan n aff2 = n <= mpBall aff2
  leq n aff2 = n < mpBall aff2

instance HasOrderAsymmetric MPAffine Rational where
  type OrderCompareType MPAffine Rational = Kleenean
  lessThan aff1 q = mpBall aff1 <= q
  leq aff1 q = mpBall aff1 < q

instance HasOrderAsymmetric Rational MPAffine where
  type OrderCompareType Rational MPAffine = Kleenean
  lessThan q aff2 = q <= mpBall aff2
  leq q aff2 = q < mpBall aff2
