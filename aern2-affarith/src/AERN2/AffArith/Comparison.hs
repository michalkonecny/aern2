{-# OPTIONS_GHC -Wno-orphans #-}

module AERN2.AffArith.Comparison
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
  lessThan aff1 n = mpBall (aff1 - n) <= 0
  leq aff1 n = mpBall (aff1 - n) < 0

instance HasOrderAsymmetric Integer MPAffine where
  type OrderCompareType Integer MPAffine = Kleenean
  lessThan n aff2 = mpBall (n - aff2) <= 0
  leq n aff2 = mpBall (n - aff2) < 0

instance HasOrderAsymmetric MPAffine Rational where
  type OrderCompareType MPAffine Rational = Kleenean
  lessThan aff1 q = mpBall (aff1 - q) <= 0
  leq aff1 q = mpBall (aff1 - q) < 0

instance HasOrderAsymmetric Rational MPAffine where
  type OrderCompareType Rational MPAffine = Kleenean
  lessThan q aff2 = mpBall (q - aff2) <= 0
  leq q aff2 = mpBall (q - aff2) < 0
