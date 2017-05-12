module AERN2.Analytic.Field
where

import Numeric.MixedTypes

import AERN2.Analytic.Type

instance CanAddAsymmetric Analytic Analytic where
  type AddType Analytic Analytic = Analytic
  add (Analytic a k a_n) (Analytic b l b_n) =
    Analytic c m c_n
    where
    c_n n = a_n n + b_n n
    c     = a + b
    m     = max k l
