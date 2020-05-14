module AERN2.Local.MinMax where

import MixedTypesNumPrelude
import AERN2.Local.Basics

instance (CanMinMaxAsymmetric a b) => CanMinMaxAsymmetric (Local a) (Local b) where
  type MinMaxType (Local a) (Local b) = Local (MinMaxType a b)
  max = liftLocal2 max
  min = liftLocal2 min
