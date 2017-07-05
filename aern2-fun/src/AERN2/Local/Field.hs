{-# LANGUAGE TemplateHaskell #-}
module AERN2.Local.Field where

import MixedTypesNumPrelude

import AERN2.Local.Basics

import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Real



instance (CanDiv a b) => (CanDiv (Local a) (Local b)) where
  type DivType (Local a) (Local b) = Local (DivType a b)
  divide = liftLocal2 divide
  type DivTypeNoCN (Local a) (Local b) = Local (DivTypeNoCN a b)
  divideNoCN = liftLocal2 divideNoCN

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanDiv $t a) => (CanDiv $t (Local a)) where
        type DivType $t (Local a) = Local (DivType $t a)
        divide c = liftLocal1 (divide c)
        type DivTypeNoCN $t (Local a) = Local (DivTypeNoCN $t a)
        divideNoCN c = liftLocal1 (divideNoCN c)
  |]))
