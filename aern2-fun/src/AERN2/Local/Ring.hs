{-# LANGUAGE TemplateHaskell #-}
module AERN2.Local.Ring where

import MixedTypesNumPrelude
import AERN2.Local.Basics
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Real



instance (CanAddAsymmetric a b) =>
  (CanAddAsymmetric (Local a) (Local b))
  where
    type AddType (Local a) (Local b) = Local (AddType a b)
    add = liftLocal2 add

instance (CanMulAsymmetric a b) =>
  (CanMulAsymmetric (Local a) (Local b))
  where
    type MulType (Local a) (Local b) = Local (MulType a b)
    mul = liftLocal2 mul

instance (CanSub a b) =>
  (CanSub (Local a) (Local b))
  where
    type SubType (Local a) (Local b) = Local (SubType a b)
    sub = liftLocal2 sub

instance (CanNeg a) => (CanNeg (Local a)) where
  type NegType (Local a) = Local (NegType a)
  negate = liftLocal1 negate

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulAsymmetric $t a) => (CanMulAsymmetric $t (Local a)) where
        type MulType $t (Local a) = Local (MulType $t a)
        mul c = liftLocal1 (mul c)

    instance (CanMulAsymmetric a $t) => (CanMulAsymmetric (Local a) $t) where
        type MulType (Local a) $t = Local (MulType a $t)
        mul f c = liftLocal1 (`mul` c) f

    instance (CanAddAsymmetric $t a) => (CanAddAsymmetric $t (Local a)) where
        type AddType $t (Local a) = Local (AddType $t a)
        add c = liftLocal1 (add c)

    instance (CanAddAsymmetric a $t) => (CanAddAsymmetric (Local a) $t) where
        type AddType (Local a) $t = Local (AddType a $t)
        add f c = liftLocal1 (`add` c) f

    instance (CanSub $t a) => (CanSub $t (Local a)) where
        type SubType $t (Local a) = Local (SubType $t a)
        sub c = liftLocal1 (sub c)

    instance (CanSub a $t) => (CanSub (Local a) $t) where
        type SubType (Local a) $t = Local (SubType a $t)
        sub f c = liftLocal1 (`sub` c) f
  |]))
