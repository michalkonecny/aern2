{-# LANGUAGE TemplateHaskell #-}
module AERN2.Frac.Ring where

import Numeric.MixedTypes
import AERN2.MP.Ball
import AERN2.Poly.Basics
import AERN2.Poly.Cheb
import AERN2.Frac.Type

import AERN2.MP.Dyadic
import AERN2.Real

import AERN2.Utils.TH

instance (PolyCoeffRing a, a ~ MPBall)
  => CanMulAsymmetric (Frac a) (Frac a)
  where
  type MulType (Frac a) (Frac a) = Frac a
  mul (Frac p0 q0 m0) (Frac p1 q1 m1) =
    Frac (p0 * p1) (q0 * q1) (m0 * m1)

instance (PolyCoeffRing a, a ~ MPBall)
  => CanMulAsymmetric (ChPoly a) (Frac a)
  where
  type MulType (ChPoly a) (Frac a) = Frac a
  mul f g =
    fromPoly f * g

instance (PolyCoeffRing a, a ~ MPBall)
  => CanAddAsymmetric (Frac a) (Frac a) where
  type AddType (Frac a) (Frac a) = Frac a
  add (Frac p0 q0 m0) (Frac p1 q1 m1) =
    Frac (p0*q1 + p1*q0) (q0*q1) (m0 * m1)

instance (CanNegSameType (ChPoly a)) => CanNeg (Frac a) where
  type NegType (Frac a) = Frac a
  negate (Frac p q m) = Frac (-p) q m

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy (ChPoly c) $t) => CanMulAsymmetric $t (Frac c) where
      type MulType $t (Frac c) = Frac c
      mul n (Frac p q dIM) = Frac (n*p) q dIM

    instance (CanMulBy (ChPoly c) $t) => CanMulAsymmetric (Frac c) $t where
      type MulType (Frac c) $t = Frac c
      mul (Frac p q dIM) n = Frac (p*n) q dIM
  |]))

instance CanAddAsymmetric MPBall (Frac MPBall) where
    type AddType MPBall (Frac MPBall) = Frac MPBall
    add n (Frac p q dIM) = Frac (n*q + p) q dIM

instance CanAddAsymmetric Integer (Frac MPBall) where
    type AddType Integer (Frac MPBall) = Frac MPBall
    add n (Frac p q dIM) = Frac (n*q + p) q dIM
