module AERN2.Frac.Ring where

import Numeric.MixedTypes
import AERN2.MP.Ball
import AERN2.Poly.Basics
import AERN2.Poly.Cheb
import AERN2.Frac.Type

instance (PolyCoeffRing a, a ~ MPBall)
  => CanMulAsymmetric (Frac a) (Frac a)
  where
  type MulType (Frac a) (Frac a) = Frac a
  mul (Frac p0 q0 m0) (Frac p1 q1 m1) =
    Frac (p0 * p1) (q0 * q1) (m0 * m1)

instance (PolyCoeffRing a, a ~ MPBall)
  => CanAddAsymmetric (Frac a) (Frac a) where
  type AddType (Frac a) (Frac a) = Frac a
  add (Frac p0 q0 m0) (Frac p1 q1 m1) =
    Frac (p0*q1 + p1*q0) (q0*q1) (m0 * m1)

instance (CanNegSameType (ChPoly a)) => CanNeg (Frac a) where
  type NegType (Frac a) = Frac a
  negate (Frac p q m) = Frac (-p) q m
