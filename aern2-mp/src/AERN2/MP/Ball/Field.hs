{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-|
    Module      :  AERN2.MP.Ball.Field
    Description :  Field operations on arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Field
(mulBalls, mulByEndpoints)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import qualified Numeric.CollectErrors as CN

import AERN2.Normalize

import AERN2.MP.Dyadic (Dyadic)
import AERN2.MP.Float.Operators
import AERN2.MP.Precision
-- import qualified AERN2.MP.ErrorBound as EB

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons (hullMPBall)

{- addition -}

instance CanAddAsymmetric MPBall MPBall where
  type AddType MPBall MPBall = MPBall
  add (MPBall a1) (MPBall a2) = normalize $ (MPBall (a1 P.+ a2))

instance CanAddAsymmetric MPBall Int where
  type AddType MPBall Int = MPBall
  add = convertSecond add
instance CanAddAsymmetric Int MPBall where
  type AddType Int MPBall = MPBall
  add = convertFirst add

instance CanAddAsymmetric MPBall Integer where
  type AddType MPBall Integer = MPBall
  add = convertSecond add
instance CanAddAsymmetric Integer MPBall where
  type AddType Integer MPBall = MPBall
  add = convertFirst add

instance CanAddAsymmetric MPBall Dyadic where
  type AddType MPBall Dyadic = MPBall
  add = convertSecond add
instance CanAddAsymmetric Dyadic MPBall where
  type AddType Dyadic MPBall = MPBall
  add = convertFirst add

instance CanAddAsymmetric MPBall Rational where
  type AddType MPBall Rational = MPBall
  add = convertPSecond add
instance CanAddAsymmetric Rational MPBall where
  type AddType Rational MPBall = MPBall
  add = convertPFirst add

instance
  (CanAddAsymmetric MPBall b)
  =>
  CanAddAsymmetric MPBall (CN b)
  where
  type AddType MPBall (CN b) = CN (AddType MPBall b)
  add = CN.liftT1 add

instance
  (CanAddAsymmetric a MPBall)
  =>
  CanAddAsymmetric (CN a) MPBall
  where
  type AddType (CN a) MPBall = CN (AddType a MPBall)
  add = CN.lift1T add

{- subtraction -}

instance CanSub MPBall MPBall

instance CanSub MPBall Integer
instance CanSub Integer MPBall

instance CanSub MPBall Int
instance CanSub Int MPBall

instance CanSub MPBall Rational
instance CanSub Rational MPBall

instance CanSub MPBall Dyadic
instance CanSub Dyadic MPBall

instance
  (CanSub MPBall b)
  =>
  CanSub MPBall (CN b)
  where
  type SubType MPBall (CN b) = CN (SubType MPBall b)
  sub = CN.liftT1 sub

instance
  (CanSub a MPBall)
  =>
  CanSub (CN a) MPBall
  where
  type SubType (CN a) MPBall = CN (SubType a MPBall)
  sub = CN.lift1T sub

{- multiplication -}

instance CanMulAsymmetric MPBall MPBall where
  mul = mulBalls
  -- mul = mulByEndpoints

mulBalls :: MPBall -> MPBall -> MPBall
mulBalls (MPBall a1) (MPBall a2) = normalize $ (MPBall (a1 P.* a2))

mulByEndpoints :: MPBall -> MPBall -> MPBall
mulByEndpoints b1 b2 =
  fromEndpoints l r
  where
  (l,r)
    | 0 <= l1 && 0 <= l2 = (l1*.l2, r1*^r2) -- 0 <= l1 <= r1, 0 <= l2 <= r2
    | r1 <= 0 && r2 <= 0 = (r1*.r2, l1*^l2) -- l1 <= r1 <= 0, l2 <= r2 <= 0
    | 0 <= l1 && r2 <= 0 = (r1*.l2, l1*^r2) -- l2 <= r2 <= 0 <= l1 <= r1
    | r1 <= 0 && 0 <= l2 = (l1*.r2, r1*^l2) -- l1 <= r1 <= 0 <= l2 <= r2
    | l1 < 0 && 0 < r1 && 0 <= l2 = (l1*.r2, r1*^r2) -- l1 < 0 < r1, 0 <= l2 <= r2
    | l1 < 0 && 0 < r1 && r2 <= 0 = (r1*.l2, l1*^l2) -- l1 < 0 < r1, l2 <= r2 <= 0
    | l2 < 0 && 0 < r2 && 0 <= l1 = (l2*.r1, r2*^r1) -- l2 < 0 < r2, 0 <= l1 <= r1
    | l2 < 0 && 0 < r2 && r1 <= 0 = (r2*.l1, l2*^l1) -- l2 < 0 < r2, l1 <= r1 <= 0
    | otherwise = -- l1 < 0 < r1, l2 < 0 < r2
      ((l1 *. r2) `min` (r1 *. l2)
      ,(l1 *^ l2) `max` (r1 *^ r2))
  (l1,r1) = endpoints b1
  (l2,r2) = endpoints b2


instance CanMulAsymmetric MPBall Int where
  type MulType MPBall Int = MPBall
  mul = convertSecond mul
instance CanMulAsymmetric Int MPBall where
  type MulType Int MPBall = MPBall
  mul = convertFirst mul

instance CanMulAsymmetric MPBall Integer where
  type MulType MPBall Integer = MPBall
  mul = convertSecond mul
instance CanMulAsymmetric Integer MPBall where
  type MulType Integer MPBall = MPBall
  mul = convertFirst mul

instance CanMulAsymmetric MPBall Dyadic where
  type MulType MPBall Dyadic = MPBall
  mul = convertSecond mul
instance CanMulAsymmetric Dyadic MPBall where
  type MulType Dyadic MPBall = MPBall
  mul = convertFirst mul

instance CanMulAsymmetric MPBall Rational where
  type MulType MPBall Rational = MPBall
  mul = convertPSecond mul
instance CanMulAsymmetric Rational MPBall where
  type MulType Rational MPBall = MPBall
  mul = convertPFirst mul

instance
  (CanMulAsymmetric MPBall b)
  =>
  CanMulAsymmetric MPBall (CN b)
  where
  type MulType MPBall (CN b) = CN (MulType MPBall b)
  mul = CN.liftT1 mul

instance
  (CanMulAsymmetric a MPBall)
  =>
  CanMulAsymmetric (CN a) MPBall
  where
  type MulType (CN a) MPBall = CN (MulType a MPBall)
  mul = CN.lift1T mul


{- division -}

instance CanDiv MPBall MPBall where
  type DivType MPBall MPBall = MPBall
  divide (MPBall a1) (MPBall a2) = normalize $ (MPBall (a1 P./ a2))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |]]
  (\ t -> [d|
    instance CanDiv MPBall $t where
      type DivType MPBall $t = MPBall
      divide = convertSecond divide
    instance CanDiv $t MPBall where
      type DivType $t MPBall = MPBall
      divide = convertFirst divide
  |]))

instance CanDiv Dyadic Dyadic where
  type DivType Dyadic Dyadic = MPBall
  divide a b = divide (mpBall a) (mpBall b)

instance CanDiv MPBall Rational where
  type DivType MPBall Rational = MPBall
  divide = convertPSecond divide
instance CanDiv Rational MPBall where
  type DivType Rational MPBall = MPBall
  divide = convertPFirst divide

instance
  (CanDiv MPBall b, CanTestZero b)
  =>
  CanDiv MPBall (CN b)
  where
  type DivType MPBall (CN b) = CN (DivType MPBall b)
  divide a b = divide (cn a) b

instance
  (CanDiv a MPBall)
  =>
  CanDiv (CN a) MPBall
  where
  type DivType (CN a) MPBall = CN (DivType a MPBall)
  divide a b = divide a (cn b)

{- integer power -}

instance CanPow MPBall Integer where
  pow = powUsingMulRecipCutNeg (mpBall 1)

instance CanPow MPBall Int where
  pow = powUsingMulRecipCutNeg (mpBall 1)

powUsingMulRecipCutNeg :: _ => MPBall -> MPBall -> e -> MPBall
powUsingMulRecipCutNeg one x e
  | even e =
      max 0 $ powUsingMulRecip one mulByEndpoints recip x e
  | otherwise = powUsingMulRecip one mulByEndpoints recip x e

instance
  (CanPow MPBall b)
  =>
  CanPow MPBall (CN b)
  where
  type PowType MPBall (CN b) = CN (PowType MPBall b)
  pow = CN.liftT1 pow

instance
  (CanPow a MPBall)
  =>
  CanPow (CN a) MPBall
  where
  type PowType (CN a) MPBall = CN (PowType a MPBall)
  pow = CN.lift1T pow

instance
  CanDivIMod MPBall MPBall
  where
  type DivIType MPBall MPBall = Integer
  divIMod x m
    | m !>! 0 = (error "Integer division for MPBall undefined", xm')
    | otherwise = error $ "modulus not positive: " ++ show m
    where
    (l, r) = endpoints $ x / m
    (dL, dR) = (floor l, floor r) 
    xmL = x - m*dL
    xmR = x - m*dR
    xm = hullMPBall xmL xmR
    xm' = min (max 0 xm) m
