{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Real.Field
    Description :  field operations on CReal
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on Cauchy Real numbers.
-}
module AERN2.Real.Field
(
-- * field ops (`add`, `sub`, `mul`, `div`) for `CReal -> CReal -> CReal`
-- * field ops for `CReal -> t -> CReal` and `t -> CReal -> CReal` where `t` is `Int`, `Integer`, `Rational`, `Dyadic`
-- * field ops for `CReal -> MPBall -> MPBall` and `CReal -> CN MPBall -> CN MPBall`
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import AERN2.MP.Ball
-- import AERN2.MP.Dyadic

import AERN2.Real.Type
import AERN2.Real.Comparisons ()
import AERN2.Real.FieldTH ()

{- field operations -}

instance Ring CReal
instance OrderedRing CReal
instance Field CReal
instance OrderedField CReal

instance
  (CanAddAsymmetric t1 t2)
  => 
  CanAddAsymmetric (CSequence t1) (CSequence t2) 
  where
  type AddType (CSequence t1) (CSequence t2) = CSequence (AddType t1 t2)
  add = lift2 add

instance
  (CanSub t1 t2)
  => 
  CanSub (CSequence t1) (CSequence t2) 
  where
  type SubType (CSequence t1) (CSequence t2) = CSequence (SubType t1 t2)
  sub = lift2 sub

instance
  (CanMulAsymmetric t1 t2, CanGiveUpIfVeryInaccurate (MulType t1 t2))
  => 
  CanMulAsymmetric (CSequence t1) (CSequence t2) 
  where
  type MulType (CSequence t1) (CSequence t2) = CSequence (MulType t1 t2)
  mul = lift2 mul

instance
  (CanDiv t1 t2, CanTestZero t2)
  => 
  CanDiv (CSequence t1) (CSequence t2) 
  where
  type DivType (CSequence t1) (CSequence t2) = CSequence (DivType t1 t2)
  divide = lift2 divide

instance
  (CanPow b e, HasOrderCertainly b Integer, HasOrderCertainly e Integer,
   HasEqCertainly b Integer, CanTestInteger e, CanTestIsIntegerType b, CanTestIsIntegerType e)
  =>
  CanPow (CSequence b) (CSequence e) 
  where
  type PowType (CSequence b) (CSequence e) = CSequence (PowType b e)
  pow = lift2 pow
  type PPowType (CSequence b) (CSequence e) = CSequence (PPowType b e)
  ppow = lift2 ppow

---------------------------------------------------
---------------------------------------------------
-- MPBall and CN MPBall mixed-type arithmetic
---------------------------------------------------
---------------------------------------------------

instance
  (CanAddAsymmetric MPBall b)
  => 
  CanAddAsymmetric MPBall (CSequence b)
  where
  type AddType MPBall (CSequence b) = AddType MPBall b
  add a s = add a (unCN $ s ? (getPrecision a))

instance
  (CanAddAsymmetric b MPBall)
  => 
  CanAddAsymmetric (CSequence b) MPBall
  where
  type AddType (CSequence b) MPBall = AddType b MPBall
  add s b = add (unCN $ s ? (getPrecision b)) b

instance
  (CanAddAsymmetric MPBall b)
  => 
  CanAddAsymmetric (CN MPBall) (CSequence b)
  where
  type AddType (CN MPBall) (CSequence b) = AddType (CN MPBall) (CN b)
  add a s = add a (s ? (getPrecision a))

instance
  (CanAddAsymmetric b MPBall)
  => 
  CanAddAsymmetric (CSequence b) (CN MPBall)
  where
  type AddType (CSequence b) (CN MPBall) = AddType (CN b) (CN MPBall)
  add s b = add (s ? (getPrecision b)) b

instance
  (CanSub MPBall b)
  => 
  CanSub MPBall (CSequence b)
  where
  type SubType MPBall (CSequence b) = SubType MPBall b
  sub a s = sub a (unCN $ s ? (getPrecision a))

instance
  (CanSub b MPBall)
  => 
  CanSub (CSequence b) MPBall
  where
  type SubType (CSequence b) MPBall = SubType b MPBall
  sub s b = sub (unCN $ s ? (getPrecision b)) b

instance
  (CanSub MPBall b)
  => 
  CanSub (CN MPBall) (CSequence b)
  where
  type SubType (CN MPBall) (CSequence b) = SubType (CN MPBall) (CN b)
  sub a s = sub a (s ? (getPrecision a))

instance
  (CanSub b MPBall)
  => 
  CanSub (CSequence b) (CN MPBall)
  where
  type SubType (CSequence b) (CN MPBall) = SubType (CN b) (CN MPBall)
  sub s b = sub (s ? (getPrecision b)) b

instance
  (CanMulAsymmetric MPBall b)
  => 
  CanMulAsymmetric MPBall (CSequence b)
  where
  type MulType MPBall (CSequence b) = MulType MPBall b
  mul a s = mul a (unCN $ s ? (getPrecision a))

instance
  (CanMulAsymmetric b MPBall)
  => 
  CanMulAsymmetric (CSequence b) MPBall
  where
  type MulType (CSequence b) MPBall = MulType b MPBall
  mul s b = mul (unCN $ s ? (getPrecision b)) b

instance
  (CanMulAsymmetric MPBall b, CanGiveUpIfVeryInaccurate (MulType MPBall b))
  => 
  CanMulAsymmetric (CN MPBall) (CSequence b)
  where
  type MulType (CN MPBall) (CSequence b) = MulType (CN MPBall) (CN b)
  mul a s = mul a (s ? (getPrecision a))

instance
  (CanMulAsymmetric b MPBall, CanGiveUpIfVeryInaccurate (MulType b MPBall))
  => 
  CanMulAsymmetric (CSequence b) (CN MPBall)
  where
  type MulType (CSequence b) (CN MPBall) = MulType (CN b) (CN MPBall)
  mul s b = mul (s ? (getPrecision b)) b

instance
  (CanDiv MPBall b, CanTestZero b)
  => 
  CanDiv MPBall (CSequence b)
  where
  type DivType MPBall (CSequence b) = DivType MPBall b
  divide a s = divide a (unCN $ s ? (getPrecision a))

instance
  (CanDiv b MPBall)
  => 
  CanDiv (CSequence b) MPBall
  where
  type DivType (CSequence b) MPBall = DivType b MPBall
  divide s b = divide (unCN $ s ? (getPrecision b)) b

instance
  (CanDiv MPBall b, CanTestZero b)
  => 
  CanDiv (CN MPBall) (CSequence b)
  where
  type DivType (CN MPBall) (CSequence b) = DivType (CN MPBall) (CN b)
  divide a s = divide a (s ? (getPrecision a))

instance
  (CanDiv b MPBall)
  => 
  CanDiv (CSequence b) (CN MPBall)
  where
  type DivType (CSequence b) (CN MPBall) = DivType (CN b) (CN MPBall)
  divide s b = divide (s ? (getPrecision b)) b

{- Prelude Num, Real, Fractional instance -}

instance
  P.Num CReal
  where
  fromInteger = convertExactly
  negate = negate
  (+) = (+)
  (*) = (*)
  abs = abs
  signum = error "Prelude.signum not implemented for Sequence"

instance
  P.Fractional CReal
  where
  fromRational = convertExactly
  recip = recip
  (/) = (/)
