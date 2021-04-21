{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Real.Arithmetic
    Description :  arithmetic operations on CReal
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arithmetic operations on Cauchy Real numbers.
-}
module AERN2.Real.Arithmetic
(
  pi
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Numeric.CollectErrors
  ( cn )

import Data.Complex

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real.Type

{- field operations -}

-- instance Ring CReal
-- instance OrderedRing CReal
-- instance Field CReal
-- instance OrderedField CReal

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
  (CanMulAsymmetric t1 t2)
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


-- TODO: add MPBall and CN MPBall mixed-type arithmetic

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

    instance
      (CanAddAsymmetric a $t)
      => 
      CanAddAsymmetric (CSequence a) $t
      where
      type AddType (CSequence a) $t = CSequence (AddType a $t)
      add = lift1T add

    instance
      (CanAddAsymmetric $t a)
      => 
      CanAddAsymmetric $t (CSequence a)
      where
      type AddType $t (CSequence a) = CSequence (AddType $t a)
      add = liftT1 add

    -- TODO: add sub and div

    instance
      (CanMulAsymmetric a $t)
      => 
      CanMulAsymmetric (CSequence a) $t
      where
      type MulType (CSequence a) $t = CSequence (MulType a $t)
      mul = lift1T mul

    instance
      (CanMulAsymmetric $t a)
      => 
      CanMulAsymmetric $t (CSequence a)
      where
      type MulType $t (CSequence a) = CSequence (MulType $t a)
      mul = liftT1 mul

  |]))

{- common elementary operations -}

pi :: CReal
pi = CSequence $ map (cn . piBallP) cseqPrecisions

-- instance P.Floating CReal where
--     pi = pi
--     sqrt = sqrt
--     exp = exp
--     sin = sin
--     cos = cos
--     log = log
--     -- (**) = (^)
--     atan = error "CReal: atan not implemented yet"
--     atanh = error "CReal: atanh not implemented yet"
--     asin = error "CReal: asin not implemented yet"
--     acos = error "CReal: acos not implemented yet"
--     sinh = error "CReal: sinh not implemented yet"
--     cosh = error "CReal: cosh not implemented yet"
--     asinh = error "CReal: asinh not implemented yet"
--     acosh = error "CReal: acosh not implemented yet"


-- {- sine, cosine of finite values -}

-- instance CanSinCos Integer where
--   type SinCosType Integer = CReal
--   cos = cos . real
--   sin = sin . real

-- instance CanSinCos Int where
--   type SinCosType Int = CReal
--   cos = cos . real
--   sin = sin . real

-- instance CanSinCos Dyadic where
--   type SinCosType Dyadic = CReal
--   cos = cos . real
--   sin = sin . real

-- instance CanSinCos Rational where
--   type SinCosType Rational = CReal
--   cos = cos . real
--   sin = sin . real

-- {- sqrt of finite values -}

-- instance CanSqrt Integer where
--   type SqrtType Integer = CReal
--   sqrt = sqrt . real

-- instance CanSqrt Int where
--   type SqrtType Int = CReal
--   sqrt = sqrt . real

-- instance CanSqrt Dyadic where
--   type SqrtType Dyadic = CReal
--   sqrt = sqrt . real

-- instance CanSqrt Rational where
--   type SqrtType Rational = CReal
--   sqrt = sqrt . real

-- {- exp of finite values -}

-- instance CanExp Integer where
--   type ExpType Integer = CReal
--   exp = exp . real

-- instance CanExp Int where
--   type ExpType Int = CReal
--   exp = exp . real

-- instance CanExp Dyadic where
--   type ExpType Dyadic = CReal
--   exp = exp . real

-- instance CanExp Rational where
--   type ExpType Rational = CReal
--   exp = exp . real

-- {- log of finite values -}

-- instance CanLog Integer where
--   type LogType Integer = CReal
--   log = log . real

-- instance CanLog Int where
--   type LogType Int = CReal
--   log = log . real

-- instance CanLog Dyadic where
--   type LogType Dyadic = CReal
--   log = log . real

-- instance CanLog Rational where
--   type LogType Rational = CReal
--   log = log . real

-- {- non-integer power of finite values -}

-- instance CanPow Integer Dyadic where
--   type PowType Integer Dyadic = CReal
--   pow b e = pow (real b) (real e)

-- instance CanPow Int Dyadic where
--   type PowType Int Dyadic = CReal
--   pow b e = pow (real b) (real e)

-- instance CanPow Dyadic Dyadic where
--   type PowType Dyadic Dyadic = CReal
--   pow b e = pow (real b) (real e)

-- instance CanPow Rational Dyadic where
--   type PowType Rational Dyadic = CReal
--   pow b e = pow (real b) (real e)

-- instance CanPow Integer Rational where
--   type PowType Integer Rational = CReal
--   pow b e = pow (real b) (real e)

-- instance CanPow Int Rational where
--   type PowType Int Rational = CReal
--   pow b e = pow (real b) (real e)

-- instance CanPow Dyadic Rational where
--   type PowType Dyadic Rational = CReal
--   pow b e = pow (real b) (real e)

-- instance CanPow Rational Rational where
--   type PowType Rational Rational = CReal
--   pow b e = pow (real b) (real e)

-- {- reals mixed with Double -}

-- instance Convertible CReal Double where
--   safeConvert r =
--     safeConvert (centre (r ? (bitsS 53)))

-- binaryWithDouble :: (Double -> Double -> Double) -> CReal -> Double -> Double
-- binaryWithDouble op r d =
--   op (convert r) d

-- instance CanAddAsymmetric CReal Double where
--   type AddType CReal Double = Double
--   add = binaryWithDouble add

-- instance CanAddAsymmetric Double CReal where
--   type AddType Double CReal = Double
--   add = flip add

-- instance CanSub CReal Double where
--   type SubType CReal Double = Double
--   sub = binaryWithDouble sub

-- instance CanSub Double CReal where
--   type SubType Double CReal = Double
--   sub = flip $ binaryWithDouble (flip sub)

-- instance CanMulAsymmetric CReal Double where
--   type MulType CReal Double = Double
--   mul = binaryWithDouble mul

-- instance CanMulAsymmetric Double CReal where
--   type MulType Double CReal = Double
--   mul = flip mul

-- instance CanDiv CReal Double where
--   type DivType CReal Double = Double
--   divide = binaryWithDouble divide

-- instance CanDiv Double CReal where
--   type DivType Double CReal = Double
--   divide = flip $ binaryWithDouble (flip divide)

-- instance CanPow CReal Double where
--   type PowType CReal Double = Double
--   pow = binaryWithDouble pow

-- instance CanPow Double CReal where
--   type PowType Double CReal = Double
--   pow = flip $ binaryWithDouble (flip pow)

-- {- reals mixed with complex -}

-- instance
--   (CanAddAsymmetric CReal t)
--   =>
--   CanAddAsymmetric CReal (Complex t)
--   where
--   type AddType CReal (Complex t) = Complex (AddType CReal t)
--   add r (a :+ i) = (r + a) :+ (z + i)
--     where
--     z = realA 0
--     _ = [z,r]

-- instance
--   (CanAddAsymmetric t CReal)
--   =>
--   CanAddAsymmetric (Complex t) CReal
--   where
--   type AddType (Complex t) CReal = Complex (AddType t CReal)
--   add (a :+ i) r = (a + r) :+ (i + z)
--     where
--     z = realA 0
--     _ = [z,r]

-- instance
--   (CanAdd CReal t, CanNegSameType t)
--   =>
--   CanSub CReal (Complex t)

-- instance
--   (CanAdd t CReal)
--   =>
--   CanSub (Complex t) CReal

-- instance
--   (CanMulAsymmetric CReal t)
--   =>
--   CanMulAsymmetric CReal (Complex t)
--   where
--   type MulType CReal (Complex t) = Complex (MulType CReal t)
--   mul r (a :+ i) = (r * a) :+ (r * i)

-- instance
--   (CanMulAsymmetric t CReal)
--   =>
--   CanMulAsymmetric (Complex t) CReal
--   where
--   type MulType (Complex t) CReal = Complex (MulType t CReal)
--   mul (a :+ i) r = (a * r) :+ (i * r)
