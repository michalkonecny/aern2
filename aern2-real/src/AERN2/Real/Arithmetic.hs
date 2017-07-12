{-|
    Module      :  AERN2.Real.Arithmetic
    Description :  arithmetic operations on CR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arithmetic operations on Cauchy Real numbers,
    except those that are defined for more general sequences
-}
module AERN2.Real.Arithmetic
(
  pi, piA
)
where

import MixedTypesNumPrelude hiding (id)
-- import qualified Prelude as P

import Data.Convertible

import Data.Complex

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Real.Type

instance (QAArrow to) => Ring (CauchyRealA to)
instance (QAArrow to) => OrderedRing (CauchyRealA to)
instance (QAArrow to) => Field (CauchyRealA to)
instance (QAArrow to) => OrderedField (CauchyRealA to)

{-|
  To get @pi@ in an arbitrary arrow, use 'piA'.
-}
pi :: CauchyReal
pi = newCR "pi" [] (\_me_src -> (seqByPrecision2CauchySeq piBallP . _acGuide))

piA :: (QAArrow to) => CauchyRealA to
piA = realA pi


{- sine, cosine of finite values -}

instance CanSinCos Integer where
  type SinCosType Integer = CauchyReal
  cos = cos . real
  sin = sin . real

instance CanSinCos Int where
  type SinCosType Int = CauchyReal
  cos = cos . real
  sin = sin . real

instance CanSinCos Dyadic where
  type SinCosType Dyadic = CauchyReal
  cos = cos . real
  sin = sin . real

instance CanSinCos Rational where
  type SinCosType Rational = CauchyReal
  cos = cos . real
  sin = sin . real

{- sqrt of finite values -}

instance CanSqrt Integer where
  type SqrtType Integer = CauchyRealCN
  sqrt = sqrt . real

instance CanSqrt Int where
  type SqrtType Int = CauchyRealCN
  sqrt = sqrt . real

instance CanSqrt Dyadic where
  type SqrtType Dyadic = CauchyRealCN
  sqrt = sqrt . real

instance CanSqrt Rational where
  type SqrtType Rational = CauchyRealCN
  sqrt = sqrt . real

{- exp of finite values -}

instance CanExp Integer where
  type ExpType Integer = CauchyReal
  exp = exp . real

instance CanExp Int where
  type ExpType Int = CauchyReal
  exp = exp . real

instance CanExp Dyadic where
  type ExpType Dyadic = CauchyReal
  exp = exp . real

instance CanExp Rational where
  type ExpType Rational = CauchyReal
  exp = exp . real

{- log of finite values -}

instance CanLog Integer where
  type LogType Integer = CauchyRealCN
  log = log . real

instance CanLog Int where
  type LogType Int = CauchyRealCN
  log = log . real

instance CanLog Dyadic where
  type LogType Dyadic = CauchyRealCN
  log = log . real

instance CanLog Rational where
  type LogType Rational = CauchyRealCN
  log = log . real

{- non-integer power of finite values -}

instance CanPow Integer Dyadic where
  type PowTypeNoCN Integer Dyadic = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

instance CanPow Int Dyadic where
  type PowTypeNoCN Int Dyadic = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

instance CanPow Dyadic Dyadic where
  type PowTypeNoCN Dyadic Dyadic = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

instance CanPow Rational Dyadic where
  type PowTypeNoCN Rational Dyadic = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

instance CanPow Integer Rational where
  type PowTypeNoCN Integer Rational = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

instance CanPow Int Rational where
  type PowTypeNoCN Int Rational = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

instance CanPow Dyadic Rational where
  type PowTypeNoCN Dyadic Rational = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

instance CanPow Rational Rational where
  type PowTypeNoCN Rational Rational = CauchyReal
  powNoCN b e = powNoCN (real b) (real e)
  pow b e = pow (real b) (real e)

{- reals mixed with Double -}

instance Convertible CauchyReal Double where
  safeConvert r =
    safeConvert (centre (r ? (bitsS 53)))

binaryWithDouble :: (Double -> Double -> Double) -> CauchyReal -> Double -> Double
binaryWithDouble op r d =
  op (convert r) d

instance CanAddAsymmetric CauchyReal Double where
  type AddType CauchyReal Double = Double
  add = binaryWithDouble add

instance CanAddAsymmetric Double CauchyReal where
  type AddType Double CauchyReal = Double
  add = flip add

instance CanSub CauchyReal Double where
  type SubType CauchyReal Double = Double
  sub = binaryWithDouble sub

instance CanSub Double CauchyReal where
  type SubType Double CauchyReal = Double
  sub = flip $ binaryWithDouble (flip sub)

instance CanMulAsymmetric CauchyReal Double where
  type MulType CauchyReal Double = Double
  mul = binaryWithDouble mul

instance CanMulAsymmetric Double CauchyReal where
  type MulType Double CauchyReal = Double
  mul = flip mul

instance CanDiv CauchyReal Double where
  type DivType CauchyReal Double = Double
  divide = binaryWithDouble divide
  type DivTypeNoCN CauchyReal Double = Double
  divideNoCN = binaryWithDouble divideNoCN

instance CanDiv Double CauchyReal where
  type DivType Double CauchyReal = Double
  divide = flip $ binaryWithDouble (flip divide)
  type DivTypeNoCN Double CauchyReal = Double
  divideNoCN = flip $ binaryWithDouble (flip divideNoCN)

instance CanPow CauchyReal Double where
  type PowTypeNoCN CauchyReal Double = Double
  type PowType CauchyReal Double = Double
  powNoCN = binaryWithDouble pow
  pow = binaryWithDouble pow

instance CanPow Double CauchyReal where
  powNoCN = flip $ binaryWithDouble (flip pow)
  type PowType Double CauchyReal = Double
  pow = flip $ binaryWithDouble (flip pow)

{- reals mixed with complex -}

instance
  (QAArrow to, CanAddAsymmetric (CauchyRealA to) t)
  =>
  CanAddAsymmetric (CauchyRealA to) (Complex t)
  where
  type AddType (CauchyRealA to) (Complex t) = Complex (AddType (CauchyRealA to) t)
  add r (a :+ i) = (r + a) :+ (z + i)
    where
    z = realA 0
    _ = [z,r]

instance
  (QAArrow to, CanAddAsymmetric t (CauchyRealA to))
  =>
  CanAddAsymmetric (Complex t) (CauchyRealA to)
  where
  type AddType (Complex t) (CauchyRealA to) = Complex (AddType t (CauchyRealA to))
  add (a :+ i) r = (a + r) :+ (i + z)
    where
    z = realA 0
    _ = [z,r]

instance
  (QAArrow to, CanAdd (CauchyRealA to) t, CanNegSameType t)
  =>
  CanSub (CauchyRealA to) (Complex t)

instance
  (QAArrow to, CanAdd t (CauchyRealA to))
  =>
  CanSub (Complex t) (CauchyRealA to)

instance
  (CanMulAsymmetric (CauchyRealA to) t)
  =>
  CanMulAsymmetric (CauchyRealA to) (Complex t)
  where
  type MulType (CauchyRealA to) (Complex t) = Complex (MulType (CauchyRealA to) t)
  mul r (a :+ i) = (r * a) :+ (r * i)

instance
  (CanMulAsymmetric t (CauchyRealA to))
  =>
  CanMulAsymmetric (Complex t) (CauchyRealA to)
  where
  type MulType (Complex t) (CauchyRealA to) = Complex (MulType t (CauchyRealA to))
  mul (a :+ i) r = (a * r) :+ (i * r)
