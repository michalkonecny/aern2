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
)
where

import Numeric.MixedTypes hiding (id)
-- import qualified Prelude as P

import Control.Category (id)
import Control.Arrow

import Data.Convertible

import Data.Complex

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Real.Type

{- elementary -}

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

expA ::
  (QAArrow to, CanExp t, ExpType t ~ CauchyReal)
  =>
  t -> CauchyRealA to
expA = realA . exp

instance CanLog Integer where
  type LogType Integer = CauchyReal
  log = log . real

instance CanLog Int where
  type LogType Int = CauchyReal
  log = log . real

instance CanLog Dyadic where
  type LogType Dyadic = CauchyReal
  log = log . real

instance CanLog Rational where
  type LogType Rational = CauchyReal
  log = log . real

logA ::
  (QAArrow to, CanLog t, LogType t ~ CauchyReal)
  =>
  t -> CauchyRealA to
logA = realA . log


{- reals mixed with Double -}

instance Convertible CauchyReal Double where
  safeConvert r =
    safeConvert (centre (r ? (bitsS 53)))

binaryWithDouble :: (Double -> Double -> Double) -> CauchyReal -> Double -> Double
binaryWithDouble op r d =
  op (convert r) d

-- instance CanAddAsymmetric CauchyReal Double where
--   type AddType CauchyReal Double = Double
--   add = binaryWithDouble add
--
-- instance CanAddAsymmetric Double CauchyReal where
--   type AddType Double CauchyReal = Double
--   add = flip add
--
-- instance CanMulAsymmetric CauchyReal Double where
--   type MulType CauchyReal Double = Double
--   mul = binaryWithDouble mul
--
-- instance CanMulAsymmetric Double CauchyReal where
--   type MulType Double CauchyReal = Double
--   mul = flip mul
--
-- instance CanDiv CauchyReal Double where
--   type DivType CauchyReal Double = Double
--   divide = binaryWithDouble divide
--
-- instance CanDiv Double CauchyReal where
--   type DivType Double CauchyReal = Double
--   divide = flip $ binaryWithDouble (flip divide)
--

{- reals mixed with complex -}

-- instance
--   (QAArrow to, CanAddAsymmetric (CauchyRealA to) t)
--   =>
--   CanAddAsymmetric (CauchyRealA to) (Complex t)
--   where
--   type AddType (CauchyRealA to) (Complex t) = Complex (AddType (CauchyRealA to) t)
--   add r (a :+ i) = (r + a) :+ (z + i)
--     where
--     z = realA 0
--     _ = [z,r]
--
-- instance
--   (QAArrow to, CanAddAsymmetric t (CauchyRealA to))
--   =>
--   CanAddAsymmetric (Complex t) (CauchyRealA to)
--   where
--   type AddType (Complex t) (CauchyRealA to) = Complex (AddType t (CauchyRealA to))
--   add (a :+ i) r = (a + r) :+ (i + z)
--     where
--     z = realA 0
--     _ = [z,r]
--
--
-- instance
--   (QAArrow to, CanAdd (CauchyRealA to) t, CanNegSameType t)
--   =>
--   CanSub (CauchyRealA to) (Complex t)
--
-- instance
--   (QAArrow to, CanAdd t (CauchyRealA to))
--   =>
--   CanSub (Complex t) (CauchyRealA to)
-- instance
--   (CanMulAsymmetric (CauchyRealA to) t)
--   =>
--   CanMulAsymmetric (CauchyRealA to) (Complex t)
--   where
--   type MulType (CauchyRealA to) (Complex t) = Complex (MulType (CauchyRealA to) t)
--   mul r (a :+ i) = (r * a) :+ (r * i)
--
-- instance
--   (CanMulAsymmetric t (CauchyRealA to))
--   =>
--   CanMulAsymmetric (Complex t) (CauchyRealA to)
--   where
--   type MulType (Complex t) (CauchyRealA to) = Complex (MulType t (CauchyRealA to))
--   mul (a :+ i) r = (a * r) :+ (i * r)
