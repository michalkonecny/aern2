{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Complex
    Description :  Exact complex numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Exact complex numbers represented by Cauchy sequences of (Complex MPBall)'s.
-}
module AERN2.Complex
(
   -- * complex numbers and conversions
   CComplex, ccomplex, HasCComplex, CanBeCComplex,
)
where

import MixedTypesNumPrelude
-- -- import qualified Prelude as P

import Data.Complex

import AERN2.Real

type CComplex = Complex CReal

type CanBeCComplex t = ConvertibleExactly t CComplex

type HasCComplex t = ConvertibleExactly CComplex t

ccomplex :: (CanBeCComplex t) => t -> CComplex
ccomplex = convertExactly

instance (HasCReals t, HasIntegers t) => (ConvertibleExactly CReal (Complex t))
  where
  safeConvertExactly n =
    do
    nT <- safeConvertExactly n
    zT <- safeConvertExactly 0
    return $ nT :+ zT

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


_test1 :: CComplex
_test1 = ccomplex 1.0

_test2 :: CComplex
_test2 = ccomplex $ creal 1

