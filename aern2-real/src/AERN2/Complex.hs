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

_test1 :: CComplex
_test1 = ccomplex 1.0

_test2 :: CComplex
_test2 = ccomplex $ creal 1

