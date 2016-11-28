{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Poly.Cheb.Ring
    Description :  Chebyshev basis ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis ring operations
-}

module AERN2.Poly.Cheb.Ring
-- (
-- )
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.TH

import AERN2.Normalize

-- import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type

{- negation -}

instance CanNegSameType c => CanNeg (ChPoly c) where
  type NegType (ChPoly c) = ChPoly c
  negate (ChPoly d x) = ChPoly d (negate x)

{- addition -}

instance
  (CanAddSameType c, HasIntegers c, CanNormalize (ChPoly c))
  =>
  CanAddAsymmetric (ChPoly c) (ChPoly c)
  where
  type AddType (ChPoly c) (ChPoly c) = ChPoly c
  add (ChPoly d1 p1) (ChPoly d2 p2)
    | d1 == d2 = normalize $ ChPoly d1 (p1 + p2)
    | otherwise = error $ "Adding polynomials with incompatible domains"

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric $t (ChPoly c) where
      type AddType $t (ChPoly c) = ChPoly c
      add n (ChPoly d2 p2) = ChPoly d2 (n + p2)

    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric (ChPoly c) $t where
      type AddType (ChPoly c) $t = ChPoly c
      add (ChPoly d1 p1) n = ChPoly d1 (n + p1)
  |]))


{- subtraction -}

instance
  (CanAddSameType c, CanNegSameType c, HasIntegers c, CanNormalize (ChPoly c))
  =>
  CanSub (ChPoly c) (ChPoly c)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis c $t, CanNegSameType c, HasIntegers c) => CanSub $t (ChPoly c)
    instance (CanAddThis c $t, HasIntegers c) => CanSub (ChPoly c) $t
  |]))


{- multiplication -}

instance
  (Ring c, CanDivBy c Integer, CanNormalize (ChPoly c))
  =>
  CanMulAsymmetric (ChPoly c) (ChPoly c)
  where
  type MulType (ChPoly c) (ChPoly c) = ChPoly c
  mul (ChPoly d1 p1) (ChPoly d2 p2)
    | d1 == d2 = normalize $ ChPoly d1 (mulCheb p1 p2)
    | otherwise = error $ "Multiplying polynomials with incompatible domains"

-- Poly level
mulCheb :: (Ring c, CanDivBy c Integer) => (Poly c) -> (Poly c) -> (Poly c)
mulCheb = mulChebDirect

mulChebDirect :: (Ring c, CanDivBy c Integer) => (Poly c) -> (Poly c) -> (Poly c)
mulChebDirect (Poly terms1) (Poly terms2) =
  Poly terms
  where
  terms =
    terms_fromListAddCoeffs $
      concat
      [ let c = a*b/2 in [(i+j, c), (abs (i-j), c)]
        |
        (i,a) <- terms_toList terms1,
        (j,b) <- terms_toList terms2
      ]

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy c $t, HasIntegers c, CanNormalize (ChPoly c)) => CanMulAsymmetric $t (ChPoly c) where
      type MulType $t (ChPoly c) = ChPoly c
      mul n (ChPoly d2 p2) = normalize $ ChPoly d2 (n * p2)

    instance (CanMulBy c $t, HasIntegers c, CanNormalize (ChPoly c)) => CanMulAsymmetric (ChPoly c) $t where
      type MulType (ChPoly c) $t = ChPoly c
      mul (ChPoly d1 p1) n = normalize $ ChPoly d1 (n * p1)
  |]))


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanDivBy c $t, HasIntegers c, CanNormalize (ChPoly c)) => CanDiv (ChPoly c) $t where
      type DivType (ChPoly c) $t = ChPoly c
      divide (ChPoly d1 p1) n = normalize $ ChPoly d1 (p1/n)
  |]))
