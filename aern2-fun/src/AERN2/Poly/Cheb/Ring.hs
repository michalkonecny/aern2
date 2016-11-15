{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Poly.Cheb.Ring
    Description :  Chebyshev basis unary sparse polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis unary sparse polynomials
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

import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type

{- negation -}

instance CanNegSameType t => CanNeg (Ball t) where
  type NegType (Ball t) = Ball t
  negate (Ball x e) = Ball (negate x) e

instance CanNegSameType c => CanNeg (ChPoly c) where
  type NegType (ChPoly c) = ChPoly c
  negate (ChPoly d x) = ChPoly d (negate x)

{- addition -}

-- PolyBall level
instance (IsBall t, CanAddSameType t) => CanAddAsymmetric (Ball t) (Ball t) where
  type AddType  (Ball t) (Ball t) = Ball t
  add (Ball x1 e1) (Ball x2 e2) =
    normaliseBall $ Ball (x1 + x2) (e1 + e2)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis t $t, IsBall t) => CanAddAsymmetric $t (Ball t) where
      type AddType $t (Ball t) = Ball t
      add n (Ball x e) = normaliseBall $ Ball (x + n) e

    instance (CanAddThis t $t, IsBall t) => CanAddAsymmetric (Ball t) $t where
      type AddType (Ball t) $t = Ball t
      add (Ball x e) n = normaliseBall $ Ball (x + n) e
  |]))

-- ChPoly level
instance (CanAddSameType c) => CanAddAsymmetric (ChPoly c) (ChPoly c) where
  type AddType (ChPoly c) (ChPoly c) = ChPoly c
  add (ChPoly d1 p1) (ChPoly d2 p2)
    | d1 == d2 = ChPoly d1 (p1 + p2)
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

-- PolyBall level
instance (IsBall t, CanAddSameType t, CanNegSameType t) => CanSub (Ball t) (Ball t)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (IsBall t, CanAddThis t $t, CanNegSameType t) => CanSub $t (Ball t)
    instance (IsBall t, CanAddThis t $t) => CanSub (Ball t) $t
  |]))

-- ChPoly level
instance (CanAddSameType c, CanNegSameType c) => CanSub (ChPoly c) (ChPoly c)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis c $t, CanNegSameType c, HasIntegers c) => CanSub $t (ChPoly c)
    instance (CanAddThis c $t, HasIntegers c) => CanSub (ChPoly c) $t
  |]))


{- multiplication -}

-- PolyBall level
instance (IsBall c, CanMulSameType c)
  =>
  CanMulAsymmetric (Ball c) (Ball c) where
  type MulType  (Ball c) (Ball c) = Ball c
  mul (Ball x1 e1) (Ball x2 e2) =
    normaliseBall $ Ball (x1e1 * x2e2) (errorBound 0)
    where
    x1e1 = updateRadius (+ e1) x1
    x2e2 = updateRadius (+ e2) x2
    -- TODO: use norm computed using root finding?
    --  is it too expensive?  check once we have benchmarking

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy t $t, IsBall t) => CanMulAsymmetric $t (Ball t) where
      type MulType $t (Ball t) = Ball t
      mul n (Ball x e) = normaliseBall $ Ball (x * n) e

    instance (CanMulBy t $t, IsBall t) => CanMulAsymmetric (Ball t) $t where
      type MulType (Ball t) $t = Ball t
      mul (Ball x e) n = normaliseBall $ Ball (x * n) e
  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanDivBy t $t, IsBall t) => CanDiv (Ball t) $t where
      type DivType (Ball t) $t = Ball t
      divide (Ball x e) n = normaliseBall $ Ball (x / n) e
  |]))


-- ChPoly level
instance (Ring c, CanDivBy c Integer) => CanMulAsymmetric (ChPoly c) (ChPoly c) where
  type MulType (ChPoly c) (ChPoly c) = ChPoly c
  mul (ChPoly d1 p1) (ChPoly d2 p2)
    | d1 == d2 = ChPoly d1 (mulCheb p1 p2)
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
    instance (CanMulBy c $t) => CanMulAsymmetric $t (ChPoly c) where
      type MulType $t (ChPoly c) = ChPoly c
      mul n (ChPoly d2 p2) = ChPoly d2 (n * p2)

    instance (CanMulBy c $t) => CanMulAsymmetric (ChPoly c) $t where
      type MulType (ChPoly c) $t = ChPoly c
      mul (ChPoly d1 p1) n = ChPoly d1 (n * p1)
  |]))


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanDivBy c $t) => CanDiv (ChPoly c) $t where
      type DivType (ChPoly c) $t = ChPoly c
      divide (ChPoly d1 p1) n = ChPoly d1 (p1/n)
  |]))
