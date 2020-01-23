{-# LANGUAGE CPP #-}
-- #define DEBUG
{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Poly.Cheb.ShiftScale
    Description :  Chebyshev basis polynomial shifting and scaling
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis polynomial shifting and scaling
-}

module AERN2.Poly.Cheb.ShiftScale
(
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace ((flip const :: (String -> a -> a)))
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

-- import Test.Hspec
-- import Test.QuickCheck



import AERN2.Normalize

-- import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryBallFun

-- import AERN2.Poly.Basics
import AERN2.Poly.Cheb.Type

{- negation -}

instance CanNegSameType c => CanNeg (ChPoly c) where
  type NegType (ChPoly c) = ChPoly c
  negate (ChPoly d x acG bnds) = 
    ChPoly d (negate x) acG (ChPolyBounds (-pmax) (-pmin))
    where
    (ChPolyBounds pmin pmax) = bnds

{- shifting -}

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric $t (ChPoly c) where
      type AddType $t (ChPoly c) = ChPoly c
      add n (ChPoly d2 p2 acG bnds) = ChPoly d2 (n + p2) acG (chPolyBounds_map (+n) bnds)

    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric (ChPoly c) $t where
      type AddType (ChPoly c) $t = ChPoly c
      add (ChPoly d1 p1 acG bnds) n = ChPoly d1 (n + p1) acG (chPolyBounds_map (+n) bnds)
  |]))


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis c $t, CanNegSameType c, HasIntegers c) => CanSub $t (ChPoly c)
    instance (CanAddThis c $t, HasIntegers c) => CanSub (ChPoly c) $t
  |]))

{- scaling -}

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy c $t, HasIntegers c, CanNormalize (ChPoly c)) => CanMulAsymmetric $t (ChPoly c) where
      type MulType $t (ChPoly c) = ChPoly c
      mul n (ChPoly d2 p2 acG bnds) = 
        normalize $ ChPoly d2 (n * p2) acG (ChPolyBounds (n*pmin) (n*pmax))
        where
        (ChPolyBounds pmin pmax) = bnds

    instance (CanMulBy c $t, HasIntegers c, CanNormalize (ChPoly c)) => CanMulAsymmetric (ChPoly c) $t where
      type MulType (ChPoly c) $t = ChPoly c
      mul = flip mul
  |]))


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance
      (CanDivCNBy c $t
      , CanEnsureCN c, EnsureNoCN c ~ c
      , CanEnsureCN (EnsureCN c)
      , HasIntegers c, CanNormalize (ChPoly c))
      =>
      CanDiv (ChPoly c) $t
      where
      type DivTypeNoCN (ChPoly c) $t = ChPoly c
      divideNoCN (ChPoly d1 p1 acG bnds) n = 
        normalize $ ChPoly d1 (p1/!n) acG (ChPolyBounds (pmin/!n) (pmax/!n))
        where
        (ChPolyBounds pmin pmax) = bnds
      type DivType (ChPoly c) $t = CN (ChPoly c)
      divide (ChPoly d1 p1 acG bnds) n =
        fmap normalize $ extractCN $ ChPoly d1 (p1/n) acG (ChPolyBounds (pmin/n) (pmax/n))
        where
        (ChPolyBounds pmin pmax) = bnds
  |]))

{- integer power -}

{-TODO: Enable as soon as we have HasIntegers (ChPoly MPBall)
  which will need convertExactlyFromSample.

instance
  (c ~ MPBall) =>
  CanPow (ChPoly c) Integer where
  pow = powUsingMul

instance
  (c ~ MPBall) =>
  CanPow (ChPoly c) Int where
  pow = powUsingMul

-}
