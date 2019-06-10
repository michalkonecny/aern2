{-# LANGUAGE CPP #-}
-- #define DEBUG
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
(
  mulCheb, mulChebDirect, mulChebDCT
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
import Text.Printf

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

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.DCT

{- negation -}

instance CanNegSameType c => CanNeg (ChPoly c) where
  type NegType (ChPoly c) = ChPoly c
  negate (ChPoly d x acG _) = ChPoly d (negate x) acG ChPolyBounds

{- addition -}

instance
  (PolyCoeffRing c, CanNormalize (ChPoly c))
  =>
  CanAddAsymmetric (ChPoly c) (ChPoly c)
  where
  type AddType (ChPoly c) (ChPoly c) = ChPoly c
  add (ChPoly d1 p1 acG1 _) (ChPoly d2 p2 acG2 _)
    | d1 == d2 = normalize $ ChPoly d1 (p1 + p2) acG ChPolyBounds
    | otherwise = error $ "Adding polynomials with incompatible domains"
    where
    acG = max acG1 acG2

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric $t (ChPoly c) where
      type AddType $t (ChPoly c) = ChPoly c
      add n (ChPoly d2 p2 acG _) = ChPoly d2 (n + p2) acG ChPolyBounds

    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric (ChPoly c) $t where
      type AddType (ChPoly c) $t = ChPoly c
      add (ChPoly d1 p1 acG _) n = ChPoly d1 (n + p1) acG ChPolyBounds
  |]))


{- subtraction -}

instance
  (PolyCoeffRing c, CanNormalize (ChPoly c))
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
  (c~MPBall)
  =>
  CanMulAsymmetric (ChPoly c) (ChPoly c)
  where
  type MulType (ChPoly c) (ChPoly c) = ChPoly c
  mul = mulCheb

-- instance
--   CanMulAsymmetric (ChPoly Rational) (ChPoly Rational)
--   where
--   type MulType (ChPoly Rational) (ChPoly Rational) = ChPoly Rational
--   mul = mulChebDirect
--
-- instance
--   CanMulAsymmetric (ChPoly Dyadic) (ChPoly Dyadic)
--   where
--   type MulType (ChPoly Dyadic) (ChPoly Dyadic) = ChPoly Dyadic
--   mul = mulChebDirect

mulCheb ::
  (PolyCoeffBall c, CanNormalize (ChPoly c))
  =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulCheb p1@(ChPoly _ (Poly terms1) _acG1 _) p2@(ChPoly _ (Poly terms2) _acG2 _) =
  maybeTrace
    (printf "mulCheb: ac p1 = %s, ac p2 = %s, acG p1 = %s, acG p2 = %s, size1+size2 = %d, using %s, ac result = %s, prec result = %s"
      (show $ getAccuracy p1) (show $ getAccuracy p2)
      (show $ getAccuracyGuide p1) (show $ getAccuracyGuide p2)
      (size1 + size2) methodS
      (show $ getAccuracy result) (show $ getPrecision result)
    ) $
  result
  where
  (result, methodS)
    | getAccuracy p1 /= Exact || getAccuracy p2 /= Exact || size1 + size2 < 1000
      -- TODO: improve the condition based on benchmarks
      = (mulChebDirect p1 p2, "mulChebDirect")
    | otherwise
      = (mulChebDCT p1 p2, "mulChebDCT")
  size1 = terms_size terms1
  size2 = terms_size terms2

mulChebDirect ::
  (PolyCoeffRing c, CanMulBy c Dyadic, CanNormalize (ChPoly c), CanSetPrecision c)
  =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebDirect _cp1@(ChPoly d1 p1 acG1 _) _cp2@(ChPoly d2 p2 acG2 _)
  | d1 /= d2 = error $ "Multiplying ChPoly's with incompatible domains"
  | otherwise =
    normalize $ ChPoly d1 (Poly terms) (max acG1 acG2) ChPolyBounds
  where
  terms =
    terms_fromListAddCoeffs $
      concat
      [ let c = a*b*(dyadic 0.5) in [(i+j, c), (abs (i-j), c)]
        |
        (i,a) <- terms_toList terms1,
        (j,b) <- terms_toList terms2
      ]
  (Poly terms1) = p1 -- setPrecision prc p1
  (Poly terms2) = p2 -- setPrecision prc p2
  -- prc = (getPrecision p1) `max` (getPrecision p2) `max` (prec $ 2 `max` ((fromAccuracy (acG1 `min` acG2)) * deg))
  -- deg = degree cp1 + degree cp2

mulChebDCT ::
  (PolyCoeffBall c, CanNormalize (ChPoly c), CanSetPrecision c)
  =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebDCT = lift2_DCT (+) (*)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy c $t, HasIntegers c, CanNormalize (ChPoly c)) => CanMulAsymmetric $t (ChPoly c) where
      type MulType $t (ChPoly c) = ChPoly c
      mul n (ChPoly d2 p2 acG _) = normalize $ ChPoly d2 (n * p2) acG ChPolyBounds

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
      divideNoCN (ChPoly d1 p1 acG _) n = normalize $ ChPoly d1 (p1/!n) acG ChPolyBounds
      type DivType (ChPoly c) $t = CN (ChPoly c)
      divide (ChPoly d1 p1 acG _) n =
        fmap normalize $ extractCN $ ChPoly d1 (p1/n) acG ChPolyBounds

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
