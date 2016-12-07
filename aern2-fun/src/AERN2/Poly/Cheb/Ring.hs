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

import Numeric.MixedTypes
-- import qualified Prelude as P
import Text.Printf

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
import AERN2.Poly.Cheb.DCT


import Debug.Trace (trace)

shouldTrace :: Bool
-- shouldTrace = False
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

{- negation -}

instance CanNegSameType c => CanNeg (ChPoly c) where
  type NegType (ChPoly c) = ChPoly c
  negate (ChPoly d x) = ChPoly d (negate x)

{- addition -}

instance
  (PolyCoeff c)
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
  (PolyCoeff c)
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
  (PolyCoeff c)
  =>
  CanMulAsymmetric (ChPoly c) (ChPoly c)
  where
  type MulType (ChPoly c) (ChPoly c) = ChPoly c
  mul = mulCheb

mulCheb ::
  (PolyCoeff c)
  =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulCheb p1@(ChPoly _ (Poly terms1)) p2@(ChPoly _ (Poly terms2)) =
  maybeTrace
    (printf "mulCheb: getAccuracy p1 = %s, getAccuracy p2 = %s, size1+size2 = %d, using %s, getAccuracy result = %s"
      (show $ getAccuracy p1) (show $ getAccuracy p2) (size1 + size2) methodS (show $ getAccuracy result)
    ) $
  result
  where
  (result, methodS)
    | getAccuracy p1 /= Exact || getAccuracy p2 /= Exact || size1 + size2 < 200 -- 1000
      = (mulChebDirect p1 p2, "mulChebDirect")
    | otherwise
      = (mulChebDCT p1 p2, "mulChebDCT")
  size1 = terms_size terms1
  size2 = terms_size terms2

mulChebDirect ::
  (PolyCoeff c)
  =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebDirect (ChPoly d1 (Poly terms1)) (ChPoly d2 (Poly terms2))
  | d1 /= d2 = error $ "Multiplying ChPoly's with incompatible domains"
  | otherwise =
    normalize $ ChPoly d1 (Poly terms)
  where
  terms =
    terms_fromListAddCoeffs $
      concat
      [ let c = a*b/2 in [(i+j, c), (abs (i-j), c)]
        |
        (i,a) <- terms_toList terms1,
        (j,b) <- terms_toList terms2
      ]

mulChebDCT ::
  (PolyCoeff c)
  =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebDCT = lift2_DCT (+) (*)

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


{- integer power -}

instance (PolyCoeff c) => CanPow (ChPoly c) Integer where
  pow = undefined -- powUsingMul

instance (PolyCoeff c) => CanPow (ChPoly c) Int where
  pow = undefined -- powUsingMul
