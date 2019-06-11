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
-- (  mulCheb, mulChebDirect, mulChebDCT )
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

-- import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryBallFun

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.ShiftScale ()
import AERN2.Poly.Cheb.Maximum
import AERN2.Poly.Cheb.DCT

{- addition -}

instance
  -- (PolyCoeffRing c, CanNormalize (ChPoly c))
  (c ~ MPBall) =>
  CanAddAsymmetric (ChPoly c) (ChPoly c)
  where
  type AddType (ChPoly c) (ChPoly c) = ChPoly c
  add cp1@(ChPoly d1 p1 acG1 bnds1) cp2@(ChPoly d2 p2 acG2 bnds2)
    | d1 == d2 = 
        case (chPolyBounds_valueIfConst bnds1, chPolyBounds_valueIfConst bnds2) of
          (Just b1, _) -> b1 + cp2
          (_, Just b2) -> cp1 + b2
          _ -> result
    | otherwise = error $ "Adding polynomials with incompatible domains"
    where
    acG = max acG1 acG2
    result = 
      normalize $ ChPoly d1 (p1 + p2) acG (chPolyBounds_forChPoly result)


{- subtraction -}

instance
  -- (PolyCoeffRing c, CanNormalize (ChPoly c)) =>
  (c ~ MPBall) =>
  CanSub (ChPoly c) (ChPoly c)

{- multiplication -}

instance
  (c~MPBall) =>
  CanMulAsymmetric (ChPoly c) (ChPoly c)
  where
  type MulType (ChPoly c) (ChPoly c) = ChPoly c
  mul = mulChebNoBounds

mulChebUseBounds ::
  -- (PolyCoeffBall c, CanNormalize (ChPoly c))
  -- =>
  (c~MPBall) =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebUseBounds cp1@(ChPoly d1 _p1 _acG1 bnds1) cp2@(ChPoly d2 _p2 _acG2 bnds2)
  | d1 == d2 = 
      case (chPolyBounds_valueIfConst bnds1, chPolyBounds_valueIfConst bnds2) of
        (Just b1, _) -> b1 * cp2
        (_, Just b2) -> cp1 * b2
        _ -> updateRadius (+ e) resultC
  | otherwise = error $ "Multiplying polynomials with incompatible domains"
  where
  resultC = mulChebNoBounds cp1C cp2C
  (cp1C, e1) = centreAsBallAndRadius cp1
  (cp2C, e2) = centreAsBallAndRadius cp2
  (ChPolyBounds pmin1 pmax1) = bnds1
  (ChPolyBounds pmin2 pmax2) = bnds2
  bnd1 = errorBound $ (abs pmin1) `max` (abs pmax1)
  bnd2 = errorBound $ (abs pmin2) `max` (abs pmax2)
  e = e1 * bnd2 + e2 * bnd1 + e1 * e2

mulChebNoBounds ::
  -- (PolyCoeffBall c, CanNormalize (ChPoly c))
  -- =>
  (c~MPBall) =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebNoBounds p1@(ChPoly _ (Poly terms1) _acG1 _) p2@(ChPoly _ (Poly terms2) _acG2 _) =
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
  -- (PolyCoeffRing c, CanMulBy c Dyadic, CanNormalize (ChPoly c), CanSetPrecision c)
  -- =>
  (c~MPBall) =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebDirect _cp1@(ChPoly d1 p1 acG1 _) _cp2@(ChPoly d2 p2 acG2 _)
  | d1 /= d2 = error $ "Multiplying ChPoly values with incompatible domains"
  | otherwise = result
  where
  result = 
    normalize $ ChPoly d1 (Poly terms) (max acG1 acG2) (chPolyBounds_forChPoly result)
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
  -- (PolyCoeffBall c, CanNormalize (ChPoly c), CanSetPrecision c)
  -- =>
  (c~MPBall) =>
  (ChPoly c) -> (ChPoly c) -> (ChPoly c)
mulChebDCT = lift2_DCT (+) (*)


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

