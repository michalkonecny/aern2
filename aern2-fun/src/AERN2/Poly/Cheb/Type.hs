{-|
    Module      :  AERN2.Poly.Cheb.Type
    Description :  Chebyshev basis unary sparse polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis unary sparse polynomials
-}

module AERN2.Poly.Cheb.Type
-- (
-- )
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.MP.ErrorBound
import AERN2.MP.Ball
-- import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

{- types -}

type PolyBall = Ball (ChPoly MPBall)

data Ball c = Ball { ball_value :: c, ball_radius :: ErrorBound }

data ChPoly c = ChPoly { chPoly_dom :: DyadicInterval, chPoly_poly :: Poly c }



instance (IsBall c) => IsBall (ChPoly c) where
  type CentreType (ChPoly c) = ChPoly c
  radius (ChPoly _dom (Poly terms)) =
    List.foldl' (+) (errorBound 0) $ map radius $ terms_coeffs terms
  centre (ChPoly dom (Poly terms)) =
    ChPoly dom (Poly (terms_map centreAsBall terms))
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)
  updateRadius updateFn (ChPoly dom (Poly terms)) =
    ChPoly dom (Poly $ terms_updateConst (updateRadius updateFn) terms)

type CanBeChPoly c t = ConvertibleExactly (DyadicInterval, t) (ChPoly c)
chPoly :: (CanBeChPoly c t) => (DyadicInterval, t) -> (ChPoly c)
chPoly = convertExactly

instance (ConvertibleExactly t c) => ConvertibleExactly (DyadicInterval, t) (ChPoly c)
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly x of
      Right c -> Right $ ChPoly dom (Poly $ terms_fromList [(0,c)])
      Left e -> Left e

{- addition -}

-- PolyBall level
instance (IsBall t, CanAddSameType t) => CanAddAsymmetric (Ball t) (Ball t) where
  type AddType  (Ball t) (Ball t) = Ball t
  add (Ball x1 e1) (Ball x2 e2) =
    Ball x (e1 + e2 + xe)
    where
    xB = x1 + x2
    x = centreAsBall xB
    xe = radius xB

-- ChPoly level
instance (CanAddSameType c) => CanAddAsymmetric (ChPoly c) (ChPoly c) where
  type AddType (ChPoly c) (ChPoly c) = ChPoly c
  add (ChPoly d1 p1) (ChPoly d2 p2)
    | d1 == d2 = ChPoly d1 (p1 + p2)
    | otherwise = error $ "Adding polynomials with incompatible domains"

{- multiplication -}

-- PolyBall level
instance (IsBall c, Ring c, CanDivBy c Integer)
  =>
  CanMulAsymmetric (Ball c) (Ball c) where
  type MulType  (Ball c) (Ball c) = Ball c
  mul (Ball x1 e1) (Ball x2 e2) =
    Ball x xe
    where
    xB = x1e1 * x2e2
    x = centreAsBall xB
    xe = radius xB
    x1e1 = updateRadius (+ e1) x1
    x2e2 = updateRadius (+ e2) x2
    -- TODO: use norm computed using root finding?
    --  is it too expensive?  check once we have benchmarking

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
