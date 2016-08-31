{-|
    Module      :  AERN2.Poly
    Description :  Unary sparse polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Unary sparse polynomials
-}

module AERN2.Poly
(
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import qualified Data.List as List
import qualified Data.Map as Map

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.MP.ErrorBound
import AERN2.MP.Ball

import AERN2.Interval

type PolyBall = Ball (ChPoly MPBall)

data Ball c = Ball { ball_value :: c, ball_radius :: ErrorBound }

instance (IsBall c, CanAddSameType c) => CanAddAsymmetric (Ball c) (Ball c) where
  type AddType  (Ball c) (Ball c) = Ball c
  add (Ball x1 e1) (Ball x2 e2) =
    Ball x (e1 + e2 + xe)
    where
    xB = x1 + x2
    x = centreAsBall xB
    xe = radius xB

data ChPoly c = ChPoly { chPoly_poly :: Poly c, chPoly_dom :: DyadicInterval }

instance (CanAddSameType c) => CanAddAsymmetric (ChPoly c) (ChPoly c) where
  type AddType (ChPoly c) (ChPoly c) = ChPoly c
  add (ChPoly p1 d1) (ChPoly p2 d2)
    | d1 == d2 = ChPoly (p1 + p2) d1
    | otherwise = error $ "Adding polynomials with incompatible domains"

data Poly c = Poly { poly_coeffs :: Terms c }

type Terms c = Map.Map Integer c

instance (CanAddSameType c) => CanAddAsymmetric (Poly c) (Poly c) where
  type AddType (Poly c) (Poly c) = Poly c
  add (Poly t1) (Poly t2) = Poly $ Map.unionWith (+) t1 t2
