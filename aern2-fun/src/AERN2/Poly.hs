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
-- (
-- )
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

import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
import AERN2.RealFun.UnaryFun

{- types -}

type PolyBall = Ball (ChPoly MPBall)

data Ball c = Ball { ball_value :: c, ball_radius :: ErrorBound }

data ChPoly c = ChPoly { chPoly_dom :: DyadicInterval, chPoly_poly :: Poly c }

data Poly c = Poly { poly_coeffs :: Terms c }

type Terms c = Map.Map Integer c

{- addition -}

-- PolyBall level
instance (IsBall c, CanAddSameType c) => CanAddAsymmetric (Ball c) (Ball c) where
  type AddType  (Ball c) (Ball c) = Ball c
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

-- Poly level
instance (CanAddSameType c) => CanAddAsymmetric (Poly c) (Poly c) where
  type AddType (Poly c) (Poly c) = Poly c
  add (Poly t1) (Poly t2) = Poly $ Map.unionWith (+) t1 t2

{- multiplication -}

{- range -}

instance CanApply (ChPoly MPBall) DyadicInterval where
  type ApplyType (ChPoly MPBall) DyadicInterval = Interval CauchyReal CauchyReal
  apply p = apply (unaryFun p)

instance ConvertibleExactly (ChPoly MPBall) UnaryFun where
  safeConvertExactly (ChPoly dom p) = Right $ UnaryFun dom eval
    where
    eval x = x -- TODO
