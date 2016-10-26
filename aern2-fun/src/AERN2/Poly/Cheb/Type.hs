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
