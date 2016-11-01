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
import AERN2.MP.Ball (IsBall(..), MPBall)
import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

{- Chebyshev polynomials with domain translation -}

data ChPoly c = ChPoly { chPoly_dom :: DyadicInterval, chPoly_poly :: Poly c }

instance HasDomain (ChPoly c) where
  type Domain (ChPoly c) = DyadicInterval
  getDomain = chPoly_dom

instance (HasDyadics c) => HasVars (ChPoly c) where
  type Var (ChPoly c) = ()
  varFn sampleFn () =
    ChPoly dom (Poly terms)
    where
    dom@(Interval l r) = getDomain sampleFn
    terms = terms_fromList [(0, c0), (1, c1)]
    c0 = coeff $ (r + l) * 0.5
    c1 = coeff $ (r - l) * 0.5
    coeff = convertExactly

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

{- Polynomial balls -}

type PolyBall = Ball (ChPoly MPBall)

polyBall :: (ConvertibleExactly t PolyBall) => t -> PolyBall
polyBall = convertExactly

data Ball c = Ball { ball_value :: c, ball_radius :: ErrorBound }

instance (ConvertibleExactly (DyadicInterval, t) c) => ConvertibleExactly (DyadicInterval, t) (Ball c)
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly (dom, x) of
      Right c -> Right $ Ball c (errorBound 0)
      Left e -> Left e

instance (HasDomain c) => HasDomain (Ball c)
  where
  type Domain (Ball c) = Domain c
  getDomain = getDomain . ball_value

instance (HasVars c) => HasVars (Ball c) where
  type Var (Ball c) = Var c
  varFn (Ball c _) var = Ball (varFn c var) (errorBound 0)
