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

import qualified Data.List as List
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

type Terms c = Map.Map Degree c

type Degree = Integer

terms_empty :: Terms c
terms_empty = Map.empty

terms_insertWith :: (c -> c -> c) -> Degree -> c -> Terms c -> Terms c
terms_insertWith = Map.insertWith

terms_toList :: Terms c -> [(Degree, c)]
terms_toList = Map.toList

terms_fromList :: [(Degree, c)] -> Terms c
terms_fromList = Map.fromList

terms_fromListAddCoeffs :: (CanAddSameType c) => [(Degree, c)] -> Terms c
terms_fromListAddCoeffs newTerms =
    foldl addTerm terms_empty newTerms
    where
    addTerm prevTerms (i,a) =
        terms_insertWith (+) i a prevTerms

terms_unionWith :: (c -> c -> c) -> Terms c -> Terms c -> Terms c
terms_unionWith = Map.unionWith

terms_degree :: Terms c -> Degree
terms_degree = fst . Map.findMax

terms_degrees :: Terms c -> [Degree]
terms_degrees = Map.keys

terms_coeffs :: Terms c -> [c]
terms_coeffs = Map.elems

terms_map :: (c1 -> c2) -> Terms c1 -> Terms c2
terms_map = Map.map


instance (IsBall c) => IsBall (ChPoly c) where
  type CentreType (ChPoly c) = ChPoly c
  radius (ChPoly _dom (Poly terms)) =
    List.foldl' (+) (errorBound 0) $ map radius $ terms_coeffs terms
  centre (ChPoly dom (Poly terms)) =
    ChPoly dom (Poly (terms_map centreAsBall terms))
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)

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

-- Poly level
instance (CanAddSameType c) => CanAddAsymmetric (Poly c) (Poly c) where
  type AddType (Poly c) (Poly c) = Poly c
  add (Poly t1) (Poly t2) = Poly $ terms_unionWith (+) t1 t2

{- multiplication -}

-- PolyBall level
instance (IsBall c, Ring c, CanDivBy c Integer)
  =>
  CanMulAsymmetric (Ball c) (Ball c) where
  type MulType  (Ball c) (Ball c) = Ball c
  mul (Ball x1 e1) (Ball x2 e2) =
    Ball x (e1*e2 + e1*x2Norm + e2*x1Norm + xe)
    where
    xB = x1 * x2
    x = centreAsBall xB
    xe = radius xB
    x1Norm = undefined :: ErrorBound -- TODO
    x2Norm = undefined :: ErrorBound -- TODO

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

{- range -}

instance CanApply (ChPoly MPBall) DyadicInterval where
  type ApplyType (ChPoly MPBall) DyadicInterval = (Interval CauchyReal CauchyReal, ErrorBound)
  apply = rangeViaUnaryFun
  -- apply = rangeViaRoots

rangeViaUnaryFun :: (ChPoly MPBall) -> DyadicInterval -> (Interval CauchyReal CauchyReal, ErrorBound)
rangeViaUnaryFun p di = (apply f di, e)
  where
  f :: UnaryFun
  (f, e) = convertExactly p

instance ConvertibleExactly (ChPoly MPBall) (UnaryFun, ErrorBound) where
  safeConvertExactly cp@(ChPoly dom p) = Right (UnaryFun dom eval, e)
    where
    e = radius cp
    eval x = x -- TODO
