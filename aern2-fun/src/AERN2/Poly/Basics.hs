{-|
    Module      :  AERN2.Poly.Basics
    Description :  Basics of unary sparse polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Basics of unary sparse polynomials
-}

module AERN2.Poly.Basics
-- (
-- )
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import qualified Data.Map as Map

-- import Test.Hspec
-- import Test.QuickCheck

-- import AERN2.MP.ErrorBound
-- import AERN2.MP.Ball
-- import AERN2.MP.Dyadic

-- import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

{- types -}

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

terms_updateConst :: (c -> c) -> Terms c -> Terms c
terms_updateConst updateFn = Map.adjust updateFn 0

terms_lookupCoeffDoubleConstTerm ::
  (HasIntegers c, CanAddSameType c) =>
  (Terms c) -> Degree -> c
terms_lookupCoeffDoubleConstTerm t i
  | i == 0 = c+c
  | otherwise = c
  where
  c = terms_lookupCoeff t i

terms_lookupCoeff ::
  (HasIntegers c) =>
  (Terms c) -> Degree -> c
terms_lookupCoeff t i =
  case Map.lookup i t of
    Just c -> c
    _ -> convertExactly 0


{- addition -}

instance (CanAddSameType c) => CanAddAsymmetric (Poly c) (Poly c) where
  type AddType (Poly c) (Poly c) = Poly c
  add (Poly t1) (Poly t2) = Poly $ terms_unionWith (+) t1 t2
