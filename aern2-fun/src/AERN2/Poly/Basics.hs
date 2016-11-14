{-# LANGUAGE TemplateHaskell #-}
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

import AERN2.TH

-- import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

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

terms_size :: Terms c -> Integer
terms_size = integer . Map.size

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

terms_filter :: (k -> a -> Bool) -> Map.Map k a -> Map.Map k a
terms_filter = Map.filterWithKey

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

{- negation -}

instance (CanNegSameType c) => CanNeg (Poly c) where
  type NegType (Poly c) = Poly c
  negate (Poly t1) = Poly $ terms_map negate t1

{- addition -}

instance (CanAddSameType c) => CanAddAsymmetric (Poly c) (Poly c) where
  type AddType (Poly c) (Poly c) = Poly c
  add (Poly t1) (Poly t2) = Poly $ terms_unionWith (+) t1 t2


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
    instance (CanAddThis c $t) => CanAddAsymmetric $t (Poly c) where
      type AddType $t (Poly c) = Poly c
      add n (Poly t2) = Poly $ terms_updateConst (+ n) t2

    instance (CanAddThis c $t) => CanAddAsymmetric (Poly c) $t where
      type AddType (Poly c) $t = Poly c
      add (Poly t1) n = Poly $ terms_updateConst (+ n) t1
  |]))

{- subtraction -}

instance (CanNegSameType c, CanAddSameType c) => CanSub (Poly c) (Poly c)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
    instance (CanNegSameType c, CanAddThis c $t) => CanSub $t (Poly c)
    instance (CanAddThis c $t) => CanSub (Poly c) $t
  |]))

{- scaling -}

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
    instance (CanMulBy c $t) => CanMulAsymmetric $t (Poly c) where
      type MulType $t (Poly c) = Poly c
      mul n (Poly t2) = Poly $ terms_map (* n) t2

    instance (CanMulBy c $t) => CanMulAsymmetric (Poly c) $t where
      type MulType (Poly c) $t = Poly c
      mul (Poly t1) n = Poly $ terms_map (* n) t1
  |]))


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
    instance (CanDivBy c $t) => CanDiv (Poly c) $t where
      type DivType (Poly c) $t = Poly c
      divide (Poly t1) n = Poly $ terms_map (/ n) t1
  |]))
