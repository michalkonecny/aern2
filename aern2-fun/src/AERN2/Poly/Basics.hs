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
import qualified Prelude as P
-- import Text.Printf

import qualified Data.Map as Map
import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.TH

-- import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

{- types -}

{-|
  a shortcut type constraint for
  types suitable as coefficients of our polynomials
-}
type PolyCoeff c =
  (Field c, CanMulBy c CauchyReal, HasDyadics c
  , IsInterval c c, IsBall c
  , HasAccuracy c, HasNorm c, CanSetPrecision c
  , Show c)

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

terms_toDescList :: Terms c -> [(Degree, c)]
terms_toDescList = Map.toDescList

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

terms_updateConst :: (HasIntegers c) => (c -> c) -> Terms c -> Terms c
terms_updateConst updateFn ts =
  case Map.lookup 0 ts of
    Nothing -> Map.insert 0 (updateFn $ convertExactly 0) ts
    Just _  -> Map.adjust updateFn 0 ts

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

{- precision -}

instance (HasPrecision c) => HasPrecision (Poly c) where
  getPrecision (Poly ts) = foldl1 max $ map getPrecision $ terms_coeffs ts

instance (CanSetPrecision c) => CanSetPrecision (Poly c) where
  setPrecision p (Poly ts) = Poly $ terms_map (setPrecision p) ts

{- accuracy -}

instance (HasAccuracy c) => HasAccuracy (Poly c) where
  getAccuracy (Poly ts) = foldl1 min $ map getAccuracy $ terms_coeffs ts

{- negation -}

instance (CanNegSameType c) => CanNeg (Poly c) where
  type NegType (Poly c) = Poly c
  negate (Poly t1) = Poly $ terms_map negate t1

{- addition -}

instance (CanAddSameType c) => CanAddAsymmetric (Poly c) (Poly c) where
  type AddType (Poly c) (Poly c) = Poly c
  add (Poly t1) (Poly t2) = Poly $ terms_unionWith (+) t1 t2


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric $t (Poly c) where
      type AddType $t (Poly c) = Poly c
      add n (Poly t2) = Poly $ terms_updateConst (+ n) t2

    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric (Poly c) $t where
      type AddType (Poly c) $t = Poly c
      add (Poly t1) n = Poly $ terms_updateConst (+ n) t1
  |]))

{- subtraction -}

instance (CanNegSameType c, CanAddSameType c) => CanSub (Poly c) (Poly c)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanNegSameType c, CanAddThis c $t, HasIntegers c) => CanSub $t (Poly c)
    instance (CanAddThis c $t, HasIntegers c) => CanSub (Poly c) $t
  |]))

{- scaling -}

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy c $t) => CanMulAsymmetric $t (Poly c) where
      type MulType $t (Poly c) = Poly c
      mul n (Poly t2) = Poly $ terms_map (* n) t2

    instance (CanMulBy c $t) => CanMulAsymmetric (Poly c) $t where
      type MulType (Poly c) $t = Poly c
      mul (Poly t1) n = Poly $ terms_map (* n) t1
  |]))


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanDivBy c $t) => CanDiv (Poly c) $t where
      type DivType (Poly c) $t = Poly c
      divide (Poly t1) n = Poly $ terms_map (/ n) t1
  |]))


  {- show -}

instance (Show c, HasIntegers c) => Show (Poly c) where
    show (Poly terms) =
        formatTerms showCf terms
        where
        showCf c =
            --(show (c::MPBall), (c == (convertExactly 0)) == Just True, (c == (convertExactly 1)) == Just True)
            (show c, False, False)

formatTerms ::
  (HasIntegers c) =>
  (c -> (String, Bool, Bool)) -> Terms c -> String
formatTerms showCf terms =
    showTerms ("", "-") $
      List.sortBy (\(a,_) (b,_) -> P.compare a b) $
            termsToShow
    where
    showTerms (connectivePos, connectiveNeg) (term : rest) =
        termS ++ (showTerms (" + ", " - ") rest)
        where
        termS =
            case s of
                '-':ss -> connectiveNeg ++ ss
                _ -> connectivePos ++ s
        s = showTerm term
    showTerms _ [] = ""
    termsToShow =
        if null termsToShow_pre
            then [(0, convertExactly 0)]
            else termsToShow_pre
    termsToShow_pre =
        filter coeffNotExactZero $
            terms_toList terms
    coeffNotExactZero (_, cf) =
        not isZero
        where
        (_, isZero, _) = showCf cf
    showTerm (deg, coeff)
        | deg == 0 = coeffS
        | isOne = showPower
        | otherwise = coeffS ++ "*" ++ showPower
        where
        (coeffS, _, isOne) = showCf coeff
        showPower
            | deg == 1 = "x"
            | otherwise = "x^" ++ show deg
