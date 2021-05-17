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
(
  PolyCoeffRing, PolyCoeffField, PolyCoeffBall
  , Poly(..), Degree, Terms
  , terms_empty
  , terms_size
  , terms_insertWith
  , terms_toList, terms_toDescList
  , terms_fromList, terms_fromListAddCoeffs
  , terms_unionWith
  , terms_map
  , terms_filterKeepConst
  , terms_filterMayLoseConst
  , terms_degree, terms_degrees
  , terms_coeffs
  , terms_updateConst, terms_updateReturnConst
  , terms_lookupCoeff, terms_lookupCoeffDoubleConstTerm
  , formatTerms
)
where

import MixedTypesNumPrelude
import qualified Prelude as P
-- import Text.Printf

import qualified Data.Map as Map
import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import Control.CollectErrors

-- import AERN2.MP.ErrorBound
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryBallFun

{- types -}

{-|
  An aggregate sub-class for
  types suitable as coefficients of our polynomials,
  loose enough to permit Integer coefficients.
-}
class
  (Ring c, HasIntegers c, HasAccuracy c, HasNorm c, Show c) =>
  PolyCoeffRing c

instance PolyCoeffRing Integer
instance PolyCoeffRing Dyadic
instance PolyCoeffRing Rational
instance PolyCoeffRing MPBall

{-|
  An aggregate sub-class for
  types suitable as coefficients of our polynomials,
  loose enough to permit Rational coefficients.
-}
class
  (PolyCoeffRing c, Field c, HasDyadics c, CanAddSubMulDivCNBy c Dyadic) =>
  PolyCoeffField c

instance PolyCoeffField Rational
instance PolyCoeffField MPBall

{-|
  An aggregate sub-class for
  types suitable as coefficients of our polynomials
-}
class
  (PolyCoeffField c, CanAddSubMulDivCNBy c CauchyReal
  , IsInterval c, CanMinMaxSameType (IntervalEndpoint c), IsBall c, CanSetPrecision c)
  =>
  PolyCoeffBall c

instance PolyCoeffBall MPBall

newtype Poly c = Poly { poly_terms :: Terms c }

instance (CanBeErrors es) => CanEnsureCE es (Poly c)

instance (CanBeErrors es) => CanExtractCE es Poly
  where
  extractCE sample_es (Poly terms) =
    fmap Poly (extractCE sample_es terms)

type Terms c = Map.Map Degree c

type Degree = Integer

instance (CanBeErrors es) => CanExtractCE es (Map.Map Degree)

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

terms_fromList :: (HasIntegers c) => [(Degree, c)] -> Terms c
terms_fromList coeffs =
  case Map.lookup 0 ts of
    Nothing -> Map.insert 0 (convertExactly 0) ts
    _ -> ts
  where
  ts = Map.fromList coeffs

terms_fromListAddCoeffs :: (CanAddSameType c, HasIntegers c) => [(Degree, c)] -> Terms c
terms_fromListAddCoeffs newTerms =
    foldl addTerm terms_empty ((0, convertExactly 0) : newTerms)
    where
    addTerm prevTerms (i,a) =
        terms_insertWith (+) i a prevTerms

terms_unionWith :: (c -> c -> c) -> Terms c -> Terms c -> Terms c
terms_unionWith = Map.unionWith

terms_filterMayLoseConst :: (Degree -> c -> Bool) -> Terms c -> Terms c
terms_filterMayLoseConst = Map.filterWithKey

terms_filterKeepConst :: (Degree -> c -> Bool) -> Terms c -> Terms c
terms_filterKeepConst cond = Map.filterWithKey cond_leaveConst
  where
  cond_leaveConst k a
    | k == 0 = True
    | otherwise = cond k a

terms_degree :: Terms c -> Degree
terms_degree ts
  | null ts = error "terms_degree called with empty terms"
  | otherwise = fst $ Map.findMax ts

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

terms_updateReturnConst :: (HasIntegers c) => (c -> c) -> Terms c -> (Terms c,c,c)
terms_updateReturnConst updateFn ts =
  case Map.lookup 0 ts of
    Nothing -> 
      let new = updateFn z in (Map.insert 0 new ts, z, new)
    Just old -> 
      let new = updateFn old in (Map.insert 0 new ts, old, new)
  where
  z = convertExactly 0

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
  getFiniteAccuracy (Poly ts) = foldl1 min $ map getFiniteAccuracy $ terms_coeffs ts

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
    instance (CanDivCNBy c $t, CanEnsureCN (DivType c $t), EnsureNoCN (DivType c $t) ~ c) => CanDiv (Poly c) $t where
      type DivType (Poly c) $t = (Poly (EnsureCN c))
      divide (Poly t1) n = Poly $ terms_map (/ n) t1
      type DivTypeNoCN (Poly c) $t = Poly c
      divideNoCN (Poly t1) n = Poly $ terms_map (/! n) t1
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
