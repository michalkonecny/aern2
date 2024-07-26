module AERN2.AffArith.Ring
  (
  )
where

import AERN2.AffArith.Conversions
import AERN2.AffArith.Type
import AERN2.MP (ErrorBound, MPBall (MPBall), errorBound, mpBallP, prec, Kleenean, mpBall)
import AERN2.MP.Float (mpFloat, (*^), (+^))
import Data.Hashable
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import GHC.Records
import MixedTypesNumPrelude
import qualified Prelude as P

instance CanNeg MPAffine where
  type NegType MPAffine = MPAffine
  negate aff =
    aff
      { centre = negate aff.centre,
        errTerms = fmap negate aff.errTerms
      }

instance CanAddAsymmetric MPAffine MPAffine where
  type AddType MPAffine MPAffine = MPAffine
  add aff1 aff2 =
    mpAffNormalise $ MPAffine {config, centre, errTerms}
    where
      config = aff1.config <> aff2.config
      (centre, eCentre) = mpBallOpOnMPFloat2 (+) aff1.centre aff2.centre

      -- add coeffs with common error variables, tracking any new errors introduced through that:
      (termsAdded, eTerms) = addErrTerms aff1.errTerms aff2.errTerms

      errTerms
        | e == 0 = termsAdded
        | otherwise = Map.insert newTermId (mpFloat e) termsAdded
        where
          newTermId = ErrorTermId (hash ("+", aff1, aff2))
          e = eCentre + eTerms

addErrTerms :: MPAffineErrorTerms -> MPAffineErrorTerms -> (MPAffineErrorTerms, ErrorBound)
addErrTerms terms1 terms2 =
  ( separateTerms `Map.union` combinedTerms,
    sumErrorBounds combinationErrors
  )
  where
    -- terms for variables that appear in only one of the operands go in unchanged:
    separateTerms = mapIntersectionComplement terms1 terms2

    -- variables that have coefficients in both operands need to be added:
    combinedTermsWithErrors = Map.intersectionWith (mpBallOpOnMPFloat2 (+)) terms1 terms2
    combinedTerms = Map.map fst combinedTermsWithErrors
    combinationErrors = map snd $ Map.elems combinedTermsWithErrors

sumErrorBounds :: [ErrorBound] -> ErrorBound
sumErrorBounds = foldl' (+) (errorBound 0)

mapIntersectionComplement :: (P.Ord k) => Map.Map k v -> Map.Map k v -> Map.Map k v
mapIntersectionComplement map1 map2 =
  map1minus2 `Map.union` map2minus1
  where
    map1minus2 = Map.filterWithKey (\errId _ -> isNothing (map2 Map.!? errId)) map1
    map2minus1 = Map.filterWithKey (\errId _ -> isNothing (map1 Map.!? errId)) map2

instance CanAddAsymmetric MPAffine Integer where
  type AddType MPAffine Integer = MPAffine
  add aff n = add aff (mpAffine aff.config n)

instance CanAddAsymmetric MPAffine Int where
  type AddType MPAffine Int = MPAffine
  add aff n = add aff (integer n)

instance CanAddAsymmetric MPAffine Rational where
  type AddType MPAffine Rational = MPAffine
  add aff q = add aff (mpAffine aff.config q)

instance CanAddAsymmetric Integer MPAffine where
  type AddType Integer MPAffine = MPAffine
  add n aff = add aff n -- use commutativity of addition

instance CanAddAsymmetric Int MPAffine where
  type AddType Int MPAffine = MPAffine
  add n aff = add aff n -- use commutativity of addition

instance CanAddAsymmetric Rational MPAffine where
  type AddType Rational MPAffine = MPAffine
  add q aff = add aff q -- use commutativity of addition

-- Subtraction defined using the default instances via add and neg:
instance CanSub MPAffine MPAffine

instance CanSub Integer MPAffine

instance CanSub MPAffine Integer

instance CanSub Int MPAffine

instance CanSub MPAffine Int

instance CanSub Rational MPAffine

instance CanSub MPAffine Rational

{-
  Scaling
-}

instance CanMulAsymmetric MPBall MPAffine where
  type MulType MPBall MPAffine = MPAffine
  mul b aff =
    mpAffNormalise $ MPAffine {config, centre, errTerms}
    where
      config = aff.config

      -- scale the centre, note the error:
      affCentreBall = MPBall aff.centre (errorBound 0)
      MPBall centre eCentre = b * affCentreBall

      -- scale all coeffs, bounding all rounding errors:
      (scaledTerms, eTerms) = scaleErrTerms b aff.errTerms

      -- assemble the new error terms from the scaled terms and a new term for rounding errors incurred while scaling:
      errTerms
        | e == 0 = scaledTerms
        | otherwise = Map.insert newTermId (mpFloat e) scaledTerms
        where
          newTermId = ErrorTermId (hash ("*", b, aff))
          e = eCentre + eTerms

scaleErrTerms :: MPBall -> MPAffineErrorTerms -> (MPAffineErrorTerms, ErrorBound)
scaleErrTerms b terms = (scaledTerms, sumErrorBounds scalingErrors)
  where
    -- scale the error terms as MPBalls:
    scaledTermsBalls = Map.map scaleCoeff terms
      where
        scaleCoeff coeff = b * MPBall coeff (errorBound 0)

    -- separate the centres and error bounds:
    scaledTerms = Map.map (\(MPBall centre _) -> centre) scaledTermsBalls
    scalingErrors = map (\(MPBall _ ce) -> ce) (Map.elems scaledTermsBalls)

instance CanMulAsymmetric Integer MPAffine where
  type MulType Integer MPAffine = MPAffine
  mul n aff = mul (mpBallP p n) aff
    where
      p = prec aff.config.precision

instance CanMulAsymmetric Int MPAffine where
  type MulType Int MPAffine = MPAffine
  mul n = mul (integer n)
instance CanMulAsymmetric Rational MPAffine where
  type MulType Rational MPAffine = MPAffine
  mul q aff = mul (mpBallP p q) aff
    where
      p = prec aff.config.precision

instance CanMulAsymmetric MPAffine MPBall where
  type MulType MPAffine MPBall = MPAffine
  mul b aff = mul aff b -- using commutativity of multiplication

instance CanMulAsymmetric MPAffine Integer where
  type MulType MPAffine Integer = MPAffine
  mul n aff = mul aff n -- using commutativity of multiplication

instance CanMulAsymmetric MPAffine Int where
  type MulType MPAffine Int = MPAffine
  mul n aff = mul aff n -- using commutativity of multiplication

instance CanMulAsymmetric MPAffine Rational where
  type MulType MPAffine Rational = MPAffine
  mul q aff = mul aff q -- using commutativity of multiplication

{-
  Multiplication
-}

instance CanMulAsymmetric MPAffine MPAffine where
  type MulType MPAffine MPAffine = MPAffine
  mul aff1 aff2 =
    mpAffNormalise $ MPAffine {config, centre, errTerms}
    where
      config = aff1.config <> aff2.config

      {-
          (centre1 + terms1) * (centre2 + terms2)
            = centre1*centre2 + centre1*terms2 + centre2*terms1 + terms1*terms2

          (quadratic) terms1*terms2 -> one error bound
      -}

      -- new centre:
      centre1Ball = MPBall aff1.centre (errorBound 0)
      centre2Ball = MPBall aff2.centre (errorBound 0)
      MPBall centre eCentre = centre1Ball * centre2Ball

      -- scaling linear terms:
      (scaledTerms1, eScaled1) = scaleErrTerms centre2Ball aff1.errTerms
      (scaledTerms2, eScaled2) = scaleErrTerms centre1Ball aff2.errTerms
      (scaledTerms, eScaledAdd) = addErrTerms scaledTerms1 scaledTerms2

      -- bounds for quadratic terms:
      {-
          |(v1*c1 + ... + vn*cn) * (w1*d1 + ... + wm*dm)|
            = |Sum_{i<=n,j<=m} vi*ci*wj*dj|
            <= Sum_{i<=n,j<=m} |ci|*|dj|
            = (|c1| + ... + |cn|) * (|d1| + ... + |dm|)
      -}
      terms1Bound = foldl' (+^) (mpFloat 0) $ map abs $ Map.elems aff1.errTerms
      terms2Bound = foldl' (+^) (mpFloat 0) $ map abs $ Map.elems aff2.errTerms
      quadraticTermsBound = terms1Bound *^ terms2Bound

      errTerms
        | e == 0 = scaledTerms
        | otherwise = Map.insert newTermId e scaledTerms
        where
          newTermId = ErrorTermId (hash ("*", aff1, aff2))
          e = quadraticTermsBound +^ mpFloat (eCentre + eScaled1 + eScaled2 + eScaledAdd)

instance HasEqAsymmetric MPAffine MPAffine where
  type EqCompareType MPAffine MPAffine = Kleenean
  equalTo aff1 aff2 = mpBall (aff1 - aff2) == 0

instance HasEqAsymmetric MPAffine Integer where
  type EqCompareType MPAffine Integer = Kleenean
  equalTo aff1 n = mpBall aff1 == n

instance HasEqAsymmetric Integer MPAffine where
  type EqCompareType Integer MPAffine = Kleenean
  equalTo n aff2 = n == mpBall aff2

instance HasEqAsymmetric MPAffine Int where
  type EqCompareType MPAffine Int = Kleenean
  equalTo aff1 n = mpBall aff1 == n

instance HasEqAsymmetric Int MPAffine where
  type EqCompareType Int MPAffine = Kleenean
  equalTo n aff2 = n == mpBall aff2

instance HasEqAsymmetric MPAffine Rational where
  type EqCompareType MPAffine Rational = Kleenean
  equalTo aff1 q = mpBall aff1 == q

instance HasEqAsymmetric Rational MPAffine where
  type EqCompareType Rational MPAffine = Kleenean
  equalTo q aff2 = q == mpBall aff2

-- instance Ring MPAffine 
-- ^^^this needs a change in mixed-types-num: conversion from integers with a sample