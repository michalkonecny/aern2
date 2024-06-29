{-# HLINT ignore "Use logBase" #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import AERN2.MP (ErrorBound, MPBall (MPBall), ShowWithAccuracy (..), ac2prec, bits, errorBound, mpBallP, prec)
import AERN2.MP.Accuracy (Accuracy (..))
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Float (MPFloat, mpFloat, (*^), (+^))
import Data.CDAR (Approx (..))
import Data.Hashable
import Data.List (foldl', foldl1')
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
import qualified Prelude as P

main :: IO ()
main = putStrLn "test"

{-
  Basic type
-}

deriving instance Generic Approx

instance Hashable Approx

instance Hashable MPFloat

instance Hashable MPBall

instance Hashable ErrorBound

newtype ErrorTermId = ErrorTermId Int
  deriving (P.Eq, P.Ord, Generic, Show)

instance Hashable ErrorTermId

data MPAffineConfig = MPAffineConfig
  { maxTerms :: Int,
    precision :: Integer -- used only when converting from integers, rationals etc
  }
  deriving (P.Eq, Generic, Show)

instance Semigroup MPAffineConfig where
  config1 <> config2 =
    MPAffineConfig
      { maxTerms = max config1.maxTerms config2.maxTerms,
        precision = max config1.precision config2.precision
      }

-- |
--  An affine form representing a real number with imprecision given
--  by linear terms in a number of error variables.
--  All the error variables have the range [-1,1].
--  The linear coefficients of the error variables combine the magnitude
--  of the error and effects of the same error variable being combined
--  via arithmetic operations used to compute this number.
--
--  @x - x@ should always be 0 with no imprecision as the linear error terms
--  within @x@ get cancelled.
--
--  @x*y - y*x@ may contain some imprecision as the imprecision arising from the two
--  products are not seen as identical and thus do not get cancelled.
--
--  The affine form contains a copy of configuration object specifying
--  the maximum number of error terms allowed and the default MPFloat precision
--  for the centre and the coefficients.
--
--  ⚠️ Warning: Arithmetic over these affine forms is safe only as far as
--  the hash function behaves injectively.  We use @hash@ to determine
--  the ID of each error variable.  It is essential that only identical sources
--  of error have the same error variable ID.
data MPAffine = MPAffine
  { config :: MPAffineConfig,
    centre :: MPFloat,
    -- | error variables and their coefficients
    errTerms :: MPAffineErrorTerms
  }
  deriving (P.Eq, Generic)

type MPAffineErrorTerms = Map.Map ErrorTermId MPFloat

instance Hashable MPAffineConfig

instance Hashable MPAffine

instance Show MPAffine where
  show = showWithAccuracy (bits 10)

instance ShowWithAccuracy MPAffine where
  showWithAccuracy displayAC (MPAffine {centre, errTerms}) =
    -- printf "[%s ± %s](prec=%s)" (show x) (showAC $ getAccuracy b) (show $ integer $ getPrecision b)
    printf "[%s%s]" (dropSomeDigits $ show centre) (termsS :: String)
    where
      termsS = concatMap showTerm (Map.toList errTerms)
      showTerm (ErrorTermId h, e :: MPFloat) =
        printf " %s{H%s}" eDS hSTrimmed
        where
          hSTrimmed = take 3 hS
          hS = show (abs h)
          eDS
            | e == 0 = "+ 0"
            | otherwise =
                case safeConvert (dyadic e) of
                  Right (eD :: Double) ->
                    if eD < 0
                      then printf "-%.4g" (-eD)
                      else printf "+%.4g" eD
                  _ -> ""
      dropSomeDigits s =
        maybe s withDotIx (List.elemIndex '.' s)
        where
          withDotIx ix =
            let maxLength = ix + displayAC_n
             in let sTrimmed = take maxLength s
                 in if length sTrimmed < maxLength
                      then sTrimmed
                      else take (maxLength - 3) sTrimmed <> "..."
      displayAC_n =
        case displayAC of
          Exact -> 1000000000
          NoInformation -> 0
          _ -> round $ (log (double 2) / log (double 10)) * integer (ac2prec displayAC)

mpAffNormalise :: MPAffine -> MPAffine
mpAffNormalise aff@(MPAffine {config, errTerms})
  | termsNoZeroesSize > maxTerms = aff {errTerms = newTerms}
  | hasSomeZeroes = aff {errTerms = termsNoZeroes}
  | otherwise = aff
  where
    termsNoZeroes = Map.filter (/= 0) errTerms

    termsNoZeroesSize = Map.size termsNoZeroes
    hasSomeZeroes = termsNoZeroesSize < Map.size errTerms

    (MPAffineConfig {maxTerms}) = config

    -- sort terms by their coefficients' abs values, smallest to largest:
    termsAscending = sortWith (abs . snd) (Map.toList termsNoZeroes)

    -- keep maxTerms - 1 terms, then replace the others with 1 new term:
    (termsToRemove, termsToKeep) = splitAt (termsNoZeroesSize - maxTerms + 1) termsAscending

    newTermId = ErrorTermId (hash aff)
    newTermCoeff = foldl1' (+^) coeffsToRemove -- sum, rounding upwards, at least 2 coeffs to remove
      where
        coeffsToRemove = map (abs . snd) termsToRemove
    newTerms = Map.fromList ((newTermId, newTermCoeff) : termsToKeep) -- maxTerms-many terms

{-
  Arithmetic helpers
-}

mpBallOpOnMPFloat2 ::
  (MPBall -> MPBall -> MPBall) ->
  (MPFloat -> MPFloat -> (MPFloat, ErrorBound))
mpBallOpOnMPFloat2 op x y = (c, e)
  where
    MPBall c e = op (MPBall x e0) (MPBall y e0)
    e0 = errorBound 0

{-
  Conversions
-}

instance ConvertibleExactly MPAffine MPBall where
  safeConvertExactly :: MPAffine -> ConvertResult MPBall
  safeConvertExactly aff = Right $ MPBall centre e
    where
      mpAffineFlattened = mpAffNormalise $ aff {config = aff.config {maxTerms = int 1}}
      (MPAffine {centre, errTerms}) = mpAffineFlattened
      e = errorBound $ abs $ snd $ head (Map.toList errTerms) -- should have one term only

type CanBeMPAffine t = ConvertibleExactly (MPAffineConfig, t) MPAffine

mpAffine :: (CanBeMPAffine t) => MPAffineConfig -> t -> MPAffine
mpAffine config t = convertExactly (config, t)

instance ConvertibleExactly (MPAffineConfig, (ErrorTermId, MPBall)) MPAffine where
  safeConvertExactly :: (MPAffineConfig, (ErrorTermId, MPBall)) -> ConvertResult MPAffine
  safeConvertExactly (config, (key, MPBall c e))
    | e == 0 =
        Right $ MPAffine {config, centre = c, errTerms = Map.empty}
    | otherwise =
        Right $ MPAffine {config, centre = c, errTerms = Map.singleton key (mpFloat e)}

mpAffineFromBall :: (Hashable errIdItem) => MPAffineConfig -> errIdItem -> MPBall -> MPAffine
mpAffineFromBall config errIdItem b =
  mpAffine config (ErrorTermId (hash errIdItem), b)

instance ConvertibleExactly (MPAffineConfig, Integer) MPAffine where
  safeConvertExactly :: (MPAffineConfig, Integer) -> ConvertResult MPAffine
  safeConvertExactly (config, n) =
    Right $ mpAffineFromBall config n (mpBallP p n)
    where
      p = prec config.precision

instance ConvertibleExactly (MPAffineConfig, Rational) MPAffine where
  safeConvertExactly :: (MPAffineConfig, Rational) -> ConvertResult MPAffine
  safeConvertExactly (config, q) =
    Right $ mpAffineFromBall config q (mpBallP p q)
    where
      p = prec config.precision

{-
  Addition and subtraction
-}

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

instance CanAddAsymmetric MPAffine Rational where
  type AddType MPAffine Rational = MPAffine
  add aff q = add aff (mpAffine aff.config q)

instance CanAddAsymmetric Integer MPAffine where
  type AddType Integer MPAffine = MPAffine
  add n aff = add aff n -- use commutativity of addition

instance CanAddAsymmetric Rational MPAffine where
  type AddType Rational MPAffine = MPAffine
  add q aff = add aff q -- use commutativity of addition

-- Subtraction defined using the default instances via add and neg:
instance CanSub MPAffine MPAffine

instance CanSub Integer MPAffine

instance CanSub MPAffine Integer

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

-- TODO

{-
  Ad-hoc tests
-}

_conf :: MPAffineConfig
_conf = MPAffineConfig {maxTerms = int 4, precision = 100}

_mpaff1 :: MPAffine
_mpaff1 =
  MPAffine
    { config = _conf,
      centre = mpFloat 1,
      errTerms =
        Map.fromList
          [ (ErrorTermId (int 1), mpFloat (dyadic (-0.5))),
            (ErrorTermId (int 2), mpFloat (dyadic 0.5))
          ]
    }

_mpaff2 :: MPAffine
_mpaff2 =
  MPAffine
    { config = _conf,
      centre = mpFloat 1,
      errTerms =
        Map.fromList
          [ (ErrorTermId (int 1), mpFloat (dyadic 0.5)),
            (ErrorTermId (int 3), mpFloat (dyadic 0.5))
          ]
    }

_a100 :: MPAffine
_a100 = mpAffine _conf 100

_aThird :: MPAffine
_aThird = mpAffine _conf (1 / 3)
