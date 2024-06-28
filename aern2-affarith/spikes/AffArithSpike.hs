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
import AERN2.MP.Float (MPFloat, mpFloat, (+^))
import Data.CDAR (Approx (..))
import Data.Hashable
import Data.List (foldl1')
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
        coeffsToRemove = map snd termsToRemove
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

addErrTerms :: MPAffineErrorTerms -> MPAffineErrorTerms -> (MPAffineErrorTerms, ErrorBound)
addErrTerms terms1 terms2 =
  (Map.unions [terms1NotIn2, terms2NotIn1, combinedTerms], e)
  where
    terms1NotIn2 = Map.filterWithKey (\errId _ -> isNothing (terms2 Map.!? errId)) terms1
    terms2NotIn1 = Map.filterWithKey (\errId _ -> isNothing (terms1 Map.!? errId)) terms2

    combinedTermsWithErrors = Map.intersectionWith (mpBallOpOnMPFloat2 (+)) terms1 terms2
    combinedTerms = Map.map fst combinedTermsWithErrors
    combinationErrors = map snd $ Map.elems combinedTermsWithErrors
    e = List.foldl' (+) (errorBound 0) combinationErrors

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

      -- add coeffs with common error variables, tracking any new errors introduced through that

      (termsAdded, eTerms) = addErrTerms aff1.errTerms aff2.errTerms
      newTermId = ErrorTermId (hash ("+", aff1, aff2))
      e = eCentre + eTerms
      errTerms
        | e == 0 = termsAdded
        | otherwise = Map.insert newTermId (mpFloat e) termsAdded

instance CanAddAsymmetric MPAffine Integer where
  type AddType MPAffine Integer = MPAffine
  add aff n = add aff (mpAffine aff.config n)

instance CanAddAsymmetric MPAffine Rational where
  type AddType MPAffine Rational = MPAffine
  add aff q = add aff (mpAffine aff.config q)

instance CanAddAsymmetric Integer MPAffine where
  type AddType Integer MPAffine = MPAffine
  add n aff = add (mpAffine aff.config n) aff

instance CanAddAsymmetric Rational MPAffine where
  type AddType Rational MPAffine = MPAffine
  add q aff = add (mpAffine aff.config q) aff

-- Subtraction defined using the default instances via add and neg:
instance CanSub MPAffine MPAffine
instance CanSub Integer MPAffine
instance CanSub MPAffine Integer
instance CanSub Rational MPAffine
instance CanSub MPAffine Rational

{-
  Ad-hoc tests
-}

_conf :: MPAffineConfig
_conf = MPAffineConfig {maxTerms = int 2, precision = 100}

_mpaff1 :: MPAffine
_mpaff1 =
  MPAffine
    { config = _conf,
      centre = mpFloat 1,
      errTerms =
        Map.fromList
          [ (ErrorTermId (int 1), mpFloat 1),
            (ErrorTermId (int 2), mpFloat 1)
          ]
    }

_mpaff2 :: MPAffine
_mpaff2 =
  MPAffine
    { config = _conf,
      centre = mpFloat 1,
      errTerms =
        Map.fromList
          [ (ErrorTermId (int 1), mpFloat 1),
            (ErrorTermId (int 3), mpFloat 1)
          ]
    }

_a100 :: MPAffine
_a100 = mpAffine _conf 100

_aThird :: MPAffine
_aThird = mpAffine _conf (1 / 3)
