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
import AERN2.MP.Float (MPFloat, mpFloat)
import Data.CDAR (Approx (..))
import Data.Foldable (Foldable (foldl'))
import Data.Hashable
import qualified Data.List as List
import qualified Data.Map as Map
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

data MPAffine = MPAffine
  { config :: MPAffineConfig,
    centre :: MPFloat,
    terms :: Map.Map ErrorTermId ErrorBound -- the term coefficients must be positive
  }
  deriving (P.Eq, Generic)

instance Hashable MPAffineConfig

instance Hashable MPAffine

instance Show MPAffine where
  show = showWithAccuracy (bits 10)

instance ShowWithAccuracy MPAffine where
  showWithAccuracy displayAC (MPAffine {centre, terms}) =
    -- printf "[%s ± %s](prec=%s)" (show x) (showAC $ getAccuracy b) (show $ integer $ getPrecision b)
    printf "[%s%s]" (dropSomeDigits $ show centre) (termsS :: String)
    where
      termsS = concatMap showTerm (Map.toList terms)
      showTerm (ErrorTermId h, e :: ErrorBound) =
        printf " ± %s{H%s}" eDS hSTrimmed
        where
          hSTrimmed = take 3 hS
          hS = show (abs h)
          eDS
            | e == 0 = "0"
            | otherwise =
                case safeConvert (dyadic e) of
                  Right (eD :: Double) -> printf "~%.4g" eD
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
mpAffNormalise aff@(MPAffine {config, terms})
  | termsNoZeroesSize > maxTerms = aff {terms = newTerms}
  | hasSomeZeroes = aff {terms = termsNoZeroes}
  | otherwise = aff
  where
    termsNoZeroes = Map.filter (> 0) terms

    termsNoZeroesSize = Map.size termsNoZeroes
    hasSomeZeroes = termsNoZeroesSize < Map.size terms

    (MPAffineConfig {maxTerms}) = config

    -- sort terms by their coefficients, smallest to largest (they are positive):
    termsAscending = sortWith snd (Map.toList termsNoZeroes)

    -- keep maxTerms - 1 terms, then replace the others with 1 new term:
    (termsToRemove, termsToKeep) = splitAt (termsNoZeroesSize - maxTerms + 1) termsAscending

    newTermId = ErrorTermId (hash aff)
    newTermCoeff = foldl' (+) (errorBound 0) coeffsToRemove -- sum, rounding upwards
      where
        coeffsToRemove = map snd termsToRemove
    newTerms = Map.fromList ((newTermId, newTermCoeff) : termsToKeep) -- maxTerms-many terms

{-
  Conversions
-}

instance ConvertibleExactly MPAffine MPBall where
  safeConvertExactly :: MPAffine -> ConvertResult MPBall
  safeConvertExactly aff = Right $ MPBall centre e
    where
      mpAffineFlattened = mpAffNormalise $ aff {config = aff.config {maxTerms = int 1}}
      (MPAffine {centre, terms}) = mpAffineFlattened
      e = snd $ head (Map.toList terms) -- should have one term only

type CanBeMPAffine t = ConvertibleExactly (MPAffineConfig, t) MPAffine

mpAffine :: (CanBeMPAffine t) => MPAffineConfig -> t -> MPAffine
mpAffine config t = convertExactly (config, t)

instance ConvertibleExactly (MPAffineConfig, (ErrorTermId, MPBall)) MPAffine where
  safeConvertExactly :: (MPAffineConfig, (ErrorTermId, MPBall)) -> ConvertResult MPAffine
  safeConvertExactly (config, (key, MPBall c e))
    | e == 0 =
        Right $ MPAffine {config, centre = c, terms = Map.empty}
    | otherwise =
        Right $ MPAffine {config, centre = c, terms = Map.singleton key e}

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
  Basic operations
-}

instance CanNeg MPAffine where
  type NegType MPAffine = MPAffine
  negate aff = aff {centre = negate aff.centre}

instance CanAddAsymmetric MPAffine MPAffine where
  type AddType MPAffine MPAffine = MPAffine
  add aff1 aff2 =
    mpAffNormalise
      $ MPAffine {config, centre, terms}
    where
      config = aff1.config <> aff2.config
      MPBall centre e = MPBall aff1.centre z + MPBall aff2.centre z
      z = errorBound 0
      termsAdded = Map.unionWith (+) aff1.terms aff2.terms
      newTermId = ErrorTermId (hash ("+", aff1, aff2))
      terms
        | e == 0 = termsAdded
        | otherwise = Map.insert newTermId e termsAdded

instance CanSub MPAffine MPAffine -- Use the default instance via add and sub.

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
      terms =
        Map.fromList
          [ (ErrorTermId (int 1), errorBound 1),
            (ErrorTermId (int 2), errorBound 1)
          ]
    }

_mpaff2 :: MPAffine
_mpaff2 =
  MPAffine
    { config = _conf,
      centre = mpFloat 1,
      terms =
        Map.fromList
          [ (ErrorTermId (int 1), errorBound 1),
            (ErrorTermId (int 3), errorBound 1)
          ]
    }

_a100 :: MPAffine
_a100 = mpAffine _conf 100

_aThird :: MPAffine
_aThird = mpAffine _conf (1 / 3)
