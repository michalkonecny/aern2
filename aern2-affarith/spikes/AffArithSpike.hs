{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import AERN2.MP (ErrorBound, MPBall, ShowWithAccuracy (..), ac2prec, bits, errorBound)
import AERN2.MP.Accuracy (Accuracy (..))
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Float
import Data.CDAR (Approx (..))
import Data.Foldable (Foldable (foldl'))
import Data.Hashable
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
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
  { maxTerms :: Int
  }
  deriving (P.Eq, Generic, Show)

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
                  Right (eD :: Double) -> printf "~%.4g" $ eD
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
          _ -> round $ (log (double 2) / log (double 10)) * (integer $ ac2prec displayAC)

{-
  Basic operations
-}

mpAffNormalise :: MPAffine -> MPAffine
mpAffNormalise aff@(MPAffine {config, terms})
  | termsSize > maxTerms = aff {terms = newTerms}
  | otherwise = aff
  where
    termsSize = Map.size terms
    (MPAffineConfig {maxTerms}) = config

    -- sort terms by their coefficients, smallest to largest (they are positive):
    termsAscending = sortWith snd (Map.toList terms)

    -- keep maxTerms - 1 terms, then replace the others with 1 new term:
    (termsToRemove, termsToKeep) = splitAt (termsSize - maxTerms + 1) termsAscending

    newTermId = ErrorTermId (hash aff)
    newTermCoeff = foldl' (+) (errorBound 0) $ map snd termsToRemove -- sum, rounding upwards
    newTerms = Map.fromList ((newTermId, newTermCoeff) : termsToKeep) -- maxTerms-many terms

_mpaff1 :: MPAffine
_mpaff1 =
  MPAffine
    { config = (MPAffineConfig {maxTerms = int 2}),
      centre = mpFloat 1,
      terms =
        Map.fromList
          [ (ErrorTermId (int 1), errorBound 1),
            (ErrorTermId (int 2), errorBound 1),
            (ErrorTermId (int 3), errorBound 1)
          ]
    }

instance Convertible MPAffine MPBall where
  safeConvert :: MPAffine -> ConvertResult MPBall
  safeConvert (MPAffine {}) = undefined

{-  TODO

  conversion to MPBall
  define CanBeMPAffine (similar to CanBeMPBall)
  define mpAffine
  instances to create mpAffine from integers and configs

-}
