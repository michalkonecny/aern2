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
    newTermCoeff = foldl' (+) (errorBound 0) coeffsToRemove -- sum, rounding upwards
      where
        coeffsToRemove = map snd termsToRemove
    newTerms = Map.fromList ((newTermId, newTermCoeff) : termsToKeep) -- maxTerms-many terms

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
  safeConvertExactly (config, (key, MPBall c e)) =
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

_mpaff1 :: MPAffine
_mpaff1 =
  MPAffine
    { config = (MPAffineConfig {maxTerms = int 2, precision = 100}),
      centre = mpFloat 1,
      terms =
        Map.fromList
          [ (ErrorTermId (int 1), errorBound 1),
            (ErrorTermId (int 2), errorBound 1),
            (ErrorTermId (int 3), errorBound 1)
          ]
    }
