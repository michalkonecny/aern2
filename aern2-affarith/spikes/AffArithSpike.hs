{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import AERN2.MP.Float
import Data.CDAR (Approx (..))
import Data.Foldable (Foldable (foldl'))
import Data.Hashable
import qualified Data.Map as Map
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import MixedTypesNumPrelude
import qualified Prelude as P

main :: IO ()
main = putStrLn "test"

{-
  Basic type
-}

deriving instance Generic Approx

instance Hashable Approx

instance Hashable MPFloat

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
    terms :: Map.Map ErrorTermId MPFloat
  }
  deriving (P.Eq, Generic, Show)

instance Hashable MPAffineConfig

instance Hashable MPAffine

{-
  Basic operations
-}

mpAffNormalise :: MPAffine -> MPAffine
mpAffNormalise aff@(MPAffine {..})
  | tooManyTerms = aff {terms = newTerms}
  | otherwise = aff
  where
    (MPAffineConfig {maxTerms}) = config

    termsSize = Map.size terms
    tooManyTerms = termsSize > maxTerms

    targetTermsSize = maxTerms - 1
    termsAscending = sortWith snd (Map.toList terms)
    (termsToRemove, termsToKeep) = splitAt (termsSize - targetTermsSize) termsAscending

    newTermId = ErrorTermId (hash aff)
    newTermCoeff = foldl' (+^) (mpFloat 0) $ map snd termsToRemove
    newTerms = Map.fromList ((newTermId, newTermCoeff) : termsToKeep)

_mpaff1 :: MPAffine
_mpaff1 =
  MPAffine
    { config = (MPAffineConfig {maxTerms = int 2}),
      centre = mpFloat 1,
      terms = Map.fromList [
        (ErrorTermId (int 1), mpFloat (int 1)),
        (ErrorTermId (int 2), mpFloat (int 1)),
        (ErrorTermId (int 3), mpFloat (int 1))
        ]
    }

{-  TODO

  define CanBeMPAffine (similar to CanBeMPBall)
  define mpAffine
  instances to create mpAffine from integers and configs

-}
