{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Use logBase" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AERN2.AffArith.Type
  ( 
    MPAffineConfig(..),
    ErrorTermId(..),
    MPAffineErrorTerms,
    MPAffine(..),
    mpBallOpOnMPFloat2,
    mpAffNormalise,
  )
where

import AERN2.MP (ErrorBound, MPBall (MPBall), ShowWithAccuracy (..), ac2prec, bits, errorBound)
import AERN2.MP.Accuracy (Accuracy (..))
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Float (MPFloat, (+^))
import Data.CDAR (Approx (..))
import Data.Hashable (Hashable (hash))
import Data.List (foldl1')
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
import qualified Prelude as P

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

-- |
--    Remove zero terms and, if necessary, reduce the number of affine terms
--    to adhere to the embedded configuration.
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
