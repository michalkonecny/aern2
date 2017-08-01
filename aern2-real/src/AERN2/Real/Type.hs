{-|
    Module      :  AERN2.Real.Type
    Description :  The type of Cauchy real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of Cauchy real numbers
-}
module AERN2.Real.Type
(
  CauchyRealP, pCR, CauchyRealCNP, pCRCN
  , CauchyRealA, CauchyReal, newCR
  , CauchyRealCNA, CauchyRealCN, newCRCN
  , CauchyRealAtAccuracy, cauchyRealAtAccuracy
  , realName, realId, realSources, realRename
  , realWithAccuracy, realWithAccuracyA, realsWithAccuracyA
  , convergentList2CauchyRealA
  , seqByPrecision2CauchyRealA
  , CanBeReal, real, CanBeRealA, realA
  , CanBeComplex, complex, CanBeComplexA, complexA
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import qualified Control.CollectErrors as CE
import Control.Arrow
-- import Text.Printf

import Data.Complex

import AERN2.MP

import AERN2.QA.Protocol
import AERN2.QA.Strategy.CachedUnsafe ()

import AERN2.AccuracySG

import AERN2.Sequence

{- Cauchy real numbers -}

type CauchyRealP = SequenceP MPBall
type CauchyRealCNP = SequenceP (CN MPBall)

pCR :: CauchyRealP
pCR = SequenceP (mpBall 0)

pCRCN :: CauchyRealCNP
pCRCN = SequenceP (cn $ mpBall 0)

type CauchyRealA to = SequenceA to MPBall
type CauchyReal = CauchyRealA (->)

type CauchyRealCNA to = SequenceA to (CN MPBall)
type CauchyRealCN = CauchyRealCNA (->)

type CauchyRealAtAccuracy = SequenceAtAccuracy MPBall
cauchyRealAtAccuracy :: CauchyReal -> AccuracySG -> CauchyRealAtAccuracy
cauchyRealAtAccuracy = SequenceAtAccuracy

realName :: SequenceA to a -> String
realName = seqName

realRename :: (String -> String) -> SequenceA to a -> SequenceA to a
realRename = seqRename

realId :: QA to p -> Maybe (QAId to)
realId = qaId

realSources :: QA to p -> [QAId to]
realSources = qaSources

{-| Get a ball approximation of the real number with at least the specified accuracy.
   (A specialisation of 'qaMakeQuery' for Cauchy reals.) -}
realWithAccuracy :: (QAArrow to) => CauchyRealA to -> AccuracySG `to` MPBall
realWithAccuracy = (?)

realWithAccuracyA :: (QAArrow to) => (Maybe (QAId to)) -> (CauchyRealA to, AccuracySG) `to` MPBall
realWithAccuracyA = qaMakeQueryA

realsWithAccuracyA :: (QAArrow to) => (Maybe (QAId to)) -> ([CauchyRealA to], AccuracySG) `to` [MPBall]
realsWithAccuracyA = qaMakeQueryOnManyA

{- constructions -}

newCR :: (QAArrow to) => String -> [AnyProtocolQA to] -> ((Maybe (QAId to), Maybe (QAId to)) -> AccuracySG `to` MPBall) -> CauchyRealA to
newCR = newSeq (mpBall 0)

newCRCN :: (QAArrow to) => String -> [AnyProtocolQA to] -> ((Maybe (QAId to), Maybe (QAId to)) -> AccuracySG `to` CN MPBall) -> CauchyRealCNA to
newCRCN = newSeq (cn $ mpBall 0)

convergentList2CauchyRealA :: (QAArrow to) => String -> [MPBall] -> (CauchyRealA to)
convergentList2CauchyRealA = convergentList2SequenceA

seqByPrecision2CauchyRealA :: (QAArrow to) => String -> (Precision -> MPBall) -> (CauchyRealA to)
seqByPrecision2CauchyRealA = seqByPrecision2SequenceA

{- conversions -}

type CanBeRealA to t = ConvertibleExactly t (CauchyRealA to)
type CanBeReal t = CanBeRealA (->) t

real :: (CanBeRealA (->) t) => t -> CauchyReal
real = convertExactly

realA :: (CanBeRealA to t) => t -> CauchyRealA to
realA = convertExactly

type CanBeComplexA to t = ConvertibleExactly t (Complex (CauchyRealA to))
type CanBeComplex t = CanBeComplexA (->) t

complex :: (CanBeComplexA (->) t) => t -> Complex CauchyReal
complex = convertExactly

complexA :: (CanBeComplexA to t) => t -> Complex (CauchyRealA to)
complexA = convertExactly

-- instance (QAArrow to) => ConvertibleExactly Rational (CauchyRealA to) where
--   safeConvertExactly x =
--     Right $ newCR (show x) [] (\me_src -> arr (makeQ me_src))
--     where
--     makeQ _ = seqByPrecision2CauchySeq (flip mpBallP x) . bits

instance ConvertibleWithPrecision CauchyReal MPBall where
  safeConvertP p r =
    Right $ setPrecision p $ r ? (accuracySG $ bits p + 10)
