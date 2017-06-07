{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.Sequence.Type
    Description :  The type of convergent sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of convergent sequences
-}
module AERN2.Sequence.Type
(
  SequenceP(..), pSeq
  , SuitableForSeq
  , seqName, seqId, seqSources, seqRename
  , seqWithAccuracy, seqWithAccuracyA, seqsWithAccuracyA
  , SequenceA, Sequence, newSeq, newSeqSimple
  , convergentList2SequenceA
  , seqByPrecision2SequenceA
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P

import Control.Arrow

import Text.Printf

import AERN2.MP

import AERN2.QA.Protocol
import AERN2.QA.Strategy.CachedUnsafe ()

import AERN2.AccuracySG

{- Cauchy sequences -}

data SequenceP a = SequenceP a deriving (Show)

pSeq :: a -> SequenceP a
pSeq a = SequenceP a

instance (Show a) => QAProtocol (SequenceP a) where
  type Q (SequenceP a) = AccuracySG
  type A (SequenceP a) = a
  -- sampleQ _ = AccuracySG NoInformation NoInformation

type SuitableForSeq a =
  (Show a, HasAccuracy a, CanAdjustToAccuracySG a
  , CanIntersectCNSameType a, CanEnsureCN a)

instance
  SuitableForSeq a
  =>
  QAProtocolCacheable (SequenceP a)
  where
  type QACache (SequenceP a) = (Maybe (a, AccuracySG))
  newQACache _ = Nothing
  lookupQACache _ cache acSG@(AccuracySG acS acG) =
    case cache of
      Just (b, AccuracySG _ bAG) | getAccuracy b >= acS && (getAccuracy b >= acG || bAG >= acG - tol) ->
        (Just (adjustToAccuracySG acSG b),
         Just (logMsg b))
      Just (b, _) -> (Nothing, Just (logMsg b))
      Nothing -> (Nothing, Just ("cache empty"))
    where
    tol = accuracySGdefaultTolerance
    logMsg b = printf "query: %s; cache: (ac=%s) %s" (show acSG) (show (getAccuracy b))  (show cache)
  updateQACache _ q b Nothing = Just (b,q)
  updateQACache _ q2 b2 (Just (b1,q1)) =
    Just (b, q1 `max` q2)
    where
    b12 = b1 `intersect` b2
    b =
      case deEnsureCN b12 of
        Just b' -> b'
        _ -> error $ printf "Sequence: updateQACache: problem computing intersection: %s /\\ %s" (show b1) (show b2)

type SequenceA to a = QA to (SequenceP a)

seqName :: SequenceA to a -> String
seqName = qaName
--
seqRename :: (String -> String) -> SequenceA to a -> SequenceA to a
seqRename f r = r {  qaName = f (qaName r)  }

seqId :: SequenceA to a -> Maybe (QAId to)
seqId = qaId

seqSources :: SequenceA to a -> [QAId to]
seqSources = qaSources

{-| Get an approximation of the limit with at least the specified accuracy.
   (A specialisation of 'qaMakeQuery' for Cauchy sequences.) -}
seqWithAccuracy :: (QAArrow to) => SequenceA to a -> AccuracySG `to` a
seqWithAccuracy = (?)

seqWithAccuracyA :: (QAArrow to) => (SequenceA to a, AccuracySG) `to` a
seqWithAccuracyA = qaMakeQueryA

seqsWithAccuracyA :: (QAArrow to) => ([SequenceA to a], AccuracySG) `to` [a]
seqsWithAccuracyA = qaMakeQueryOnManyA

type Sequence a = SequenceA (->) a

instance (Show a) => Show (Sequence a) where
  show r = show $ r ? (accuracySG (bits 100))

{- constructions -}

newSeq :: (QAArrow to, SuitableForSeq a) => a -> String -> [AnyProtocolQA to] -> AccuracySG `to` a -> SequenceA to a
newSeq sampleA name sources makeQ =
  newQA name sources (pSeq sampleA) (AccuracySG NoInformation NoInformation) makeQ

newSeqSimple :: (QAArrow to, SuitableForSeq a) => a -> AccuracySG `to` a -> SequenceA to a
newSeqSimple sampleA = newSeq sampleA "simple" []

convergentList2SequenceA ::
  (QAArrow to, SuitableForSeq a) =>
  String -> [a] -> (SequenceA to a)
convergentList2SequenceA name balls@(sampleA : _) =
  newSeq sampleA name [] (arr $ convergentList2CauchySeq balls . bits)
convergentList2SequenceA name [] =
  error $ "convergentList2SequenceA: empty sequence " ++ name

seqByPrecision2SequenceA :: (QAArrow to, SuitableForSeq a) => String -> (Precision -> a) -> (SequenceA to a)
seqByPrecision2SequenceA name byPrec =
  newSeq sampleA name [] (arr $ seqByPrecision2CauchySeq byPrec . bits)
    where
    sampleA = byPrec (prec 0)
