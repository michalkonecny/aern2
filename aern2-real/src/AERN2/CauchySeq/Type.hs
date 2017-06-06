{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.CauchySeq.Type
    Description :  The type of Cauchy sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of Cauchy sequences
-}
module AERN2.CauchySeq.Type
(
  CauchySeqP, pSeq
  , seqName, seqId, seqSources, seqRename
  , seqWithAccuracy, seqWithAccuracyA, seqsWithAccuracyA
  , CauchySeqA, CauchySeq, newSeq
  , convergentList2CauchySeqA
  , seqByPrecision2CauchySeqA
  , pickNonZeroSeqA
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

data CauchySeqP a = CauchySeqP a deriving (Show)

pSeq :: a -> CauchySeqP a
pSeq a = CauchySeqP a

instance (Show a) => QAProtocol (CauchySeqP a) where
  type Q (CauchySeqP a) = AccuracySG
  type A (CauchySeqP a) = a
  -- sampleQ _ = AccuracySG NoInformation NoInformation

type SuitableForSeq a =
  (HasAccuracy a, Show a, CanIntersectSameType a,
   CanSetPrecision a, CanReduceSizeUsingAccuracyGuide a)

instance
  SuitableForSeq a
  =>
  QAProtocolCacheable (CauchySeqP a)
  where
  type QACache (CauchySeqP a) = (Maybe (a, AccuracySG))
  newQACache _ = Nothing
  lookupQACache _ cache acSG@(AccuracySG acS acG) =
    case cache of
      Just (b, AccuracySG _ bAG) | getAccuracy b >= acS && (getAccuracy b >= acG || bAG >= acG - tol) ->
        (Just ((setPrecisionAtLeastAccuracy acS . reduceSizeUsingAccuracyGuide acG) b),
         Just (logMsg b))
      Just (b, _) -> (Nothing, Just (logMsg b))
      Nothing -> (Nothing, Just ("cache empty"))
    where
    tol = accuracySGdefaultTolerance
    logMsg b = printf "query: %s; cache: (ac=%s) %s" (show acSG) (show (getAccuracy b))  (show cache)
  updateQACache _ q b Nothing = Just (b,q)
  updateQACache _ q2 b2 (Just (b1,q1)) = Just (b1 `intersect` b2, q1 `max` q2)

type CauchySeqA to a = QA to (CauchySeqP a)

seqName :: CauchySeqA to a -> String
seqName = qaName
--
seqRename :: (String -> String) -> CauchySeqA to a -> CauchySeqA to a
seqRename f r = r {  qaName = f (qaName r)  }

seqId :: CauchySeqA to a -> Maybe (QAId to)
seqId = qaId

seqSources :: CauchySeqA to a -> [QAId to]
seqSources = qaSources

{-| Get an approximation of the limit with at least the specified accuracy.
   (A specialisation of 'qaMakeQuery' for Cauchy sequences.) -}
seqWithAccuracy :: (QAArrow to) => CauchySeqA to a -> AccuracySG `to` a
seqWithAccuracy = (?)

seqWithAccuracyA :: (QAArrow to) => (CauchySeqA to a, AccuracySG) `to` a
seqWithAccuracyA = qaMakeQueryA

seqsWithAccuracyA :: (QAArrow to) => ([CauchySeqA to a], AccuracySG) `to` [a]
seqsWithAccuracyA = qaMakeQueryOnManyA

type CauchySeq a = CauchySeqA (->) a

instance (Show a) => Show (CauchySeq a) where
  show r = show $ r ? (accuracySG (bits 100))

{- constructions -}

newSeq :: (QAArrow to, SuitableForSeq a) => a -> String -> [AnyProtocolQA to] -> AccuracySG `to` a -> CauchySeqA to a
newSeq sampleA name sources makeQ =
  newQA name sources (pSeq sampleA) (AccuracySG NoInformation NoInformation) makeQ

convergentList2CauchySeqA ::
  (QAArrow to, SuitableForSeq a) =>
  String -> [a] -> (CauchySeqA to a)
convergentList2CauchySeqA name balls@(sampleA : _) =
  newSeq sampleA name [] (arr $ convergentList2CauchySeq balls . bits)
convergentList2CauchySeqA name [] =
  error $ "convergentList2CauchySeqA: empty sequence " ++ name

seqByPrecision2CauchySeqA :: (QAArrow to, SuitableForSeq a) => String -> (Precision -> a) -> (CauchySeqA to a)
seqByPrecision2CauchySeqA name byPrec =
  newSeq sampleA name [] (arr $ seqByPrecision2CauchySeq byPrec . bits)
    where
    sampleA = byPrec (prec 0)

{- non-zero picking -}

{-|
  Given a list @[(a1,b1),(a2,b2),...]@ and assuming that
  at least one of @a1,a2,...@ is non-zero, pick one of them
  and return the corresponding pair @(ai,bi)@.

  If none of @a1,a2,...@ is zero, either throw an exception
  or loop forever.
 -}
pickNonZeroSeqA ::
  (QAArrow to, CanPickNonZero a)
  =>
  [(CauchySeqA to a, s)] `to` Maybe (CauchySeqA to a, s)
pickNonZeroSeqA =
  startFromAccuracy (bits 0)
  where
  startFromAccuracy ac =
    proc seqsAndS -> do
      balls <- seqsWithAccuracyA -< (map fst seqsAndS, accuracySG ac)
      let maybeNonZero = pickNonZero $ zip balls seqsAndS
      case maybeNonZero of
        Just (_,result) -> returnA -< Just result
        _ -> startFromAccuracy (ac + 1) -< seqsAndS

instance (CanPickNonZero a) => CanPickNonZero (CauchySeq a) where
  pickNonZero = pickNonZeroSeqA
