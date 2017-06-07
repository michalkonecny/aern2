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

-- import Data.Maybe (catMaybes)

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
    b = deEnsureCN $ b1 `intersect` b2 -- throws exception if the intersection is empty and b is not of CollectNumErrors type

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
  [(SequenceA to a, s)] `to` Maybe (SequenceA to a, s)
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

instance (CanPickNonZero a) => CanPickNonZero (Sequence a) where
  pickNonZero = pickNonZeroSeqA

{- TODO: move the following to Comparison

{-| "parallel if" -}
instance
  (CanUnionSameType t, SuitableForSeq t)
  =>
  HasIfThenElse (Sequence (Maybe Bool)) (Sequence t)
  where
  ifThenElse b e1 e2 =
    newSeq (e1 ? (bitsS 0)) "pif" [AnyProtocolQA e1, AnyProtocolQA e2] makeQ
    where
    makeQ ac =
      if (b ? ac) then (e1 ? ac) else (e2 ? ac)

-- trisection ::
--   (q -> r) ->
--   (q,q) ->
--   Sequence (Maybe (q,q))
-- trisection f (l,r) =
--   if (f l) * (f r) >= 0 -- for r ~ Cauchy real this is "parallel if"
--     then newSeqSimple Nothing (const Nothing)
--     else aux l r
--   where
--   aux a b =
--     | b - a < eps = Just (a,b)
--     | otherwise = pick [tryM m1, tryM m2]
--     where
--     tryM m = newSeqSimple sampleResult withAC
--       where
--       withAC ac =
--         if (f m) * (f a) < 0 then
--
--     m1 = (2*a + b)/3
--     m2 = (a + 2*b)/3

{-|
  Parallel picking.
-}
pick ::
  (QAArrow to)
  =>
  [(SequenceA to (Maybe a))] `to` a
pick = aux (bitsS 0)
  where
  aux ac =
    proc options ->
      do
      mas <- qaMakeQueryOnManyA -< (options, ac)
      case catMaybes mas of
        [] -> aux (ac + 1) -< options
        (a : _) -> returnA -< a
-}
