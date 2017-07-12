{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- #define DEBUG
{-|
    Module      :  AERN2.Sequence.Type
    Description :  The type of fast convergent sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of fast convergent sequences
-}
module AERN2.Sequence.Type
(
  SequenceP(..), pSeq
  , FastConvSeqP, EffortConvSeqP
  , SuitableForSeq
  , seqName, seqId, seqSources, seqRename
  , seqWithAccuracy, seqWithAccuracyA, seqsWithAccuracyA
  , SequenceA, Sequence
  , FastConvSeqA, EffortConvSeqA, FastConvSeq, EffortConvSeq
  , newSeq, newSeqSimple
  , convergentList2SequenceA
  , seqByPrecision2SequenceA
  , fmapSeq
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

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.Arrow

import Text.Printf

import Control.CollectErrors

import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.QA.Strategy.CachedUnsafe ()

import AERN2.AccuracySG

{- QA protocol -}

data SequenceP a = SequenceP { unSequenceP :: a} deriving (Show)
type FastConvSeqP a = SequenceP a -- synonym, emphasising stric accuracy requirement
type EffortConvSeqP a = SequenceP a -- synonym, emphasising accuracy guide

pSeq :: a -> SequenceP a
pSeq a = SequenceP a

instance (Show a) => QAProtocol (SequenceP a) where
  type Q (SequenceP a) = AccuracySG
  type A (SequenceP a) = a
  -- sampleQ _ = AccuracySG NoInformation NoInformation

class
  (Show a, Show (EnsureNoCN a), Show (EnsureCN a), HasAccuracy a, CanAdjustToAccuracySG a
  , CanEnsureCN a, CanEnsureCN (EnsureCN a), HasAccuracy (EnsureNoCN a), CanIntersectCNSameType a)
  =>
  SuitableForSeq a

instance SuitableForSeq MPBall
instance SuitableForSeq (CN MPBall)
instance SuitableForSeq Bool
instance SuitableForSeq t => SuitableForSeq (Maybe t)

instance
  SuitableForSeq a
  =>
  QAProtocolCacheable (SequenceP a)
  where
  type QACache (SequenceP a) = (Maybe (a, AccuracySG))
  newQACache _ = Nothing
  lookupQACache _ cache acSG@(AccuracySG acS acG) =
    case cache of
      Just (b, AccuracySG _ bAG)
        | getAccuracy b >= acS && (getAccuracy b >= acG || bAG >= acG - tol) ->
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
        Right b' -> b'
        Left es ->
          error $
            printf "Sequence: updateQACache: problem computing intersection: %s /\\ %s: %s"
              (show b1) (show b2) (show es)

instance Functor SequenceP where
  fmap f (SequenceP a) = SequenceP (f a)

{- Seqeuences -}

type SequenceA to a = QA to (SequenceP a)
type Sequence a = SequenceA (->) a

type FastConvSeqA to a = SequenceA to a -- synonym, emphasising stric accuracy requirement
type EffortConvSeqA to a = SequenceA to a -- synonym, emphasising accuracy guide
type FastConvSeq a = Sequence a -- synonym, emphasising stric accuracy requirement
type EffortConvSeq a = Sequence a -- synonym, emphasising accuracy guide

instance (Show a) => Show (Sequence a) where
  show r = show $ r ? default_acSG

fmapSeq ::
  (Arrow to) =>
  (a -> b) -> (SequenceA to a) -> (SequenceA to b)
fmapSeq f = mapQAsameQ (fmap f) f

seqName :: SequenceA to a -> String
seqName = qaName
--
seqRename :: (String -> String) -> SequenceA to a -> SequenceA to a
seqRename = qaRename

seqId :: SequenceA to a -> Maybe (QAId to)
seqId = qaId

seqSources :: SequenceA to a -> [QAId to]
seqSources = qaSources

{-| Get an approximation of the limit with at least the specified accuracy.
   (A specialisation of 'qaMakeQuery' for Cauchy sequences.) -}
seqWithAccuracy :: (QAArrow to) => SequenceA to a -> Maybe (QAId to) -> AccuracySG `to` a
seqWithAccuracy = (?<-)

seqWithAccuracyA :: (QAArrow to) => (Maybe (QAId to)) -> (SequenceA to a, AccuracySG) `to` a
seqWithAccuracyA = qaMakeQueryA

seqsWithAccuracyA :: (QAArrow to) => (Maybe (QAId to)) -> ([SequenceA to a], AccuracySG) `to` [a]
seqsWithAccuracyA = qaMakeQueryOnManyA

{- constructions -}

newSeq ::
  (QAArrow to, SuitableForSeq a)
  =>
  a -> String -> [AnyProtocolQA to] -> ((Maybe (QAId to), Maybe (QAId to)) -> AccuracySG `to` a) -> SequenceA to a
newSeq sampleA name sources makeQ =
  newQA name sources (pSeq sampleA) Nothing makeQ

newSeqSimple ::
  (QAArrow to, SuitableForSeq a)
  =>
  a -> ((Maybe (QAId to), Maybe (QAId to)) -> AccuracySG `to` a) -> SequenceA to a
newSeqSimple sampleA = newSeq sampleA "simple" []

convergentList2SequenceA ::
  (QAArrow to, SuitableForSeq a) =>
  String -> [a] -> (SequenceA to a)
convergentList2SequenceA name balls@(sampleA : _) =
  newSeq sampleA name [] (\_src -> arr $ convergentList2CauchySeq balls . bits)
convergentList2SequenceA name [] =
  error $ "convergentList2SequenceA: empty sequence " ++ name

seqByPrecision2SequenceA :: (QAArrow to, SuitableForSeq a) => String -> (Precision -> a) -> (SequenceA to a)
seqByPrecision2SequenceA name byPrec =
  newSeq sampleA name [] (\_src -> arr $ seqByPrecision2CauchySeq byPrec . bits)
    where
    sampleA = byPrec (prec 0)

{- CollectErrors instances -}

instance
  (SuitableForCE es, CanEnsureCE es a)
  =>
  CanEnsureCE es (SequenceP a)
  where
  type EnsureCE es (SequenceP a) = SequenceP (EnsureCE es a)
  type EnsureNoCE es (SequenceP a) = SequenceP (EnsureNoCE es a)

  ensureCE sample_es = fmap (ensureCE sample_es)
  deEnsureCE sample_es (SequenceP a) = fmap SequenceP (deEnsureCE sample_es a)
  ensureNoCE sample_es (SequenceP a) =
    (\(ma,es) -> (fmap SequenceP ma, es)) (ensureNoCE sample_es a)

  noValueECE sample_vCE es = SequenceP (noValueECE (fmap unSequenceP sample_vCE) es)
  prependErrorsECE sample_vCE es1 = fmap (prependErrorsECE (fmap unSequenceP sample_vCE) es1)

  -- getMaybeValueECE sample_es (SequenceP a) = fmap SequenceP (getMaybeValueECE sample_es a)
  -- getErrorsECE sample_vCE (SequenceP a) = getErrorsECE (fmap unSequenceP sample_vCE) a

instance
  (Arrow to, SuitableForCE es, CanEnsureCE es a)
  =>
  CanEnsureCE es (SequenceA to a)
  where
  type EnsureCE es (SequenceA to a) = SequenceA to (EnsureCE es a)
  type EnsureNoCE es (SequenceA to a) = SequenceA to (EnsureNoCE es a)

  ensureCE sample_es = fmapSeq (ensureCE sample_es)
  deEnsureCE sample_es = Right . fmapSeq (removeEither . deEnsureCE sample_es)
    where
    removeEither (Right a) = a
    removeEither (Left es) = error $ "Sequence deEnsureCE: " ++ show es
  ensureNoCE sample_es = (\v -> (Just v, mempty)) . fmapSeq (removeES . ensureNoCE sample_es)
    where
    removeES (Just a, es) | not (hasCertainError es) = a
    removeES (_, es) = error $ "WithGlobalParam ensureNoCE: " ++ show es
    -- es =

  noValueECE _sample_vCE _es =
    error "noValueECE not implemented for Sequence yet"

  prependErrorsECE (_sample_vCE :: Maybe (SequenceA to a)) es1 =
    fmapSeq (prependErrorsECE (Nothing :: Maybe a) es1)

  -- getMaybeValueECE sample_es = Just . fmapSeq (removeJust . getMaybeValueECE sample_es)
  --   where
  --   removeJust (Just a) = a
  --   removeJust _ = error "getMaybeValueECE failed for a Sequence"
  -- getErrorsECE _sample_mv _s =
  --   error "getErrorsECE not implemented for Sequence yet"

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |]]
  (\ t -> [d|

    instance
      (QAArrow to, ConvertibleExactly $t a, CanSetPrecision a, SuitableForSeq a)
      =>
      ConvertibleExactly $t (SequenceA to a)
      where
      safeConvertExactly x =
        Right $ newSeq a (show x) [] (\_src -> arr $ flip setPrecisionAtLeastAccuracy a . bits)
        where
        a = convertExactly x

  |]))
