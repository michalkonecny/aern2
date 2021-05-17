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
import Numeric.CollectErrors ( CN )

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
  (Show a, HasAccuracy a, CanAdjustToAccuracySG a, CanIntersectSameType a)
  =>
  SuitableForSeq a

instance SuitableForSeq (CN MPBall)
instance SuitableForSeq (CN Bool)
instance SuitableForSeq (CN Kleenean)

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
    b = b1 `intersect` b2

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

instance
  (QAArrow to, ConvertibleWithPrecision Rational a, CanSetPrecision a, SuitableForSeq a)
  =>
  ConvertibleExactly Rational (SequenceA to a)
  where
  safeConvertExactly x =
    Right $ newSeq a (show x) [] (\_src -> arr $ seqByPrecision2CauchySeq (flip convertP x) . bits)
    where
    a = convertP (prec 2) x

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
