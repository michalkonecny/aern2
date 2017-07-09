{-|
    Module      :  AERN2.Sequence.Branching
    Description :  branching operations for sequences
    Copyright   :  (c) Michal Konecny, Eike Neumann
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Branching operations for sequences
-}
module AERN2.Sequence.Branching
(
  SeqBoolP, SeqBoolA, SeqBool, pBool
  , SequenceAtAccuracy(..)
  , pickNonZeroSeqA, pick
)
where

import MixedTypesNumPrelude hiding (id)
-- import qualified Prelude as P

import Control.Arrow

import Data.Maybe (catMaybes)

import AERN2.MP

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type
import AERN2.Sequence.Helpers (ensureAccuracyA)
import AERN2.Sequence.Comparison

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
  Maybe (QAId to) ->
  [(SequenceA to a, s)] `to` Maybe (SequenceA to a, s)
pickNonZeroSeqA src =
  startFromAccuracy (bits 0)
  where
  startFromAccuracy ac =
    proc seqsAndS -> do
      balls <- seqsWithAccuracyA src -< (map fst seqsAndS, accuracySG ac)
      let maybeNonZero = pickNonZero $ zip balls seqsAndS
      case maybeNonZero of
        Just (_,result) -> returnA -< Just result
        _ -> startFromAccuracy (ac + 1) -< seqsAndS

instance (CanPickNonZero a) => CanPickNonZero (Sequence a) where
  pickNonZero = pickNonZeroSeqA Nothing

{-| "parallel if" -}
instance
  (QAArrow to, HasIfThenElse b t
  , SuitableForSeq b, SuitableForSeq t, SuitableForSeq (IfThenElseType b t))
  =>
  HasIfThenElse (SequenceA to b) (SequenceA to t)
  where
  type IfThenElseType (SequenceA to b) (SequenceA to t) = (SequenceA to (IfThenElseType b t))
  ifThenElse (b::SequenceA to b) (e1::SequenceA to t) e2 =
    newSeq sampleT "pif" [AnyProtocolQA b, AnyProtocolQA e1, AnyProtocolQA e2] makeQ
    where
    sampleT = undefined :: (IfThenElseType b t)
    makeQ (me,_src) =
      proc ac ->
        do
        ensureAccuracyA
          (proc [q] -> qaMake3Queries (b,e1,e2) me -< (q,q,q))
          (\(b',e1',e2') -> if b' then e1' else e2')
            -< (ac, ([ac], Nothing))

pick ::
  (QAArrow to)
  =>
  (Maybe (QAId to)) ->
  [(SequenceA to (Maybe a))] `to` a
pick src = aux (bitsS 0)
  where
  aux ac =
    proc options ->
      do
      mas <- qaMakeQueryOnManyA src -< (options, ac)
      case catMaybes mas of
        [] -> aux (ac + 1) -< options
        (a : _) -> returnA -< a

{- A very old idea, now superseded by pick:

{- tolerant comparisons -}

instance (CanNegSameType b, Arrow to) => CanNeg (() `to` b) where
  negate tob = arr negate <<< tob

instance (QAArrow to) => HasTolerantEqAsymmetric (CauchyRealA to) (CauchyRealA to) where
  type TolerantEqCompareType (CauchyRealA to) (CauchyRealA to) = () `to` (Tolerant Bool)
  tolerantEqualTo a (e,b) =
    proc () ->
      do
      aB <- qaMakeQueryA -< (a, ac)
      bB <- qaMakeQueryA -< (b, ac)
      returnA -< tolerantEqualTo aB (e,bB)
    where
    ac =
      case getNormLog (dyadic e) of
        NormBits n -> bits (max 0 (1 - n))
        NormZero -> Exact
-}