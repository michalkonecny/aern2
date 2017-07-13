{-|
    Module      :  AERN2.Sequence
    Description :  fast convergent sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A type of fast convergent sequences parametrised by the arrow in which the elements of the
    sequence are queried
-}
module AERN2.Sequence
(
  module AERN2.AccuracySG
  -- * The protocol and type of fast converging sequences
  , SequenceP(..), pSeq
  , seqName, seqId, seqSources, seqRename
  , seqWithAccuracy, (?), seqWithAccuracyA, seqsWithAccuracyA
  , SequenceA, Sequence, newSeq, newSeqSimple
  , SequenceAtAccuracy(..)
  , (-:-), (-:-||), (-:-|)
  , convergentList2SequenceA
  , seqByPrecision2SequenceA
  -- * picking one of several staged computations running in parallel
  , pick
  -- * auxiliary functions for making new sequence operations
  , unaryOp, binaryOp, binaryOpWithPureArg
  , getSeqFnNormLog
  , getInitQ1FromSimple, getInitQ1TFromSimple, getInitQ1Q2FromSimple
  -- , binaryWithBall
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import Control.Arrow

-- import AERN2.Norm
-- import AERN2.MP.Precision

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type
import AERN2.Sequence.Helpers
import AERN2.Sequence.Comparison
import AERN2.Sequence.Branching
import AERN2.Sequence.Ring ()
import AERN2.Sequence.Field ()
import AERN2.Sequence.Elementary ()
import AERN2.Sequence.PreludeOps ()

-- instance
--   (QAArrow to
--   , OrderedRing a
--   , SuitableForSeq a
--   , SuitableForSeq (EqCompareType a a)
--   , SuitableForSeq (EqCompareType a Int)
--   , SuitableForSeq (EqCompareType a Integer)
--   , HasNorm (EnsureNoCN a)
--   , CanSetPrecision a, CanSetPrecision (EnsureCN a))
--   =>
--   Ring (SequenceA to a)
