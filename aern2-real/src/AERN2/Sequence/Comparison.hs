{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Sequence.Comparison
    Description :  comparison operations on sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Comparison operations on convergent sequences.
-}
module AERN2.Sequence.Comparison
(
  SeqBoolP, SeqBoolA, SeqBool, pBool
  , SequenceAtAccuracy(..)
)
where

import Numeric.MixedTypes hiding (id)
-- import qualified Prelude as P

import Control.Category (id)
import Control.Arrow

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type
import AERN2.Sequence.Helpers

{- "Sequenced/Staged" Boolean -}

type SeqBoolP = SequenceP (Maybe Bool)

pBool :: SeqBoolP
pBool = SequenceP Nothing

type SeqBoolA to = SequenceA to (Maybe Bool)
type SeqBool = SeqBoolA (->)

{- Boolean ops on sequences -}

instance (QAArrow to, HasBools b, SuitableForSeq b) => ConvertibleExactly Bool (SequenceA to b) where
  safeConvertExactly bool =
    do
    b <- safeConvertExactly bool
    Right $ newSeq b (show b) [] $ arr $ const b

instance
  (QAArrow to, CanNeg a, SuitableForSeq a, SuitableForSeq (NegType a))
  =>
  CanNeg (SequenceA to a)
  where
  type NegType (SequenceA to a) = SequenceA to (NegType a)
  negate = unaryOp "neg" negate (getInitQ1FromSimple $ arr id)

instance
  (QAArrow to, CanAndOrAsymmetric a b
  , SuitableForSeq a, SuitableForSeq b, SuitableForSeq (AndOrType a b))
  =>
  CanAndOrAsymmetric (SequenceA to a) (SequenceA to b)
  where
  type AndOrType (SequenceA to a) (SequenceA to b) = SequenceA to (AndOrType a b)
  and2 = binaryOp "and" and2 (getInitQ1Q2FromSimple $ arr $ \q -> (q,q))
  or2 = binaryOp "or" or2 (getInitQ1Q2FromSimple $ arr $ \q -> (q,q))

{- equality & order -}

instance
  (QAArrow to, HasEqAsymmetric a b
  , SuitableForSeq a, SuitableForSeq b, SuitableForSeq (EqCompareType a b))
  =>
  HasEqAsymmetric (SequenceA to a) (SequenceA to b)
  where
  type EqCompareType (SequenceA to a) (SequenceA to b) = SequenceA to (EqCompareType a b)
  equalTo = lift2 "==" (==)
  notEqualTo = lift2 "/=" (/=)

instance
  (QAArrow to, HasOrderAsymmetric a b
  , SuitableForSeq a, SuitableForSeq b, SuitableForSeq (OrderCompareType a b))
  =>
  HasOrderAsymmetric (SequenceA to a) (SequenceA to b)
  where
  type OrderCompareType (SequenceA to a) (SequenceA to b) = SequenceA to (OrderCompareType a b)
  lessThan = lift2 "<" (<)
  leq = lift2 "<=" (<=)
  greaterThan = lift2 ">" (>)
  geq = lift2 ">=" (>=)

{-| SequenceAtAccuracy exists only so that we can QuickCheck that
   Sequence satisfies properties whose statement relies on an instance of HasEqCertainly.
   Sequence is not an instance but SequenceAtAccuracy is.
-}
data SequenceAtAccuracy a = SequenceAtAccuracy (Sequence a) AccuracySG
  deriving (Show)

instance
  (HasEqAsymmetric a b, SuitableForSeq a, SuitableForSeq b, SuitableForSeq (EqCompareType a b))
  =>
  HasEqAsymmetric (SequenceAtAccuracy a) (SequenceAtAccuracy b)
  where
  type EqCompareType (SequenceAtAccuracy a) (SequenceAtAccuracy b) = EqCompareType a b
  equalTo = delift2 (==)

instance
  (HasOrderAsymmetric a b, SuitableForSeq a, SuitableForSeq b, SuitableForSeq (OrderCompareType a b))
  =>
  HasOrderAsymmetric (SequenceAtAccuracy a) (SequenceAtAccuracy b)
  where
  type OrderCompareType (SequenceAtAccuracy a) (SequenceAtAccuracy b) = OrderCompareType a b
  lessThan = delift2 (<)
  leq = delift2 (<=)
  greaterThan = delift2 (>)
  geq = delift2 (>=)
--
delift2 ::
  (Sequence a -> Sequence b -> Sequence c) ->
  SequenceAtAccuracy a -> SequenceAtAccuracy b -> c
delift2 rel (SequenceAtAccuracy x1 ac1) (SequenceAtAccuracy x2 ac2) =
  (rel x1 x2) ? (max ac1 ac2)

{- abs -}

instance
  (QAArrow to, CanAbs a, SuitableForSeq a, SuitableForSeq (AbsType a))
  =>
  CanAbs (SequenceA to a)
  where
  type AbsType (SequenceA to a) = SequenceA to (AbsType a)
  abs = unaryOp "abs" abs (getInitQ1FromSimple $ arr id)

{- min/max -}

instance
  (QAArrow to
  , CanMinMaxAsymmetric a b, SuitableForSeq a, SuitableForSeq b, SuitableForSeq (MinMaxType a b))
  =>
  CanMinMaxAsymmetric (SequenceA to a) (SequenceA to b)
  where
  type MinMaxType (SequenceA to a) (SequenceA to b) = SequenceA to (MinMaxType a b)
  min = lift2 "min" min
  max = lift2 "max" max


instance
  (CanMinMaxAsymmetric a MPBall, SuitableForSeq a
  , CanSetPrecision (MinMaxType a MPBall))
  =>
  CanMinMaxAsymmetric (Sequence a) MPBall
  where
  type MinMaxType (Sequence a) MPBall = MinMaxType a MPBall
  min = binaryWithEncl min
  max = binaryWithEncl max
--
instance
  (CanMinMaxAsymmetric MPBall b, SuitableForSeq b
  , CanSetPrecision (MinMaxType MPBall b))
  =>
  CanMinMaxAsymmetric MPBall (Sequence b)
  where
  type MinMaxType MPBall (Sequence b) = MinMaxType MPBall b
  min = flip $ binaryWithEncl (flip min)
  max = flip $ binaryWithEncl (flip max)

lift2 ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq b, SuitableForSeq c)
  =>
  String -> (a -> b -> c) -> SequenceA to a -> SequenceA to b -> SequenceA to c
lift2 name op aSeq bSeq =
  newSeq (op sampleA sampleB) name [AnyProtocolQA aSeq, AnyProtocolQA bSeq] makeQ
  where
  SequenceP sampleA = qaProtocol aSeq
  SequenceP sampleB = qaProtocol bSeq
  makeQ =
    proc ac ->
      do
      a <- seqWithAccuracy aSeq -< ac
      b <- seqWithAccuracy bSeq -< ac
      returnA -< op a b

lift2T ::
  (QAArrow to, SuitableForSeq a, SuitableForSeq c)
  =>
  String -> (a -> t -> c) -> SequenceA to a -> t -> SequenceA to c
lift2T name op aSeq b =
  newSeq (op sampleA b) name [AnyProtocolQA aSeq] makeQ
  where
  SequenceP sampleA = qaProtocol aSeq
  makeQ =
    proc ac ->
      do
      a <- seqWithAccuracy aSeq -< ac
      returnA -< op a b

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

    instance
      (QAArrow to
      , CanMinMaxAsymmetric a $t, SuitableForSeq a, SuitableForSeq (MinMaxType a $t))
      =>
      CanMinMaxAsymmetric (SequenceA to a) $t
      where
      type MinMaxType (SequenceA to a) $t = SequenceA to (MinMaxType a $t)
      min = binaryOpWithPureArg "min" min (getInitQ1TFromSimple id)
      max = binaryOpWithPureArg "max" max (getInitQ1TFromSimple id)

    instance
      (QAArrow to
      , CanMinMaxAsymmetric $t b, SuitableForSeq b, SuitableForSeq (MinMaxType $t b))
      =>
      CanMinMaxAsymmetric $t (SequenceA to b)
      where
      type MinMaxType $t (SequenceA to b) = SequenceA to (MinMaxType $t b)
      min = flip $ binaryOpWithPureArg "min" (flip min) (getInitQ1TFromSimple id)
      max = flip $ binaryOpWithPureArg "max" (flip max) (getInitQ1TFromSimple id)

    instance
      (QAArrow to, HasEqAsymmetric a $t
      , SuitableForSeq a, SuitableForSeq (EqCompareType a $t))
      =>
      HasEqAsymmetric (SequenceA to a) $t
      where
      type EqCompareType (SequenceA to a) $t = SequenceA to (EqCompareType a $t)
      equalTo = lift2T "==" (==)
      notEqualTo = lift2T "/=" (/=)

    instance
      (QAArrow to, HasOrderAsymmetric a $t
      , SuitableForSeq a, SuitableForSeq (OrderCompareType a $t))
      =>
      HasOrderAsymmetric (SequenceA to a) $t
      where
      type OrderCompareType (SequenceA to a) $t = SequenceA to (OrderCompareType a $t)
      lessThan = lift2T "<" (<)
      leq = lift2T "<=" (<=)
      greaterThan = lift2T ">" (>)
      geq = lift2T ">=" (>=)

  |]))
