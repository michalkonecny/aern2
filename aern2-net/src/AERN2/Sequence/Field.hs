{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Sequence.Field
    Description :  field operations on sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on convergent sequences.
-}
module AERN2.Sequence.Field
(
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.Arrow

import Control.CollectErrors

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type
import AERN2.Sequence.Helpers
import AERN2.Sequence.Ring (mulGetInitAC)

{- division -}

instance
  (QAArrow to, CanDiv a b, HasNorm (EnsureNoCN a), HasNorm (EnsureNoCN b)
  , SuitableForSeq a, SuitableForSeq b
  , SuitableForSeq (DivType a b), SuitableForSeq (DivTypeNoCN a b))
  =>
  CanDiv (SequenceA to a) (SequenceA to b)
  where
  type DivType (SequenceA to a) (SequenceA to b) = SequenceA to (DivType a b)
  divide = binaryOp "/" divide divGetInitQ1Q2
  type DivTypeNoCN (SequenceA to a) (SequenceA to b) = SequenceA to (DivTypeNoCN a b)
  divideNoCN = binaryOp "/" divideNoCN divGetInitQ1Q2

divGetInitQ1Q2 ::
  (QAArrow to
  , HasNorm (EnsureNoCN a), HasNorm (EnsureNoCN b)
  , SuitableForSeq a, SuitableForSeq b)
  =>
  Maybe (QAId to) -> SequenceA to a -> SequenceA to b -> AccuracySG `to` ((AccuracySG, Maybe a), (AccuracySG, Maybe b))
divGetInitQ1Q2 me a1 a2 =
  proc q ->
    do
    -- In a Fractional instance, optimising 3/x and not optimising x/3 etc.
    -- In a Fractional instance, x/3 should be replaced by (1/3)*x etc.
    b1 <- seqWithAccuracy a1 me -< q
    let jPre2 = mulGetInitAC b1 q
    b2 <- seqWithAccuracy a2 me -< jPre2
    let jInit1 = divGetInitAC1 b2 q
    let jInit2 = divGetInitAC2 b1 b2 q
    returnA -< ((jInit1, Just b1), (jInit2, Just b2))

divGetInitAC1 ::
  (HasNorm (EnsureNoCN denom), CanEnsureCN denom)
  =>
  denom -> AccuracySG -> AccuracySG
divGetInitAC1 denom acSG =
  case ensureNoCN denom of
    (Just denomNoCN, _) ->
      case getNormLog denomNoCN of
        NormBits denomNL -> max acSG0 (acSG - denomNL)
        NormZero -> acSG0 -- denominator == 0, we have no chance...
    _ -> acSG0

divGetInitAC2 ::
  (HasNorm (EnsureNoCN numer), CanEnsureCN numer
  , HasNorm (EnsureNoCN denom), CanEnsureCN denom)
  =>
  numer -> denom -> AccuracySG -> AccuracySG
divGetInitAC2 numer denom acSG =
  case (ensureNoCN numer, ensureNoCN denom) of
    ((Just numerNoCN, _), (Just denomNoCN, _)) ->
      case (getNormLog numerNoCN, getNormLog denomNoCN) of
        (_, NormZero) -> acSG0 -- denominator == 0, we have no chance...
        (NormZero, _) -> acSG0 -- numerator == 0, it does not matter
        (NormBits numerNL, NormBits denomNL) -> max acSG0 (acSG + numerNL - 2 * denomNL)
    _ -> acSG0


instance
  (CanDiv a MPBall, SuitableForSeq a
  , CanSetPrecision (DivType a MPBall), CanSetPrecision (DivTypeNoCN a MPBall))
  =>
  CanDiv (Sequence a) MPBall
  where
  type DivType (Sequence a) MPBall = DivType a MPBall
  divide = binaryWithEnclTranslateAC (\ _ -> divGetInitAC1) divide
  type DivTypeNoCN (Sequence a) MPBall = DivTypeNoCN a MPBall
  divideNoCN = binaryWithEnclTranslateAC (\ _ -> divGetInitAC1) divideNoCN

instance
  (CanDiv MPBall b, SuitableForSeq b
  , HasNorm (EnsureNoCN b), CanEnsureCN b
  , CanSetPrecision (DivType MPBall b)
  , CanSetPrecision (DivTypeNoCN MPBall b))
  =>
  CanDiv MPBall (Sequence b)
  where
  type DivType MPBall (Sequence b) = DivType MPBall b
  divide = flip (binaryWithEnclTranslateAC (flip divGetInitAC2) (flip divide))
  type DivTypeNoCN MPBall (Sequence b) = DivTypeNoCN MPBall b
  divideNoCN = flip (binaryWithEnclTranslateAC (flip divGetInitAC2) (flip divideNoCN))

instance
  (CanDiv (SequenceA to a) b
  , CanEnsureCE es b
  , CanEnsureCE es (DivType (SequenceA to a) b)
  , CanEnsureCE es (DivTypeNoCN (SequenceA to a) b)
  , CanBeErrors es)
  =>
  CanDiv (SequenceA to a) (CollectErrors es  b)
  where
  type DivType (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (DivType (SequenceA to a) b)
  divide = lift2TLCE divide
  type DivTypeNoCN (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (DivTypeNoCN (SequenceA to a) b)
  divideNoCN = lift2TLCE divideNoCN

instance
  (CanDiv a (SequenceA to b)
  , CanEnsureCE es a
  , CanEnsureCE es (DivType a (SequenceA to b))
  , CanEnsureCE es (DivTypeNoCN a (SequenceA to b))
  , CanBeErrors es)
  =>
  CanDiv (CollectErrors es a) (SequenceA to b)
  where
  type DivType (CollectErrors es  a) (SequenceA to b) =
    EnsureCE es (DivType a (SequenceA to b))
  divide = lift2TCE divide
  type DivTypeNoCN (CollectErrors es  a) (SequenceA to b) =
    EnsureCE es (DivTypeNoCN a (SequenceA to b))
  divideNoCN = lift2TCE divideNoCN

divGetInitQ1T ::
  (Arrow to, HasNorm (EnsureNoCN denom), CanEnsureCN denom)
  =>
  Maybe (QAId to) -> SequenceA to numer -> denom -> AccuracySG `to` (AccuracySG, Maybe numer)
divGetInitQ1T _me _numerSeq denom =
  arr $ \q -> (divGetInitAC1 denom q, Nothing)

divGetInitQ2T ::
  (QAArrow to
  , HasNorm (EnsureNoCN numer), CanEnsureCN numer
  , HasNorm (EnsureNoCN denom), CanEnsureCN denom)
  =>
  Maybe (QAId to) -> numer -> SequenceA to denom -> AccuracySG `to` (AccuracySG, Maybe denom)
divGetInitQ2T me numer denomSeq =
  proc q ->
    do
    denom <- seqWithAccuracy denomSeq me -< q
    returnA -< (divGetInitAC2 numer denom q, Just denom)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanDiv a $t, SuitableForSeq a
    , SuitableForSeq (DivType a $t), SuitableForSeq (DivTypeNoCN a $t))
    =>
    CanDiv (SequenceA to a) $t
    where
    type DivType (SequenceA to a) $t = SequenceA to (DivType a $t)
    divide = binaryOpWithPureArg "/" divide divGetInitQ1T
    type DivTypeNoCN (SequenceA to a) $t = SequenceA to (DivTypeNoCN a $t)
    divideNoCN = binaryOpWithPureArg "/" divideNoCN divGetInitQ1T

  instance
    (QAArrow to, CanDiv $t b, SuitableForSeq b
    , SuitableForSeq (DivType $t b)
    , SuitableForSeq (DivTypeNoCN $t b)
    , HasNorm (EnsureNoCN b))
    =>
    CanDiv $t (SequenceA to b)
    where
    type DivType $t (SequenceA to b) = SequenceA to (DivType $t b)
    divide = flip $ binaryOpWithPureArg "/" (flip divide) (\ me -> flip (divGetInitQ2T me))
    type DivTypeNoCN $t (SequenceA to b) = SequenceA to (DivTypeNoCN $t b)
    divideNoCN = flip $ binaryOpWithPureArg "/" (flip divideNoCN) (\ me -> flip (divGetInitQ2T me))

  |]))
