{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Sequence.Elementary
    Description :  elementary functions on sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary functions on fast converging sequences.
-}
module AERN2.Sequence.Elementary
()
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
import AERN2.Sequence.Ring ()
import AERN2.Sequence.Field ()

{- exp -}

instance
  (QAArrow to, CanExp a
  , CanEnsureCN (ExpType a), HasNorm (EnsureNoCN (ExpType a))
  , SuitableForSeq a, SuitableForSeq (ExpType a))
  =>
  CanExp (SequenceA to a)
  where
  type ExpType (SequenceA to a) = SequenceA to (ExpType a)
  exp = unaryOp "exp" exp expGetInitQ1
    where
    expGetInitQ1 me a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog me a1 exp -< q
        let jInit = case m_a1NormLog of
                Just expNL -> q + expNL
                _ -> q
        returnA -< (jInit, Just b)

{- log -}

instance
  (QAArrow to, CanLog a, CanSetPrecision a
  , CanEnsureCN a, HasNorm (EnsureNoCN a)
  , SuitableForSeq a, SuitableForSeq (LogType a))
  =>
  CanLog (SequenceA to a)
  where
  type LogType (SequenceA to a) = SequenceA to (LogType a)
  log = unaryOp "log" log logGetInitQ1
    where
    logGetInitQ1 me a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog me a1 id -< q
        let jInit = case m_a1NormLog of
                Just a1normLog -> q - a1normLog
                _ -> q
        returnA -< (jInit, Just $ setPrecisionAtLeastAccuracy ((_acGuide q)+5) b)
        -- the @setPrecisionAtLeastAccuracy (q+5)@ above improves
        -- efficiency for exact low-precision arguments

{- power -}

instance
  (QAArrow to, CanPow a e
  , CanEnsureCN a, HasNorm (EnsureNoCN a)
  , HasIntegerBounds e
  , SuitableForSeq a, SuitableForSeq e
  , SuitableForSeq (PowTypeNoCN a e)
  , SuitableForSeq (PowType a e))
  =>
  CanPow (SequenceA to a) (SequenceA to e)
  where
  type PowTypeNoCN (SequenceA to a) (SequenceA to e) = SequenceA to (PowTypeNoCN a e)
  powNoCN = binaryOp "^" powNoCN powGetInitQ1Q2
  type PowType (SequenceA to a) (SequenceA to e) = SequenceA to (PowType a e)
  pow = binaryOp "^" pow powGetInitQ1Q2

powGetInitQ1Q2 ::
  (QAArrow to
  , HasNorm (EnsureNoCN b), CanEnsureCN b, HasIntegerBounds e)
  =>
  Maybe (QAId to) -> SequenceA to b -> SequenceA to e ->
  AccuracySG `to` ((AccuracySG, Maybe b), (AccuracySG, Maybe e))
powGetInitQ1Q2 me base e =
  proc q ->
    do
    baseB <- seqWithAccuracy base me -< q
    eB <- seqWithAccuracy e me -< q
    let jInit1 = powGetInitAC1 baseB eB q
    let jInit2 = powGetInitAC2 baseB eB q
    returnA -< ((jInit1, Just baseB), (jInit2, Just eB))

powGetInitAC1 ::
  (HasNorm (EnsureNoCN base), CanEnsureCN base, HasIntegerBounds e)
  =>
  base -> e -> AccuracySG -> AccuracySG
powGetInitAC1 base e acSG =
  let eI = snd (integerBounds e) + 1 in
  case ensureNoCN base of
    (Just baseNoCN, _) ->
      case getNormLog baseNoCN of
        NormBits baseNL -> acSG + (baseNL * (eI - 1))
        NormZero -> acSG0  -- base == 0, the query does not matter
    _ -> acSG0

powGetInitAC2 ::
  (HasNorm (EnsureNoCN base), CanEnsureCN base, HasIntegerBounds e)
  =>
  base -> e -> AccuracySG -> AccuracySG
powGetInitAC2 base e acSG =
  let eI = snd (integerBounds e) + 1 in
  case ensureNoCN base of
    (Just baseNoCN, _) ->
      case getNormLog baseNoCN of
        NormBits baseNL -> acSG + baseNL * eI
        NormZero -> acSG0  -- base == 0, the query does not matter
    _ -> acSG0


powGetInitQ1T ::
  (QAArrow to, HasNorm (EnsureNoCN base), CanEnsureCN base, HasIntegerBounds e)
  =>
  (Maybe (QAId to)) -> SequenceA to base -> e -> AccuracySG `to` (AccuracySG, Maybe base)
powGetInitQ1T me baseSeq e =
  proc q ->
    do
    base <- seqWithAccuracy baseSeq me -< q
    returnA -< (powGetInitAC1 base e q, Just base)

powGetInitQ2T ::
  (QAArrow to, HasNorm (EnsureNoCN base), CanEnsureCN base, HasIntegerBounds e)
  =>
  (Maybe (QAId to)) -> base -> SequenceA to e -> AccuracySG `to` (AccuracySG, Maybe e)
powGetInitQ2T me base eSeq =
  proc q ->
    do
    e <- seqWithAccuracy eSeq me -< q
    returnA -< (powGetInitAC1 base e q, Just e)

instance
  (CanPow a MPBall, SuitableForSeq a
  , HasNorm (EnsureNoCN a), CanEnsureCN a
  , CanSetPrecision (PowTypeNoCN a MPBall)
  , CanSetPrecision (PowType a MPBall))
  =>
  CanPow (Sequence a) MPBall
  where
  type PowTypeNoCN (Sequence a) MPBall = PowTypeNoCN a MPBall
  powNoCN base e = binaryWithEnclTranslateAC powGetInitAC1 powNoCN base e
  type PowType (Sequence a) MPBall = PowType a MPBall
  pow base e = binaryWithEnclTranslateAC powGetInitAC1 pow base e

instance
  (CanPow MPBall e, SuitableForSeq e
  , HasIntegerBounds e
  , CanSetPrecision (PowTypeNoCN MPBall e)
  , CanSetPrecision (PowType MPBall e))
  =>
  CanPow MPBall (Sequence e)
  where
  type PowTypeNoCN MPBall (Sequence e) = PowTypeNoCN MPBall e
  powNoCN =
    flip (binaryWithEnclTranslateAC (flip powGetInitAC2) (flip powNoCN))
  type PowType MPBall (Sequence e) = PowType MPBall e
  pow =
    flip (binaryWithEnclTranslateAC (flip powGetInitAC2) (flip pow))

instance
  (CanPow (SequenceA to a) b
  , CanEnsureCE es b
  , CanEnsureCE es (PowTypeNoCN (SequenceA to a) b)
  , CanEnsureCE es (PowType (SequenceA to a) b)
  , CanBeErrors es)
  =>
  CanPow (SequenceA to a) (CollectErrors es  b)
  where
  type PowTypeNoCN (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (PowTypeNoCN (SequenceA to a) b)
  powNoCN = lift2TLCE powNoCN
  type PowType (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (PowType (SequenceA to a) b)
  pow = lift2TLCE pow

instance
  (CanPow a (SequenceA to b)
  , CanEnsureCE es a
  , CanEnsureCE es (PowType a (SequenceA to b))
  , CanEnsureCE es (PowTypeNoCN a (SequenceA to b))
  , CanBeErrors es)
  =>
  CanPow (CollectErrors es a) (SequenceA to b)
  where
  type PowTypeNoCN (CollectErrors es  a) (SequenceA to b) =
    EnsureCE es (PowTypeNoCN a (SequenceA to b))
  powNoCN = lift2TCE powNoCN
  type PowType (CollectErrors es  a) (SequenceA to b) =
    EnsureCE es (PowType a (SequenceA to b))
  pow = lift2TCE pow

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |], [t| Rational |]]
  (\ t -> [d|

    instance
      (QAArrow to, CanPow a $t
      , CanSetPrecision a
      , CanEnsureCN a, HasNorm (EnsureNoCN a)
      , SuitableForSeq a
      , SuitableForSeq (PowTypeNoCN a $t)
      , SuitableForSeq (PowType a $t))
      =>
      CanPow (SequenceA to a) $t where
      type PowTypeNoCN (SequenceA to a) $t = SequenceA to (PowTypeNoCN a $t)
      powNoCN = binaryOpWithPureArg "^" powNoCN powGetInitQ1T
      type PowType (SequenceA to a) $t = SequenceA to (PowType a $t)
      pow = binaryOpWithPureArg "^" pow powGetInitQ1T

    instance
      (QAArrow to, CanPow $t a
      , CanSetPrecision a
      , HasIntegerBounds a
      , SuitableForSeq a
      , SuitableForSeq (PowType $t a)
      , SuitableForSeq (PowTypeNoCN $t a))
      =>
      CanPow $t (SequenceA to a) where
      type PowTypeNoCN $t (SequenceA to a) = SequenceA to (PowTypeNoCN $t a)
      powNoCN = flip $ binaryOpWithPureArg "^" (flip powNoCN) (\me -> flip (powGetInitQ2T me))
      type PowType $t (SequenceA to a) = SequenceA to (PowType $t a)
      pow = flip $ binaryOpWithPureArg "^" (flip pow) (\me -> flip (powGetInitQ2T me))

  |]))

{- sqrt -}

instance
  (QAArrow to, CanSqrt a
  , CanMinMaxThis a Integer
  , CanEnsureCN (SqrtType a), HasNorm (EnsureNoCN (SqrtType a))
  , SuitableForSeq a, SuitableForSeq (SqrtType a))
  =>
  CanSqrt (SequenceA to a)
  where
  type SqrtType (SequenceA to a) = SequenceA to (SqrtType a)
  sqrt = unaryOp "sqrt" sqrt sqrtGetInitQ1
    where
    sqrtGetInitQ1 me a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog me a1 sqrtSafe -< q
        let jInit = case m_a1NormLog of
                Just sqrtNormLog
                  | sqrtNormLog < 0 -> max acSG0 (q - 1 - 2*sqrtNormLog) -- nearer 0
                  | otherwise -> max acSG0 (q - 1 - sqrtNormLog)
                _ -> acSG0
        returnA -< (jInit, Just b)
    sqrtSafe x =
      sqrt (max 0 x)

{- sine, cosine -}

instance
  (QAArrow to, CanSinCos a
  , CanEnsureCN (SinCosType a), HasNorm (EnsureNoCN (SinCosType a))
  , SuitableForSeq a, SuitableForSeq (SinCosType a))
  =>
  CanSinCos (SequenceA to a)
  where
  type SinCosType (SequenceA to a) = SequenceA to (SinCosType a)
  cos = unaryOp "cos" cos cosGetInitQ1
    where
    cosGetInitQ1 me a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog me a1 sin -< q
        let jInit = case m_a1NormLog of
                Just sinNormLog -> q + sinNormLog
                _ -> acSG0 -- this should never happen
        returnA -< (jInit, Just b)
  sin = unaryOp "sin" sin sinGetInitQ1
    where
    sinGetInitQ1 me a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog me a1 cos -< q
        let jInit = case m_a1NormLog of
                Just cosNormLog -> q + cosNormLog
                _ -> acSG0 -- this should never happen
        returnA -< (jInit, Just b)
