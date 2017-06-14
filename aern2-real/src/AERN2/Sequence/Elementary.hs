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

import Numeric.MixedTypes
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
    expGetInitQ1 a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog a1 exp -< q
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
    logGetInitQ1 a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog a1 id -< q
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
  , SuitableForSeq (PowType a e))
  =>
  CanPow (SequenceA to a) (SequenceA to e)
  where
  type PowType (SequenceA to a) (SequenceA to e) = SequenceA to (PowType a e)
  pow =
    binaryOp "^" pow getInitQ1Q2
    where
    getInitQ1Q2 base e =
      proc q ->
        do
        baseB <- seqWithAccuracy base -< q
        eB <- seqWithAccuracy e -< q
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
    Left _ -> acSG0
    Right baseNoCN ->
      case getNormLog baseNoCN of
        NormBits baseNL -> acSG + (baseNL * (eI - 1))
        NormZero -> acSG0  -- base == 0, the query does not matter

powGetInitAC2 ::
  (HasNorm (EnsureNoCN base), CanEnsureCN base, HasIntegerBounds e)
  =>
  base -> e -> AccuracySG -> AccuracySG
powGetInitAC2 base e acSG =
  let eI = snd (integerBounds e) + 1 in
  case ensureNoCN base of
    Left _ -> acSG0
    Right baseNoCN ->
      case getNormLog baseNoCN of
        NormBits baseNL -> acSG + baseNL * eI
        NormZero -> acSG0  -- base == 0, the query does not matter


powGetInitQ1T ::
  (QAArrow to, HasNorm (EnsureNoCN base), CanEnsureCN base, HasIntegerBounds e)
  =>
  SequenceA to base -> e -> AccuracySG `to` (AccuracySG, Maybe base)
powGetInitQ1T baseSeq e =
  proc q ->
    do
    base <- seqWithAccuracy baseSeq -< q
    returnA -< (powGetInitAC1 base e q, Just base)

powGetInitQ2T ::
  (QAArrow to, HasNorm (EnsureNoCN base), CanEnsureCN base, HasIntegerBounds e)
  =>
  base -> SequenceA to e -> AccuracySG `to` (AccuracySG, Maybe e)
powGetInitQ2T base eSeq =
  proc q ->
    do
    e <- seqWithAccuracy eSeq -< q
    returnA -< (powGetInitAC1 base e q, Just e)

instance
  (CanPow a MPBall, SuitableForSeq a
  , HasNorm (EnsureNoCN a), CanEnsureCN a
  , CanSetPrecision (PowType a MPBall))
  =>
  CanPow (Sequence a) MPBall
  where
  type PowType (Sequence a) MPBall = PowType a MPBall
  pow base e = binaryWithEnclTranslateAC powGetInitAC1 pow base e

instance
  (CanPow MPBall e, SuitableForSeq e
  , HasIntegerBounds e
  , CanSetPrecision (PowType MPBall e))
  =>
  CanPow MPBall (Sequence e)
  where
  type PowType MPBall (Sequence e) = PowType MPBall e
  pow =
    flip (binaryWithEnclTranslateAC (flip powGetInitAC2) (flip pow))

instance
  (CanPow (SequenceA to a) b
  , CanEnsureCE es (PowType (SequenceA to a) b)
  , SuitableForCE es)
  =>
  CanPow (SequenceA to a) (CollectErrors es  b)
  where
  type PowType (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (PowType (SequenceA to a) b)
  pow = lift2TLCE pow

instance
  (CanPow a (SequenceA to b)
  , CanEnsureCE es (PowType a (SequenceA to b))
  , SuitableForCE es)
  =>
  CanPow (CollectErrors es a) (SequenceA to b)
  where
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
      , SuitableForSeq (PowType a $t))
      =>
      CanPow (SequenceA to a) $t where
      type PowType (SequenceA to a) $t = SequenceA to (PowType a $t)
      pow = binaryOpWithPureArg "^" pow powGetInitQ1T

    instance
      (QAArrow to, CanPow $t a
      , CanSetPrecision a
      , HasIntegerBounds a
      , SuitableForSeq a
      , SuitableForSeq (PowType $t a))
      =>
      CanPow $t (SequenceA to a) where
      type PowType $t (SequenceA to a) = SequenceA to (PowType $t a)
      pow = flip $ binaryOpWithPureArg "^" (flip pow) (flip powGetInitQ2T)

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
    sqrtGetInitQ1 a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog a1 sqrtSafe -< q
        let jInit = case m_a1NormLog of
                Just sqrtNormLog -> max acSG0 (q - 1 - sqrtNormLog)
                _ -> acSG0
        returnA -< (jInit, Just b)
    sqrtSafe x = sqrt (max 0 x)

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
    cosGetInitQ1 a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog a1 sin -< q
        let jInit = case m_a1NormLog of
                Just sinNormLog -> q + sinNormLog
                _ -> acSG0 -- this should never happen
        returnA -< (jInit, Just b)
  sin = unaryOp "sin" sin sinGetInitQ1
    where
    sinGetInitQ1 a1 =
      proc q ->
        do
        (m_a1NormLog, b) <- getSeqFnNormLog a1 cos -< q
        let jInit = case m_a1NormLog of
                Just cosNormLog -> q + cosNormLog
                _ -> acSG0 -- this should never happen
        returnA -< (jInit, Just b)
