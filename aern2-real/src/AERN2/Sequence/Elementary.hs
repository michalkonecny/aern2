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
(
  -- expA, logA, pi, piA, sqrtA, sinA, cosA
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Control.Arrow

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
    getInitQ1Q2 a1 a2 =
      proc q ->
        do
        (m_a1NormLog, b1) <- getSeqFnNormLog a1 id -< q
        b2 <- seqWithAccuracy a2 -< q
        let b2I = snd (integerBounds b2) + 1
        let (jInit1, jInit2) =
              case m_a1NormLog of
                Just a1NL -> (q + (a1NL * (b2I - 1)), q + a1NL * b2I)
                _ -> (q, q) -- base == 0, the query does not matter that much
        returnA -< ((jInit1, Just b1), (jInit2, Just b2))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |]]
  (\ t -> [d|

    instance
      (QAArrow to, CanPow a a
      , ConvertibleExactly $t a
      , CanSetPrecision a
      , CanEnsureCN a, HasNorm (EnsureNoCN a)
      , HasIntegerBounds a
      , SuitableForSeq a
      , SuitableForSeq (PowType a a))
      =>
      CanPow $t (SequenceA to a) where
      type PowType $t (SequenceA to a) = SequenceA to (PowType a a)
      pow = convertFirst pow

  |]))

-- instance (QAArrow to) => CanPow Int (SequenceA to a) where
--   type PowType Int (SequenceA to a) = SequenceA to a
--   pow = convertFirst pow
--
-- instance CanPow CauchyReal MPBall where
--   type PowType CauchyReal MPBall = MPBall
--   pow = binaryWithBall pow
--
-- instance CanPow MPBall CauchyReal where
--   type PowType MPBall CauchyReal = MPBall
--   pow = flip $ binaryWithBall (flip pow)
--
-- instance CanPow Integer Dyadic where
--   type PowType Integer Dyadic = CauchyReal
--   pow b e = pow (real b) (real e)
--
-- instance CanPow Int Dyadic where
--   type PowType Int Dyadic = CauchyReal
--   pow b e = pow (real b) (real e)
--
-- instance CanPow Dyadic Dyadic where
--   type PowType Dyadic Dyadic = CauchyReal
--   pow b e = pow (real b) (real e)
--
-- instance CanPow Rational Dyadic where
--   type PowType Rational Dyadic = CauchyReal
--   pow b e = pow (real b) (real e)
--
-- instance CanPow Integer Rational where
--   type PowType Integer Rational = CauchyReal
--   pow b e = pow (real b) (real e)
--
-- instance CanPow Int Rational where
--   type PowType Int Rational = CauchyReal
--   pow b e = pow (real b) (real e)
--
-- instance CanPow Dyadic Rational where
--   type PowType Dyadic Rational = CauchyReal
--   pow b e = pow (real b) (real e)
--
-- instance CanPow Rational Rational where
--   type PowType Rational Rational = CauchyReal
--   pow b e = pow (real b) (real e)
--
--
-- {-|
--   To get @pi@ in an arbitrary arrow, use 'piA'.
-- -}
-- pi :: CauchyReal
-- pi = newCR "pi" [] (seqByPrecision2CauchySeq piBallP . _acGuide)
--
-- piA :: (QAArrow to) => SequenceA to a
-- piA = realA pi
--
-- {- sqrt -}
--
-- instance (QAArrow to) => CanSqrt (SequenceA to a) where
--   sqrt = unaryOp "sqrt" sqrt sqrtGetInitQ1
--     where
--     sqrtGetInitQ1 a1 =
--       proc q ->
--         do
--         (a1NormLog, b) <- getCRFnNormLog a1 sqrtSafe -< q
--         let jInit = case a1NormLog of
--                 NormBits sqrtNormLog -> max acSG0 (q - 1 - sqrtNormLog)
--                 NormZero -> q
--         returnA -< (jInit, Just b)
--     sqrtSafe x =
--       case x < 0 of
--         Just True -> error "sqrt of a negative argument"
--         _ -> sqrt (max 0 x)
--
--
-- instance CanSqrt Integer where
--   type SqrtType Integer = CauchyReal
--   sqrt = sqrt . real
--
-- instance CanSqrt Int where
--   type SqrtType Int = CauchyReal
--   sqrt = sqrt . real
--
-- instance CanSqrt Dyadic where
--   type SqrtType Dyadic = CauchyReal
--   sqrt = sqrt . real
--
-- instance CanSqrt Rational where
--   type SqrtType Rational = CauchyReal
--   sqrt = sqrt . real
--
-- sqrtA ::
--   (QAArrow to, CanSqrt t, SqrtType t ~ CauchyReal)
--   =>
--   t -> SequenceA to a
-- sqrtA = realA . sqrt
--
-- {- sine, cosine -}
--
-- instance (QAArrow to) => CanSinCos (SequenceA to a) where
--   cos = unaryOp "cos" cos cosGetInitQ1
--     where
--     cosGetInitQ1 a1 =
--       proc q ->
--         do
--         (a1NormLog, b) <- getCRFnNormLog a1 sin -< q
--         let jInit = case a1NormLog of
--                 NormBits sinNormLog -> q + sinNormLog
--                 NormZero -> q -- this should never happen
--         returnA -< (jInit, Just b)
--   sin = unaryOp "sin" sin sinGetInitQ1
--     where
--     sinGetInitQ1 a1 =
--       proc q ->
--         do
--         (a1NormLog, b) <- getCRFnNormLog a1 cos -< q
--         let jInit = case a1NormLog of
--                 NormBits cosNormLog -> q + cosNormLog
--                 NormZero -> q -- this should never happen
--         returnA -< (jInit, Just b)
--
-- instance CanSinCos Integer where
--   type SinCosType Integer = CauchyReal
--   cos = cos . real
--   sin = sin . real
--
-- instance CanSinCos Int where
--   type SinCosType Int = CauchyReal
--   cos = cos . real
--   sin = sin . real
--
-- instance CanSinCos Dyadic where
--   type SinCosType Dyadic = CauchyReal
--   cos = cos . real
--   sin = sin . real
--
-- instance CanSinCos Rational where
--   type SinCosType Rational = CauchyReal
--   cos = cos . real
--   sin = sin . real
--
-- cosA ::
--   (QAArrow to, CanSinCos t, SinCosType t ~ CauchyReal)
--   =>
--   t -> SequenceA to a
-- cosA = realA . cos
--
-- sinA ::
--   (QAArrow to, CanSinCos t, SinCosType t ~ CauchyReal)
--   =>
--   t -> SequenceA to a
-- sinA = realA . sin
