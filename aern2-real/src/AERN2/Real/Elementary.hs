{-|
    Module      :  AERN2.Real.Elementary
    Description :  elementary functions on CR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary functions on Cauchy Real numbers.
-}
module AERN2.Real.Elementary
(
  pi, piA, sqrtA, expA, logA, sinA, cosA
)
where

import Numeric.MixedTypes -- hiding (id)
-- import qualified Prelude as P

-- import Control.Category (id)
import Control.Arrow

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA
import AERN2.Real.Type
import AERN2.Real.Aux
import AERN2.Real.Ring ()
import AERN2.Real.Field ()

{-
  To get @pi@ in an arbitrary arrow, use 'piA'.
-}
pi :: CauchyReal
pi = newCR "pi" [] (seqByPrecision2CauchySeq piBallP)

piA :: (QAArrow to) => CauchyRealA to
piA = qaArr pi

{- sqrt -}

instance (QAArrow to) => CanSqrt (CauchyRealA to) where
  sqrt = unaryOp "sqrt" sqrt sqrtGetInitQ1
    where
    sqrtGetInitQ1 a1 =
      proc q ->
        do
        (a1NormLog, b) <- getCRFnNormLog a1 sqrtSafe -< q
        let jInit = case a1NormLog of
                NormBits sqrtNormLog -> max 0 (q - 1 - sqrtNormLog)
                NormZero -> q
        returnA -< (jInit, Just b)
    sqrtSafe x =
      case x < 0 of
        Just True -> error "sqrt of a negative argument"
        _ -> sqrt (max 0 x)


instance CanSqrt Integer where
  type SqrtType Integer = CauchyReal
  sqrt = sqrt . real

instance CanSqrt Int where
  type SqrtType Int = CauchyReal
  sqrt = sqrt . real

instance CanSqrt Dyadic where
  type SqrtType Dyadic = CauchyReal
  sqrt = sqrt . real

instance CanSqrt Rational where
  type SqrtType Rational = CauchyReal
  sqrt = sqrt . real

sqrtA ::
  (QAArrow to, CanSqrt t, SqrtType t ~ CauchyReal)
  =>
  t -> CauchyRealA to
sqrtA = qaArr . sqrt

{- exp -}

instance (QAArrow to) => CanExp (CauchyRealA to) where
  exp = unaryOp "exp" exp expGetInitQ1
    where
    expGetInitQ1 a1 =
      proc q ->
        do
        (a1NormLog, b) <- getCRFnNormLog a1 exp -< q
        let jInit = case a1NormLog of
                NormBits expNormLog -> q + expNormLog
                NormZero -> q -- this should never happen
        returnA -< (jInit, Just b)

instance CanExp Integer where
  type ExpType Integer = CauchyReal
  exp = exp . real

instance CanExp Int where
  type ExpType Int = CauchyReal
  exp = exp . real

instance CanExp Dyadic where
  type ExpType Dyadic = CauchyReal
  exp = exp . real

instance CanExp Rational where
  type ExpType Rational = CauchyReal
  exp = exp . real

expA ::
  (QAArrow to, CanExp t, ExpType t ~ CauchyReal)
  =>
  t -> CauchyRealA to
expA = qaArr . exp

{- log -}

instance (QAArrow to) => CanLog (CauchyRealA to) where
  log = unaryOp "log" log logGetInitQ1
    where
    logGetInitQ1 a1 =
      proc q ->
        do
        (a1NormLog, b) <- getCRFnNormLog a1 id -< q
        let jInit = case a1NormLog of
                NormBits normLog -> q - normLog
                NormZero -> q
        returnA -< (jInit, Just $ setPrecisionAtLeastAccuracy (q+5) b)
        -- the @setPrecisionAtLeastAccuracy (q+5)@ above improves
        -- efficiency for exact low-precision arguments

instance CanLog Integer where
  type LogType Integer = CauchyReal
  log = log . real

instance CanLog Int where
  type LogType Int = CauchyReal
  log = log . real

instance CanLog Dyadic where
  type LogType Dyadic = CauchyReal
  log = log . real

instance CanLog Rational where
  type LogType Rational = CauchyReal
  log = log . real

logA ::
  (QAArrow to, CanLog t, LogType t ~ CauchyReal)
  =>
  t -> CauchyRealA to
logA = qaArr . log

{- power -}

instance (QAArrow to) => CanPow (CauchyRealA to) (CauchyRealA to) where
  pow =
    binaryOp "^" pow getInitQ1Q2
    where
    getInitQ1Q2 a1 a2 =
      proc q ->
        do
        (a1NormLog, b1) <- getCRFnNormLog a1 id -< q
        (_a2NormLog, b2) <- getCRFnNormLog a2 id -< q
        let b2I = snd (integerBounds b2) + 1
        let (jInit1, jInit2) = case a1NormLog of
                NormZero -> (q, q) -- base == 0, the query does not matter that much
                NormBits a1NL -> (q + (a1NL * (b2I - 1)), q + a1NL * b2I)
        returnA -< ((jInit1, Just b1), (jInit2, Just b2))

instance (QAArrow to) => CanPow Integer (CauchyRealA to) where
  type PowType Integer (CauchyRealA to) = CauchyRealA to
  pow = convertFirst pow

instance (QAArrow to) => CanPow Int (CauchyRealA to) where
  type PowType Int (CauchyRealA to) = CauchyRealA to
  pow = convertFirst pow

instance (QAArrow to) => CanPow (CauchyRealA to) Dyadic where
  type PowType (CauchyRealA to) Dyadic = CauchyRealA to
  pow = convertSecond pow

instance (QAArrow to) => CanPow Dyadic (CauchyRealA to) where
  type PowType Dyadic (CauchyRealA to) = CauchyRealA to
  pow = convertFirst pow

instance (QAArrow to) => CanPow (CauchyRealA to) Rational where
  type PowType (CauchyRealA to) Rational = CauchyRealA to
  pow = convertSecond pow

instance (QAArrow to) => CanPow Rational (CauchyRealA to) where
  type PowType Rational (CauchyRealA to) = CauchyRealA to
  pow = convertFirst pow

instance CanPow CauchyReal MPBall where
  type PowType CauchyReal MPBall = MPBall
  pow = binaryWithBall pow

instance CanPow MPBall CauchyReal where
  type PowType MPBall CauchyReal = MPBall
  pow = flip $ binaryWithBall (flip pow)

instance CanPow Integer Dyadic where
  type PowType Integer Dyadic = CauchyReal
  pow b e = pow (real b) (real e)

instance CanPow Int Dyadic where
  type PowType Int Dyadic = CauchyReal
  pow b e = pow (real b) (real e)

instance CanPow Dyadic Dyadic where
  type PowType Dyadic Dyadic = CauchyReal
  pow b e = pow (real b) (real e)

instance CanPow Rational Dyadic where
  type PowType Rational Dyadic = CauchyReal
  pow b e = pow (real b) (real e)

instance CanPow Integer Rational where
  type PowType Integer Rational = CauchyReal
  pow b e = pow (real b) (real e)

instance CanPow Int Rational where
  type PowType Int Rational = CauchyReal
  pow b e = pow (real b) (real e)

instance CanPow Dyadic Rational where
  type PowType Dyadic Rational = CauchyReal
  pow b e = pow (real b) (real e)

instance CanPow Rational Rational where
  type PowType Rational Rational = CauchyReal
  pow b e = pow (real b) (real e)


{- sine, cosine -}

instance (QAArrow to) => CanSinCos (CauchyRealA to) where
  cos = unaryOp "cos" cos cosGetInitQ1
    where
    cosGetInitQ1 a1 =
      proc q ->
        do
        (a1NormLog, b) <- getCRFnNormLog a1 sin -< q
        let jInit = case a1NormLog of
                NormBits sinNormLog -> q + sinNormLog
                NormZero -> q -- this should never happen
        returnA -< (jInit, Just b)
  sin = unaryOp "sin" sin sinGetInitQ1
    where
    sinGetInitQ1 a1 =
      proc q ->
        do
        (a1NormLog, b) <- getCRFnNormLog a1 cos -< q
        let jInit = case a1NormLog of
                NormBits cosNormLog -> q + cosNormLog
                NormZero -> q -- this should never happen
        returnA -< (jInit, Just b)

instance CanSinCos Integer where
  type SinCosType Integer = CauchyReal
  cos = cos . real
  sin = sin . real

instance CanSinCos Int where
  type SinCosType Int = CauchyReal
  cos = cos . real
  sin = sin . real

instance CanSinCos Dyadic where
  type SinCosType Dyadic = CauchyReal
  cos = cos . real
  sin = sin . real

instance CanSinCos Rational where
  type SinCosType Rational = CauchyReal
  cos = cos . real
  sin = sin . real

cosA ::
  (QAArrow to, CanSinCos t, SinCosType t ~ CauchyReal)
  =>
  t -> CauchyRealA to
cosA = qaArr . cos

sinA ::
  (QAArrow to, CanSinCos t, SinCosType t ~ CauchyReal)
  =>
  t -> CauchyRealA to
sinA = qaArr . sin
