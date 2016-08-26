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
pi = newCR "pi" (seqByPrecision2CauchySeq piBallP)

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
        returnA -< (jInit, Just b)

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

-- {- TODO CR power -}
--
-- instance (QAArrow to) => CanPow (CauchyRealA to) (CauchyRealA to) where
--   pow = -- adapt algorithm for division

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
