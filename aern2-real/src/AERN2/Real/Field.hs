{-|
    Module      :  AERN2.Real.Field
    Description :  field operations on CR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on Cauchy Real numbers.
-}
module AERN2.Real.Field
(
)
where

import Numeric.MixedTypes hiding (id)
-- import qualified Prelude as P

import Control.Category (id)
import Control.Arrow

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA
import AERN2.AccuracySG
import AERN2.Real.Type
import AERN2.Real.Helpers
import AERN2.Real.Ring ()

{- division -}

instance (QAArrow to) => CanDiv (CauchyRealA to) (CauchyRealA to) where
  divide =
    binaryOp "/" divide getInitQ1Q2
    where
    getInitQ1Q2 a1 a2 =
      proc q ->
        do
        -- In a Fractional instance, optimising 3/x and not optimising x/3 etc.
        -- In a Fractional instance, x/3 should be replaced by (1/3)*x etc.
        (a1NormLog, b1) <- getCRFnNormLog a1 id -< q
        let jPre2 = case a1NormLog of
                NormZero -> acSG0 -- numerator == 0, it does not matter
                NormBits a1NL -> max acSG0 (q + a1NL)
        (a2NormLog, b2) <- getCRFnNormLog a2 id -< jPre2
        let jInit1 = case a2NormLog of
                NormBits a2NL -> max acSG0 (q - a2NL)
                NormZero -> acSG0 -- denominator == 0, we have no chance...
        let jInit2 = case (a1NormLog, a2NormLog) of
                (_, NormZero) -> acSG0 -- denominator == 0, we have no chance...
                (NormZero, _) -> acSG0 -- numerator == 0, it does not matter
                (NormBits a1NL, NormBits a2NL) -> max acSG0 (q + a1NL - 2 * a2NL)
        returnA -< ((jInit1, Just b1), (jInit2, Just b2))


divGetInitQ1T ::
  (Arrow to, HasNorm t)
  =>
  r -> t -> AccuracySG `to` (AccuracySG, Maybe MPBall)
divGetInitQ1T _a1 n =
  proc q ->
    do
    let jInit1 = case nNormLog of
            NormBits nNL -> max acSG0 (q - nNL)
            NormZero -> acSG0 -- denominator == 0, we have no chance...
    returnA -< (jInit1, Nothing)
  where
  nNormLog = getNormLog n


divGetInitQ1TL ::
  (Arrow to, HasNorm t)
  =>
  (CauchyRealA to) -> t -> AccuracySG `to` (AccuracySG, Maybe MPBall)
divGetInitQ1TL a2 n =
  proc q ->
    do
    (a2NormLog, b2) <- getCRFnNormLog a2 id -< q
    let jInit2 = case (nNormLog, a2NormLog) of
            (_, NormZero) -> acSG0 -- denominator == 0, we have no chance...
            (NormZero, _) -> acSG0 -- numerator == 0, it does not matter
            (NormBits nNL, NormBits a2NL) -> max acSG0 (q + nNL - 2 * a2NL)
    returnA -< ((jInit2, Just b2))
  where
  nNormLog = getNormLog n

instance (QAArrow to) => CanDiv (CauchyRealA to) Integer where
  type DivType (CauchyRealA to) Integer = CauchyRealA to
  divide = binaryOpWithPureArg "/" divide divGetInitQ1T

instance (QAArrow to) => CanDiv Integer (CauchyRealA to) where
  type DivType Integer (CauchyRealA to) = CauchyRealA to
  divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL

instance (QAArrow to) => CanDiv (CauchyRealA to) Int where
  type DivType (CauchyRealA to) Int = CauchyRealA to
  divide = binaryOpWithPureArg "/" divide divGetInitQ1T

instance (QAArrow to) => CanDiv Int (CauchyRealA to) where
  type DivType Int (CauchyRealA to) = CauchyRealA to
  divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL

instance (QAArrow to) => CanDiv (CauchyRealA to) Dyadic where
  type DivType (CauchyRealA to) Dyadic = CauchyRealA to
  divide = binaryOpWithPureArg "/" divide divGetInitQ1T

instance (QAArrow to) => CanDiv Dyadic (CauchyRealA to) where
  type DivType Dyadic (CauchyRealA to) = CauchyRealA to
  divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL

instance (QAArrow to) => CanDiv (CauchyRealA to) Rational where
  type DivType (CauchyRealA to) Rational = CauchyRealA to
  divide = binaryOpWithPureArg "/" divide divGetInitQ1T

instance (QAArrow to) => CanDiv Rational (CauchyRealA to) where
  type DivType Rational (CauchyRealA to) = CauchyRealA to
  divide = flip $ binaryOpWithPureArg "/" (flip divide) divGetInitQ1TL

instance CanDiv CauchyReal MPBall where
  type DivType CauchyReal MPBall = MPBall
  divide = binaryWithBall divide

instance CanDiv MPBall CauchyReal where
  type DivType MPBall CauchyReal = MPBall
  divide = flip $ binaryWithBall (flip divide)


{- integer power -}

instance (QAArrow to) => CanPow (CauchyRealA to) Integer where
  pow = powUsingMulRecip

instance (QAArrow to) => CanPow (CauchyRealA to) Int where
  pow = powUsingMulRecip
