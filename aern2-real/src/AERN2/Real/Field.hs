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

import AERN2.QA
import AERN2.Real.Type
import AERN2.Real.Aux

{- negation -}

instance (ArrowChoice to) => CanNeg (CauchyRealA to) where
  negate = unaryOp "-" negate (getInitQ1FromSimple $ proc q -> returnA -< q)

{- addition -}

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) (CauchyRealA to) where
  add = binaryOp "+" add (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) Integer where
  type AddType (CauchyRealA to) Integer = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (ArrowChoice to) => CanAddAsymmetric Integer (CauchyRealA to) where
  type AddType Integer (CauchyRealA to) = CauchyRealA to
  add = flip add

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) Int where
  type AddType (CauchyRealA to) Int = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (ArrowChoice to) => CanAddAsymmetric Int (CauchyRealA to) where
  type AddType Int (CauchyRealA to) = CauchyRealA to
  add = flip add

instance (ArrowChoice to) => CanAddAsymmetric (CauchyRealA to) Rational where
  type AddType (CauchyRealA to) Rational = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (ArrowChoice to) => CanAddAsymmetric Rational (CauchyRealA to) where
  type AddType Rational (CauchyRealA to) = CauchyRealA to
  add = flip add

{- MPBall + CauchyReal = MPBall, only allowed in the (->) arrow  -}

mpBallSimilarTo :: MPBall -> CauchyReal -> MPBall
mpBallSimilarTo b r =
  qaMakeQuery r $ getAccuracyIfExactUsePrec b

getAccuracyIfExactUsePrec :: MPBall -> Accuracy
getAccuracyIfExactUsePrec ball =
  case getAccuracy ball of
    Exact -> bits (getPrecision ball)
    result -> result

instance CanAddAsymmetric CauchyReal MPBall where
  type AddType CauchyReal MPBall = MPBall
  add r b = add (mpBallSimilarTo b r) b

instance CanAddAsymmetric MPBall CauchyReal where
  type AddType MPBall CauchyReal = MPBall
  add = flip add

{- subtraction -}

instance (ArrowChoice to) => CanSub (CauchyRealA to) (CauchyRealA to) where
  sub = binaryOp "-" sub (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance (ArrowChoice to) => CanSub (CauchyRealA to) Integer
instance (ArrowChoice to) => CanSub Integer (CauchyRealA to)
instance (ArrowChoice to) => CanSub (CauchyRealA to) Int
instance (ArrowChoice to) => CanSub Int (CauchyRealA to)
instance (ArrowChoice to) => CanSub (CauchyRealA to) Rational
instance (ArrowChoice to) => CanSub Rational (CauchyRealA to)
instance CanSub CauchyReal MPBall
instance CanSub MPBall CauchyReal

{- multiplication -}

instance (ArrowChoice to) => CanMulAsymmetric (CauchyRealA to) (CauchyRealA to) where
  mul =
    binaryOp "*" mul getInitQ1Q2
    where
    getInitQ1Q2 a1 a2 =
      proc q ->
        do
        (a1NormLog, b1) <- getCRFnNormLog a1 id -< q
        let jInit2 = case a1NormLog of
                NormBits a1NL -> max (bits 0) (q + a1NL)
                NormZero -> bits 0
        -- favouring 2*x over x*2 in a Num instance
        (a2NormLog, b2) <- getCRFnNormLog a2 id -< jInit2
        let jInit1 = case a2NormLog of
                NormBits a2NL -> max (bits 0) (q + a2NL)
                NormZero -> bits 0
        returnA -< ((jInit1, Just b1), (jInit2, Just b2))

mulGetInitQ1T ::
  (Arrow to, HasNorm t)
  =>
  r -> t -> Accuracy `to` (Accuracy, Maybe MPBall)
mulGetInitQ1T _a1 n =
  proc q ->
    do
    let jInit1 = case nNormLog of
            NormBits nNL -> max (bits 0) (q + nNL)
            NormZero -> bits 0
    returnA -< (jInit1, Nothing)
  where
  nNormLog = getNormLog n

instance (ArrowChoice to) => CanMulAsymmetric (CauchyRealA to) Integer where
  type MulType (CauchyRealA to) Integer = CauchyRealA to
  mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

instance (ArrowChoice to) => CanMulAsymmetric Integer (CauchyRealA to) where
  type MulType Integer (CauchyRealA to) = CauchyRealA to
  mul = flip mul

instance (ArrowChoice to) => CanMulAsymmetric (CauchyRealA to) Int where
  type MulType (CauchyRealA to) Int = CauchyRealA to
  mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

instance (ArrowChoice to) => CanMulAsymmetric Int (CauchyRealA to) where
  type MulType Int (CauchyRealA to) = CauchyRealA to
  mul = flip mul

instance (ArrowChoice to) => CanMulAsymmetric (CauchyRealA to) Rational where
  type MulType (CauchyRealA to) Rational = CauchyRealA to
  mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

instance (ArrowChoice to) => CanMulAsymmetric Rational (CauchyRealA to) where
  type MulType Rational (CauchyRealA to) = CauchyRealA to
  mul = flip mul

instance CanMulAsymmetric CauchyReal MPBall where
  type MulType CauchyReal MPBall = MPBall
  mul r b = mul (mpBallSimilarTo b r) b

instance CanMulAsymmetric MPBall CauchyReal where
  type MulType MPBall CauchyReal = MPBall
  mul = flip mul
