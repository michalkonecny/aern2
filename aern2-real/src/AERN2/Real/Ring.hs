{-|
    Module      :  AERN2.Real.Ring
    Description :  ring operations on CR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Ring operations on Cauchy Real numbers.
-}
module AERN2.Real.Ring
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
import AERN2.Real.Aux

{- negation -}

instance (QAArrow to) => CanNeg (CauchyRealA to) where
  negate = unaryOp "-" negate (getInitQ1FromSimple $ proc q -> returnA -< q)

{- addition -}

instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) (CauchyRealA to) where
  add = binaryOp "+" add (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) Integer where
  type AddType (CauchyRealA to) Integer = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (QAArrow to) => CanAddAsymmetric Integer (CauchyRealA to) where
  type AddType Integer (CauchyRealA to) = CauchyRealA to
  add = flip add

instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) Int where
  type AddType (CauchyRealA to) Int = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (QAArrow to) => CanAddAsymmetric Int (CauchyRealA to) where
  type AddType Int (CauchyRealA to) = CauchyRealA to
  add = flip add

instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) Dyadic where
  type AddType (CauchyRealA to) Dyadic = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (QAArrow to) => CanAddAsymmetric Dyadic (CauchyRealA to) where
  type AddType Dyadic (CauchyRealA to) = CauchyRealA to
  add = flip add

instance (QAArrow to) => CanAddAsymmetric (CauchyRealA to) Rational where
  type AddType (CauchyRealA to) Rational = CauchyRealA to
  add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

instance (QAArrow to) => CanAddAsymmetric Rational (CauchyRealA to) where
  type AddType Rational (CauchyRealA to) = CauchyRealA to
  add = flip add

instance CanAddAsymmetric CauchyReal MPBall where
  type AddType CauchyReal MPBall = MPBall
  add = binaryWithBall add

instance CanAddAsymmetric MPBall CauchyReal where
  type AddType MPBall CauchyReal = MPBall
  add = flip add

{- subtraction -}

instance (QAArrow to) => CanSub (CauchyRealA to) (CauchyRealA to) where
  sub = binaryOp "-" sub (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

instance (QAArrow to) => CanSub (CauchyRealA to) Integer
instance (QAArrow to) => CanSub Integer (CauchyRealA to)
instance (QAArrow to) => CanSub (CauchyRealA to) Int
instance (QAArrow to) => CanSub Int (CauchyRealA to)
instance (QAArrow to) => CanSub (CauchyRealA to) Dyadic
instance (QAArrow to) => CanSub Dyadic (CauchyRealA to)
instance (QAArrow to) => CanSub (CauchyRealA to) Rational
instance (QAArrow to) => CanSub Rational (CauchyRealA to)
instance CanSub CauchyReal MPBall
instance CanSub MPBall CauchyReal

{- multiplication -}

instance (QAArrow to) => CanMulAsymmetric (CauchyRealA to) (CauchyRealA to) where
  mul =
    binaryOp "*" mul getInitQ1Q2
    where
    getInitQ1Q2 a1 a2 =
      proc q ->
        do
        (a1NormLog, b1) <- getCRFnNormLog a1 id -< q
        let jInit2 = case a1NormLog of
                NormBits a1NL -> max acSG0 (q + a1NL)
                NormZero -> acSG0
        -- favouring 2*x over x*2 in a Num instance
        (a2NormLog, b2) <- getCRFnNormLog a2 id -< jInit2
        let jInit1 = case a2NormLog of
                NormBits a2NL -> max acSG0 (q + a2NL)
                NormZero -> acSG0
        returnA -< ((jInit1, Just b1), (jInit2, Just b2))

mulGetInitQ1T ::
  (Arrow to, HasNorm t)
  =>
  r -> t -> AccuracySG `to` (AccuracySG, Maybe MPBall)
mulGetInitQ1T _a1 n =
  proc q ->
    do
    let jInit1 = case nNormLog of
            NormBits nNL -> max acSG0 (q + nNL)
            NormZero -> acSG0
    returnA -< (jInit1, Nothing)
  where
  nNormLog = getNormLog n

instance (QAArrow to) => CanMulAsymmetric (CauchyRealA to) Integer where
  type MulType (CauchyRealA to) Integer = CauchyRealA to
  mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

instance (QAArrow to) => CanMulAsymmetric Integer (CauchyRealA to) where
  type MulType Integer (CauchyRealA to) = CauchyRealA to
  mul = flip mul

instance (QAArrow to) => CanMulAsymmetric (CauchyRealA to) Int where
  type MulType (CauchyRealA to) Int = CauchyRealA to
  mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

instance (QAArrow to) => CanMulAsymmetric Int (CauchyRealA to) where
  type MulType Int (CauchyRealA to) = CauchyRealA to
  mul = flip mul

instance (QAArrow to) => CanMulAsymmetric (CauchyRealA to) Dyadic where
  type MulType (CauchyRealA to) Dyadic = CauchyRealA to
  mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

instance (QAArrow to) => CanMulAsymmetric Dyadic (CauchyRealA to) where
  type MulType Dyadic (CauchyRealA to) = CauchyRealA to
  mul = flip mul

instance (QAArrow to) => CanMulAsymmetric (CauchyRealA to) Rational where
  type MulType (CauchyRealA to) Rational = CauchyRealA to
  mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

instance (QAArrow to) => CanMulAsymmetric Rational (CauchyRealA to) where
  type MulType Rational (CauchyRealA to) = CauchyRealA to
  mul = flip mul

instance CanMulAsymmetric CauchyReal MPBall where
  type MulType CauchyReal MPBall = MPBall
  mul = binaryWithBall mul

instance CanMulAsymmetric MPBall CauchyReal where
  type MulType MPBall CauchyReal = MPBall
  mul = flip mul
