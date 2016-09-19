{-|
    Module      :  AERN2.Real.Comparison
    Description :  comparison operations on CR
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Comparison operations on Cauchy Real numbers.
-}
module AERN2.Real.Comparison
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
import AERN2.Real.Type
import AERN2.Real.Aux

-- import AERN2.Tolerant

{-

{- tolerant comparisons -}

instance (CanNegSameType b, Arrow to) => CanNeg (() `to` b) where
  negate tob = arr negate <<< tob

instance (QAArrow to) => HasTolerantEqAsymmetric (CauchyRealA to) (CauchyRealA to) where
  type TolerantEqCompareType (CauchyRealA to) (CauchyRealA to) = () `to` (Tolerant Bool)
  tolerantEqualTo a (e,b) =
    proc () ->
      do
      aB <- qaMakeQueryA -< (a, ac)
      bB <- qaMakeQueryA -< (b, ac)
      returnA -< tolerantEqualTo aB (e,bB)
    where
    ac =
      case getNormLog (dyadic e) of
        NormBits n -> bits (max 0 (1 - n))
        NormZero -> Exact
-}

{- abs -}

instance (QAArrow to) => CanAbs (CauchyRealA to) where
  abs = unaryOp "abs" abs (getInitQ1FromSimple id)

{- min/max -}

instance (QAArrow to) => CanMinMaxAsymmetric (CauchyRealA to) (CauchyRealA to) where
  min = binaryOp "min" min (getInitQ1Q2FromSimple $ arr $ \q -> (q,q))
  max = binaryOp "max" max (getInitQ1Q2FromSimple $ arr $ \q -> (q,q))

instance (QAArrow to) => CanMinMaxAsymmetric (CauchyRealA to) Integer where
  type MinMaxType (CauchyRealA to) Integer = CauchyRealA to
  min = binaryOpWithPureArg "min" min (getInitQ1TFromSimple id)
  max = binaryOpWithPureArg "max" max (getInitQ1TFromSimple id)

instance (QAArrow to) => CanMinMaxAsymmetric Integer (CauchyRealA to) where
  type MinMaxType Integer (CauchyRealA to) = CauchyRealA to
  min = flip min
  max = flip max

instance (QAArrow to) => CanMinMaxAsymmetric (CauchyRealA to) Int where
  type MinMaxType (CauchyRealA to) Int = CauchyRealA to
  min = binaryOpWithPureArg "min" min (getInitQ1TFromSimple id)
  max = binaryOpWithPureArg "max" max (getInitQ1TFromSimple id)

instance (QAArrow to) => CanMinMaxAsymmetric Int (CauchyRealA to) where
  type MinMaxType Int (CauchyRealA to) = CauchyRealA to
  min = flip min
  max = flip max

instance (QAArrow to) => CanMinMaxAsymmetric (CauchyRealA to) Dyadic where
  type MinMaxType (CauchyRealA to) Dyadic = CauchyRealA to
  min = binaryOpWithPureArg "min" min (getInitQ1TFromSimple id)
  max = binaryOpWithPureArg "max" max (getInitQ1TFromSimple id)

instance (QAArrow to) => CanMinMaxAsymmetric Dyadic (CauchyRealA to) where
  type MinMaxType Dyadic (CauchyRealA to) = CauchyRealA to
  min = flip min
  max = flip max

instance (QAArrow to) => CanMinMaxAsymmetric (CauchyRealA to) Rational where
  type MinMaxType (CauchyRealA to) Rational = CauchyRealA to
  min = binaryOpWithPureArg "min" min (getInitQ1TFromSimple id)
  max = binaryOpWithPureArg "max" max (getInitQ1TFromSimple id)

instance (QAArrow to) => CanMinMaxAsymmetric Rational (CauchyRealA to) where
  type MinMaxType Rational (CauchyRealA to) = CauchyRealA to
  min = flip min
  max = flip max

instance CanMinMaxAsymmetric CauchyReal MPBall where
  type MinMaxType CauchyReal MPBall = MPBall
  min = binaryWithBall min
  max = binaryWithBall max

instance CanMinMaxAsymmetric MPBall CauchyReal where
  type MinMaxType MPBall CauchyReal = MPBall
  min = flip min
  max = flip max
