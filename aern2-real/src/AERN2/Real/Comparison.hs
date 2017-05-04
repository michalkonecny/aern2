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
  CauchyBoolP(..), CauchyBoolA, CauchyBool
  , CauchyRealAtAccuracy(..)
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

{- "Cauchy" Boolean -}

data CauchyBoolP = CauchyBoolP deriving Show

pCB :: CauchyBoolP
pCB = CauchyBoolP

instance QAProtocol CauchyBoolP where
  type Q CauchyBoolP = AccuracySG
  type A CauchyBoolP = Maybe Bool

instance QAProtocolCacheable CauchyBoolP where
  type QACache CauchyBoolP = Maybe Bool
  newQACache _ = Nothing
  lookupQACache _ cache _ac = Just cache
  updateQACache _ Nothing _ mb = mb
  updateQACache _ (Just b) _ _mb = Just b

type CauchyBoolA to = QA to CauchyBoolP
type CauchyBool = CauchyBoolA (->)

instance (QAArrow to) => ConvertibleExactly Bool (CauchyBoolA to) where
  safeConvertExactly b = Right $
    newQA (show b) [] pCB (accuracySG $ bits 0) $
      proc _ -> returnA -< Just b

instance (QAArrow to) => CanNeg (CauchyBoolA to) where
  type NegType (CauchyBoolA to) = CauchyBoolA to
  negate qa =
    newQA "neg" [AnyProtocolQA qa] pCB (accuracySG $ bits 0) (qaMakeQuery qa >>> arr negate)

instance (QAArrow to) => CanAndOrAsymmetric (CauchyBoolA to) (CauchyBoolA to) where
  type AndOrType (CauchyBoolA to) (CauchyBoolA to) = CauchyBoolA to
  and2 qa1 qa2 =
    newQA "and" [AnyProtocolQA qa1, AnyProtocolQA qa2] pCB (accuracySG $ bits 0) $
      proc ac ->
        do
        b1 <- qaMakeQuery qa1 -< ac
        b2 <- qaMakeQuery qa2 -< ac
        returnA -< b1 `and2` b2
  or2 qa1 qa2 =
    newQA "or" [AnyProtocolQA qa1, AnyProtocolQA qa2] pCB (accuracySG $ bits 0) $
      proc ac ->
        do
        b1 <- qaMakeQuery qa1 -< ac
        b2 <- qaMakeQuery qa2 -< ac
        returnA -< b1 `or2` b2


{- equality & order -}

instance (QAArrow to) => HasEqAsymmetric (CauchyRealA to) (CauchyRealA to) where
  type EqCompareType (CauchyRealA to) (CauchyRealA to) = CauchyBoolA to
  equalTo = liftRel "==" (==)
  notEqualTo = liftRel "/=" (/=)

instance (QAArrow to) => HasOrderAsymmetric (CauchyRealA to) (CauchyRealA to) where
  type OrderCompareType (CauchyRealA to) (CauchyRealA to) = CauchyBoolA to
  lessThan = liftRel "<" (<)
  leq = liftRel "<=" (<=)
  greaterThan = liftRel ">" (>)
  geq = liftRel ">=" (>=)

liftRel ::
  (QAArrow to) =>
  String ->
  (MPBall -> MPBall -> Maybe Bool) ->
  (CauchyRealA to -> CauchyRealA to -> CauchyBoolA to)
liftRel relName rel a b =
  newQA relName [AnyProtocolQA a, AnyProtocolQA b] pCB (accuracySG $ bits 0) $
    proc ac ->
      do
      b1 <- realWithAccuracy a -< ac
      b2 <- realWithAccuracy b -< ac
      returnA -< rel b1 b2

{-| CauchyRealAtAccuracy exists only so that we can QuickCheck that
   CauchyReal satisfies properties whose statement relies on an instance of HasEqCertainly.
   CauchyReal is not an instance but CauchyRealAtAccuracy is.
-}
data CauchyRealAtAccuracy = CauchyRealAtAccuracy CauchyReal AccuracySG
  deriving (Show)

instance HasEqAsymmetric CauchyRealAtAccuracy CauchyRealAtAccuracy where
  type EqCompareType CauchyRealAtAccuracy CauchyRealAtAccuracy = Maybe Bool
  equalTo = deliftRel (==)

instance HasOrderAsymmetric CauchyRealAtAccuracy CauchyRealAtAccuracy where
  type OrderCompareType CauchyRealAtAccuracy CauchyRealAtAccuracy = Maybe Bool
  lessThan = deliftRel (<)
  leq = deliftRel (<=)

deliftRel ::
  (CauchyReal -> CauchyReal -> CauchyBool) ->
  CauchyRealAtAccuracy -> CauchyRealAtAccuracy -> Maybe Bool
deliftRel rel (CauchyRealAtAccuracy x1 ac1) (CauchyRealAtAccuracy x2 ac2) =
  qaMakeQuery (rel x1 x2) (max ac1 ac2)

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
