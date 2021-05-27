{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.WithGlobalParam.Comparison
    Description :  comparison operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Comparison operations on WithGlobalParam objects.
-}
module AERN2.WithGlobalParam.Comparison
(
)
where

import MixedTypesNumPrelude hiding (id)
-- import qualified Prelude as P

-- import Control.Category (id)
import Control.Arrow

import Control.CollectErrors

-- import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA.Protocol

import AERN2.WithGlobalParam.Type
import AERN2.WithGlobalParam.Helpers

{- Boolean ops -}

instance (QAArrow to, HasBools b, SuitableForWGParam prm b) => ConvertibleExactly Bool (WithGlobalParamA to prm b) where
  safeConvertExactly bool =
    do
    b <- safeConvertExactly bool
    Right $ newWGParam Nothing b (show b) [] $ \_me_src -> arr $ const b

instance
  (QAArrow to, CanNeg a, SuitableForWGParam prm a, SuitableForWGParam prm (NegType a))
  =>
  CanNeg (WithGlobalParamA to prm a)
  where
  type NegType (WithGlobalParamA to prm a) = WithGlobalParamA to prm (NegType a)
  negate = unaryOp "neg" negate

instance
  (QAArrow to, CanAndOrAsymmetric a b
  , SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm (AndOrType a b))
  =>
  CanAndOrAsymmetric (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type AndOrType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (AndOrType a b)
  and2 = binaryOp "and" and2
  or2 = binaryOp "or" or2

{- equality & order -}

instance
  (QAArrow to, HasEqAsymmetric a b
  , SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm (EqCompareType a b))
  =>
  HasEqAsymmetric (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type EqCompareType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (EqCompareType a b)
  equalTo = binaryOp "==" (==)
  notEqualTo = binaryOp "/=" (/=)

instance
  (QAArrow to, HasOrderAsymmetric a b
  , SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm (OrderCompareType a b))
  =>
  HasOrderAsymmetric (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type OrderCompareType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (OrderCompareType a b)
  lessThan = binaryOp "<" (<)
  leq = binaryOp "<=" (<=)
  greaterThan = binaryOp ">" (>)
  geq = binaryOp ">=" (>=)

{- comparing CollectErrors and WithGlobalParams -}

instance
  (HasEqAsymmetric (WithGlobalParamA to prm a) b
  , CanEnsureCE es b
  , CanEnsureCE es (EqCompareType (WithGlobalParamA to prm a) b)
  , IsBool (EnsureCE es (EqCompareType (WithGlobalParamA to prm a) b))
  , CanBeErrors es)
  =>
  HasEqAsymmetric (WithGlobalParamA to prm a) (CollectErrors es b)
  where
  type EqCompareType (WithGlobalParamA to prm a) (CollectErrors es b) =
    EnsureCE es (EqCompareType (WithGlobalParamA to prm a) b)
  equalTo = lift2TLCE equalTo

instance
  (HasEqAsymmetric a (WithGlobalParamA to prm b)
  , CanEnsureCE es a
  , CanEnsureCE es (EqCompareType a (WithGlobalParamA to prm b))
  , IsBool (EnsureCE es (EqCompareType a (WithGlobalParamA to prm b)))
  , CanBeErrors es)
  =>
  HasEqAsymmetric (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type EqCompareType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (EqCompareType a (WithGlobalParamA to prm b))
  equalTo = lift2TCE equalTo

instance
  (HasOrderAsymmetric (WithGlobalParamA to prm a) b
  , CanEnsureCE es b
  , CanEnsureCE es (OrderCompareType (WithGlobalParamA to prm a) b)
  , IsBool (EnsureCE es (OrderCompareType (WithGlobalParamA to prm a) b))
  , CanBeErrors es)
  =>
  HasOrderAsymmetric (WithGlobalParamA to prm a) (CollectErrors es  b)
  where
  type OrderCompareType (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (OrderCompareType (WithGlobalParamA to prm a) b)
  lessThan = lift2TLCE lessThan
  leq = lift2TLCE leq
  greaterThan = lift2TLCE greaterThan
  geq = lift2TLCE geq

instance
  (HasOrderAsymmetric a (WithGlobalParamA to prm b)
  , CanEnsureCE es a
  , CanEnsureCE es (OrderCompareType a (WithGlobalParamA to prm b))
  , IsBool (EnsureCE es (OrderCompareType a (WithGlobalParamA to prm b)))
  , CanBeErrors es)
  =>
  HasOrderAsymmetric (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type OrderCompareType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (OrderCompareType a (WithGlobalParamA to prm b))
  lessThan = lift2TCE lessThan
  leq = lift2TCE leq
  greaterThan = lift2TCE greaterThan
  geq = lift2TCE geq

{- abs -}

instance
  (QAArrow to, CanAbs a, SuitableForWGParam prm a, SuitableForWGParam prm (AbsType a))
  =>
  CanAbs (WithGlobalParamA to prm a)
  where
  type AbsType (WithGlobalParamA to prm a) = WithGlobalParamA to prm (AbsType a)
  abs = unaryOp "abs" abs

{- min/max -}

instance
  (QAArrow to
  , CanMinMaxAsymmetric a b, SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm (MinMaxType a b))
  =>
  CanMinMaxAsymmetric (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type MinMaxType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (MinMaxType a b)
  min = binaryOp "min" min
  max = binaryOp "max" max

instance
  (CanMinMaxAsymmetric (WithGlobalParamA to prm a) b
  , CanEnsureCE es b
  , CanEnsureCE es (MinMaxType (WithGlobalParamA to prm a) b)
  , CanBeErrors es)
  =>
  CanMinMaxAsymmetric (WithGlobalParamA to prm a) (CollectErrors es  b)
  where
  type MinMaxType (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (MinMaxType (WithGlobalParamA to prm a) b)
  min = lift2TLCE min
  max = lift2TLCE max

instance
  (CanMinMaxAsymmetric a (WithGlobalParamA to prm b)
  , CanEnsureCE es a
  , CanEnsureCE es (MinMaxType a (WithGlobalParamA to prm b))
  , CanBeErrors es)
  =>
  CanMinMaxAsymmetric (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type MinMaxType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (MinMaxType a (WithGlobalParamA to prm b))
  min = lift2TCE min
  max = lift2TCE max

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

    instance
      (QAArrow to
      , CanMinMaxAsymmetric a $t, SuitableForWGParam prm a, SuitableForWGParam prm (MinMaxType a $t))
      =>
      CanMinMaxAsymmetric (WithGlobalParamA to prm a) $t
      where
      type MinMaxType (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (MinMaxType a $t)
      min = binaryOpWithPureArg "min" min
      max = binaryOpWithPureArg "max" max

    instance
      (QAArrow to
      , CanMinMaxAsymmetric $t b, SuitableForWGParam prm b, SuitableForWGParam prm (MinMaxType $t b))
      =>
      CanMinMaxAsymmetric $t (WithGlobalParamA to prm b)
      where
      type MinMaxType $t (WithGlobalParamA to prm b) = WithGlobalParamA to prm (MinMaxType $t b)
      min = flip $ binaryOpWithPureArg "min" (flip min)
      max = flip $ binaryOpWithPureArg "max" (flip max)

    instance
      (QAArrow to, HasEqAsymmetric a $t
      , SuitableForWGParam prm a, SuitableForWGParam prm (EqCompareType a $t))
      =>
      HasEqAsymmetric (WithGlobalParamA to prm a) $t
      where
      type EqCompareType (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (EqCompareType a $t)
      equalTo = binaryOpWithPureArg "==" (==)
      notEqualTo = binaryOpWithPureArg "/=" (/=)

    instance
      (QAArrow to, HasEqAsymmetric $t a
      , SuitableForWGParam prm a, SuitableForWGParam prm (EqCompareType $t a))
      =>
      HasEqAsymmetric $t (WithGlobalParamA to prm a)
      where
      type EqCompareType $t (WithGlobalParamA to prm a) = WithGlobalParamA to prm (EqCompareType $t a)
      equalTo = flip $ binaryOpWithPureArg "==" (flip (==))
      notEqualTo = flip $ binaryOpWithPureArg "/=" (flip (/=))

    instance
      (QAArrow to, HasOrderAsymmetric a $t
      , SuitableForWGParam prm a, SuitableForWGParam prm (OrderCompareType a $t))
      =>
      HasOrderAsymmetric (WithGlobalParamA to prm a) $t
      where
      type OrderCompareType (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (OrderCompareType a $t)
      lessThan = binaryOpWithPureArg "<" (<)
      leq = binaryOpWithPureArg "<=" (<=)
      greaterThan = binaryOpWithPureArg ">" (>)
      geq = binaryOpWithPureArg ">=" (>=)

    instance
      (QAArrow to, HasOrderAsymmetric $t a
      , SuitableForWGParam prm a, SuitableForWGParam prm (OrderCompareType $t a))
      =>
      HasOrderAsymmetric $t (WithGlobalParamA to prm a)
      where
      type OrderCompareType $t (WithGlobalParamA to prm a) = WithGlobalParamA to prm (OrderCompareType $t a)
      lessThan = flip $ binaryOpWithPureArg "<" (flip (<))
      leq = flip $ binaryOpWithPureArg "<=" (flip (<=))
      greaterThan = flip $ binaryOpWithPureArg ">" (flip (>))
      geq = flip $ binaryOpWithPureArg ">=" (flip (>=))

  |]))
