{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.WithGlobalParam.Elementary
    Description :  elementary functions on sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Elementary functions on fast converging sequences.
-}
module AERN2.WithGlobalParam.Elementary
()
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import Control.Arrow

import Control.CollectErrors

import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.WithGlobalParam.Type
import AERN2.WithGlobalParam.Helpers
import AERN2.WithGlobalParam.Ring ()
import AERN2.WithGlobalParam.Field ()

{- exp -}

instance
  (QAArrow to, CanExp a
  , SuitableForWGParam prm  a, SuitableForWGParam prm  (ExpType a))
  =>
  CanExp (WithGlobalParamA to prm a)
  where
  type ExpType (WithGlobalParamA to prm a) = WithGlobalParamA to prm (ExpType a)
  exp = unaryOp "exp" exp

{- log -}

instance
  (QAArrow to, CanLog a
  , SuitableForWGParam prm  a, SuitableForWGParam prm  (LogType a))
  =>
  CanLog (WithGlobalParamA to prm a)
  where
  type LogType (WithGlobalParamA to prm a) = WithGlobalParamA to prm (LogType a)
  log = unaryOp "log" log

{- power -}

instance
  (QAArrow to, CanPow a e
  , SuitableForWGParam prm  a, SuitableForWGParam prm  e
  , SuitableForWGParam prm  (PowTypeNoCN a e)
  , SuitableForWGParam prm  (PowType a e))
  =>
  CanPow (WithGlobalParamA to prm a) (WithGlobalParamA to prm e)
  where
  type PowTypeNoCN (WithGlobalParamA to prm a) (WithGlobalParamA to prm e) = WithGlobalParamA to prm (PowTypeNoCN a e)
  powNoCN = binaryOp "^!" powNoCN
  type PowType (WithGlobalParamA to prm a) (WithGlobalParamA to prm e) = WithGlobalParamA to prm (PowType a e)
  pow = binaryOp "^" pow

instance
  (CanPow (WithGlobalParamA to prm a) b
  , CanEnsureCE es b
  , CanEnsureCE es (PowTypeNoCN (WithGlobalParamA to prm a) b)
  , CanEnsureCE es (PowType (WithGlobalParamA to prm a) b)
  , CanBeErrors es)
  =>
  CanPow (WithGlobalParamA to prm a) (CollectErrors es  b)
  where
  type PowTypeNoCN (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (PowTypeNoCN (WithGlobalParamA to prm a) b)
  powNoCN = lift2TLCE powNoCN
  type PowType (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (PowType (WithGlobalParamA to prm a) b)
  pow = lift2TLCE pow

instance
  (CanPow a (WithGlobalParamA to prm b)
  , CanEnsureCE es a
  , CanEnsureCE es (PowType a (WithGlobalParamA to prm b))
  , CanEnsureCE es (PowTypeNoCN a (WithGlobalParamA to prm b))
  , CanBeErrors es)
  =>
  CanPow (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type PowTypeNoCN (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (PowTypeNoCN a (WithGlobalParamA to prm b))
  powNoCN = lift2TCE powNoCN
  type PowType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (PowType a (WithGlobalParamA to prm b))
  pow = lift2TCE pow

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Dyadic |], [t| Rational |]]
  (\ t -> [d|

    instance
      (QAArrow to, CanPow a $t
      , SuitableForWGParam prm  a
      , SuitableForWGParam prm  (PowTypeNoCN a $t)
      , SuitableForWGParam prm  (PowType a $t))
      =>
      CanPow (WithGlobalParamA to prm a) $t where
      type PowTypeNoCN (WithGlobalParamA to prm a) $t =
        WithGlobalParamA to prm (PowTypeNoCN a $t)
      powNoCN = binaryOpWithPureArg "^" powNoCN
      type PowType (WithGlobalParamA to prm a) $t =
        WithGlobalParamA to prm (PowType a $t)
      pow = binaryOpWithPureArg "^" pow

    instance
      (QAArrow to, CanPow $t a
      , SuitableForWGParam prm  a
      , SuitableForWGParam prm  (PowTypeNoCN $t a)
      , SuitableForWGParam prm  (PowType $t a))
      =>
      CanPow $t (WithGlobalParamA to prm a) where
      type PowTypeNoCN $t (WithGlobalParamA to prm a) =
        WithGlobalParamA to prm (PowTypeNoCN $t a)
      powNoCN = flip $ binaryOpWithPureArg "^" (flip powNoCN)
      type PowType $t (WithGlobalParamA to prm a) =
        WithGlobalParamA to prm (PowType $t a)
      pow = flip $ binaryOpWithPureArg "^" (flip pow)

  |]))

{- sqrt -}

instance
  (QAArrow to, CanSqrt a
  , CanMinMaxThis a Integer
  , SuitableForWGParam prm  a, SuitableForWGParam prm  (SqrtType a))
  =>
  CanSqrt (WithGlobalParamA to prm a)
  where
  type SqrtType (WithGlobalParamA to prm a) = WithGlobalParamA to prm (SqrtType a)
  sqrt = unaryOp "sqrt" sqrt

{- sine, cosine -}

instance
  (QAArrow to, CanSinCos a
  , SuitableForWGParam prm  a, SuitableForWGParam prm  (SinCosType a))
  =>
  CanSinCos (WithGlobalParamA to prm a)
  where
  type SinCosType (WithGlobalParamA to prm a) = WithGlobalParamA to prm (SinCosType a)
  cos = unaryOp "cos" cos
  sin = unaryOp "sin" sin
