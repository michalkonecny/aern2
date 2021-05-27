{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.WithGlobalParam.Ring
    Description :  ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Ring operations on WithGlobalParam objects.
-}
module AERN2.WithGlobalParam.Ring
(
)
where

import MixedTypesNumPrelude hiding (id)
-- import qualified Prelude as P

-- import Control.Arrow

import Control.CollectErrors

import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.WithGlobalParam.Type
import AERN2.WithGlobalParam.Helpers

{- addition -}

instance
  (QAArrow to, CanAddAsymmetric a b, SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm (AddType a b))
  =>
  CanAddAsymmetric (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type AddType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (AddType a b)
  add = binaryOp "+" add

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanAddAsymmetric a $t, SuitableForWGParam prm a, SuitableForWGParam prm (AddType a $t))
    =>
    CanAddAsymmetric (WithGlobalParamA to prm a) $t
    where
    type AddType (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (AddType a $t)
    add = binaryOpWithPureArg "+" add

  instance
    (QAArrow to, CanAddAsymmetric $t b, SuitableForWGParam prm b, SuitableForWGParam prm (AddType $t b))
    =>
    CanAddAsymmetric $t (WithGlobalParamA to prm b)
    where
    type AddType $t (WithGlobalParamA to prm b) = WithGlobalParamA to prm (AddType $t b)
    add = flip $ binaryOpWithPureArg "+" (flip add)

  |]))

instance
  (CanAddAsymmetric (WithGlobalParamA to prm a) b
  , CanEnsureCE es b
  , CanEnsureCE es (AddType (WithGlobalParamA to prm a) b)
  , CanBeErrors es)
  =>
  CanAddAsymmetric (WithGlobalParamA to prm a) (CollectErrors es  b)
  where
  type AddType (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (AddType (WithGlobalParamA to prm a) b)
  add = lift2TLCE add

instance
  (CanAddAsymmetric a (WithGlobalParamA to prm b)
  , CanEnsureCE es a
  , CanEnsureCE es (AddType a (WithGlobalParamA to prm b))
  , CanBeErrors es)
  =>
  CanAddAsymmetric (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type AddType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (AddType a (WithGlobalParamA to prm b))
  add = lift2TCE add


{- subtraction -}

instance
  (QAArrow to, CanSub a b, SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm (SubType a b))
  =>
  CanSub (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type SubType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (SubType a b)
  sub = binaryOp "-" sub


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanSub a $t, SuitableForWGParam prm a, SuitableForWGParam prm (SubType a $t))
    =>
    CanSub (WithGlobalParamA to prm a) $t
    where
    type SubType (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (SubType a $t)
    sub = binaryOpWithPureArg "-" sub

  instance
    (QAArrow to, CanSub $t b, SuitableForWGParam prm b, SuitableForWGParam prm (SubType $t b))
    =>
    CanSub $t (WithGlobalParamA to prm b)
    where
    type SubType $t (WithGlobalParamA to prm b) = WithGlobalParamA to prm (SubType $t b)
    sub = flip $ binaryOpWithPureArg "-" (flip sub)

  |]))

instance
  (CanSub (WithGlobalParamA to prm a) b
  , CanEnsureCE es b
  , CanEnsureCE es (SubType (WithGlobalParamA to prm a) b)
  , CanBeErrors es)
  =>
  CanSub (WithGlobalParamA to prm a) (CollectErrors es  b)
  where
  type SubType (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (SubType (WithGlobalParamA to prm a) b)
  sub = lift2TLCE sub

instance
  (CanSub a (WithGlobalParamA to prm b)
  , CanEnsureCE es a
  , CanEnsureCE es (SubType a (WithGlobalParamA to prm b))
  , CanBeErrors es)
  =>
  CanSub (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type SubType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (SubType a (WithGlobalParamA to prm b))
  sub = lift2TCE sub


{- multiplication -}

instance
  (QAArrow to, CanMulAsymmetric a b
  , SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm (MulType a b))
  =>
  CanMulAsymmetric (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type MulType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (MulType a b)
  mul =
    binaryOp "*" mul

instance
  (CanMulAsymmetric (WithGlobalParamA to prm a) b
  , CanEnsureCE es b
  , CanEnsureCE es (MulType (WithGlobalParamA to prm a) b)
  , CanBeErrors es)
  =>
  CanMulAsymmetric (WithGlobalParamA to prm a) (CollectErrors es  b)
  where
  type MulType (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (MulType (WithGlobalParamA to prm a) b)
  mul = lift2TLCE mul

instance
  (CanMulAsymmetric a (WithGlobalParamA to prm b)
  , CanEnsureCE es a
  , CanEnsureCE es (MulType a (WithGlobalParamA to prm b))
  , CanBeErrors es)
  =>
  CanMulAsymmetric (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type MulType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (MulType a (WithGlobalParamA to prm b))
  mul = lift2TCE mul

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanMulAsymmetric a $t, SuitableForWGParam prm a, SuitableForWGParam prm (MulType a $t))
    =>
    CanMulAsymmetric (WithGlobalParamA to prm a) $t
    where
    type MulType (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (MulType a $t)
    mul = binaryOpWithPureArg "*" mul

  instance
    (QAArrow to, CanMulAsymmetric $t b, SuitableForWGParam prm b, SuitableForWGParam prm (MulType $t b))
    =>
    CanMulAsymmetric $t (WithGlobalParamA to prm b)
    where
    type MulType $t (WithGlobalParamA to prm b) = WithGlobalParamA to prm (MulType $t b)
    mul = flip $ binaryOpWithPureArg "*" (flip mul)

  |]))
