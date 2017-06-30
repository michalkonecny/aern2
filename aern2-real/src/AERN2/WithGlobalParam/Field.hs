{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.WithGlobalParam.Field
    Description :  field operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on WithGlobalParam objects.
-}
module AERN2.WithGlobalParam.Field
(
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import Control.Arrow

import Control.CollectErrors

import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.WithGlobalParam.Type
import AERN2.WithGlobalParam.Helpers
-- import AERN2.WithGlobalParam.Ring

{- division -}

instance
  (QAArrow to, CanDiv a b
  , SuitableForWGParam prm  a, SuitableForWGParam prm  b
  , SuitableForWGParam prm  (DivType a b), SuitableForWGParam prm  (DivTypeNoCN a b))
  =>
  CanDiv (WithGlobalParamA to prm a) (WithGlobalParamA to prm b)
  where
  type DivType (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (DivType a b)
  divide = binaryOp "/" divide
  type DivTypeNoCN (WithGlobalParamA to prm a) (WithGlobalParamA to prm b) = WithGlobalParamA to prm (DivTypeNoCN a b)
  divideNoCN = binaryOp "/" divideNoCN

instance
  (CanDiv (WithGlobalParamA to prm a) b
  , CanEnsureCE es (DivType (WithGlobalParamA to prm a) b)
  , CanEnsureCE es (DivTypeNoCN (WithGlobalParamA to prm a) b)
  , SuitableForCE es)
  =>
  CanDiv (WithGlobalParamA to prm a) (CollectErrors es  b)
  where
  type DivType (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (DivType (WithGlobalParamA to prm a) b)
  divide = lift2TLCE divide
  type DivTypeNoCN (WithGlobalParamA to prm a) (CollectErrors es  b) =
    EnsureCE es (DivTypeNoCN (WithGlobalParamA to prm a) b)
  divideNoCN = lift2TLCE divideNoCN

instance
  (CanDiv a (WithGlobalParamA to prm b)
  , CanEnsureCE es (DivType a (WithGlobalParamA to prm b))
  , CanEnsureCE es (DivTypeNoCN a (WithGlobalParamA to prm b))
  , SuitableForCE es)
  =>
  CanDiv (CollectErrors es a) (WithGlobalParamA to prm b)
  where
  type DivType (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (DivType a (WithGlobalParamA to prm b))
  divide = lift2TCE divide
  type DivTypeNoCN (CollectErrors es  a) (WithGlobalParamA to prm b) =
    EnsureCE es (DivTypeNoCN a (WithGlobalParamA to prm b))
  divideNoCN = lift2TCE divideNoCN

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanDiv a $t, SuitableForWGParam prm  a
    , SuitableForWGParam prm  (DivType a $t), SuitableForWGParam prm  (DivTypeNoCN a $t))
    =>
    CanDiv (WithGlobalParamA to prm a) $t
    where
    type DivType (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (DivType a $t)
    divide = binaryOpWithPureArg "/" divide
    type DivTypeNoCN (WithGlobalParamA to prm a) $t = WithGlobalParamA to prm (DivTypeNoCN a $t)
    divideNoCN = binaryOpWithPureArg "/" divideNoCN

  instance
    (QAArrow to, CanDiv $t b, SuitableForWGParam prm  b
    , SuitableForWGParam prm  (DivType $t b)
    , SuitableForWGParam prm  (DivTypeNoCN $t b))
    =>
    CanDiv $t (WithGlobalParamA to prm b)
    where
    type DivType $t (WithGlobalParamA to prm b) = WithGlobalParamA to prm (DivType $t b)
    divide = flip $ binaryOpWithPureArg "/" (flip divide)
    type DivTypeNoCN $t (WithGlobalParamA to prm b) = WithGlobalParamA to prm (DivTypeNoCN $t b)
    divideNoCN = flip $ binaryOpWithPureArg "/" (flip divideNoCN)

  |]))
