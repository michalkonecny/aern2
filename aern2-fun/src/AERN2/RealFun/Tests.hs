{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.RealFun.Tests
    Description :  Test support for real number function operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Test support for real number function operations
-}

module AERN2.RealFun.Tests
(
  FnAndDescr(..)
  , specFnPointwiseOp1, specFnPointwiseOp2
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (flip const)
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Data.Typeable

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import Numeric.CatchingExceptions

-- import AERN2.Interval

-- import AERN2.MP.Dyadic
import AERN2.MP.Enclosure

import AERN2.RealFun.Operations

data FnAndDescr f = FnAndDescr f String

instance Show f => Show (FnAndDescr f) where
  show (FnAndDescr f descr) =
    show f ++ "[" ++ descr ++ "]"

instance (HasDomain f) => HasDomain (FnAndDescr f) where
  type Domain (FnAndDescr f) = Domain f
  getDomain (FnAndDescr f _) = getDomain f

specFnPointwiseOp2 ::
  ( HasDomain f, CanMapInside (Domain f) x
  , CanApply f x, ApplyType f x ~ v
  , HasEqCertainly v v
  , Arbitrary (FnAndDescr f), ArbitraryWithDom (FnAndDescr f), Show f
  , Arbitrary x, Show x
  ) =>
  (T f) -> (T x) ->
  String ->
  (f -> f -> f) ->
  (CatchingNumExceptions v -> CatchingNumExceptions v -> CatchingNumExceptions v) ->
  (FnAndDescr f -> FnAndDescr f) ->
  (FnAndDescr f -> FnAndDescr f) ->
  Spec
specFnPointwiseOp2
    (T fName :: T f) (T _xName :: T x)
    opName opFn opVal reshapeFn1 reshapeFn2
  =
  it ("pointwise " ++ opName ++ " on " ++ fName ++
      " corresponds to " ++ opName ++ " on values") $ property $
      \ (SameDomFnPair (f1Pre,f2Pre) :: SameDomFnPair (FnAndDescr f)) (xPres :: [x]) ->
          let FnAndDescr f1 _d1 = reshapeFn1 f1Pre in
          let FnAndDescr f2 _d2 = reshapeFn2 f2Pre in
          and $ flip map xPres $ \xPre ->
            let x = mapInside (getDomain f1) xPre in
            let v1 = apply f1 x in
            let v2 = apply f2 x in
            let vrE = opVal (catchingNumExceptions v1) (catchingNumExceptions v2) in
            case _numEXC_maybeValue vrE of
              Just vr | not (hasCertainException vrE) -> apply (opFn f1 f2) x ?==? vr
              _ -> True


specFnPointwiseOp1 ::
  ( HasDomain f, CanMapInside (Domain f) x
  , CanApply f x, ApplyType f x ~ v
  , HasEqCertainly v v
  , Arbitrary (FnAndDescr f), ArbitraryWithDom (FnAndDescr f), Show f
  , Arbitrary x, Show x
  ) =>
  (T f) -> (T x) ->
  String ->
  (f -> f) ->
  (CatchingNumExceptions v -> CatchingNumExceptions v) ->
  (FnAndDescr f -> FnAndDescr f) ->
  Spec
specFnPointwiseOp1
    (T fName :: T f) (T _xName :: T x)
    opName opFn opVal reshapeFn1
  =
  it ("pointwise " ++ opName ++ " on " ++ fName ++
      " corresponds to " ++ opName ++ " on values") $ property $
      \ (f1Pre :: FnAndDescr f) (xPres :: [x]) ->
          let FnAndDescr f1 _d1 = reshapeFn1 f1Pre in
          and $ flip map xPres $ \xPre ->
            let x = mapInside (getDomain f1) xPre in
            let v1 = apply f1 x in
            let vrE = opVal (catchingNumExceptions v1) in
            case _numEXC_maybeValue vrE of
              Just vr | not (hasCertainException vrE) -> apply (opFn f1) x ?==? vr
              _ -> True
