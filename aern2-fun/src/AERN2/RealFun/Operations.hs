{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.RealFun.Operations
    Description :  Classes for real number function operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Classes for real number function operations
-}

module AERN2.RealFun.Operations
(
  HasDomain(..)
  , SameDomFnPair(..), ArbitraryWithDom(..)
  , CanApply(..)
  , CanApplyApprox(..), sampledRange
  , HasVars(..), specEvalUnaryVarFn
  , HasConstFunctions, constFn, specEvalConstFn
  , specFnPointwiseOp1, specFnPointwiseOp2
  , CanMaximiseOverDom(..), CanMinimiseOverDom(..)
  , CanIntegrateOverDom(..)
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
import Text.Printf

-- import Data.Typeable

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

import AERN2.Interval

import AERN2.MP.Dyadic
import AERN2.MP.Enclosure

{- domain -}

class HasDomain f where
  type Domain f
  getDomain :: f -> Domain f

data SameDomFnPair f = SameDomFnPair (f,f) deriving Show

instance (ArbitraryWithDom f, Arbitrary f) => (Arbitrary (SameDomFnPair f)) where
  arbitrary =
    do
    f1 <- arbitrary
    f2 <- arbitraryWithDom (getDomain f1)
    return $ SameDomFnPair (f1,f2)

class (HasDomain f) => ArbitraryWithDom f where
  arbitraryWithDom :: (Domain f) -> Gen f

{- evaluation -}

class CanApply f x where
  type ApplyType f x
  {-| compute @f(x)@  -}
  apply :: f {-^ @f@ -} -> x {-^ @x@ -} -> ApplyType f x

{-|
  Give an unsafe etimate of the function's range which is fast to compute.
  Intended to be used in optimisation heuristics.
-}
class CanApplyApprox f x where
  type ApplyApproxType f x
  {-| compute a cheap and unsafe approximation of @f(x)@  -}
  applyApprox :: f {-^ @f@ -} -> x {-^ @x@ -} -> ApplyApproxType f x

{-|
  Evaluate a function on a regular grid of the given size and return
  the largerst and smallest values found.  Useful for making instances
  of class 'CanApplyApprox'.
-}
sampledRange ::
  (CanApply f t, ApplyType f t ~ t,
   CanMinMaxSameType t, ConvertibleExactly Dyadic t, Show t)
  =>
  DyadicInterval -> Integer -> f -> Interval t t
sampledRange (Interval l r) depth f =
    maybeTrace
    ( "sampledRange:"
    ++ "\n samplePointsT = " ++ (show samplePointsT)
    ++ "\n samples = " ++ show samples
    ) $
    Interval minValue maxValue
    where
    minValue = foldl1 min samples
    maxValue = foldl1 max samples
    samples = map (apply f) samplePointsT
    samplePointsT = map convertExactly samplePoints
    _ = minValue : samplePointsT
    samplePoints :: [Dyadic]
    samplePoints = [(l*i + r*(size - i))*(1/size) | i <- [0..size]]
    size = 2^depth


{- constructing basic functions -}

type HasConstFunctions t f = (HasDomain f, ConvertibleExactly (Domain f, t) f)

constFn :: (HasConstFunctions t f) => (Domain f, t) -> f
constFn = convertExactly

specEvalConstFn ::
  (HasConstFunctions c f
  , CanMapInside (Domain f) x
  , CanApply f x
  , HasEqCertainly c (ApplyType f x)
  , Arbitrary c, Arbitrary f, Arbitrary x
  , Show c, Show f, Show x)
  =>
  T c-> T f -> T x -> Spec
specEvalConstFn (T cName :: T c) (T fName :: T f) (T xName :: T x) =
  it (printf "Evaluating %s-constant functions %s on %s" cName fName xName) $ do
    property $
      \ (c :: c) (sampleFn :: f) (xPres :: [x]) ->
        let dom = getDomain sampleFn in
        and $ flip map xPres $ \xPre ->
          apply (constFn (dom, c) :: f) (mapInside dom xPre) ?==? c

class HasVars f where
  type Var f
  {-| the function @x@, ie the function that project the domain to the given variable @x@  -}
  varFn ::
    f {-^ sample function with the same domain -}->
    Var f {-^ @x@ -} ->
    f

specEvalUnaryVarFn ::
  (HasVars f, Var f ~ ()
  , HasDomain f, CanMapInside (Domain f) x
  , CanApply f x
  , HasEqCertainly x (ApplyType f x)
  , Arbitrary f, Arbitrary x
  , Show f, Show x)
  =>
  T f -> T x -> Spec
specEvalUnaryVarFn (T fName :: T f) (T xName :: T x) =
  it (printf "Evaluating variable functions %s on %s" fName xName) $ do
    property $
      \ (sampleFn :: f) (xPres :: [x]) ->
        and $ flip map xPres $ \xPre ->
          let x = mapInside (getDomain sampleFn) xPre in
          apply (varFn sampleFn () :: f) x ?==? x

{- pointwise operations -}

specFnPointwiseOp2 ::
  ( HasDomain f, CanMapInside (Domain f) x
  , CanApply f x, ApplyType f x ~ v
  , HasEqCertainly v v
  , Arbitrary f, ArbitraryWithDom f, Show f
  , Arbitrary x, Show x
  ) =>
  (T f) -> (T x) -> (T c) ->
  String ->
  (f -> f -> f) ->
  (v -> v -> v) ->
  (f -> f) ->
  (f -> f) ->
  Spec
specFnPointwiseOp2 (T fName :: T f) (T _xName :: T x) (T vName :: T v) opName opFn opVal reshapeFn1 reshapeFn2 =
  it ("pointwise " ++ opName ++ " on " ++ fName ++ " corresponds to " ++ opName ++ " on " ++ vName) $ do
    property $
      \ (SameDomFnPair (f1Pre,f2Pre) :: SameDomFnPair f) (xPres :: [x]) ->
          let f1 = reshapeFn1 f1Pre in
          let f2 = reshapeFn2 f2Pre in
          and $ flip map xPres $ \xPre ->
            let x = mapInside (getDomain f1) xPre in
            let v1 = apply f1 x in
            let v2 = apply f2 x in
            apply (opFn f1 f2) x ?==? opVal v1 v2

specFnPointwiseOp1 ::
  ( HasDomain f, CanMapInside (Domain f) x
  , CanApply f x, ApplyType f x ~ v
  , HasEqCertainly v v
  , Arbitrary f, ArbitraryWithDom f, Show f
  , Arbitrary x, Show x
  ) =>
  (T f) -> (T x) -> (T c) ->
  String ->
  (f -> f) ->
  (v -> v) ->
  (f -> f) ->
  Spec
specFnPointwiseOp1 (T fName :: T f) (T _xName :: T x) (T vName :: T v) opName opFn opVal reshapeFn1 =
  it ("pointwise " ++ opName ++ " on " ++ fName ++ " corresponds to " ++ opName ++ " on " ++ vName) $ do
    property $
      \ (f1Pre :: f) (xPres :: [x]) ->
          let f1 = reshapeFn1 f1Pre in
          and $ flip map xPres $ \xPre ->
            let x = mapInside (getDomain f1) xPre in
            let v1 = apply f1 x in
            apply (opFn f1) x ?==? opVal v1

{- range computation -}

class CanMaximiseOverDom f d where
  type MaximumOverDomType f d
  maximumOverDom :: f -> d -> MaximumOverDomType f d

class CanMinimiseOverDom f d where
  type MinimumOverDomType f d
  minimumOverDom :: f -> d -> MinimumOverDomType f d

{- integration -}

class CanIntegrateOverDom f bounds where
  type IntegralOverDomType f bounds
  integrateOverDom :: f -> bounds -> IntegralOverDomType f bounds
