{-# LANGUAGE CPP #-}
-- #define DEBUG
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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
  , HasFnConstructorInfo(..)
  , HasConstFunctions, constFn, specEvalConstFn
  , HasVars(..), specEvalUnaryVarFn
  , CanMaximiseOverDom(..), CanMinimiseOverDom(..)
  , specCanMaximiseOverDom
  , CanIntegrateOverDom(..)
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
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
    samplePoints = [(l*i + r*(size - i))*(dyadic (1/size)) | i <- [0..size]]
    size = round $ 2^depth


{- constructing basic functions -}

class HasFnConstructorInfo f where
  type FnConstructorInfo f
  getFnConstructorInfo :: f -> FnConstructorInfo f

type HasConstFunctions t f =
  (ConvertibleExactly (FnConstructorInfo f, t) f)

constFn :: (HasConstFunctions t f) => (FnConstructorInfo f) -> t -> f
constFn = curry convertExactly

specEvalConstFn ::
  _ => T c-> T f -> T x -> Spec
specEvalConstFn (T cName :: T c) (T fName :: T f) (T xName :: T x) =
  it (printf "Evaluating %s-constant functions %s on %s" cName fName xName) $
    property $
      \ (c :: c) (constrInfo :: FnConstructorInfo f) (xPres :: [x]) ->
        let f = constFn constrInfo c :: f in
        let dom = getDomain f in
        and $ flip map xPres $ \xPre ->
          apply f (mapInside dom xPre) ?==? c

class HasVars f where
  type Var f
  {-| the function @x@, ie the function that project the domain to the given variable @x@  -}
  varFn ::
    FnConstructorInfo f {-^ eg domain and/or accuracy guide -}->
    Var f {-^ @x@ -} ->
    f

specEvalUnaryVarFn ::
  _ => T f -> T x -> Spec
specEvalUnaryVarFn (T fName :: T f) (T xName :: T x) =
  it (printf "Evaluating variable functions %s on %s" fName xName) $ property $
    \ (constrInfo :: FnConstructorInfo f) (xPres :: [x]) ->
      and $ flip map xPres $ \xPre ->
        let f = varFn constrInfo () :: f in
        let x = mapInside (getDomain f) xPre in
        apply f x ?==? x


{- range computation -}

class CanMaximiseOverDom f d where
  type MaximumOverDomType f d
  maximumOverDom :: f -> d -> MaximumOverDomType f d

class CanMinimiseOverDom f d where
  type MinimumOverDomType f d
  minimumOverDom :: f -> d -> MinimumOverDomType f d

-- specCanMaximiseOverDom ::
--   _ => (T f) -> Spec
-- specCanMaximiseOverDom (T fName :: T f) =
--   describe ("CanMaximiseOverDom " ++ fName) $ do
--     it "is consistent over a split domain" $ property $
--         \ (f :: f) ->
--           let dom = getDomain f in
--           let (dom1, dom2) = split dom in
--           let maxOnDom = maximumOverDom f dom in
--           let maxOnDom1 = maximumOverDom f dom1 in
--           let maxOnDom2 = maximumOverDom f dom2 in
--           maxOnDom ?>=? maxOnDom1
--           &&
--           maxOnDom ?>=? maxOnDom2

specCanMaximiseOverDom ::
  _ => (T f) -> (T x) -> Spec
specCanMaximiseOverDom (T fName :: T f) (T _xName :: T x) =
  describe ("CanMaximiseOverDom " ++ fName) $ do
    it "is consistent with evaluation" $ property $
      \ (f :: f) (xPres :: [x]) ->
        let dom = getDomain f in
        let maxOnDom = maximumOverDom f dom in
        and $ flip map xPres $ \xPre ->
          let x = mapInside dom xPre in
          let v1 = apply f x in
          maxOnDom ?>=? v1


{- integration -}

class CanIntegrateOverDom f bounds where
  type IntegralOverDomType f bounds
  integrateOverDom :: f -> bounds -> IntegralOverDomType f bounds
