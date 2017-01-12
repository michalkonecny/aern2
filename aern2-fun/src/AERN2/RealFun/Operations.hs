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
  , CanApply(..)
  , CanApplyApprox(..)
  , HasVars(..), specEvalUnaryVarFn
  , HasConstFunctions, constFn, specEvalConstFn
  , CanMaximiseOverDom(..), CanMinimiseOverDom(..)
  , CanIntegrateOverDom(..)
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
import Text.Printf

-- import Data.Typeable

-- import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck

-- import AERN2.MP.Dyadic
import AERN2.MP.Enclosure
-- import AERN2.MP.Ball

class HasDomain f where
  type Domain f
  getDomain :: f -> Domain f

class CanApply f x where
  type ApplyType f x
  {-| compute @f(x)@  -}
  apply :: f {-^ @f@ -} -> x {-^ @x@ -} -> ApplyType f x

class CanApplyApprox f x where
  type ApplyApproxType f x
  {-| compute a cheap and unsafe approximation of @f(x)@  -}
  applyApprox :: f {-^ @f@ -} -> x {-^ @x@ -} -> ApplyApproxType f x

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
      \ (sampleFn :: f) (xPre :: x) ->
        let x = mapInside (getDomain sampleFn) xPre in
        apply (varFn sampleFn () :: f) x ?==? x



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
  it (printf "Evaluating constant (%s) functions %s on %s" cName fName xName) $ do
    property $
      \ (c :: c) (sampleFn :: f) (x :: x) ->
        let dom = getDomain sampleFn in
        apply (constFn (dom, c) :: f) (mapInside dom x) ?==? c

class CanMaximiseOverDom f d where
  type MaximumOverDomType f d
  maximumOverDom :: f -> d -> MaximumOverDomType f d

class CanMinimiseOverDom f d where
  type MinimumOverDomType f d
  minimumOverDom :: f -> d -> MinimumOverDomType f d

class CanIntegrateOverDom f bounds where
  type IntegralOverDomType f bounds
  integrateOverDom :: f -> bounds -> IntegralOverDomType f bounds
