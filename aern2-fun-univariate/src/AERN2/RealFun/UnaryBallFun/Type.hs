{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.RealFun.UnaryBallFun.Type
    Description :  type definition and basics
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Type definitions and basics.
-}

module AERN2.RealFun.UnaryBallFun.Type
(
  UnaryBallFun(..), unaryBallFun,
  lift1, lift2, lift2withX
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import Control.CollectErrors

import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace = if shouldTrace then trace else const id
_dummy :: ()
_dummy = maybeTrace "dummy" ()

data UnaryBallFun =
  UnaryBallFun
  {
    unaryBallFun_Domain :: DyadicInterval
    ,
    {-| For convergent sequence of *open* balls the resulting sequence should also converge. -}
    unaryBallFun_Eval :: CN MPBall -> CN MPBall
  }

instance HasDomain UnaryBallFun where
  type Domain UnaryBallFun = DyadicInterval
  getDomain = unaryBallFun_Domain

type CanBeUnaryBallFun t = ConvertibleExactly t UnaryBallFun
unaryBallFun :: (CanBeUnaryBallFun t) => t -> UnaryBallFun
unaryBallFun = convertExactly

instance (CanBeMPBall t, Show t, Typeable t)
  =>
  ConvertibleExactly (DyadicInterval, t) UnaryBallFun
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly x of
      Right b -> Right $ UnaryBallFun dom (const $ cn (b :: MPBall))
      _err -> convError "unable to convert to constant function: " (dom,x)

instance HasFnConstructorInfo UnaryBallFun where
  type FnConstructorInfo UnaryBallFun = DyadicInterval
  getFnConstructorInfo = getDomain

instance HasVars UnaryBallFun where
  type Var UnaryBallFun = ()
  varFn dom () =
    UnaryBallFun dom cn

instance HasAccuracy UnaryBallFun where
  getAccuracy _f = Exact
  getFiniteAccuracy _ = error "getFiniteAccuracy not defined for UnaryBallFun"

instance HasAccuracyGuide UnaryBallFun where
  getAccuracyGuide _f = NoInformation

instance CanSetAccuracyGuide UnaryBallFun where
  setAccuracyGuide _ f = f

instance (CanBeErrors es) => CanEnsureCE es UnaryBallFun where
  type EnsureCE es UnaryBallFun = UnaryBallFun
  type EnsureNoCE es UnaryBallFun = UnaryBallFun
  ensureCE _sample_es = id
  deEnsureCE _sample_es = Right
  ensureNoCE _sample_es v = (Just v, mempty)
  noValueECE _sample_vCE _es = error "UnaryBallFun noValueCE not implemented yet"
  prependErrorsECE _sample_vCE _es = error "UnaryBallFun prependErrorsECE not implemented yet"

instance CanMinMaxAsymmetric UnaryBallFun UnaryBallFun where
  min = lift2 min
  max = lift2 max

instance CanNeg UnaryBallFun where
  negate = lift1 negate

instance CanAddAsymmetric UnaryBallFun UnaryBallFun where
  add = lift2 add

instance CanSub UnaryBallFun UnaryBallFun where
  type SubType UnaryBallFun UnaryBallFun = UnaryBallFun
  sub = lift2 sub

instance CanMulAsymmetric UnaryBallFun UnaryBallFun where
  mul = lift2 mul

instance CanDiv UnaryBallFun UnaryBallFun where
  type DivTypeNoCN UnaryBallFun UnaryBallFun = UnaryBallFun
  divideNoCN = lift2 divide
  type DivType UnaryBallFun UnaryBallFun = UnaryBallFun
  divide = lift2 divide

instance CanPow UnaryBallFun UnaryBallFun where
  type PowTypeNoCN UnaryBallFun UnaryBallFun = UnaryBallFun
  powNoCN = lift2 pow
  type PowType UnaryBallFun UnaryBallFun = UnaryBallFun
  pow = lift2 pow

instance CanSinCos UnaryBallFun where
  sin = lift1 sin
  cos = lift1 cos

lift1 :: (CN MPBall -> CN MPBall) -> UnaryBallFun -> UnaryBallFun
lift1 op (UnaryBallFun dom1 f1) =
  UnaryBallFun dom1 (\ x -> op (f1 x))

lift2 :: (CN MPBall -> CN MPBall -> CN MPBall) -> UnaryBallFun -> UnaryBallFun -> UnaryBallFun
lift2 op (UnaryBallFun dom1 f1) (UnaryBallFun _dom2 f2) =
  UnaryBallFun dom1 (\ x -> op (f1 x) (f2 x))

lift2withX :: (CN MPBall -> CN MPBall -> CN MPBall -> CN MPBall) -> UnaryBallFun -> UnaryBallFun -> UnaryBallFun
lift2withX op (UnaryBallFun dom1 f1) (UnaryBallFun _dom2 f2) =
  UnaryBallFun dom1 (\ x -> op x (f1 x) (f2 x))

lift2T :: (CN MPBall -> t -> CN MPBall) -> UnaryBallFun -> t -> UnaryBallFun
lift2T op (UnaryBallFun dom1 f1) t =
  UnaryBallFun dom1 (\ x -> op (f1 x) t)


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| CauchyReal |]]
  (\ t -> [d|
    instance CanAddAsymmetric UnaryBallFun $t where
      add = lift2T add
    instance CanAddAsymmetric $t UnaryBallFun where
      type AddType $t UnaryBallFun = UnaryBallFun
      add = flip $ lift2T (flip add)

    instance CanMulAsymmetric UnaryBallFun $t where
      mul = lift2T mul
    instance CanMulAsymmetric $t UnaryBallFun where
      type MulType $t UnaryBallFun = UnaryBallFun
      mul = flip $ lift2T (flip mul)

    instance CanSub UnaryBallFun $t where
      type SubType UnaryBallFun $t = UnaryBallFun
      sub = lift2T sub
    instance CanSub $t UnaryBallFun where
      type SubType $t UnaryBallFun = UnaryBallFun
      sub = flip $ lift2T (flip sub)

    instance CanDiv UnaryBallFun $t where
      type DivTypeNoCN UnaryBallFun $t = UnaryBallFun
      divideNoCN = lift2T divide
      type DivType UnaryBallFun $t = UnaryBallFun
      divide = lift2T divide
    instance CanDiv $t UnaryBallFun where
      type DivTypeNoCN $t UnaryBallFun = UnaryBallFun
      divideNoCN = flip $ lift2T (flip divide)
      type DivType $t UnaryBallFun = UnaryBallFun
      divide = flip $ lift2T (flip divide)

    instance CanPow UnaryBallFun $t where
      type PowTypeNoCN UnaryBallFun $t = UnaryBallFun
      powNoCN = lift2T pow
      type PowType UnaryBallFun $t = UnaryBallFun
      pow = lift2T pow
  |]))
