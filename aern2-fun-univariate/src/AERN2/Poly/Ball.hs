{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.Poly.Ball
    Description :  Polynomial enclosures with large radii
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Polynomial enclosures with large radii
-}

module AERN2.Poly.Ball
-- (
-- )
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (flip const)
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

-- import qualified Data.Map as Map
-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck


import Control.CollectErrors

import AERN2.Normalize

import AERN2.MP hiding (ball_value, ball_error)
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic


import AERN2.Real hiding (ball_value)

import AERN2.Interval
import AERN2.RealFun.Operations

import AERN2.Poly.Cheb as ChPoly


{- examples -}

_pb_const1 :: PolyBall
_pb_const1 =
    constFn (dom, bits 100) 1
    where
    dom = dyadicInterval (0.0,1.0)

_pb_X :: PolyBall
_pb_X =
    varFn (dom, bits 100) ()
    where
    dom = dyadicInterval (0.0,1.0)

{- type -}

type PolyBall = Ball (ChPoly MPBall)

polyBall :: (ConvertibleExactly t PolyBall) => t -> PolyBall
polyBall = convertExactly

data Ball t = Ball { ball_value :: t, ball_radius :: ErrorBound }

instance (Show t) => Show (Ball t) where
  show (Ball c e) = "Ball " ++ (show c) ++ "+-" ++ (show e)

instance (CanBeErrors es) => CanEnsureCE es (Ball t)
instance (CanBeErrors es) => CanExtractCE es Ball where
  extractCE sample_es (Ball cCE e) =
    case ensureNoCE sample_es cCE of
      (Just c, es) -> CollectErrors (Just (Ball c e)) es
      (_, es) -> CollectErrors Nothing es

degree :: PolyBall -> Integer
degree (Ball c _e) = ChPoly.degree c

ballLift1R :: (IsBall t) => (t -> t1) -> (Ball t -> t1)
ballLift1R f (Ball c e) = f (updateRadius (+ e) c)

ballLift1TR :: (IsBall t) => (t -> t1 -> t2) -> (Ball t -> t1 -> t2)
ballLift1TR f (Ball c e) = f (updateRadius (+ e) c)

ballLift1T :: (IsBall t) => (t -> t1 -> t) -> (Ball t -> t1 -> Ball t)
ballLift1T f (Ball c e) t = Ball fceC fceE
  where
  fceC = centreAsBall fce
  fceE = radius fce
  fce = f (updateRadius (+e) c) t

ballLift1TCN ::
  (IsBall t, CanEnsureCN t) =>
  (t -> t1 -> EnsureCN t) -> (Ball t -> t1 -> CN (Ball t))
ballLift1TCN f b@(Ball c e) t =
  case deEnsureCN fceCN of
    Right fce ->
      let
        fceC = centreAsBall fce
        fceE = radius fce
      in
      cn $ Ball fceC fceE
    Left es ->
      noValueECN (Just b) es
  where
  fceCN = f (updateRadius (+e) c) t

-- instance CanExtractCE es Ball where
--   extractCE sample_es

ballLift1 :: (IsBall t) => (t -> t) -> (Ball t -> Ball t)
ballLift1 f (Ball c1 e1) = Ball fceC fceE
  where
  fceC = centreAsBall fce
  fceE = radius fce
  fce = f (updateRadius (+e1) c1)

ballLift2 :: (IsBall t) => (t -> t -> t) -> (Ball t -> Ball t -> Ball t)
ballLift2 f (Ball c1 e1) (Ball c2 e2) = Ball fceC fceE
  where
  fceC = centreAsBall fce
  fceE = radius fce
  fce = f (updateRadius (+e1) c1) (updateRadius (+e2) c2)

instance (IsBall t) => IsBall (Ball t) where
  type CentreType (Ball t) = t
  centre = ball_value
  radius = ball_radius
  updateRadius updateFn (Ball c r) = Ball c (updateFn r)
  centreAsBallAndRadius (Ball c r) = (Ball c (errorBound 0), r)

instance (IsBall t, CanNormalize t) => CanNormalize (Ball t) where
  normalize (Ball x e) = Ball (centreAsBall xN) (radius xN + e)
    where
    xN = normalize x

instance
  HasFnConstructorInfo t
  =>
  HasFnConstructorInfo (Ball t)
  where
  type FnConstructorInfo (Ball t) = FnConstructorInfo t
  getFnConstructorInfo (Ball c _) = getFnConstructorInfo c

instance (ConvertibleExactly (i, t2) t) =>
  ConvertibleExactly (i, t2) (Ball t)
  where
  safeConvertExactly (constrInfo, x) =
    case safeConvertExactly (constrInfo, x) of
      Right c -> Right $ Ball c (errorBound 0)
      Left e -> Left e

-- instance (ConvertibleExactly (t, t2) t) =>
--   ConvertibleExactly (Ball t, t2) (Ball t)
--   where
--   safeConvertExactly (Ball sample eb, x) =
--     case safeConvertExactly (sample, x) of
--       Right c -> Right $ Ball c eb
--       Left e -> Left e

instance (HasDomain t) => HasDomain (Ball t)
  where
  type Domain (Ball t) = Domain t
  getDomain = getDomain . ball_value

instance (HasVars t) => HasVars (Ball t) where
  type Var (Ball t) = Var t
  varFn constrInfo var = Ball (varFn constrInfo var) (errorBound 0)

{- precision -}

instance (HasPrecision t, IsBall t) => HasPrecision (Ball t) where
  getPrecision = ballLift1R getPrecision

instance (CanSetPrecision t, IsBall t) => CanSetPrecision (Ball t) where
  setPrecision prc (Ball c e) = Ball (setPrecision prc c) e

{- accuracy -}

instance (HasAccuracy t, IsBall t) => HasAccuracy (Ball t) where
  getAccuracy= ballLift1R getAccuracy
  getFiniteAccuracy= ballLift1R getFiniteAccuracy

instance (HasAccuracyGuide t, IsBall t) => HasAccuracyGuide (Ball t) where
  getAccuracyGuide = ballLift1R getAccuracyGuide

instance (CanSetAccuracyGuide t, IsBall t) => CanSetAccuracyGuide (Ball t) where
  setAccuracyGuide acGuide = ballLift1 (setAccuracyGuide acGuide)

instance
  (IsBall t, CanNormalize t, CanReduceSizeUsingAccuracyGuide t)
  =>
  CanReduceSizeUsingAccuracyGuide (Ball t)
  where
  reduceSizeUsingAccuracyGuide ac (Ball c e) =
    normalize $ Ball (reduceSizeUsingAccuracyGuide ac c) e

{- negation -}

instance CanNegSameType t => CanNeg (Ball t) where
  type NegType (Ball t) = Ball t
  negate (Ball x e) = Ball (negate x) e

{- addition -}

instance (IsBall t, CanNormalize t, CanAddSameType t) => CanAddAsymmetric (Ball t) (Ball t) where
  type AddType  (Ball t) (Ball t) = Ball t
  add (Ball x1 e1) (Ball x2 e2) =
    normalize $ Ball (x1 + x2) (e1 + e2)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis t $t, IsBall t, CanNormalize t) => CanAddAsymmetric $t (Ball t) where
      type AddType $t (Ball t) = Ball t
      add n (Ball x e) = normalize $ Ball (x + n) e

    instance (CanAddThis t $t, IsBall t, CanNormalize t) => CanAddAsymmetric (Ball t) $t where
      type AddType (Ball t) $t = Ball t
      add (Ball x e) n = normalize $ Ball (x + n) e
  |]))

{- subtraction -}

instance (IsBall t, CanNormalize t, CanAddSameType t, CanNegSameType t) => CanSub (Ball t) (Ball t)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (IsBall t, CanNormalize t, CanAddThis t $t, CanNegSameType t) => CanSub $t (Ball t)
    instance (IsBall t, CanNormalize t, CanAddThis t $t) => CanSub (Ball t) $t
  |]))

{- multiplication -}

multiplyWithBounds :: PolyBall -> MPBall -> PolyBall -> MPBall -> PolyBall
multiplyWithBounds (Ball p ep) bp (Ball q eq) bq =
  makeExactCentre res
  where
  res = Ball (p * q) e
  e = ep*(errorBound bq) + (errorBound bp)*eq + ep*eq

multiplyAccurate :: PolyBall -> PolyBall -> PolyBall
multiplyAccurate f g =
  multiplyWithAccuracy (min ((getFiniteAccuracy f) + 1) ((getFiniteAccuracy g) + 1)) f g

multiplyWithAccuracy :: Accuracy -> PolyBall -> PolyBall -> PolyBall
multiplyWithAccuracy ac f@(Ball p _) g@(Ball q _) =
  multiplyWithBounds f (rangeWithAccuracy p) g (rangeWithAccuracy q)
  where
  rangeWithAccuracy h =
    let
    Interval a' b' = chPoly_dom h
    pr = getPrecision h
    a = setPrecision pr $ mpBall a'
    b = setPrecision pr $ mpBall b'
    in
    max (abs $ maximumOptimisedWithAccuracy ac h a b 5 5)
        (abs $ minimumOptimisedWithAccuracy ac h a b 5 5)

instance
  -- (IsBall t, CanNormalize t, CanMulSameType t)
  -- =>
  -- CanMulAsymmetric (Ball t) (Ball t) where
  -- type MulType  (Ball t) (Ball t) = Ball t
  CanMulAsymmetric PolyBall PolyBall where
  type MulType PolyBall PolyBall = PolyBall
  mul = ballLift2 mul
  -- mul = multiplyWithAccuracy (bits 0)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy t $t, IsBall t, CanNormalize t) => CanMulAsymmetric $t (Ball t) where
      type MulType $t (Ball t) = Ball t
      mul = flip (ballLift1T (flip mul))

    instance (CanMulBy t $t, IsBall t, CanNormalize t) => CanMulAsymmetric (Ball t) $t where
      type MulType (Ball t) $t = Ball t
      mul = ballLift1T mul
  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanDivCNBy t $t, IsBall t, CanNormalize t, CanEnsureCN t) => CanDiv (Ball t) $t where
      type DivTypeNoCN (Ball t) $t = Ball t
      divideNoCN = ballLift1T divideNoCN
      type DivType (Ball t) $t = EnsureCN (Ball t)
      divide = ballLift1TCN divide
  |]))

{- evaluation -}

instance CanApply PolyBall MPBall where
  type ApplyType PolyBall MPBall = MPBall
  apply (Ball c e) y = updateRadius (+e) (apply c y)

instance CanApplyApprox PolyBall DyadicInterval where
  type ApplyApproxType PolyBall DyadicInterval = MPBall
  applyApprox (Ball c e) y =
    updateRadius (+e) (applyApprox c y)

{- maximisation -}

instance CanMaximiseOverDom PolyBall DyadicInterval where
  type MaximumOverDomType PolyBall DyadicInterval = MPBall
  maximumOverDom (Ball c e) di =
    maximumOverDom (updateRadius (+e) c) di

instance CanMinimiseOverDom PolyBall DyadicInterval where
  type MinimumOverDomType PolyBall DyadicInterval = MPBall
  minimumOverDom (Ball c e) di =
    minimumOverDom (updateRadius (+e) c) di

{- integration -}

instance CanIntegrateOverDom PolyBall DyadicInterval where
  type IntegralOverDomType PolyBall DyadicInterval = MPBall
  integrateOverDom (Ball c e) di =
    integrateOverDom (updateRadius (+e) c) di


instance CanSinCosSameType t => CanSinCos (Ball t) where
  type SinCosType (Ball t) = Ball t
  sin (Ball x e) = Ball (sin x) e
  cos (Ball x e) = Ball (cos x) e
