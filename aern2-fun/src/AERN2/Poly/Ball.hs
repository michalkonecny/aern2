{-# LANGUAGE TemplateHaskell #-}
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

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import qualified Data.Map as Map
-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.TH

import AERN2.Normalize

import AERN2.MP.ErrorBound
import AERN2.MP.Ball (MPBall, IsBall(..), mpBall, getPrecision, setPrecision)
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic
import AERN2.MP.Accuracy


import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations

import AERN2.Poly.Cheb

{- examples -}

_pb_const1 :: PolyBall
_pb_const1 =
    constFn (dom, 1)
    where
    dom = dyadicInterval (0.0,1.0)

_pb_X :: PolyBall
_pb_X =
    varFn sampleFn ()
    where
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

{- type -}

type PolyBall = Ball (ChPoly MPBall)

polyBall :: (ConvertibleExactly t PolyBall) => t -> PolyBall
polyBall = convertExactly

data Ball c = Ball { ball_value :: c, ball_radius :: ErrorBound }

lift1PolyBall :: (ChPoly MPBall -> c) -> (PolyBall -> c)
lift1PolyBall f (Ball c e) = f (updateRadius (+ e) c)

lift2PolyBall :: (ChPoly MPBall -> c -> c) -> (PolyBall -> c -> c)
lift2PolyBall f (Ball c e) = f (updateRadius (+ e) c)

instance (IsBall c) => IsBall (Ball c) where
  type CentreType (Ball c) = c
  centre = ball_value
  radius = ball_radius
  updateRadius updateFn (Ball c r) = Ball c (updateFn r)
  centreAsBallAndRadius (Ball c r) = (Ball c (errorBound 0), r)

instance (IsBall c) => CanNormalize (Ball c) where
  normalize (Ball x e) = Ball (centreAsBall x) (radius x + e)

instance (ConvertibleExactly (DyadicInterval, t) c) =>
  ConvertibleExactly (DyadicInterval, t) (Ball c)
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly (dom, x) of
      Right c -> Right $ Ball c (errorBound 0)
      Left e -> Left e

instance (ConvertibleExactly (c, t) c) =>
  ConvertibleExactly (Ball c, t) (Ball c)
  where
  safeConvertExactly (Ball sample eb, x) =
    case safeConvertExactly (sample, x) of
      Right c -> Right $ Ball c eb
      Left e -> Left e

instance (HasDomain c) => HasDomain (Ball c)
  where
  type Domain (Ball c) = Domain c
  getDomain = getDomain . ball_value

instance (HasVars c) => HasVars (Ball c) where
  type Var (Ball c) = Var c
  varFn (Ball c _) var = Ball (varFn c var) (errorBound 0)

{- accuracy -}

instance (HasAccuracy c, IsBall c) => HasAccuracy (Ball c) where
  getAccuracy (Ball p e) =
    getAccuracy (updateRadius (+ e) p)


{- negation -}

instance CanNegSameType t => CanNeg (Ball t) where
  type NegType (Ball t) = Ball t
  negate (Ball x e) = Ball (negate x) e

{- addition -}

instance (IsBall t, CanAddSameType t) => CanAddAsymmetric (Ball t) (Ball t) where
  type AddType  (Ball t) (Ball t) = Ball t
  add (Ball x1 e1) (Ball x2 e2) =
    normalize $ Ball (x1 + x2) (e1 + e2)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanAddThis t $t, IsBall t) => CanAddAsymmetric $t (Ball t) where
      type AddType $t (Ball t) = Ball t
      add n (Ball x e) = normalize $ Ball (x + n) e

    instance (CanAddThis t $t, IsBall t) => CanAddAsymmetric (Ball t) $t where
      type AddType (Ball t) $t = Ball t
      add (Ball x e) n = normalize $ Ball (x + n) e
  |]))

{- subtraction -}

instance (IsBall t, CanAddSameType t, CanNegSameType t) => CanSub (Ball t) (Ball t)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (IsBall t, CanAddThis t $t, CanNegSameType t) => CanSub $t (Ball t)
    instance (IsBall t, CanAddThis t $t) => CanSub (Ball t) $t
  |]))

{- multiplication -}

multiplyWithBounds :: PolyBall -> MPBall -> PolyBall -> MPBall -> PolyBall
multiplyWithBounds (Ball p ep) bp (Ball q eq) bq =
  Ball (p * q) $
    ep*(errorBound bq) + (errorBound bp)*eq + ep*eq

multiplyAccurate :: PolyBall -> PolyBall -> PolyBall
multiplyAccurate f g =
  multiplyWithAccuracy (min ((getAccuracy f) + 1) ((getAccuracy g) + 1)) f g

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

instance (IsBall c, CanMulSameType c)
  =>
  CanMulAsymmetric (Ball c) (Ball c) where
  type MulType  (Ball c) (Ball c) = Ball c
  mul (Ball x1 e1) (Ball x2 e2) =
    normalize $ Ball (x1e1 * x2e2) (errorBound 0)
    where
    x1e1 = updateRadius (+ e1) x1
    x2e2 = updateRadius (+ e2) x2
    -- TODO: use norm computed using root finding?
    --  is it too expensive?  check once we have benchmarking

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanMulBy t $t, IsBall t) => CanMulAsymmetric $t (Ball t) where
      type MulType $t (Ball t) = Ball t
      mul n (Ball x e) = normalize $ Ball (x * n) e

    instance (CanMulBy t $t, IsBall t) => CanMulAsymmetric (Ball t) $t where
      type MulType (Ball t) $t = Ball t
      mul (Ball x e) n = normalize $ Ball (x * n) e
  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |], [t| CauchyReal |]]
  (\ t -> [d|
    instance (CanDivBy t $t, IsBall t) => CanDiv (Ball t) $t where
      type DivType (Ball t) $t = Ball t
      divide (Ball x e) n = normalize $ Ball (x / n) e
  |]))

{- evaluation -}

instance CanApply PolyBall MPBall where
  type ApplyType PolyBall MPBall = MPBall
  apply (Ball x e) y = updateRadius (+e) (apply x y)

{- -}

instance (Show c) => Show (Ball c) where
  show (Ball c e) = "Ball " ++ (show c) ++ "+-" ++ (show e)
