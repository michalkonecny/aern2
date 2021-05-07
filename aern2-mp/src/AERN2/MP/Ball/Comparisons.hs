{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.Ball.Comparisons
    Description :  Comparisons of arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Comparisons of arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Comparisons
(
  -- * Auxiliary types
  module AERN2.Norm
  -- * Ball operations (see also instances)
  , reducePrecionIfInaccurate
  -- * Helpers for constructing ball functions
  , byEndpointsMP
  -- * intersection and hull
  , intersectCNMPBall
  , hullMPBall
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import qualified Control.CollectErrors as CE
import Control.CollectErrors
    ( CollectErrors(getMaybeValue), CanBeErrors )
import qualified Numeric.CollectErrors as CN

import AERN2.Norm
import AERN2.MP.Dyadic (Dyadic)
import AERN2.MP.Float (MPFloat)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()

{- comparisons -}

instance HasEqAsymmetric MPBall MPBall where
  type EqCompareType MPBall MPBall = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Integer where
  type EqCompareType MPBall Integer = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Integer MPBall where
  type EqCompareType Integer MPBall = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Int where
  type EqCompareType MPBall Int = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Int MPBall where
  type EqCompareType Int MPBall = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Rational where
  type EqCompareType MPBall Rational = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Rational MPBall where
  type EqCompareType Rational MPBall = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Dyadic where
  type EqCompareType MPBall Dyadic = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Dyadic MPBall where
  type EqCompareType Dyadic MPBall = Kleenean
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance
  (HasEqAsymmetric MPBall b
  , IsBool (EqCompareType MPBall b)
  , CanBeErrors es)
  =>
  HasEqAsymmetric MPBall (CollectErrors es b)
  where
  type EqCompareType MPBall (CollectErrors es b) =
    CollectErrors es (EqCompareType MPBall b)
  equalTo = CE.liftT1 equalTo

instance
  (HasEqAsymmetric a MPBall
  , IsBool (EqCompareType a MPBall)
  , CanBeErrors es)
  =>
  HasEqAsymmetric (CollectErrors es a) MPBall
  where
  type EqCompareType (CollectErrors es a) MPBall =
    CollectErrors es (EqCompareType a MPBall)
  equalTo = CE.lift1T equalTo

instance HasOrderAsymmetric MPBall MPBall where
  type OrderCompareType MPBall MPBall = Kleenean
  lessThan b1 b2
    | r1 < l2 = CertainTrue
    | r2 <= l1 = CertainFalse
    | otherwise = TrueOrFalse
    where
    (l1, r1) = endpoints b1
    (l2, r2) = endpoints b2
  leq b1 b2
    | r1 <= l2 = CertainTrue
    | r2 < l1 = CertainFalse
    | otherwise = TrueOrFalse
    where
    (l1, r1) = endpoints b1
    (l2, r2) = endpoints b2

instance HasOrderAsymmetric Integer MPBall where
  type OrderCompareType Integer MPBall = Kleenean
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Integer where
  type OrderCompareType MPBall Integer = Kleenean
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Int MPBall where
  type OrderCompareType Int MPBall = Kleenean
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Int where
  type OrderCompareType MPBall Int = Kleenean
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Dyadic MPBall where
  type OrderCompareType Dyadic MPBall = Kleenean
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Dyadic where
  type OrderCompareType MPBall Dyadic = Kleenean
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric MPBall Rational where
  type OrderCompareType MPBall Rational = Kleenean
  lessThan b1 q2
    | r1 < l2 = CertainTrue
    | r2 <= l1 = CertainFalse
    | otherwise = TrueOrFalse
    where
    (l1, r1) = endpoints b1
    l2 = q2
    r2 = q2
  leq b1 q2
    | r1 <= l2 = CertainTrue
    | r2 < l1 = CertainFalse
    | otherwise = TrueOrFalse
    where
    (l1, r1) = endpoints b1
    l2 = q2
    r2 = q2

instance HasOrderAsymmetric Rational MPBall where
  type OrderCompareType Rational MPBall = Kleenean
  lessThan q1 b2
    | r1 < l2 = CertainTrue
    | r2 <= l1 = CertainFalse
    | otherwise = TrueOrFalse
    where
    (l2, r2) = endpoints b2
    l1 = q1
    r1 = q1
  leq q1 b2
    | r1 <= l2 = CertainTrue
    | r2 < l1 = CertainFalse
    | otherwise = TrueOrFalse
    where
    (l2, r2) = endpoints b2
    l1 = q1
    r1 = q1

instance
  (HasOrderAsymmetric MPBall b
  , IsBool (OrderCompareType MPBall b)
  , CanBeErrors es)
  =>
  HasOrderAsymmetric MPBall (CollectErrors es  b)
  where
  type OrderCompareType MPBall (CollectErrors es  b) =
    CollectErrors es (OrderCompareType MPBall b)
  lessThan = CE.liftT1 lessThan
  leq = CE.liftT1 leq
  greaterThan = CE.liftT1 greaterThan
  geq = CE.liftT1 geq

instance
  (HasOrderAsymmetric a MPBall
  , IsBool (OrderCompareType a MPBall)
  , CanBeErrors es)
  =>
  HasOrderAsymmetric (CollectErrors es a) MPBall
  where
  type OrderCompareType (CollectErrors es  a) MPBall =
    CollectErrors es (OrderCompareType a MPBall)
  lessThan = CE.lift1T lessThan
  leq = CE.lift1T leq
  greaterThan = CE.lift1T greaterThan
  geq = CE.lift1T geq

instance CanTestZero MPBall
instance CanTestPosNeg MPBall

instance CanTestInteger MPBall where
  certainlyNotInteger b =
    (rN - lN) == 1 && lN !<! b && b !<! rN
    where
      (lN, rN) = integerBounds b
  certainlyIntegerGetIt b
    | rN == lN = Just lN
    | otherwise = Nothing
    where
      (lN, rN) = integerBounds b

instance CanMinMaxAsymmetric MPBall MPBall where
  min = byEndpointsMP min
  max = byEndpointsMP max

instance CanMinMaxAsymmetric MPBall Integer where
  type MinMaxType MPBall Integer = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Integer MPBall where
  type MinMaxType Integer MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Int where
  type MinMaxType MPBall Int = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Int MPBall where
  type MinMaxType Int MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Dyadic where
  type MinMaxType MPBall Dyadic = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Dyadic MPBall where
  type MinMaxType Dyadic MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Rational where
  type MinMaxType MPBall Rational = MPBall
  min = convertPSecond min
  max = convertPSecond max
instance CanMinMaxAsymmetric Rational MPBall where
  type MinMaxType Rational MPBall = MPBall
  min = convertPFirst min
  max = convertPFirst max

instance
  (CanMinMaxAsymmetric MPBall b
  , CanBeErrors es)
  =>
  CanMinMaxAsymmetric MPBall (CollectErrors es  b)
  where
  type MinMaxType MPBall (CollectErrors es  b) =
    CollectErrors es (MinMaxType MPBall b)
  min = CE.liftT1 min
  max = CE.liftT1 max

instance
  (CanMinMaxAsymmetric a MPBall
  , CanBeErrors es)
  =>
  CanMinMaxAsymmetric (CollectErrors es a) MPBall
  where
  type MinMaxType (CollectErrors es  a) MPBall =
    CollectErrors es (MinMaxType a MPBall)
  min = CE.lift1T min
  max = CE.lift1T max

{- intersection -}

instance CanIntersectAsymmetric MPBall MPBall where
  intersect a b
    | l > r =
        CN.noValueNumErrorCertain $ CN.NumError $ "intersect: empty intersection: " ++ show a ++ "; " ++ show b
    | otherwise = cn $ setPrecision p $ fromMPFloatEndpoints l r
    where
    p  = getPrecision a
    l = max aL bL
    r = min aR bR
    (aL,aR) = endpoints a
    (bL,bR) = endpoints b

intersectCNMPBall :: CN MPBall -> CN MPBall -> CN MPBall
intersectCNMPBall = intersect
  -- case (fst $ ensureNoCN x, fst $ ensureNoCN y) of 
  --   (Nothing, Nothing) -> x
  --   (Just _ , Nothing) -> x
  --   (Nothing, Just _ ) -> y
  --   (Just _ , Just _ ) -> lift2CE intersect x y

instance
  (CanIntersectAsymmetric MPBall b
  , CanBeErrors es)
  =>
  CanIntersectAsymmetric MPBall (CollectErrors es b)
  where
  type IntersectionType MPBall (CollectErrors es b) =
    CollectErrors es (IntersectionType MPBall b)
  intersect = CE.liftT1 intersect

instance
  (CanIntersectAsymmetric a MPBall
  , CanBeErrors es)
  =>
  CanIntersectAsymmetric (CollectErrors es a) MPBall
  where
  type IntersectionType (CollectErrors es  a) MPBall =
    CollectErrors es (IntersectionType a MPBall)
  intersect = CE.lift1T intersect

{- hull -}

hullMPBall :: MPBall -> MPBall -> MPBall
hullMPBall a b = 
  fromEndpoints rL rR
  where
  rL = min aL bL
  rR = max aR bR
  (aL,aR) = endpoints a
  (bL,bR) = endpoints b

{- union -}

instance CanUnionAsymmetric MPBall MPBall where
  union a b =
    case getMaybeValue (a `intersect` b) of
      Just _ -> CN.prependErrorPotential err r
      _ -> CN.prependErrorCertain err r
    where
    err = CN.NumError $ "union of enclosures: not enclosing the same value"
    r = cn $ hullMPBall a b


instance
  (CanUnionAsymmetric MPBall b
  , CanBeErrors es)
  =>
  CanUnionAsymmetric MPBall (CollectErrors es b)
  where
  type UnionType MPBall (CollectErrors es b) =
    CollectErrors es (UnionType MPBall b)
  union = CE.liftT1 union

instance
  (CanUnionAsymmetric a MPBall
  , CanBeErrors es)
  =>
  CanUnionAsymmetric (CollectErrors es a) MPBall
  where
  type UnionType (CollectErrors es  a) MPBall =
    CollectErrors es (UnionType a MPBall)
  union = CE.lift1T union

{-|
  Compute an MPBall function from *exact* MPFloat operations on interval endpoints.
  This works only for *non-decreasing* operations, eg addition, min, max.
-}
byEndpointsMP ::
    (MPFloat -> MPFloat -> MPFloat) ->
    (MPBall -> MPBall -> MPBall)
byEndpointsMP op b1 b2 =
    fromEndpoints (l1 `op` l2) (r1 `op` r2)
    where
    (l1,r1) = endpoints b1
    (l2,r2) = endpoints b2

{-  random generation -}
