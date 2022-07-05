{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.Enclosure
    Description :  Enclosure operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Enclosure classes and operations.
-}
module AERN2.MP.Enclosure
(
  IsBall(..), ballFunctionUsingLipschitz
  , IsInterval(..), endpointL, endpointR
  , fromEndpointsAsIntervals, endpointsAsIntervals, endpointLAsInterval, endpointRAsInterval
  , intervalFunctionByEndpoints, intervalFunctionByEndpointsUpDown
  , CanPlusMinus(..), (+-)
  , CanTestContains(..), CanMapInside(..), specCanMapInside
  , CanIntersectAsymmetric(..), CanIntersect
  , CanIntersectBy, CanIntersectSameType
  , CanIntersectCNBy, CanIntersectCNSameType
  , CanUnionAsymmetric(..), CanUnion
  , CanUnionBy, CanUnionSameType
  , CanUnionCNBy, CanUnionCNSameType
  )
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import Control.Arrow

import Test.Hspec
import Test.QuickCheck

import qualified Numeric.CollectErrors as CN

import AERN2.Kleenean
import AERN2.MP.ErrorBound
import Control.CollectErrors (CollectErrors(getMaybeValue))
-- import AERN2.MP.Accuracy

{- ball-specific operations -}

class IsBall t where
  type CentreType t
  centre :: t -> CentreType t
  centreAsBallAndRadius :: t-> (t,ErrorBound)
  centreAsBall :: t -> t
  centreAsBall = fst . centreAsBallAndRadius
  radius :: t -> ErrorBound
  radius = snd . centreAsBallAndRadius
  updateRadius :: (ErrorBound -> ErrorBound) -> (t -> t)
  {-|  When the radius of the ball is implicitly contributed to by imprecision in the centre
     (eg if the centre is a polynomial with inexact coefficients), move all that imprecision
     to the explicit radius, making the centre exact.  This may lose some information,
     but as a ball is equivalent to the original.
     For MPBall this function is pointless because it is equivalent to the identity.  -}
  makeExactCentre :: t -> t
  makeExactCentre v =
    updateRadius (+r) c
    where
    (c, r) = centreAsBallAndRadius v

instance IsBall t => IsBall (CN t) where
    type CentreType (CN t) = CN (CentreType t)
    centre = fmap centre
    centreAsBall = fmap centreAsBall
    updateRadius f = fmap (updateRadius f)
    centreAsBallAndRadius = error $ "centreAsBallAndRadius not defined for CN types"

{-|
    Computes a ball function @f@ on the centre and updating the error bound using a Lipschitz constant.
-}
ballFunctionUsingLipschitz ::
  (IsBall t, HasEqCertainly t t)
  =>
  (t -> t) {-^ @fThin@: a version of @f@ that works well on thin balls -} ->
  (t -> ErrorBound) {-^ @fLip@: a Lipschitz function of @f@ over large balls -} ->
  (t -> t) {-^ @f@ on *large* balls -}
ballFunctionUsingLipschitz fThin fLip x
  | r == 0 = fThin c
  | otherwise = updateRadius (+ (fLip x)*r) (fThin c)
  where
  (c, r) = centreAsBallAndRadius x


{- interval-specific operations -}
class IsInterval i where
  type IntervalEndpoint i
  endpoints :: i -> (IntervalEndpoint i, IntervalEndpoint i)
  fromEndpoints :: IntervalEndpoint i -> IntervalEndpoint i -> i

instance (IsInterval t) => (IsInterval (CN t)) where
    type (IntervalEndpoint (CN t)) = CN (IntervalEndpoint t)
    fromEndpoints l u = CN.lift2 fromEndpoints l u
    endpoints = CN.liftPair endpoints

endpointL :: (IsInterval i) => i -> IntervalEndpoint i
endpointL = fst . endpoints

endpointR :: (IsInterval i) => i -> IntervalEndpoint i
endpointR = snd . endpoints

endpointsAsIntervals :: 
  (IsInterval i) => i -> (i,i)
endpointsAsIntervals x = (lI,rI)
  where
  lI = fromEndpoints l l
  rI = fromEndpoints r r
  (l,r) = endpoints x

endpointLAsInterval :: (IsInterval i) => i -> i
endpointLAsInterval = fst . endpointsAsIntervals

endpointRAsInterval :: (IsInterval i) => i -> i
endpointRAsInterval = snd . endpointsAsIntervals

fromEndpointsAsIntervals :: 
  (IsInterval i, CanMinMaxSameType (IntervalEndpoint i)) =>
  i -> i -> i
fromEndpointsAsIntervals l r = 
  fromEndpoints lMP uMP
  where
  lMP = min llMP rlMP
  uMP = max luMP ruMP
  (llMP, luMP) = endpoints l
  (rlMP, ruMP) = endpoints r

{- plusMinus (+-) operator -}

class CanPlusMinus t1 t2 where
  type PlusMinusType t1 t2
  type PlusMinusType t1 t2 = t1
  {-| Operator for constructing or enlarging enclosures such as balls or intervals -}
  plusMinus :: t1 -> t2 -> PlusMinusType t1 t2

infixl 6  +-

{-| Operator for constructing or enlarging enclosures such as balls or intervals -}
(+-) :: (CanPlusMinus t1 t2) => t1 -> t2 -> PlusMinusType t1 t2
(+-) = plusMinus


{-|
    Computes a *monotone* ball function @f@ on intervals using the interval endpoints.
-}
intervalFunctionByEndpoints ::
  (IsInterval t, CanMinMaxSameType (IntervalEndpoint t), HasEqCertainly t t)
  =>
  (t -> t) {-^ @fThin@: a version of @f@ that works well on thin intervals -} ->
  (t -> t) {-^ @f@ on *large* intervals -}
intervalFunctionByEndpoints fThin x
  | l !==! u = fThin l
  | otherwise = fromEndpointsAsIntervals (fThin l) (fThin u)
  where
  (l,u) = endpointsAsIntervals x

{-|
    Computes a *monotone* ball function @f@ on intervals using the interval endpoints.
-}
intervalFunctionByEndpointsUpDown ::
  (IsInterval t)
  =>
  (IntervalEndpoint t -> IntervalEndpoint t) {-^ @fDown@: a version of @f@ working on endpoints, rounded down -} ->
  (IntervalEndpoint t -> IntervalEndpoint t) {-^ @fUp@: a version of @f@ working on endpoints, rounded up -} ->
  (t -> t) {-^ @f@ on intervals rounding *outwards* -}
intervalFunctionByEndpointsUpDown fDown fUp x =
  fromEndpoints (fDown l) (fUp u)
  where
  (l,u) = endpoints x


{- containment -}

class CanTestContains dom e where
  {-| Test if @e@ is inside @dom@. -}
  contains :: dom {-^ @dom@ -} -> e  {-^ @e@ -} -> Bool

instance (CanTestContains dom e) => CanTestContains (CN dom) (CN e) where
  contains domCN aCN =

    case (getMaybeValue domCN, getMaybeValue aCN) of
      (Just dom, Just a) -> dom `contains` a
      _ -> False

class CanMapInside dom e where
  {-| Return some value contained in @dom@.
      The returned value does not have to equal the given @e@
      even if @e@ is already inside @dom@.
      All elements of @dom@ should be covered with roughly the same probability
      when calling this function for evenly distributed @e@'s.

      This function is intended mainly for generating values inside @dom@
      for randomised tests.
  -}
  mapInside :: dom {-^ @dom@ -} -> e  {-^ @e@ -} -> e

specCanMapInside ::
  (CanMapInside d e, CanTestContains d e
  , Arbitrary d, Arbitrary e, Show d, Show e)
  =>
  T d -> T e -> Spec
specCanMapInside (T dName :: T d) (T eName :: T e) =
  it ("CanMapInside " ++ dName ++ " " ++ eName) $ do
    property $
      \ (d :: d) (e :: e) ->
        contains d $ mapInside d e

{- intersection -}

type CanIntersect e1 e2 =
  (CanIntersectAsymmetric e1 e2, CanIntersectAsymmetric e1 e2
  , IntersectionType e1 e2 ~ IntersectionType e2 e1)

{-| A set intersection (usually partial) -}
class CanIntersectAsymmetric e1 e2 where
  type IntersectionType e1 e2
  type IntersectionType e1 e2 = CN e1
  intersect :: e1 -> e2 -> IntersectionType e1 e2

type CanIntersectBy e1 e2 =
  (CanIntersect e1 e2, IntersectionType e1 e2 ~ e1)

type CanIntersectSameType e1 =
  (CanIntersectBy e1 e1)

type CanIntersectCNBy e1 e2 =
  (CanIntersect e1 e2, IntersectionType e1 e2 ~ CN e1)

type CanIntersectCNSameType e1 =
  (CanIntersectCNBy e1 e1)

instance
  CanIntersectAsymmetric Bool Bool
  where
  intersect b1 b2
    | b1 == b2 = cn b1
    | otherwise =
      CN.noValueNumErrorCertain $ CN.NumError "empty Boolean intersection"

instance
  CanIntersectAsymmetric Kleenean Kleenean
  where
  intersect CertainTrue CertainFalse =
    CN.noValueNumErrorCertain $ CN.NumError "empty Kleenean intersection"
  intersect CertainFalse CertainTrue =
    CN.noValueNumErrorCertain $ CN.NumError "empty Kleenean intersection"
  intersect TrueOrFalse k2 = cn k2
  intersect k1 _ = cn k1

instance 
  (CanIntersectAsymmetric a b, IntersectionType a b ~ CN c)
  =>
  CanIntersectAsymmetric (CN a) (CN b) 
  where
  type IntersectionType (CN a) (CN b) = IntersectionType a b
  intersect = CN.lift2CN intersect

instance
  (CanIntersectAsymmetric (CN Bool) (CN b))
  =>
  CanIntersectAsymmetric Bool (CN b)
  where
  type IntersectionType Bool (CN b) = IntersectionType (CN Bool) (CN b)
  intersect b1 = intersect (cn b1)

instance
  (CanIntersectAsymmetric (CN a) (CN Bool))
  =>
  CanIntersectAsymmetric (CN a) Bool
  where
  type IntersectionType (CN a) Bool = IntersectionType (CN a) (CN Bool)
  intersect b1 b2 = intersect b1 (cn b2)

instance
  (CanIntersectAsymmetric (CN Kleenean) (CN b))
  =>
  CanIntersectAsymmetric Kleenean (CN b)
  where
  type IntersectionType Kleenean (CN b) = IntersectionType (CN Kleenean) (CN b)
  intersect k1 = intersect (cn k1)

instance
  (CanIntersectAsymmetric (CN a) (CN Kleenean))
  =>
  CanIntersectAsymmetric (CN a) Kleenean
  where
  type IntersectionType (CN a) Kleenean = IntersectionType (CN a) (CN Kleenean)
  intersect k1 k2 = intersect k1 (cn k2)

{- set union -}

type CanUnion e1 e2 =
  (CanUnionAsymmetric e1 e2, CanUnionAsymmetric e1 e2
  , UnionType e1 e2 ~ UnionType e2 e1)

{-| A set union (usually partial) -}
class CanUnionAsymmetric e1 e2 where
  type UnionType e1 e2
  type UnionType e1 e2 = CN e1
  union :: e1 -> e2 -> UnionType e1 e2

type CanUnionBy e1 e2 =
  (CanUnion e1 e2, UnionType e1 e2 ~ e1)

type CanUnionSameType e1 =
  (CanUnionBy e1 e1)

type CanUnionCNBy e1 e2 =
  (CanUnion e1 e2, UnionType e1 e2 ~ CN e1)

type CanUnionCNSameType e1 =
  (CanUnionCNBy e1 e1)

instance 
  (CanUnionAsymmetric a b, UnionType a b ~ CN c)
  =>
  CanUnionAsymmetric (CN a) (CN b) 
  where
  type UnionType (CN a) (CN b) = UnionType a b
  union = CN.lift2CN union

instance (CanUnionSameType t, CN.CanTakeCNErrors t) =>
  HasIfThenElse Kleenean t
  where
  type IfThenElseType Kleenean t = t
  ifThenElse CertainTrue e1 _  = e1
  ifThenElse CertainFalse _ e2 = e2
  ifThenElse TrueOrFalse e1 e2 = e1 `union` e2

