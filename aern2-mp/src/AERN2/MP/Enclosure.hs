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
  IsBall(..)
  , IsInterval(..), intervalFunctionByEndpoints, intervalFunctionByEndpointsUpDown
  , CanTestContains(..), CanMapInside(..), specCanMapInside
  , CanIntersectAssymetric(..), CanIntersect
  , CanIntersectCNBy, CanIntersectCNSameType
  , CanUnionAssymetric(..), CanUnion
  , CanUnionCNBy, CanUnionCNSameType
  )
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Test.Hspec
import Test.QuickCheck

import Control.CollectErrors

import AERN2.MP.ErrorBound

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
  makeExactCentre :: (IsBall t) => t -> t
  makeExactCentre v =
    updateRadius (+r) c
    where
    (c, r) = centreAsBallAndRadius v

{- interval-specific operations -}

class IsInterval i e where
  fromEndpoints :: e -> e -> i
  endpoints :: i -> (e,e)

{-|
    Computes a *monotone* ball function @f@ on intervals using the interval endpoints.
-}
intervalFunctionByEndpoints ::
  (IsInterval t t, HasEqCertainly t t)
  =>
  (t -> t) {-^ @fThin@: a version of @f@ that works well on thin intervals -} ->
  (t -> t) {-^ @f@ on *large* intervals -}
intervalFunctionByEndpoints fThin x
  | l !==! u = fThin l
  | otherwise = fromEndpoints (fThin l) (fThin u)
  where
  (l,u) = endpoints x

{-|
    Computes a *monotone* ball function @f@ on intervals using the interval endpoints.
-}
intervalFunctionByEndpointsUpDown ::
  (IsInterval t e)
  =>
  (e -> e) {-^ @fDown@: a version of @f@ working on endpoints, rounded down -} ->
  (e -> e) {-^ @fUp@: a version of @f@ working on endpoints, rounded up -} ->
  (t -> t) {-^ @f@ on intervals rounding *outwards* -}
intervalFunctionByEndpointsUpDown fDown fUp x =
  fromEndpoints (fDown l) (fUp u)
  where
  (l,u) = endpoints x


{- containment -}

class CanTestContains dom e where
  {-| Test if @e@ is inside @dom@. -}
  contains :: dom {-^ @dom@ -} -> e  {-^ @e@ -} -> Bool

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
  (CanIntersectAssymetric e1 e2, CanIntersectAssymetric e1 e2
  , IntersectionType e1 e2 ~ IntersectionType e2 e1)

class CanIntersectAssymetric e1 e2 where
  type IntersectionType e1 e2
  type IntersectionType e1 e2 = EnsureCN e1
  intersect :: e1 -> e2 -> IntersectionType e1 e2

type CanIntersectCNBy e1 e2 =
  (CanIntersect e1 e2, IntersectionType e1 e2 ~ EnsureCN e1
  , CanIntersect (EnsureCN e1) e2, IntersectionType (EnsureCN e1) e2 ~ EnsureCN e1)
type CanIntersectCNSameType e1 =
  (CanIntersectCNBy e1 e1
  , CanIntersect (EnsureCN e1) (EnsureCN e1), IntersectionType (EnsureCN e1) (EnsureCN e1) ~ EnsureCN e1)

instance CanIntersectAssymetric Bool Bool where
  intersect b1 b2
    | b1 == b2 = cn b1
    | otherwise =
      noValueNumErrorCertainCN $ NumError "empty Boolean intersection"

instance
  (CanIntersectCNSameType a, CanEnsureCN a)
  =>
  CanIntersectAssymetric (Maybe a) (Maybe a)
  where
  type IntersectionType (Maybe a) (Maybe a) = EnsureCN (Maybe a)
  intersect (ma :: Maybe a) mb =
    case (ma, mb) of
      (Just a, Just b) -> justCN sample_a (intersect a b)
      (Just a, Nothing) -> justCN sample_a (ensureCN a)
      (Nothing, Just b) -> justCN sample_a (ensureCN b)
      _ -> cn (Nothing :: Maybe a)
    where
    sample_a = Nothing :: Maybe a

justCN :: (CanEnsureCN a) => Maybe a -> EnsureCN a -> EnsureCN (Maybe a)
justCN (_sample_a :: Maybe a) aCN =
  case deEnsureCN aCN of
    Right a -> cn (Just (a :: a))
    _ -> cn (Nothing :: Maybe a)


-- --- Version that removes inner CN:
-- instance
--   (CanIntersectCNSameType a, CanEnsureCN a)
--   =>
--   CanIntersectAssymetric (Maybe a) (Maybe a)
--   where
--   type IntersectionType (Maybe a) (Maybe a) = CN (Maybe (WithoutCN (IntersectionType a a)))
--   intersect (Just a) (Just b) = fmap Just (intersect a b)
--   intersect (Just a) Nothing = fmap Just (ensureCN a)
--   intersect Nothing (Just b) = fmap Just (ensureCN b)
--   intersect Nothing Nothing = cn Nothing
--
instance
  (CanIntersectAssymetric e1 e2, SuitableForCE es
  , CanEnsureCE es e1, CanEnsureCE es e2
  , CanEnsureCE es (IntersectionType e1 e2))
  =>
  CanIntersectAssymetric (CollectErrors es e1) (CollectErrors es e2)
  where
  type IntersectionType (CollectErrors es e1) (CollectErrors es e2) =
    EnsureCE es (IntersectionType e1 e2)
  intersect = lift2CE intersect

{- union -}

type CanUnion e1 e2 =
  (CanUnionAssymetric e1 e2, CanUnionAssymetric e1 e2
  , UnionType e1 e2 ~ UnionType e2 e1)

class CanUnionAssymetric e1 e2 where
  type UnionType e1 e2
  type UnionType e1 e2 = EnsureCN e1
  union :: e1 -> e2 -> UnionType e1 e2

type CanUnionCNBy e1 e2 =
  (CanUnion e1 e2, UnionType e1 e2 ~ EnsureCN e1
  , CanUnion (EnsureCN e1) e2, UnionType (EnsureCN e1) e2 ~ EnsureCN e1)

type CanUnionCNSameType e1 =
  (CanUnionCNBy e1 e1
  , CanUnion (EnsureCN e1) (EnsureCN e1), UnionType (EnsureCN e1) (EnsureCN e1) ~ EnsureCN e1)

instance
  (CanUnionAssymetric e1 e2, SuitableForCE es
  , CanEnsureCE es e1, CanEnsureCE es e2
  , CanEnsureCE es (UnionType e1 e2))
  =>
  CanUnionAssymetric (CollectErrors es e1) (CollectErrors es e2)
  where
  type UnionType (CollectErrors es e1) (CollectErrors es e2) =
    EnsureCE es (UnionType e1 e2)
  union = lift2CE union

instance (CanUnionCNSameType t, CanEnsureCN t) =>
  HasIfThenElse (Maybe Bool) t
  where
  type IfThenElseType (Maybe Bool) t = EnsureCN t
  ifThenElse (Just b) e1 e2 = cn $ if b then e1 else e2
  ifThenElse Nothing e1 e2 = e1 `union` e2
