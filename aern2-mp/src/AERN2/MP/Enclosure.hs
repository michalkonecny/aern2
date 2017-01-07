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
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Test.Hspec
import Test.QuickCheck

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

class CanTestContains d e where
  contains :: d -> e -> Bool

class CanMapInside d e where
  mapInside :: d -> e -> e

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
