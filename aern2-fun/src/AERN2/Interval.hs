{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Interval
    Description :  Intervals for use as function domains
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Intervals for use as function domains
-}

module AERN2.Interval
(
  Interval(..), singleton
  , width, split
  , arbitraryNonEmptyInterval
  , arbitraryNonEmptySmallInterval
  , intersect, intersects
  , DyadicInterval, CanBeDyadicInterval, dyadicInterval
  , RealInterval, CanBeRealInterval, realInterval
)
where

import MixedTypesNumPrelude
import qualified Prelude as P
import Text.Printf
-- import Text.Regex.TDFA

import Data.Maybe

import GHC.Generics
import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
import Test.QuickCheck



import AERN2.MP.Dyadic
import AERN2.MP.Ball hiding (intersect)
import qualified AERN2.MP.Ball as MPBall

import AERN2.Real

{- type -}

data Interval l r = Interval { endpointL :: l, endpointR :: r }
  deriving (P.Eq, Generic)

instance (Show l, Show r) => Show (Interval l r) where
    show (Interval l r) =
      printf "Interval (%s) (%s)" (show l) (show r)
      -- printf "[%s,%s]" (show l) (show r)

instance (Read l, Read r) => Read (Interval l r) where
  readsPrec _pr intervalS
    | prefix1 == "Interval (" =
      case reads afterP1 of
        [(l,afterL)] ->
          if prefix2 == ") ("
            then
              case reads afterP2 of
                [(r,')':rest)] -> [(Interval l r, rest)]
                _ -> []
            else []
            where
            (prefix2, afterP2) = splitAt (length ") (") afterL
        _ -> []
    | otherwise = []
    where
    (prefix1, afterP1) = splitAt (length "Interval (") intervalS

singleton :: a -> Interval a a
singleton a = Interval a a

instance IsInterval (Interval e e) e where
  fromEndpoints l r = Interval l r
  endpoints (Interval l r) = (l,r)

width :: (CanSub r l) => Interval l r -> SubType r l
width (Interval l r) = r - l

split ::
  (CanAddSameType t, CanMulBy t Dyadic)
  =>
  (Interval t t) -> (Interval t t, Interval t t)
split (Interval l r) = (Interval l m, Interval m r)
  where
  m = (l + r)*(dyadic 0.5)

instance
  (Arbitrary l, Arbitrary r, HasOrderCertainlyAsymmetric l r)
  =>
  Arbitrary (Interval l r)
  where
  arbitrary =
    do
    l <- arbitrary
    r <- arbitrary
    if l !<=! r then return (Interval l r) else arbitrary

arbitraryNonEmptyInterval ::
  (Arbitrary l, Arbitrary r, HasOrderCertainlyAsymmetric l r)
  =>
  Gen (Interval l r)
arbitraryNonEmptyInterval =
  do
  l <- arbitrary
  r <- arbitrary
  if l !<! r then return (Interval l r) else arbitraryNonEmptyInterval

arbitraryNonEmptySmallInterval ::
  (Arbitrary e, CanAddThis e Integer)
  =>
  Gen (Interval e e)
arbitraryNonEmptySmallInterval =
  do
  l <- arbitrary
  w <- growingElements [1..10]
  return (Interval l (l+w))

{- containment -}

instance
  (HasOrderAsymmetric l l',  OrderCompareType l l' ~ Bool,
  HasOrderAsymmetric r' r,  OrderCompareType r' r ~ Bool)
  =>
  CanTestContains (Interval l r) (Interval l' r')
  where
  contains (Interval l r) (Interval l' r') =
    l <= l' && r' <= r

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|
    instance
      (HasOrderAsymmetric l $t,  OrderCompareType l $t ~ Bool,
      HasOrderAsymmetric $t r,  OrderCompareType $t r ~ Bool)
      =>
      CanTestContains (Interval l r) $t
      where
      contains (Interval l r) e = l <= e && e <= r
  |]))

instance
  (CanSubSameType e, CanAddSubMulBy t e
  , HasIntegerBounds t, CanSubThis t Integer, CanDivBy t Integer)
  =>
  CanMapInside (Interval e e) t
  where
  mapInside (Interval l r) x =
    l + xUnit * (r - l)
    where
    xUnit = (x - xL) / (max 1 $ xU - xL)
    (xL,xU) = integerBounds x

{- intersection -}

instance
  (CanMinMaxSameType l, CanMinMaxSameType r, HasOrderCertainly l r)
  =>
  CanIntersectAssymetric (Interval l r) (Interval l r)
  where
  type IntersectionType (Interval l r) (Interval l r) = CN (Interval l r)
  intersect (Interval l1 r1) (Interval l2 r2)
    | l !<=! r = pure (Interval l r)
    | l !>! r = noValueNumErrorCertainCN err
    | otherwise = prependErrorsCN [(ErrorPotential, err)] $ pure (Interval l r)
    where
    l = l1 `max` l2
    r = r1 `min` r2
    err = NumError "empty intersection"

intersects ::
  (CanMinMaxSameType l, CanMinMaxSameType r, HasOrderCertainly l r)
  =>
  Interval l r -> Interval l r -> Bool
intersects i1 i2 = null $ getErrorsCN $ intersect i1 i2

{- comparison -}

instance
  (HasEqAsymmetric l1 l2, HasEqAsymmetric r1 r2
  , EqCompareType l1 l2 ~ EqCompareType r1 r2
  , CanAndOrSameType (EqCompareType l1 l2))
  =>
  HasEqAsymmetric (Interval l1 r1) (Interval l2 r2)
  where
  type EqCompareType (Interval l1 r1) (Interval l2 r2) = EqCompareType l1 l2
  equalTo (Interval l1 r1) (Interval l2 r2) =
    (l1 == l2) && (r1 == r2)

{- Dyadic intervals -}

type DyadicInterval = Interval Dyadic Dyadic
type CanBeDyadicInterval t = ConvertibleExactly t DyadicInterval

dyadicInterval :: (CanBeDyadicInterval t) => t -> DyadicInterval
dyadicInterval = convertExactly

instance
  (CanBeDyadic l, CanBeDyadic r, HasOrderCertainly l r, Show l, Show r,
   Typeable l, Typeable r)
  =>
  ConvertibleExactly (l, r) DyadicInterval where
  safeConvertExactly (l,r)
    | l !<=! r = Right $ Interval (dyadic l) (dyadic r)
    | otherwise = convError "endpoints are not in the correct order" (l,r)

instance ConvertibleExactly Dyadic DyadicInterval where
  safeConvertExactly d =
    Right $ Interval d d

instance ConvertibleExactly Integer DyadicInterval where
  safeConvertExactly n =
    do
    nD <- safeConvertExactly n
    Right $ Interval nD nD

instance ConvertibleExactly Rational DyadicInterval where
  safeConvertExactly r =
    do
    rD <- safeConvertExactly r
    Right $ Interval rD rD

instance ConvertibleExactly MPBall DyadicInterval where
  safeConvertExactly ball =
    Right $ Interval (centre l) (centre r)
    where
    l,r :: MPBall
    (l,r) = endpoints ball

instance ConvertibleExactly DyadicInterval MPBall where
  safeConvertExactly (Interval lD rD) =
    Right $ MPBall.fromEndpoints (mpBall lD) (mpBall rD)

{- CauchyReal intervals -}

type RealInterval = Interval CauchyReal CauchyReal
type CanBeRealInterval t = ConvertibleExactly t RealInterval

realInterval :: (CanBeRealInterval t) => t -> RealInterval
realInterval = convertExactly

instance
  (CanBeReal l, CanBeReal r, HasOrderCertainly l r, Show l, Show r,
   Typeable l, Typeable r)
  =>
  ConvertibleExactly (l, r) RealInterval where
  safeConvertExactly (l,r)
    | l !<=! r = Right $ Interval (real l) (real r)
    | otherwise = convError "endpoints are not in the correct order" (l,r)
