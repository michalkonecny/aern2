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
  Interval(..), singleton, endpoints, fromEndpoints
  , width, split, contains
  , DyadicInterval, CanBeDyadicInterval, dyadicInterval
  , RealInterval, CanBeRealInterval, realInterval
)
where

import Numeric.MixedTypes
import qualified Prelude as P
import Text.Printf

import GHC.Generics
import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.MP.Dyadic
import AERN2.MP.Ball hiding (contains, endpoints, fromEndpoints)
import qualified AERN2.MP.Ball as MPBall

import AERN2.Real

data Interval l r = Interval l r
  deriving (P.Eq, Generic)

instance (Show l, Show r) => Show (Interval l r) where
    show (Interval l r) = printf "[%s,%s]" (show l) (show r)

singleton :: a -> Interval a a
singleton a = Interval a a

endpoints :: Interval l r -> (l,r)
endpoints (Interval l r) = (l,r)

fromEndpoints :: (l,r) -> Interval l r
fromEndpoints (l,r) = Interval l r

width :: (CanSub r l) => Interval l r -> SubType r l
width (Interval l r) = r - l

split ::
  (CanAddSameType t, CanMulBy t Dyadic)
  =>
  (Interval t t) -> (Interval t t, Interval t t)
split (Interval l r) = (Interval l m, Interval m r)
  where
  m = (l + r)*(dyadic 0.5)

contains ::
  (HasOrderAsymmetric l l',  OrderCompareType l l' ~ Bool,
  HasOrderAsymmetric r' r,  OrderCompareType r' r ~ Bool)
  =>
  Interval l r -> Interval l' r' -> Bool
contains (Interval l r) (Interval l' r') =
  l <= l' && r' <= r

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

instance ConvertibleExactly MPBall DyadicInterval where
  safeConvertExactly ball =
    Right $ Interval (centre l) (centre r)
    where
    (l,r) = MPBall.endpoints ball

instance ConvertibleExactly DyadicInterval MPBall where
  safeConvertExactly (Interval lD rD) =
    Right $ MPBall.fromEndpoints (mpBall lD) (mpBall rD)

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
