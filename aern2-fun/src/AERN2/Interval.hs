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
  Interval(..), singletonInterval, intervalWidth, splitInterval,
  DyadicInterval, CanBeDyadicInterval, dyadicInterval
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
import Text.Printf

import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.MP.Dyadic
import AERN2.MP.Ball

data Interval l r = Interval l r

instance (Show l, Show r) => Show (Interval l r) where
    show (Interval l r) = printf "[%s,%s]" (show l) (show r)

singletonInterval :: a -> Interval a a
singletonInterval a = Interval a a

intervalWidth :: (CanSub r l) => Interval l r -> SubType r l
intervalWidth (Interval l r) = r - l

splitInterval ::
  (CanAddSameType t, CanMulBy t Dyadic)
  =>
  (Interval t t) -> (Interval t t, Interval t t)
splitInterval (Interval l r) = (Interval l m, Interval m r)
  where
  m = (l + r)*(dyadic 0.5)

type DyadicInterval = Interval Dyadic Dyadic
type CanBeDyadicInterval t = ConvertibleExactly t DyadicInterval

dyadicInterval :: (CanBeDyadicInterval t) => t -> DyadicInterval
dyadicInterval = convertExactly

instance
  (CanBeDyadic l, CanBeDyadic r, HasOrder l r, Show l, Show r,
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
    (l,r) = endpoints ball

instance ConvertibleExactly DyadicInterval MPBall where
  safeConvertExactly (Interval lD rD) =
    Right $ fromEndpoints (mpBall lD) (mpBall rD)
