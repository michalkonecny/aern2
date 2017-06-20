{-|
    Module      :  Values
    Description :  common functions for MPBall benchmarks
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Values where

import MixedTypesNumPrelude
-- import Prelude

import Test.QuickCheck

import Control.DeepSeq

import AERN2.Utils.Bench

import AERN2.MP.Float
import AERN2.MP.Ball
import AERN2.MP.Ball.Tests () -- instance Arbitrary MPBall

ballsExactPositive :: [MPBall]
ballsExactPositive = filter (!>! 0) ballsExact

ballsSmallExact :: [MPBall]
ballsSmallExact = map centreAsBall ballsSmall

ballsExact :: [MPBall]
ballsExact = map centreAsBall balls

ballsSmall :: [MPBall]
ballsSmall = map makeSmall balls
  where
  makeSmall :: MPBall -> MPBall
  makeSmall b
    | (abs b) !<! 1000000 = b
    | otherwise = 100000 * (b/(1000000+(abs $ centreAsBall b)))

ballsPositive :: [MPBall]
ballsPositive = filter (!>! 0) balls

balls :: [MPBall]
balls = listFromGen arbitrary

instance NFData MPFloat where rnf x = rnf $ x > 0
instance NFData ErrorBound where rnf = rnf . mpFloat
instance NFData MPBall
