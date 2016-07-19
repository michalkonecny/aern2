{-|
    Module      :  AERN2.MP.BallSpec
    Description :  hspec tests for MPBall
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module AERN2.MP.BallSpec (spec) where

-- import Numeric.MixedTypes
import AERN2.MP.Ball.Tests

import Test.Hspec

spec :: Spec
spec = specMPBall
