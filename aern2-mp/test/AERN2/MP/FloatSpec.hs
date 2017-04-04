{-|
    Module      :  AERN2.MP.FloatSpec
    Description :  hspec tests for MPFloat
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module AERN2.MP.FloatSpec (spec) where

-- import Numeric.MixedTypes
import AERN2.MP.Float.Tests

import Test.Hspec

spec :: Spec
spec = specMPFloat
