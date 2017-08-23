{-|
    Module      :  AERN2.MP.FloatSpec
    Description :  hspec tests for MPFloat
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module AERN2.MP.UseMPFR.FloatSpec (spec) where

-- import MixedTypesNumPrelude
import AERN2.MP.UseMPFR.Float.Tests

import Test.Hspec

spec :: Spec
spec = specMPFloat
