{-|
    Module      :  AERN2.RealSpec
    Description :  hspec tests for CauchyReal
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module AERN2.RealSpec (spec) where

-- import MixedTypesNumPrelude
import AERN2.Real.Tests

import Test.Hspec

spec :: Spec
spec = specCReal
