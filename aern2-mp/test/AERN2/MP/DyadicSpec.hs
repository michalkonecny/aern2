{-|
    Module      :  AERN2.MP.DyadicSpec
    Description :  hspec tests for Dyadic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module AERN2.MP.DyadicSpec (spec) where

-- import Numeric.MixedTypes
import AERN2.MP.Dyadic

import Test.Hspec

spec :: Spec
spec = specDyadic
