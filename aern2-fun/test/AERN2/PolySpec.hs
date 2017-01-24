{-|
    Module      :  AERN2.RealSpec
    Description :  hspec tests for CauchyReal
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module AERN2.PolySpec (spec) where

-- import Numeric.MixedTypes
import AERN2.Poly.Cheb.Tests

import Test.Hspec

spec :: Spec
spec = specChPoly
