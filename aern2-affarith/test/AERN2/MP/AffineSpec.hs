-- |
--    Module      :  AERN2.MP.BallSpec
--    Description :  hspec tests for MPBall
--    Copyright   :  (c) Michal Konecny
--    License     :  BSD3
--
--    Maintainer  :  mikkonecny@gmail.com
--    Stability   :  experimental
--    Portability :  portable
module AERN2.MP.AffineSpec (spec) where

-- import MixedTypesNumPrelude
import AERN2.MP.Affine.Tests (specMPAffine)
import Test.Hspec

spec :: Spec
spec = specMPAffine
