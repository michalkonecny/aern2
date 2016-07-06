{-|
    Module      :  Numeric.MixedType.LiteralsSpec
    Description :  hspec tests for Literals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}

module Numeric.MixedTypes.LiteralsSpec (spec) where

import Numeric.MixedTypes
import qualified Prelude as P

import Control.Exception (evaluate)

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "numeric conversions" $ do
    it "convert int to integer and back" $ do
      property $ \ x -> (int $ integer x) P.== x
    it "throws exception when converting large integer to int" $ do
      (evaluate $ int (integer (maxBound :: Int) P.+ 1)) `shouldThrow` anyException
    it "convert int to rational and back" $ do
      property $ \ x -> (round $ rational x) P.== (x :: Int)
    it "convert integer to rational and back" $ do
      property $ \ x -> (round $ rational x) P.== (x :: Integer)
    it "convert double to rational and back" $ do
      property $ \ x -> (double $ toRational x) P.== (x :: Double)
