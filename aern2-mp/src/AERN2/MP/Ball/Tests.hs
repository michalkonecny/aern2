{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.Ball.Tests
    Description :  Tests for operations on arbitrary precision balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for operations on arbitrary precision balls.

    To run the tests using stack, execute:

    @
    stack test aern2-mp --test-arguments "-a 1000 -m MPBall"
    @
-}
module AERN2.MP.Ball.Tests
  (
    specMPBall, tMPBall
  )
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Data.Ratio
-- import Text.Printf

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

-- import AERN2.Norm
import AERN2.MP.Precision

import AERN2.MP.Ball.Type
-- import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons ()
import AERN2.MP.Ball.Field ()
import AERN2.MP.Ball.Elementary ()

instance Arbitrary MPBall where
  arbitrary =
    do
      c <- finiteMPFloat
      e <- smallEB
      return (reducePrecionIfInaccurate $ MPBall c e)
    where
      smallEB =
        do
          e <- arbitrary
          if (mpBall e) !<! 10
            then return e
            else smallEB
      finiteMPFloat =
        do
          x <- arbitrary
          if isFinite x
            then return x
            else finiteMPFloat

{-|
  A runtime representative of type @MPBall@.
  Used for specialising polymorphic tests to concrete types.
-}
tMPBall :: T MPBall
tMPBall = T "MPBall"

-- tCNMPBall :: T (CN MPBall)
-- tCNMPBall = T "(CN MPBall)"

specMPBall :: Spec
specMPBall =
  describe ("MPBall") $ do
    specCanSetPrecision tMPBall (printArgsIfFails2 "`contains`" contains)
    specConversion tInteger tMPBall mpBall (fst . integerBounds)
    describe "order" $ do
      specHasEqNotMixed tMPBall
      specHasEq tInt tMPBall tRational
      specCanTestZero tMPBall
      specHasOrderNotMixed tMPBall
      specHasOrder tInt tMPBall tRational
    describe "min/max/abs" $ do
      specCanNegNum tMPBall
      specCanAbs tMPBall
      specCanMinMaxNotMixed tMPBall
      specCanMinMax tMPBall tInteger tMPBall
    describe "ring" $ do
      specCanAddNotMixed tMPBall
      specCanAddSameType tMPBall
      specCanAdd tInt tMPBall tRational
      specCanAdd tInteger tMPBall tInt
      specCanSubNotMixed tMPBall
      specCanSub tMPBall tInteger
      specCanSub tInteger tMPBall
      specCanSub tMPBall tInt
      specCanSub tInt tMPBall
      specCanMulNotMixed tMPBall
      specCanMulSameType tMPBall
      specCanMul tInt tMPBall tRational
      -- specCanPow tMPBall tInteger
    describe "field" $ do
      specCanDivNotMixed tMPBall
      specCanDiv tInteger tMPBall
      specCanDiv tMPBall tInt
      specCanDiv tMPBall tRational
    describe "elementary" $ do
      specCanExpReal tMPBall
      specCanLogReal tMPBall
      specCanSqrtReal tMPBall
      specCanSinCosReal tMPBall
