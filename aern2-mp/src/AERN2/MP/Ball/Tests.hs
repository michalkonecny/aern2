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
import AERN2.MP.Dyadic (Dyadic, dyadic)

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
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
    specMPBallPDyadic
    describe "order" $ do
      specHasEqNotMixed tMPBall
      specHasEq tInt tMPBall tRational
      specCanTestZero tMPBall
      specHasOrderNotMixed tMPBall
      specHasOrder tInt tMPBall tRational
    describe "min/max/abs" $ do
      specCanNegNum tMPBall
      specResultIsValid1 abs "abs" tMPBall
      specCanAbs tMPBall
      specResultIsValid2 min "min" tMPBall tMPBall
      specResultIsValid2 max "max" tMPBall tMPBall
      specCanMinMaxNotMixed tMPBall
      specCanMinMax tMPBall tInteger tMPBall
    describe "ring" $ do
      specResultIsValid2 add "add" tMPBall tMPBall
      specCanAddNotMixed tMPBall
      specCanAddSameType tMPBall
      specCanAdd tInt tMPBall tRational
      specCanAdd tInteger tMPBall tInt
      specResultIsValid2 sub "sub" tMPBall tMPBall
      specCanSubNotMixed tMPBall
      specCanSub tMPBall tInteger
      specCanSub tInteger tMPBall
      specCanSub tMPBall tInt
      specCanSub tInt tMPBall
      specResultIsValid2 mul "mul" tMPBall tMPBall
      specCanMulNotMixed tMPBall
      specCanMulSameType tMPBall
      specCanMul tInt tMPBall tRational
      -- specCanPow tMPBall tInteger
    describe "field" $ do
      specResultIsValid2Pre (\_ y -> isCertainlyNonZero y) divide "divide" tMPBall tMPBall
      specCanDivNotMixed tMPBall
      specCanDiv tInteger tMPBall
      specCanDiv tMPBall tInt
      specCanDiv tMPBall tRational
    describe "elementary" $ do
      specCanExpReal tMPBall
      specCanLogReal tMPBall
      specCanSqrtReal tMPBall
      specCanSinCosReal tMPBall

{-|
  @mpBallP p@ applied to a Dyadic must use precision @p@,
  not the (possibly much lower) precision of the Dyadic.
-}
specMPBallPDyadic :: Spec
specMPBallPDyadic =
  describe "mpBallP of Dyadic" $ do
    it "mpBallP (prec 100) (dyadic 1.5) has precision 100" $ do
      getPrecision (mpBallP (prec 100) (dyadic 1.5)) `shouldBe` (prec 100)
    it "sin (mpBallP (prec 100) (dyadic 1.5)) has accuracy at least 90 bits" $ do
      getAccuracy (sin (mpBallP (prec 100) (dyadic 1.5))) >= (bits 90) `shouldBe` True
    it "mpBallP p d has precision p and contains d" $ do
      property $ \(p :: Precision) (d :: Dyadic) ->
        let b = mpBallP p d
         in getPrecision b == p && b `contains` d
