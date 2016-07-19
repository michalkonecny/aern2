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

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Data.Ratio
-- import Text.Printf

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

-- import AERN2.Norm
import AERN2.MP.Precision

import AERN2.MP.Float as MPFloat

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()
import AERN2.MP.Ball.Comparisons ()
import AERN2.MP.Ball.Field ()
import AERN2.MP.Ball.Elementary ()

instance Arbitrary MPBall where
  arbitrary =
    do
      c <- finiteMPFloat
      e <- arbitrary
      return (MPBall c e)
    where
      finiteMPFloat =
        do
          x <- arbitrary
          if (-MPFloat.infinity) < x && x < MPFloat.infinity
            then return x
            else finiteMPFloat

{-|
  A runtime representative of type @MPFloat@.
  Used for specialising polymorphic tests to concrete types.
-}
tMPBall :: T MPBall
tMPBall = T "MPBall"

specMPBall :: Spec
specMPBall =
  describe ("MPBall") $ do
    specCanSetPrecision tMPBall (?==?)
    specCanNegNum tMPBall
    specCanAbs tMPBall
    specCanMinMaxNotMixed tMPBall
    specCanMinMax tMPBall tInteger tMPBall
