{-|
    Module      :  AERN2.Real.Tests
    Description :  Tests for operations on cauchy real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for operations on cauchy real numbers.

    To run the tests using stack, execute:

    @
    stack test aern2-real --test-arguments "-a 1000 -m Real"
    @
-}

module AERN2.Real.Tests
  (
    specCauchyReal, tCauchyReal
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
-- import AERN2.MP.Accuracy
--
-- import AERN2.MP.Float as MPFloat

import AERN2.Real.Type
import AERN2.Real.Comparison ()
import AERN2.Real.Ring ()
import AERN2.Real.Field ()
import AERN2.Real.Elementary ()

instance Arbitrary CauchyReal where
  arbitrary =
    do
      undefined -- TODO
    --   c <- finiteMPFloat
    --   e <- arbitrary
    --   return (MPBall c e)
    -- where
    --   finiteMPFloat =
    --     do
    --       x <- arbitrary
    --       if (-MPFloat.infinity) < x && x < MPFloat.infinity
    --         then return x
    --         else finiteMPFloat

{-|
  A runtime representative of type @CauchyReal@.
  Used for specialising polymorphic tests to concrete types.
-}
tCauchyReal :: T CauchyReal
tCauchyReal = T "CauchyReal"

specCauchyReal :: Spec
specCauchyReal =
  describe ("CauchyReal") $ do
    undefined
    -- specConversion tInteger tCauchyReal real (fst . integerBounds)
    -- describe "order" $ do
    --   specHasTolerantEqNotMixed tCauchyReal
    --   specHasTolerantEq tInt tCauchyReal tRational
    --   specCanPickZero tCauchyReal
    --   specHasTolerantOrderNotMixed tCauchyReal
    --   specHasTolerantOrder tInt tCauchyReal tRational
    -- describe "min/max/abs" $ do
    --   specCRFastConvergent1 abs
    --   specCRFastConvergent2 max
    --   specCRFastConvergent2 min
    -- describe "ring" $ do
    --   specCRFastConvergent1 negate
    --   specCRFastConvergent2 add
    --   specCRFastConvergent2T tInteger add
    --   specCRFastConvergent2T tRational add
    --   specCRFastConvergent2T tDyadic add
    --   specCRFastConvergent2 sub
    --   specCRFastConvergent2T tInteger sub
    --   specCRFastConvergent2T tRational sub
    --   specCRFastConvergent2T tDyadic sub
    --   specCRFastConvergent2 mul
    --   specCRFastConvergent2T tInteger mul
    --   specCRFastConvergent2T tRational mul
    --   specCRFastConvergent2T tDyadic mul
    -- describe "field" $ do
    --   specCRFastConvergent2 div -- TODO: specCRFastConvergentN should use CatchingNumExceptions
    --   specCRFastConvergent2T tInteger div
    --   specCRFastConvergent2T tRational div
    --   specCRFastConvergent2T tDyadic div
    -- describe "elementary" $ do
    --   specCRFastConvergent1 sqrt
    --   specCRFastConvergent1 exp
    --   specCRFastConvergent1 log
    --   specCRFastConvergent2 pow
    --   specCRFastConvergent2T tInteger pow
    --   specCRFastConvergent2T tRational pow
    --   specCRFastConvergent2T tDyadic pow
    --   specCRFastConvergent1 cos
    --   specCRFastConvergent1 sin
