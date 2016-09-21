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
    specCauchyReal, tCauchyReal, tCauchyRealAtAccuracy
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
import AERN2.MP.Accuracy
--
import qualified AERN2.MP.Ball.Type as MB
import AERN2.MP.Dyadic
import AERN2.MP.Float

import AERN2.Real.Type
import AERN2.Real.Comparison (CauchyRealAtAccuracy(..))
import AERN2.Real.Ring ()
import AERN2.Real.Field ()
import AERN2.Real.Elementary ()

instance Arbitrary CauchyRealAtAccuracy where
  arbitrary =
    CauchyRealAtAccuracy <$>
      arbitrary <*>
      ((\x -> bits (1 + (abs (3 * x)))) <$> (arbitrary :: Gen Integer))

instance Arbitrary CauchyReal where
  arbitrary =
    frequency
      [(int 1, real <$> (arbitrary :: Gen Integer)),
       (int 1, real <$> (arbitrary :: Gen Rational)),
       (int 2, (*) <$> (arbitrary :: Gen Integer) <*> arbitrarySignedBinary)
      ]
      where
      arbitrarySignedBinary =
        signedBinary2Real <$> infiniteListOf (elements [-1,0,1])
      signedBinary2Real sbits =
        newCR "random" [] $ \ ac ->
          balls !! ((fromAccuracy ac) + 1)
        where
        balls = nextBit (MB.mpBall (0,1)) sbits
        nextBit ball (sbit:rest) =
          ball : nextBit newBall rest
          where
          newBall =
            case sbit of
              (-1) -> MB.fromEndpointsMP l m
              0 -> MB.fromEndpointsMP l2 r2
              1 -> MB.fromEndpointsMP m r
              _ -> error "in Arbitrary CauchyReal"
          (l,r) = MB.endpointsMP ball
          m = mpFloat mDy
          l2 = mpFloat l2Dy
          r2 = mpFloat r2Dy
          mDy = ((dyadic l) + (dyadic r)) * 0.5
          l2Dy = ((dyadic l) + mDy) * 0.5
          r2Dy = ((dyadic r) + mDy) * 0.5
        nextBit _ _ = error "in Arbitrary CauchyReal"

{-|
  A runtime representative of type @CauchyReal@.
  Used for specialising polymorphic tests to concrete types.
-}
tCauchyReal :: T CauchyReal
tCauchyReal = T "CauchyReal"

tCauchyRealAtAccuracy :: T CauchyRealAtAccuracy
tCauchyRealAtAccuracy = T "CauchyReal(ac)"

specCauchyReal :: Spec
specCauchyReal =
  describe ("CauchyReal") $ do
    undefined
    -- specConversion tInteger tCauchyReal real (fst . integerBounds)
    -- describe "order" $ do
    --   specHasEqNotMixed tCauchyReal
    --   specHasEq tInt tCauchyReal tRational
    --   specCanPickZero tCauchyReal
    --   specHasOrderNotMixed tCauchyReal
    --   specHasOrder tInt tCauchyReal tRational
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
