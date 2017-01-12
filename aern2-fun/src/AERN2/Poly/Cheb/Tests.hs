{-|
    Module      :  AERN2.Poly.Cheb.Tests
    Description :  Tests for Chebyshev-basis polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for Chebyshev-basis polynomials

    To run the tests using stack, execute:

    @
    stack test aern2-fun --test-arguments "-a 100 -m ChPoly"
    @
-}

module AERN2.Poly.Cheb.Tests
  (
    specChPoly, tChPolyMPBall
  )
where

import Debug.Trace

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Data.Ratio
-- import Text.Printf

import qualified Data.Set as Set

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

-- import Numeric.CatchingExceptions

import AERN2.MP
import AERN2.MP.Dyadic
import AERN2.MP.Ball.Tests

import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval ()
import AERN2.Poly.Cheb.Ring ()
-- import AERN2.Poly.Cheb.Field ()
-- import AERN2.Poly.Cheb.Maximum ()
-- import AERN2.Poly.Cheb.Integration ()

instance (Arbitrary c, IsBall c, Show c) => Arbitrary (ChPoly c) where
  arbitrary =
    do
    dom <- arbitraryNonEmptyInterval
    arbitraryWithDom dom

instance (Arbitrary c, IsBall c, Show c) => ArbitraryWithDom (ChPoly c) where
  arbitraryWithDom dom =
    do
    deg <- growingElements [0..200]
    termSize <- growingElements [0..deg]
    coeffs <- (map centreAsBall) <$> vector (int $ 1 + termSize)
    terms <-
      -- trace ("coeffs=" ++ show coeffs) $
      makeTerms coeffs termSize deg
    return $ ChPoly dom (Poly terms)
    where
    makeTerms coeffs termSize deg =
      withTermDegrees <$> pickDegrees
      where
      withTermDegrees termDegrees =
        terms_fromList $ zip (0 : Set.toList termDegrees) coeffs
      pickDegrees = aux (Set.fromAscList [1..deg]) (deg - termSize)
        where
        aux prev 0 = return prev
        aux prev s
          | s < 0 = error ""
          | otherwise =
            do
            i <- choose (0,Set.size prev - 1)
            aux (Set.deleteAt (int i) prev) (s-1)


{-|
  A runtime representative of type @ChPoly MPBall@.
  Used for specialising polymorphic tests to concrete types.
-}
tChPolyMPBall :: T (ChPoly MPBall)
tChPolyMPBall = T "ChPolyMPBall"

-- precondAnyReal :: CauchyReal -> Accuracy -> Bool
-- precondAnyReal _x _ac = True
--
-- precondPositiveReal :: CauchyReal -> Accuracy -> Bool
-- precondPositiveReal x ac = qaMakeQuery x ac !>! 0
--
-- precondNonZeroReal :: CauchyReal -> Accuracy -> Bool
-- precondNonZeroReal x ac = qaMakeQuery x ac !/=! 0
--
-- precondSmallReal :: CauchyReal -> Accuracy -> Bool
-- precondSmallReal x ac = abs (qaMakeQuery x ac) !<! 1000
--
-- precondPositiveSmallReal :: CauchyReal -> Accuracy -> Bool
-- precondPositiveSmallReal x ac = 0 !<! b && b !<! 1000
--   where b = qaMakeQuery x ac
--
-- precondAnyT :: t -> Bool
-- precondAnyT _t = True
--
-- precondNonZeroT :: (HasEqCertainly t Integer) => t -> Bool
-- precondNonZeroT t = t !/=! 0
--
-- precondSmallT :: (HasOrderCertainly t Integer) => t -> Bool
-- precondSmallT t = -1000 !<=! t && t !<=! 1000

specChPoly :: Spec
specChPoly =
  describe ("ChPoly") $ do
    specEvalConstFn tMPBall tChPolyMPBall tMPBall
    specEvalUnaryVarFn tChPolyMPBall tMPBall
    specFnPointwiseOp2 tChPolyMPBall tMPBall tMPBall "+" (+) (+)
    return ()
    -- specConversion tInteger tCauchyReal real (fst . integerBounds)
    -- describe "order" $ do
    --   specHasEqNotMixed tCauchyRealAtAccuracy
    --   -- specHasEq tInt tCauchyRealAtAccuracy tRational
    --   -- specCanPickNonZero tCauchyRealAtAccuracy
    --   specHasOrderNotMixed tCauchyRealAtAccuracy
    --   -- specHasOrder tInt tCauchyRealAtAccuracy tRational
    -- describe "min/max/abs" $ do
    --   specCRrespectsAccuracy1 "abs" abs precondAnyReal
    --   specCRrespectsAccuracy2 "max" max precondAnyReal precondAnyReal
    --   specCRrespectsAccuracy2 "min" min precondAnyReal precondAnyReal
    -- describe "ring" $ do
    --   specCRrespectsAccuracy1 "negate" negate precondAnyReal
    --   specCRrespectsAccuracy2 "+" add precondAnyReal precondAnyReal
    --   specCRrespectsAccuracy2T tInteger "+" add precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2T tRational "+" add precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2T tDyadic "+" add precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2 "a-b" sub precondAnyReal precondAnyReal
    --   specCRrespectsAccuracy2T tInteger "a-b" sub precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2T tRational "a-b" sub precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2T tDyadic "a-b" sub precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2 "*" mul precondAnyReal precondAnyReal
    --   specCRrespectsAccuracy2T tInteger "*" mul precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2T tRational "*" mul precondAnyReal precondAnyT
    --   specCRrespectsAccuracy2T tDyadic "*" mul precondAnyReal precondAnyT
    -- describe "field" $ do
    --   specCRrespectsAccuracy2 "/" divide precondAnyReal precondNonZeroReal
    --   specCRrespectsAccuracy2T tInteger "/" divide precondAnyReal precondNonZeroT
    --   specCRrespectsAccuracy2T tRational "/" divide precondAnyReal precondNonZeroT
    --   specCRrespectsAccuracy2T tDyadic "/" divide precondAnyReal precondNonZeroT
    -- describe "elementary" $ do
    --   specCRrespectsAccuracy1 "sqrt" sqrt precondPositiveReal
    --   specCRrespectsAccuracy1 "exp" exp precondSmallReal
    --   specCRrespectsAccuracy1 "log" log precondPositiveSmallReal
    --   specCRrespectsAccuracy2 "pow" pow precondPositiveSmallReal precondSmallReal
    --   specCRrespectsAccuracy2T tInteger "pow" pow precondNonZeroReal precondSmallT
    --   specCRrespectsAccuracy2T tRational "pow" pow precondPositiveSmallReal precondSmallT
    --   specCRrespectsAccuracy2T tDyadic "pow" pow precondPositiveSmallReal precondSmallT
    --   specCRrespectsAccuracy1 "cos" cos precondAnyReal
    --   specCRrespectsAccuracy1 "sine" sin precondAnyReal
