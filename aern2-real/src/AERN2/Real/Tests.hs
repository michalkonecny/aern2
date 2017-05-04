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

-- import Numeric.CatchingExceptions

-- import AERN2.Norm
import AERN2.MP.Accuracy
--
import qualified AERN2.MP.Ball.Type as MB
import AERN2.MP.Dyadic
import AERN2.MP.Float

import AERN2.QA
import AERN2.AccuracySG

import AERN2.Real.Type
import AERN2.Real.Comparison (CauchyRealAtAccuracy(..))
import AERN2.Real.Ring ()
import AERN2.Real.Field ()
import AERN2.Real.Elementary ()

instance Arbitrary CauchyRealAtAccuracy where
  arbitrary =
    CauchyRealAtAccuracy <$>
      arbitrary <*>
      ((accuracySG . bits) <$> (arbitrarySmall 1000 :: Gen Integer))

instance Arbitrary CauchyReal where
  arbitrary =
    frequency
      [(int 1, real <$> (arbitrarySmall 1000000 :: Gen Integer)),
       (int 1, real <$> (arbitrarySmall 1000000 :: Gen Rational)),
       (int 2, (*) <$> (arbitrarySmall 1000000 :: Gen Integer) <*> arbitrarySignedBinary)
      ]
      where
      arbitrarySignedBinary =
        signedBinary2Real <$> infiniteListOf (elements [-1,0,1])
      signedBinary2Real sbits =
        newCR "random" [] $ \ (AccuracySG _ acG) ->
          case acG of
            NoInformation -> balls !! 0
            Exact -> error "signedBinary2Real: cannot request the number Exactly"
            _ -> balls !! (fromAccuracy acG + 1)
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
          mDy = ((dyadic l) + (dyadic r)) * (dyadic 0.5)
          l2Dy = ((dyadic l) + mDy) * (dyadic 0.5)
          r2Dy = ((dyadic r) + mDy) * (dyadic 0.5)
        nextBit _ _ = error "in Arbitrary CauchyReal"

arbitrarySmall :: (Arbitrary a, HasOrderCertainly a Integer) => Integer -> Gen a
arbitrarySmall limit = aux
  where
  aux =
    do
    x <- arbitrary
    if -limit !<=! x && x !<=! limit
      then return x
      else aux


{-|
  A runtime representative of type @CauchyReal@.
  Used for specialising polymorphic tests to concrete types.
-}
tCauchyReal :: T CauchyReal
tCauchyReal = T "CauchyReal"

tCauchyRealAtAccuracy :: T CauchyRealAtAccuracy
tCauchyRealAtAccuracy = T "CauchyReal(ac)"

specCRrespectsAccuracy1 ::
  String ->
  (CauchyReal -> CauchyReal) ->
  (CauchyReal -> AccuracySG -> Bool) ->
  Spec
specCRrespectsAccuracy1 opName op precond =
  it (opName ++ " respects accuracy requests") $ do
    property $
      \ (x :: CauchyReal) (ac :: Accuracy) ->
        let acSG = accuracySG ac in
        ac < (bits 1000) && precond x acSG ==>
        getAccuracy ((op x) ? acSG) >= ac

precondAnyReal :: CauchyReal -> AccuracySG -> Bool
precondAnyReal _x _ac = True

precondPositiveReal :: CauchyReal -> AccuracySG -> Bool
precondPositiveReal x ac = (x ? ac) !>! 0

precondNonZeroReal :: CauchyReal -> AccuracySG -> Bool
precondNonZeroReal x ac = (x ? ac) !/=! 0

precondSmallReal :: CauchyReal -> AccuracySG -> Bool
precondSmallReal x ac = abs (x ? ac) !<! 1000

precondPositiveSmallReal :: CauchyReal -> AccuracySG -> Bool
precondPositiveSmallReal x ac = 0 !<! b && b !<! 1000
  where b = x ? ac

specCRrespectsAccuracy2 ::
  String ->
  (CauchyReal -> CauchyReal -> CauchyReal) ->
  (CauchyReal -> AccuracySG -> Bool) ->
  (CauchyReal -> AccuracySG -> Bool) ->
  Spec
specCRrespectsAccuracy2 opName op precond1 precond2 =
  it (opName ++ " respects accuracy requests") $ do
    property $
      \ (x :: CauchyReal) (y :: CauchyReal) (ac :: Accuracy) ->
        let acSG = accuracySG ac in
        ac < (bits 1000) && precond1 x acSG && precond2 y acSG  ==>
        getAccuracy ((op x y) ? acSG) >= ac

specCRrespectsAccuracy2T ::
  (Arbitrary t, Show t) =>
  T t ->
  String ->
  (CauchyReal -> t -> CauchyReal) ->
  (CauchyReal -> AccuracySG -> Bool) ->
  (t -> Bool) ->
  Spec
specCRrespectsAccuracy2T (T tName :: T t) opName op precond1 precond2 =
  it (opName ++ " with " ++ tName ++ " respects accuracy requests") $ do
    property $
      \ (x :: CauchyReal) (t :: t) (ac :: Accuracy) ->
        let acSG = accuracySG ac in
        ac < (bits 1000) && precond1 x acSG && precond2 t  ==>
        getAccuracy ((op x t) ? acSG) >= ac

precondAnyT :: t -> Bool
precondAnyT _t = True

precondNonZeroT :: (HasEqCertainly t Integer) => t -> Bool
precondNonZeroT t = t !/=! 0

precondSmallT :: (HasOrderCertainly t Integer) => t -> Bool
precondSmallT t = -1000 !<=! t && t !<=! 1000

specCauchyReal :: Spec
specCauchyReal =
  describe ("CauchyReal") $ do
    -- specConversion tInteger tCauchyReal real (fst . integerBounds)
    describe "order" $ do
      specHasEqNotMixed tCauchyRealAtAccuracy
      -- specHasEq tInt tCauchyRealAtAccuracy tRational
      -- specCanPickNonZero tCauchyRealAtAccuracy
      specHasOrderNotMixed tCauchyRealAtAccuracy
      -- specHasOrder tInt tCauchyRealAtAccuracy tRational
    describe "min/max/abs" $ do
      specCRrespectsAccuracy1 "abs" abs precondAnyReal
      specCRrespectsAccuracy2 "max" max precondAnyReal precondAnyReal
      specCRrespectsAccuracy2 "min" min precondAnyReal precondAnyReal
    describe "ring" $ do
      specCRrespectsAccuracy1 "negate" negate precondAnyReal
      specCRrespectsAccuracy2 "+" add precondAnyReal precondAnyReal
      specCRrespectsAccuracy2T tInteger "+" add precondAnyReal precondAnyT
      specCRrespectsAccuracy2T tRational "+" add precondAnyReal precondAnyT
      specCRrespectsAccuracy2T tDyadic "+" add precondAnyReal precondAnyT
      specCRrespectsAccuracy2 "a-b" sub precondAnyReal precondAnyReal
      specCRrespectsAccuracy2T tInteger "a-b" sub precondAnyReal precondAnyT
      specCRrespectsAccuracy2T tRational "a-b" sub precondAnyReal precondAnyT
      specCRrespectsAccuracy2T tDyadic "a-b" sub precondAnyReal precondAnyT
      specCRrespectsAccuracy2 "*" mul precondAnyReal precondAnyReal
      specCRrespectsAccuracy2T tInteger "*" mul precondAnyReal precondAnyT
      specCRrespectsAccuracy2T tRational "*" mul precondAnyReal precondAnyT
      specCRrespectsAccuracy2T tDyadic "*" mul precondAnyReal precondAnyT
    describe "field" $ do
      specCRrespectsAccuracy2 "/" divide precondAnyReal precondNonZeroReal
      specCRrespectsAccuracy2T tInteger "/" divide precondAnyReal precondNonZeroT
      specCRrespectsAccuracy2T tRational "/" divide precondAnyReal precondNonZeroT
      specCRrespectsAccuracy2T tDyadic "/" divide precondAnyReal precondNonZeroT
    describe "elementary" $ do
      specCRrespectsAccuracy1 "sqrt" sqrt precondPositiveReal
      specCRrespectsAccuracy1 "exp" exp precondSmallReal
      specCRrespectsAccuracy1 "log" log precondPositiveSmallReal
      specCRrespectsAccuracy2 "pow" pow precondPositiveSmallReal precondSmallReal
      specCRrespectsAccuracy2T tInteger "pow" pow precondNonZeroReal precondSmallT
      specCRrespectsAccuracy2T tRational "pow" pow precondPositiveSmallReal precondSmallT
      specCRrespectsAccuracy2T tDyadic "pow" pow precondPositiveSmallReal precondSmallT
      specCRrespectsAccuracy1 "cos" cos precondAnyReal
      specCRrespectsAccuracy1 "sine" sin precondAnyReal