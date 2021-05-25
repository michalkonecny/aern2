{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Real.Tests
    Description :  Tests for operations on Cauchy real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for operations on Cauchy real numbers.

    To run the tests using stack, execute:

    @
    stack test aern2-real --test-arguments "-a 1000 -m Real"
    @
-}
module AERN2.Real.Tests
  (
    specCReal, tCReal
  )
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Data.Ratio
-- import Text.Printf

import qualified Numeric.CollectErrors as CN

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

-- import AERN2.Norm
import AERN2.MP.Accuracy
--
import AERN2.MP
import AERN2.MP.Dyadic

import AERN2.Real.Type
import AERN2.Real.Field ()
import AERN2.Real.Elementary ()

instance Arbitrary CReal where
  arbitrary =
    frequency
      [(int 1, creal <$> (arbitrarySmall 1000000 :: Gen Integer)),
       (int 1, creal <$> (arbitrarySmall 1000000 :: Gen Rational)),
       (int 2, (*) <$> (arbitrarySmall 1000000 :: Gen Integer) <*> arbitrarySignedBinary)
      ]
      where
      arbitrarySignedBinary =
        signedBinary2Real <$> infiniteListOf (elements [-1,0,1])
      signedBinary2Real sbits =
        crealFromPrecFunction $ \ p -> cn $ balls !! p
        where
        balls = nextBit (mpBall (0,1)) $ zip sbits (map prec [10..])
        nextBit ball ((sbit, p):rest) =
          ball : nextBit newBall rest
          where
          newBall =
            case sbit of
              (-1) -> fromEndpointsAsIntervals l m
              0 -> fromEndpointsAsIntervals l2 r2
              1 -> fromEndpointsAsIntervals m r
              _ -> error "in Arbitrary CReal"
          (l_,r_) = endpointsAsIntervals ball
          l = setPrecision p l_
          r = setPrecision p r_
          m = (l + r) * (dyadic 0.5)
          l2 = (l + m) * (dyadic 0.5)
          r2 = (r + m) * (dyadic 0.5)
        nextBit _ _ = error "in Arbitrary CReal"

arbitrarySmall :: (Arbitrary a, HasOrderCertainly a Integer) => Integer -> Gen a
arbitrarySmall bound = aux
  where
  aux =
    do
    x <- arbitrary
    if -bound !<=! x && x !<=! bound
      then return x
      else aux


{-|
  A runtime representative of type @CReal@.
  Used for specialising polymorphic tests to concrete types.
-}
tCReal :: T CReal
tCReal = T "CReal"

specCRrespectsAccuracy1 ::
  String ->
  (CReal -> CReal) ->
  (CReal -> Accuracy -> Bool) ->
  Spec
specCRrespectsAccuracy1 opName op precond =
  it (opName ++ " respects accuracy requests") $ do
    property $
      \ (x :: CReal) (ac :: Accuracy) ->
        ac < (bits 1000) && precond x ac ==>
        case CN.toEither ((op x) ? ac) of
          Right v -> getAccuracy v >=$ ac
          _ -> property True

specCRrespectsAccuracy2 ::
  String ->
  (CReal -> CReal -> CReal) ->
  (CReal -> Accuracy -> Bool) ->
  (CReal -> Accuracy -> Bool) ->
  Spec
specCRrespectsAccuracy2 opName op precond1 precond2 =
  it (opName ++ " respects accuracy requests") $ do
    property $
      \ (x :: CReal) (y :: CReal) (ac :: Accuracy) ->
        ac < (bits 1000) && precond1 x ac && precond2 y ac ==>
        case CN.toEither ((op x y) ? ac) of
          Right v -> getAccuracy v >=$ ac
          _ -> property True

(>=$) :: Accuracy -> Accuracy -> Property
(>=$) = printArgsIfFails2 ">=" (>=)

precondAnyReal :: CReal -> Accuracy -> Bool
precondAnyReal _x _ac = True

precondPositiveReal :: CReal -> Accuracy -> Bool
precondPositiveReal x ac = (x ? ac) !>! 0

precondNonZeroReal :: CReal -> Accuracy -> Bool
precondNonZeroReal x ac = (x ? ac) !/=! 0

precondSmallReal :: CReal -> Accuracy -> Bool
precondSmallReal x ac = abs (x ? ac) !<! 1000

precondPositiveSmallReal :: CReal -> Accuracy -> Bool
precondPositiveSmallReal x ac = 0 !<! b && b !<! 1000
  where b = x ? ac

-- specCRrespectsAccuracy2 ::
--   String ->
--   (CReal -> CReal -> CReal) ->
--   (CReal -> Accuracy -> Bool) ->
--   (CReal -> Accuracy -> Bool) ->
--   Spec
-- specCRrespectsAccuracy2 opName op =
--   specCRrespectsAccuracy2CN opName (\ a b -> cn (op a b))

specCRrespectsAccuracy2T ::
  (Arbitrary t, Show t) =>
  T t ->
  String ->
  (CReal -> t -> CReal) ->
  (CReal -> Accuracy -> Bool) ->
  (t -> Bool) ->
  Spec
specCRrespectsAccuracy2T  (T tName :: T t) opName op precond1 precond2 =
  it (opName ++ " with " ++ tName ++ " respects accuracy requests") $ do
    property $
      \ (x :: CReal) (t :: t) (ac :: Accuracy) ->
        ac < (bits 1000) && precond1 x ac && precond2 t ==>
        case CN.toEither ((op x t) ? ac) of
          Right v -> getAccuracy v >=$ ac
          _ -> property True

precondAnyT :: t -> Bool
precondAnyT _t = True

precondNonZeroT :: (HasEqCertainly t Integer) => t -> Bool
precondNonZeroT t = t !/=! 0

precondSmallT :: (HasOrderCertainly t Integer) => t -> Bool
precondSmallT t = -1000 !<=! t && t !<=! 1000

specCReal :: Spec
specCReal =
  describe ("CReal") $ do
    -- specConversion tInteger tCReal creal (fst . integerBounds)
    -- describe "order" $ do
    --   specHasEqNotMixed tCReal
    --   specHasEq tInt tCReal tRational
    --   specCanPickNonZero tCReal
    --   specHasOrderNotMixed tCReal
    --   specHasOrder tInt tCReal tRational
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
      -- specCRrespectsAccuracy2T tDyadic "pow" pow precondPositiveSmallReal precondSmallT
      specCRrespectsAccuracy1 "cos" cos precondAnyReal
      specCRrespectsAccuracy1 "sine" sin precondAnyReal
