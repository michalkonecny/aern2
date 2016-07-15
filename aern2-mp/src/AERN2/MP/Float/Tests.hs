{-|
    Module      :  AERN2.MP.Float.Tests
    Description :  Tests for operations on arbitrary precision floats
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for operations on arbitrary precision floats
-}

module AERN2.MP.Float.Tests
  (
    specMPFloat
    , (=~=)
    -- , nan, infinity, itisNaN, itisInfinite
  )
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Data.Ratio

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC


import AERN2.Norm
import AERN2.MP.Precision

import AERN2.MP.Float.Type
-- import AERN2.MP.Float.Arithmetic
import AERN2.MP.Float.Conversions
import AERN2.MP.Float.Operators
import AERN2.MP.Float.Constants

instance Arbitrary MPFloat where
  arbitrary =
    do
    shouldNaN <- frequencyElements [(19, False),(1, True)]
    shouldInfinity <- frequencyElements [(19, False),(1, True)]
    aux shouldNaN shouldInfinity
    where
      aux shouldNaN shouldInfinity
        | shouldNaN = return nan
        | shouldInfinity =
            elements [infinity, -infinity]
        | otherwise =
          do
          (p :: Precision) <- arbitrary
          (s :: Integer) <- arbitrary
          ex <- choose (-20,10)
          let resultR = s * (10.0^ex)
          let result = fromRationalUp p resultR
          return result

frequencyElements :: ConvertibleExactly t Int => [(t, a)] -> Gen a
frequencyElements elems = frequency [(int n, return e) | (n,e) <- elems]

{- approximate comparison -}

infix 4 =~=

(=~=) :: MPFloat -> MPFloat -> Bool
x =~= y
  | itisNaN x && itisNaN y = True
  | itisNaN x || itisNaN y = False
  | itisInfinite x || itisInfinite y = x == y
  | otherwise =
    case norm of
      NormZero -> x == y -- both should be zero
      NormBits b ->
        abs (x -. y) <= 2.0^(b-p)
  where
    p = integer $ (getPrecision x) `min` (getPrecision y)
    norm = (getNormLog x) `max` (getNormLog y)

tMPFloat :: T MPFloat
tMPFloat = T "MPFloat"

specMPFloat :: Spec
specMPFloat =
  describe ("MPFloat") $ do
    specCanSetPrecision tMPFloat (=~=)
    specCanNegNum tMPFloat
    specCanAbs tMPFloat
    specCanMinMaxNotMixed tMPFloat
    -- specCanMinMax tMPFloat tInteger tMPFloat
    it "0 * infinity = NaN" $ do
      itisNaN (zero *^ infinity)
      &&
      itisNaN (zero *. infinity)
    it "infinity / infinity = NaN" $ do
      itisNaN (infinity /^ infinity)
      &&
      itisNaN (infinity /. infinity)
    it "infinity - infinity = NaN" $ do
      itisNaN (infinity -^ infinity)
      &&
      itisNaN (infinity -. infinity)
    it "x +. y <= x +^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        not (itisNaN (x +. y))
        ==>
        x +. y <= x +^ y
    it "x +. y =~= x +^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        x +. y =~= x +^ y
    it "x -. y <= x -^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        not (itisNaN (x -. y))
        ==>
        x -. y <= x -^ y
    it "x -. y =~= x -^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        x -. y =~= x -^ y
    it "x *. y <= x *^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        not (itisNaN (x *. y))
        ==>
        x *. y <= x *^ y
    it "x *. y =~= x *^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        x *. y =~= x *^ y
    it "x /. y <= x /^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        not (itisNaN (x /. y))
        ==>
        x /. y <= x /^ y
    it "x /. y =~= x /^ y" $ do
      property $ \ (x :: MPFloat) (y :: MPFloat) ->
        x /. y =~= x /^ y
