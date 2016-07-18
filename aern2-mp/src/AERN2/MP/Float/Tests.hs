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
    specMPFloat, tMPFloat
    , (=~=), approxEqual, approxEqualWithArgs, testAndReportLR
    -- , nan, infinity, itisNaN, itisInfinite
  )
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Data.Ratio
import Text.Printf
import Data.Maybe

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
    giveSpecialValue <- frequencyElements [(9, False),(1, True)]
    aux giveSpecialValue
    where
      aux giveSpecialValue
        | giveSpecialValue =
            elements [nan, infinity, -infinity, zero, one]
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

(=~=) :: MPFloat -> MPFloat -> Property
x =~= y =
  approxEqualWithArgs [(x, "L"),(y, "R")] x y

approxEqual ::
  Integer {-^ precision to guide tolerance -} ->
  MPFloat ->
  MPFloat ->
  Bool
approxEqual p x y
  | itisNaN x && itisNaN y = True
  | itisNaN x && itisInfinite y = True
  | itisInfinite x && itisNaN y = True
  | itisNaN x || itisNaN y = False
  | itisInfinite x || itisInfinite y = x == y
  | otherwise =
      abs (x -. y) <= 0.5^p

approxEqualWithArgs :: [(MPFloat, String)] -> MPFloat -> MPFloat -> Property
approxEqualWithArgs args l r =
  counterexample description $ approxEqual e l r
  where
    e = foldl min 0 $ catMaybes $ map getNminusP args
    getNminusP (x,_) =
      case norm of
        NormZero -> Nothing -- ideally infinity
        NormBits b -> Just (b-pI)
      where
      norm = getNormLog x
      pI = integer $ getPrecision x

    description =
      printf "args: %s\n, L = %s (p=%s), R = %s (p=%s), L-.R = %s"
        argsS
        (show l) (show $ getPrecision l)
        (show r) (show $ getPrecision r)
        (show $ l -. r)
    argsS =
      unwords
        [printf "%s = %s (p=%s)" argS (show arg) (show $ getPrecision arg) | (arg, argS) <- args]


testAndReportLR ::
  (MPFloat -> MPFloat -> Bool) -> MPFloat -> MPFloat -> Property
testAndReportLR test l r =
  counterexample description $ test l r
  where
    description =
      printf "L = %s (p=%s), R = %s (p=%s), L-.R = %s"
        (show l) (show $ getPrecision l)
        (show r) (show $ getPrecision r)
        (show $ l -. r)


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
    describe "special values" $ do
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
    describe "approximate addition" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          not (itisNaN (x +. y))
          ==>
          x +. y <= x +^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x +. y =~= x +^ y
      it "absorbs 0" $ do
        property $ \ (x :: MPFloat) ->
          (not $ itisNaN x) ==>
            x +. (mpFloat 0) == x
      it "approximately commutative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x +. y =~= y +. x
      it "approximately associative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) (z :: MPFloat) ->
          let (=~~=) = approxEqualWithArgs [(x,"x"),(y,"y"),(z,"z")]; infix 4 =~~= in
          (x +. y) +. z =~~= x +. (y +. z)
    describe "approximate subtraction" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          not (itisNaN (x -. y))
          ==>
          x -. y <= x -^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x -. y =~= x -^ y
      it "same as negate and add" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x -. y =~= x +. (-y)
    describe "approximate multiplication" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          not (itisNaN (x *. y))
          ==>
          x *. y <= x *^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x *. y =~= x *^ y
      it "absorbs 1" $ do
        property $ \ (x :: MPFloat) ->
          (not $ itisNaN x) ==>
            x *. (mpFloat 1) == x
      it "approximately commutative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x *. y =~= y *. x
      it "approximately associative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) (z :: MPFloat) ->
          let (=~~=) = approxEqualWithArgs [(x,"x"),(y,"y"),(z,"z")]; infix 4 =~~= in
          (x *. y) *. z =~~= x *. (y *. z)
      it "approximately distributes over addition" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) (z :: MPFloat) ->
          let (=~~=) = approxEqualWithArgs [(x,"x"),(y,"y"),(z,"z")]; infix 4 =~~= in
          x *. (y +. z) =~~= (x *. y) +. (x *. z)
    describe "approximate multiplication" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          not (itisNaN (x /. y))
          ==>
          x /. y <= x /^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x /. y =~= x /^ y
      it "recip(recip x) = x" $ do
        property $ \ (x :: MPFloat) ->
          (isNonZero x) ==>
            one /. (one /. x) =~= x
      it "x/1 = x" $ do
        property $ \ (x :: MPFloat) ->
          (x /. one) =~= x
      it "x/x = 1" $ do
        property $ \ (x :: MPFloat) ->
          (isNonZero x && (not $ itisInfinite x) && (not $ itisNaN x)) ==>
            (x /. x) =~= one
      it "x/y = x*(1/y)" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          (isNonZero y) ==>
            let (=~~=) = approxEqualWithArgs [(x,"x"),(y,"y")]; infix 4 =~~= in
            (x /. y) =~~= x *. (one /. y)
