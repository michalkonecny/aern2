{-|
    Module      :  AERN2.MP.Float.Tests
    Description :  Tests for operations on arbitrary precision floats
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for operations on arbitrary precision floats.

    @
    stack test aern2-mp --test-arguments --qc-max-success=1000
    @
-}

module AERN2.MP.Float.Tests
  (
    specMPFloat, tMPFloat
    , (=~=), approxEqual, approxEqualWithArgs
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
import AERN2.MP.Float.Arithmetic
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
approxEqualWithArgs argsPre l r =
  counterexample description $ approxEqual e l r
  where
    args = argsPre ++ [(l, "L"), (r, "R"), (l-.r,"L-R")]
    e =
      (foldl min 1000000000000 $ catMaybes $ map getNminusP args)
      - (length argsPre)
    getNminusP (x,_) =
      case norm of
        NormZero -> Nothing -- ideally infinity
        NormBits b -> Just (pI-b)
      where
      norm = getNormLog x
      pI = integer $ getPrecision x
    description =
      printf "args:\n%s tolerance: <= %s (e=%d)" argsS (show (double $ 0.5^e)) e
    argsS =
      unlines
        [printf "    %s = %s (p=%s)" argS (show arg) (show $ getPrecision arg) | (arg, argS) <- args]

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
          (not $ itisNaN $ x +. y) ==>
          x +. y <= y +^ x
          &&
          x +^ y >= y +. x
      it "approximately associative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) (z :: MPFloat) ->
          (not $ itisNaN $ x +. y +. z) ==>
          (x +. y) +. z <= x +^ (y +^ z)
          &&
          (x +^ y) +^ z >= x +. (y +. z)
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
          (not $ itisNaN $ x -. y) ==>
          x -. y <= x +^ (-y)
          &&
          x -^ y >= x +. (-y)
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
          not (itisNaN (x *. y)) ==>
          x *. y <= y *^ x
          &&
          x *^ y >= y *. x
      it "approximately associative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) (z :: MPFloat) ->
          (x >= 0 && y >= 0 && z >= 0
           && x < infinity && y < infinity && z < infinity) ==>
          (x *. y) *. z <= x *^ (y *^ z)
          &&
          (x *^ y) *^ z >= x *. (y *. z)
      it "approximately distributes over addition" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) (z :: MPFloat) ->
          (x >= 0 && y >= 0 && z >= 0
           && x < infinity && y < infinity && z < infinity) ==>
          x *. (y +. z) <= (x *^ y) +^ (x *^ z)
          &&
          x *^ (y +^ z) >= (x *. y) +. (x *. z)
    describe "approximate division" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          not (itisNaN (x /. y))
          ==>
          x /. y <= x /^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          let
            (=~~=) = approxEqualWithArgs [(x,"x"),(y,"y")]
            infix 4 =~~=
          in
          x /. y =~~= x /^ y
      it "recip(recip x) = x" $ do
        property $ \ (x :: MPFloat) ->
          (x > 0 || x < 0) ==>
          one /. (one /^ x) <= x
          &&
          one /^ (one /. x) >= x
      it "x/1 = x" $ do
        property $ \ (x :: MPFloat) ->
          not (itisNaN x) ==>
          (x /. one) == x
      it "x/x = 1" $ do
        property $ \ (x :: MPFloat) ->
          (isNonZero x && (not $ itisNaN $ x /. x)) ==>
            (x /. x) <= one
            &&
            (x /^ x) >= one
      it "x/y = x*(1/y)" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          (y > 0 && x >= 0 && x/.y >= 0) ==>
          (x /. y) <= x *^ (one /^ y)
          &&
          (x /^ y) >= x *. (one /. y)
    describe "approximate sqrt" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) ->
          not (itisNaN (sqrtDown x))
          ==>
          sqrtDown x <= sqrtUp x
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) ->
          (x >= 0)
          ==>
          sqrtDown x =~= sqrtUp x
      it "sqrt(x) >= 0" $ do
        property $ \ (x :: MPFloat) ->
          (x >= 0)
          ==>
          sqrtUp x >= 0
      it "sqrt(x)^2 ~ x" $ do
        property $ \ (x :: MPFloat) ->
          (x >= 0)
          ==>
          (sqrtDown x) *. (sqrtDown x) <= x
          &&
          (sqrtUp x) *^ (sqrtUp x) >= x
    describe "approximate log" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) ->
          (x > 0)
          ==>
          logDown x <= logUp x
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) ->
          (x > 0)
          ==>
          logDown x =~= logUp x
      it "log(1/x) == -(log x)" $ do
        property $ \ (x :: MPFloat) ->
          (x > 0)
          ==>
          logDown (one /. x) <= -(logDown x)
          &&
          logUp (one /^ x) >= -(logUp x)
      it "log(x*y) = log(x)+log(y)" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          (x > 0 && y > 0)
          ==>
          logDown (x *. y) <= (logUp x) +^ (logUp y)
          &&
          logUp (x *^ y) >= (logDown x) +. (logDown y)
