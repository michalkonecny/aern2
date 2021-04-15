{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.Float.Tests
    Description :  Tests for operations on arbitrary precision floats
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for operations on arbitrary precision floats.

    To run the tests using stack, execute:

    @
    stack test aern2-mp --test-arguments "-a 1000 -m MPFloat"
    @
-}
module AERN2.MP.Float.Tests
  (
    specMPFloat, tMPFloat
    , enforceRangeMP
    , approxEqual, approxEqualWithArgs
    , frequencyElements
  )
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Data.Ratio
import Text.Printf
-- import Data.Maybe

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

-- import qualified Control.CollectErrors as CE
-- import Numeric.CollectErrors (CN)
-- import qualified Numeric.CollectErrors as CN

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Float.Auxi

import AERN2.MP.Float.Type
import AERN2.MP.Float.Arithmetic
import AERN2.MP.Float.Conversions

import AERN2.MP.Float.Operators

instance Arbitrary MPFloat where
  arbitrary =
    do
    giveSpecialValue <- frequencyElements [(9, False),(1, True)]
    aux giveSpecialValue
    where
      aux giveSpecialValue
        | giveSpecialValue =
            elements [nan, infinity, -infinity, zero, one, -one]
        | otherwise =
          do
          (p :: Precision) <- arbitrary
          (s :: Integer) <- arbitrary
          ex <- choose (-20,10)
          let resultR = s * (10.0^ex)
          let result = ceduCentre $ fromRationalCEDU p resultR
          return result

frequencyElements :: ConvertibleExactly t Int => [(t, a)] -> Gen a
frequencyElements elems = frequency [(int n, return e) | (n,e) <- elems]

{-| 
    @enforceRange (Just l, Just u) a@ where @l < u@ returns an arbitrary value @b@ with @u < b < l@.
    Moreover, the returned values are distributed roughly evenly if the input values @a@ are distributed 
    roughly evenly in a large neighbourhood of the interval @[l,r]@.
    In most cases, when @l<a<u@, then @b=a@.
-}
enforceRangeMP ::
    (Maybe Integer, Maybe Integer) -> MPFloat -> MPFloat
enforceRangeMP _ a
    | isNaN a = a -- pass NaN unchanged
enforceRangeMP (Just l_, Just u_) a
    | not (l < u) = error "enforceRange: inconsistent range"
    | isInfinite a = (u -^ l)/^two
    | l < a && a < u = a
    | l < b && b < u = b
    | otherwise = (u -^ l)/^two
    where
    l = mpFloat l_
    u = mpFloat u_
    b = l +^ ((abs a) `mod` (u-^l))
enforceRangeMP (Just l_, _) a
    | isInfinite a = abs a
    | l < a = a
    | l == a = a +^ one
    | otherwise = (two*^l -^ a)
    where
    l = mpFloat l_
enforceRangeMP (_, Just u_) a
    | isInfinite a = - (abs a)
    | a < u = a
    | a == u = a -. one
    | otherwise = (two*.u -. a)
    where
    u = mpFloat u_
enforceRangeMP _ a = a

instance CanDivIMod MPFloat MPFloat where
  type DivIType MPFloat MPFloat = Integer
  divIMod x m
    | (not (isFinite m)) = ((d :: Integer), xm)
    | (not (isFinite x)) = ((d :: Integer), xm)
    | m > zero = (d, xm)
    | otherwise = ((d :: Integer), xm)
    where
    d = floor (x /^ m)
    xm = x -^ (mpFloat d)*^m
    -- errM :: (CanEnsureCN t) => t -> EnsureCN t
    -- errM s = CN.noValueNumErrorCertain $ CN.OutOfDomain $ "modulus not finite and positive: " ++ show m
    -- errX :: (CanEnsureCN t) => t -> EnsureCN t
    -- errX s = CN.noValueNumErrorCertain$ CN.OutOfDomain $ "modulus input not finite: " ++ show x


{- approximate comparison -}

-- infix 4 =~=

-- (=~=) :: MPFloat -> MPFloat -> Property
-- l =~= r =
--   approxEqualWithArgs 1 [(l, "L"),(r, "R")] l r

{-|
  Assert equality of two MPFloat's with tolerance @1/2^p@.
-}
approxEqual ::
  Integer {-^ @p@ precision to guide tolerance -} ->
  MPFloat {-^ LHS of equation-} ->
  MPFloat {-^ RHS of equation -}->
  Bool
approxEqual e x y
  | isNaN x && isNaN y = True
  | isNaN x && isInfinite y = True
  | isInfinite x && isNaN y = True
  | isNaN x || isNaN y = False
  | isInfinite x || isInfinite y = x == y
  | otherwise =
      abs (x -. y) <= 0.5^e

{-|
  Assert equality of two MPFloat's with tolerance derived from the size and precision
  of the given list of input and intermediate values.
  The result is expected to have at least as many significant digits
  as the (highest) nominal precision of the input and intermediate numbers
  minus the given precision loss parameter.

  When the assertion fails, report the given values using the given names.
-}
approxEqualWithArgs ::
  Integer {-^ bits of extra precision loss allowed -} ->
  [(MPFloat, String)] {-^ intermediate values from which to determine tolerance, their names to report when the equality fails -} ->
  MPFloat {-^ LHS of equation-} ->
  MPFloat {-^ RHS of equation -}->
  Property
approxEqualWithArgs precLoss args l r =
  counterexample description $ approxEqual e l r
  where
  description =
    printf "args:\n%s tolerance: <= 2^(%d)" argsS (-e)
  argsS =
    unlines
      [printf "    %s = %s (p=%s)" argS (show arg) (show $ getPrecision arg) 
      | (arg, argS) <- args ++ [(l, "L"), (r, "R"), (abs(r-.l), "|R-L|")]
      ]

  e = p - resNorm - precLoss
  resNorm =
    case (getNormLog l, getNormLog r) of
     (NormBits nl, NormBits nr) -> nl `max` nr; 
     (NormBits nl, _) -> nl
     (_, NormBits nr) -> nr
     _ -> 0
  p = foldl max 2 $ map (integer . getPrecision . fst) args

  {-
    args = argsPre ++ [(l, "L"), (r, "R"), (abs (l-.r),"|L-R|")]
    e =
      (foldl min 1000000 $ catMaybes $ map getAbsPrecBits args)
      - (length argsPre)
    getAbsPrecBits (x,_) =
      case getNormLog x of
        NormZero -> Nothing -- ideally infinity
        NormBits b -> Just (pI-b-precLoss)
      where
      pI = integer $ getPrecision x
  -}

{-|
  A runtime representative of type @MPFloat@.
  Used for specialising polymorphic tests to concrete types.
-}
tMPFloat :: T MPFloat
tMPFloat = T "MPFloat"

trueForNotFinite :: 
  (CanTestFinite t1, CanTestFinite t2) => 
  (t1 -> t2 -> Bool) -> 
  (t1 -> t2 -> Bool)
trueForNotFinite rel a b 
  | isFinite a && isFinite b = rel a b
  | otherwise = True

specMPFloat :: Spec
specMPFloat =
  let
    infix 4 <=%, >=%, ==%
    (<=%), (>=%) :: 
      (CanTestFinite t1, CanTestFinite t2, 
       HasOrderAsymmetric t1 t2, OrderCompareType t1 t2 ~ Bool) => 
      t1 -> t2 -> Bool
    (==%) :: 
      (CanTestFinite t1, CanTestFinite t2, 
       HasEqAsymmetric t1 t2, EqCompareType t1 t2 ~ Bool) => 
      t1 -> t2 -> Bool
    (<=%) = trueForNotFinite (<=)
    (>=%) = trueForNotFinite (>=) 
    (==%) = trueForNotFinite (==) 
  in
  describe ("MPFloat") $ do
    specCanSetPrecision tMPFloat 
      (printArgsIfFails2 "=~=" (\xPrec x -> approxEqualWithArgs 1 [(xPrec, "xPrec")] x xPrec))
    specCanRound tMPFloat
    specCanNegNum tMPFloat
    specCanAbs tMPFloat
    specCanMinMaxNotMixed tMPFloat
    -- specCanMinMax tMPFloat tInteger tMPFloat
    describe "special values" $ do
      it "0 * infinity = NaN" $ do
        isNaN (zero *^ infinity)
        &&
        isNaN (zero *. infinity)
      it "infinity / infinity = NaN" $ do
        isNaN (infinity /^ infinity)
        &&
        isNaN (infinity /. infinity)
      it "infinity - infinity = NaN" $ do
        isNaN (infinity -^ infinity)
        &&
        isNaN (infinity -. infinity)
    describe "approximate addition" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x +. y <=% x +^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          let
            (=~~=) = approxEqualWithArgs 1 [(x,"x"), (y,"y")]
            infix 4 =~~=
          in
          x +. y =~~= x +^ y
      it "absorbs 0" $ do
        property $ \ (x :: MPFloat) ->
          not (isNaN x) ==>
            x +. zero == x
      it "approximately commutative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x +. y <=% y +^ x
          &&
          x +^ y >=% y +. x
      it "approximately associative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) (z :: MPFloat) ->
          (x +. y) +. z <=% x +^ (y +^ z)
          &&
          (x +^ y) +^ z >=% x +. (y +. z)
    describe "approximate subtraction" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x -. y <=% x -^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          let
            (=~~=) = approxEqualWithArgs 1 [(x,"x"), (y,"y")]
            infix 4 =~~=
          in
          x -. y =~~= x -^ y
      it "same as negate and add" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x -. y <=% x +^ (-y)
          &&
          x -^ y >=% x +. (-y)
    describe "approximate multiplication" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x *. y <=% x *^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          let
            (=~~=) = approxEqualWithArgs 1 [(x,"x"), (y,"y")]
            infix 4 =~~=
          in
          x *. y =~~= x *^ y
      it "absorbs 1" $ do
        property $ \ (x :: MPFloat) ->
            x *. one ==% x
      it "approximately commutative" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x *. y <=% y *^ x
          &&
          x *^ y >=% y *. x
      it "approximately associative" $ do
        property $ \ (x_ :: MPFloat) (y_ :: MPFloat) (z_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          let y = enforceRangeMP (Just 0, Nothing) y_ in
          let z = enforceRangeMP (Just 0, Nothing) z_ in
          (not (isInfinite x) && not (isInfinite y) && not (isInfinite z)) ==>
          (x *. y) *. z <=% x *^ (y *^ z)
          &&
          (x *^ y) *^ z >=% x *. (y *. z)
      it "approximately distributes over addition" $ do
        property $ \ (x_ :: MPFloat) (y_ :: MPFloat) (z_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          let y = enforceRangeMP (Just 0, Nothing) y_ in
          let z = enforceRangeMP (Just 0, Nothing) z_ in
          (not (isInfinite x) && not (isInfinite y) && not (isInfinite z)) ==>
          x *. (y +. z) <=% (x *^ y) +^ (x *^ z)
          &&
          x *^ (y +^ z) >=% (x *. y) +. (x *. z)
    describe "approximate division" $ do
      it "down <= up" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          x /. y <=% x /^ y
      it "up ~ down" $ do
        property $ \ (x :: MPFloat) (y :: MPFloat) ->
          let
            (=~~=) = approxEqualWithArgs 10 [(x,"x"), (y,"y"), (x /. y,"x/.y"), (x /^ y,"x/^y")]
            infix 4 =~~=
          in
          isFinite y && y /= 0
          ==>
          x /. y =~~= x /^ y
      it "recip(recip x) = x" $ do
        property $ \ (x :: MPFloat) ->
          (not (isFinite x) || x > 0 || x < 0) ==>
          one /. (one /^ x) <=% x
          &&
          one /^ (one /. x) >=% x
      it "x/1 = x" $ do 
        property $ \ (x :: MPFloat) ->
          (x /. one) <=% x
          &&
          (x /^ one) >=% x
      it "x/x = 1" $ do
        property $ \ (x :: MPFloat) ->
          -- (isCertainlyNonZero x && (not $ isNaN $ x /. x)) ==>
            (x /. x) <=% one
            &&
            (x /^ x) >=% one
      it "x/y = x*(1/y)" $ do
        property $ \ (x_ :: MPFloat) (y_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          let y = enforceRangeMP (Just 0, Nothing) y_ in
          (x /. y) <=% x *^ (one /^ y)
          &&
          (x /^ y) >=% x *. (one /. y)
    describe "approximate sqrt" $ do
      it "down <= up" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          sqrtDown x <=% sqrtUp x
      it "up ~ down" $ do
        property $ \ (x_ :: MPFloat) ->
          let 
            x = enforceRangeMP (Just 0, Nothing) x_ 
            (=~~=) = approxEqualWithArgs 2 [(x,"x")]
            infix 4 =~~=
          in
          sqrtDown x =~~= sqrtUp x
      it "sqrt(x) >= 0" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          sqrtUp x >=% 0
      it "sqrt(x)^2 ~ x" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          (sqrtDown x) *. (sqrtDown x) <=% x
          &&
          (sqrtUp x) *^ (sqrtUp x) >=% x
    describe "approximate exp" $ do
      it "down <= up" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          expDown x <=% expUp x
      it "up ~ down" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          let
            (=~~=) = approxEqualWithArgs 3 [(x,"x")]
            infix 4 =~~=
          in
          expDown x =~~= expUp x
      it "exp(-x) == 1/(exp x)" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          one /. (expUp x) <=% expUp (-x)
          &&
          one /^ (expDown x) >=% expDown (-x)
      it "exp(x+y) = exp(x)*exp(y)" $ do
        property $ \ (x_ :: MPFloat) (y_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          let y = enforceRangeMP (Just (-1000000), Just 1000000) y_ in
          expDown (x +. y) <=% (expUp x) *^ (expUp y)
          &&
          expUp (x +^ y) >=% (expDown x) *. (expDown y)
    describe "approximate log" $ do
      it "down <= up" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          logDown x <=% logUp x
      -- TODO: fix accuracy of CDAR mBounds logA x for x near 1
      -- it "up ~ down" $ do
      --   property $ \ (x_ :: MPFloat) ->
      --     let x = enforceRangeMP (Just 0, Nothing) x_ in
      --     let
      --       (=~~=) = approxEqualWithArgs 10 [(x,"x")]
      --       infix 4 =~~=
      --     in
      --     logDown x =~~= logUp x
      it "log(1/x) == -(log x)" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          logDown (one /. x) <=% -(logDown x)
          &&
          logUp (one /^ x) >=% -(logUp x)
      it "log(x*y) = log(x)+log(y)" $ do
        property $ \ (x_ :: MPFloat) (y_ :: MPFloat) ->
          let x = enforceRangeMP (Just 0, Nothing) x_ in
          let y = enforceRangeMP (Just 0, Nothing) y_ in
          logDown (x *. y) <=% (logUp x) +^ (logUp y)
          &&
          logUp (x *^ y) >=% (logDown x) +. (logDown y)
      it "log(exp x) == x" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000), Just 10000) x_ in
          logDown (expDown x) <=% x
          &&
          logUp (expUp x) >=% x
    describe "approximate sine" $ do
      it "down <= up" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          sinDown x <=% sinUp x
      -- TODO: fix accuracy of CDAR mBounds sine
      -- it "up ~ down" $ do
      --   property $ \ (x_ :: MPFloat) ->
      --     let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
      --     let
      --       (=~~=) = approxEqualWithArgs 1 [(x,"x")]
      --       infix 4 =~~=
      --     in
      --     sinDown x =~~= sinUp x
      -- it "sin(pi/2) ~ 1" $ do
      --   property $ \ (p :: Precision) ->
      --     let
      --       piA = ceduCentre $ piCEDU p
      --       (=~~=) = approxEqualWithArgs 1 [(piA,"pi")]
      --       infix 4 =~~=
      --     in
      --     sinUp(piA/.(setPrecision (p+10) $ mpFloat 2)) =~~= one
      it "in [-1,1]" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          sinDown x <=% 1
          &&
          sinUp x >=% -1
    describe "approximate cosine" $ do
      it "down <= up" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          cosDown x <=% cosUp x
      -- TODO: fix accuracy of CDAR mBounds cosine
      -- it "up ~ down" $ do
      --   property $ \ (x_ :: MPFloat) ->
      --     let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
      --     let
      --       (=~~=) = approxEqualWithArgs 1 [(x,"x")]
      --       infix 4 =~~=
      --     in
      --     cosDown x =~~= cosUp x
      it "in [-1,1]" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          cosDown x <=% 1
          &&
          cosUp x >=% -1
      -- TODO: fix accuracy of CDAR mBounds cosine
      -- it "cos(pi)=-1" $ do
      --   property $ \ (p :: Precision) ->
      --     let
      --       piA = ceduCentre $ piCEDU p
      --       (=~~=) = approxEqualWithArgs 2 [(piA,"pi")]
      --       infix 4 =~~=
      --     in
      --     cosUp(piA) =~~= (-one)
      it "cos(x)^2 + sin(x)^2 = 1" $ do
        property $ \ (x_ :: MPFloat) ->
          let x = enforceRangeMP (Just (-1000000), Just 1000000) x_ in
          let
            cosxU = cosUp x
            cosxD = cosDown x
            cosx2U = (cosxU *^ cosxU) `max` (cosxD *^ cosxD)
            cosx2D
              | cosxD > 0 = cosxD *. cosxD
              | cosxU < 0 = cosxU *. cosxU
              | otherwise = mpFloat 0
            sinxU = sinUp x
            sinxD = sinDown x
            sinx2U = (sinxU *^ sinxU) `max` (sinxD *^ sinxD)
            sinx2D
              | sinxD > 0 = sinxD *. sinxD
              | sinxU < 0 = sinxU *. sinxU
              | otherwise = mpFloat 0
          in
          (isFinite x ) ==>
          (cosx2D +. sinx2D) <=% 1
          &&
          (cosx2U +^ sinx2U) >=% 1
