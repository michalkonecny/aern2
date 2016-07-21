{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  AERN2.MP.Dyadic
    Description :  Dyadics with exact ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers with exact ring operations.

    TODO: this module is unfinished

    Currently, we use hmpfr when compiling with ghc 7.10 and higher
    and haskell-mpfr when compiling with ghc 7.8.
-}

module AERN2.MP.Dyadic
(
   -- * Dyadic numbers and their basic operations
   Dyadic
   -- * Dyadic constructors
   , CanBeDyadic, dyadic
   -- * tests
   , specDyadic, tDyadic
)
where

import Numeric.MixedTypes
import qualified Prelude as P

import Data.Typeable

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

import Data.Ratio (denominator, numerator)

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Float

{-| Exact dyadic type based on MPFloat. -}
newtype Dyadic = Dyadic { dyadicMPFloat :: MPFloat }
  deriving (Show, P.Eq, P.Ord, CanRound, HasPrecision, HasNorm, Typeable)

{-- conversions --}

instance ConvertibleExactly Dyadic MPFloat where
  safeConvertExactly = Right . dyadicMPFloat

instance ConvertibleExactly Dyadic Rational where
  safeConvertExactly = safeConvertExactly . dyadicMPFloat

type CanBeDyadic t = ConvertibleExactly t Dyadic
dyadic :: (CanBeDyadic t) => t -> Dyadic
dyadic = convertExactly

instance ConvertibleExactly MPFloat Dyadic where
  safeConvertExactly = Right . Dyadic

instance ConvertibleExactly Integer Dyadic where
  safeConvertExactly = fmap Dyadic . safeConvertExactly

instance ConvertibleExactly Int Dyadic where
  safeConvertExactly = fmap Dyadic . safeConvertExactly

instance ConvertibleExactly Rational Dyadic where
  safeConvertExactly q
    | isDyadic = Right $ Dyadic (fromRationalUp (prec $ max 2 (dp + np + 1)) q)
    | otherwise = convError "this number is not dyadic" q
    where
    isDyadic = d == 2^dp
    dp = integerLog2 d
    d = denominator q
    np = integerLog2 (max 1 $ abs $ numerator q)

{-- comparisons --}

instance HasEqAsymmetric Dyadic Dyadic
instance HasEqAsymmetric Dyadic Integer where
  equalTo = convertSecond equalTo
instance HasEqAsymmetric Integer Dyadic where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Dyadic Int where
  equalTo = convertSecond equalTo
instance HasEqAsymmetric Int Dyadic where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Dyadic Rational where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Rational Dyadic where
  equalTo = convertSecond equalTo

instance CanTestZero Dyadic

instance HasOrderAsymmetric Dyadic Dyadic
instance HasOrderAsymmetric Dyadic Integer where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric Integer Dyadic where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Dyadic Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric Int Dyadic where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Rational Dyadic where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric Dyadic Rational where
  lessThan = convertFirst lessThan
  leq = convertFirst leq

instance CanTestPosNeg Dyadic

{- unary functions -}

instance CanNeg Dyadic where
  negate = lift1 negate

instance CanAbs Dyadic where
  abs = lift1 abs

lift1 :: (MPFloat -> MPFloat) -> (Dyadic -> Dyadic)
lift1 op (Dyadic x) = Dyadic (op x)

{- min/max -}

instance CanMinMaxAsymmetric Dyadic Dyadic
instance CanMinMaxAsymmetric Integer Dyadic where
  type MinMaxType Integer Dyadic = Dyadic
  min = convertFirst min
  max = convertFirst max
instance CanMinMaxAsymmetric Dyadic Integer where
  type MinMaxType Dyadic Integer = Dyadic
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Int Dyadic where
  type MinMaxType Int Dyadic = Dyadic
  min = convertFirst min
  max = convertFirst max
instance CanMinMaxAsymmetric Dyadic Int where
  type MinMaxType Dyadic Int = Dyadic
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Rational Dyadic where
  type MinMaxType Rational Dyadic = Dyadic
  min = convertFirst min
  max = convertFirst max
instance CanMinMaxAsymmetric Dyadic Rational where
  type MinMaxType Dyadic Rational = Dyadic
  min = convertSecond min
  max = convertSecond max

{- addition -}

instance CanAddAsymmetric Dyadic Dyadic where
  add = lift2 addDown addUp

instance CanAddAsymmetric Integer Dyadic where
  type AddType Integer Dyadic = Dyadic
  add = convertFirst add
instance CanAddAsymmetric Dyadic Integer where
  type AddType Dyadic Integer = Dyadic
  add = convertSecond add

instance CanAddAsymmetric Int Dyadic where
  type AddType Int Dyadic = Dyadic
  add = convertFirst add
instance CanAddAsymmetric Dyadic Int where
  type AddType Dyadic Int = Dyadic
  add = convertSecond add

instance CanAddAsymmetric Rational Dyadic where
  type AddType Rational Dyadic = Dyadic
  add = convertFirst add
instance CanAddAsymmetric Dyadic Rational where
  type AddType Dyadic Rational = Dyadic
  add = convertSecond add

{- subtraction -}

instance CanSub Dyadic Dyadic where
  sub = lift2 subDown subUp

instance CanSub Integer Dyadic where
  type SubType Integer Dyadic = Dyadic
  sub = convertFirst sub
instance CanSub Dyadic Integer where
  type SubType Dyadic Integer = Dyadic
  sub = convertSecond sub

instance CanSub Int Dyadic where
  type SubType Int Dyadic = Dyadic
  sub = convertFirst sub
instance CanSub Dyadic Int where
  type SubType Dyadic Int = Dyadic
  sub = convertSecond sub

instance CanSub Rational Dyadic where
  type SubType Rational Dyadic = Dyadic
  sub = convertFirst sub
instance CanSub Dyadic Rational where
  type SubType Dyadic Rational = Dyadic
  sub = convertSecond sub

{- multiplication -}

instance CanMulAsymmetric Dyadic Dyadic where
  mul = lift2 mulDown mulUp

instance CanMulAsymmetric Integer Dyadic where
  type MulType Integer Dyadic = Dyadic
  mul = convertFirst mul
instance CanMulAsymmetric Dyadic Integer where
  type MulType Dyadic Integer = Dyadic
  mul = convertSecond mul

instance CanMulAsymmetric Int Dyadic where
  type MulType Int Dyadic = Dyadic
  mul = convertFirst mul
instance CanMulAsymmetric Dyadic Int where
  type MulType Dyadic Int = Dyadic
  mul = convertSecond mul

instance CanMulAsymmetric Rational Dyadic where
  type MulType Rational Dyadic = Dyadic
  mul = convertFirst mul
instance CanMulAsymmetric Dyadic Rational where
  type MulType Dyadic Rational = Dyadic
  mul = convertSecond mul

instance CanPow Dyadic Integer where
  pow = powUsingMul
instance CanPow Dyadic Int where
  pow = powUsingMul


lift2 ::
  (MPFloat -> MPFloat -> MPFloat) ->
  (MPFloat -> MPFloat -> MPFloat) ->
  (Dyadic -> Dyadic -> Dyadic)
lift2 opDown opUp (Dyadic x0) (Dyadic y0) = Dyadic (opExact x0 y0)
  where
    opExact x y
      | rUp == rDown = rUp
      | otherwise = opExact xH yH
      where
      rUp = opUp x y
      rDown = opDown x y
      xH = setPrecision pH x
      yH = setPrecision pH y
      pH = precisionTimes2 p
      p = getPrecision rUp

instance Arbitrary Dyadic where
  arbitrary =
    do
      c <- finiteMPFloat
      return (Dyadic c)
    where
      finiteMPFloat =
        do
          x <- arbitrary
          if (-infinity) < x && x < infinity
            then return x
            else finiteMPFloat

{-|
  A runtime representative of type @Dyadic@.
  Used for specialising polymorphic tests to concrete types.
-}
tDyadic :: T Dyadic
tDyadic = T "Dyadic"

specDyadic :: Spec
specDyadic =
  describe ("Dyadic") $ do
    specConversion tInteger tDyadic dyadic round
    specConversion tDyadic tRational rational dyadic
    describe "order" $ do
      specHasEqNotMixed tDyadic
      specHasEq tInt tDyadic tRational
      specCanTestZero tDyadic
      specHasOrderNotMixed tDyadic
      specHasOrder tInt tDyadic tRational
    describe "min/max/abs" $ do
      specCanNegNum tDyadic
      specCanAbs tDyadic
      specCanMinMaxNotMixed tDyadic
      specCanMinMax tDyadic tInteger tDyadic
      it "min Dyadic Rational (dyadic only)" $ do
        property $ \ (x :: Dyadic) (y :: Dyadic) ->
          x `min` y == x `min` (rational y)
      it "max Dyadic Rational (dyadic only)" $ do
        property $ \ (x :: Dyadic) (y :: Dyadic) ->
          x `max` y == x `max` (rational y)
    describe "ring" $ do
      specCanAddNotMixed tDyadic
      specCanAddSameType tDyadic
      specCanAdd tInt tDyadic tInteger
      specCanAdd tInteger tDyadic tInt
      it "Dyadic + Rational (dyadic only)" $ do
        property $ \ (x :: Dyadic) (y :: Dyadic) ->
          x + y == x + (rational y)
      specCanSubNotMixed tDyadic
      specCanSub tDyadic tInteger
      specCanSub tInteger tDyadic
      specCanSub tDyadic tInt
      specCanSub tInt tDyadic
      it "Dyadic - Rational (dyadic only)" $ do
        property $ \ (x :: Dyadic) (y :: Dyadic) ->
          x - y == x - (rational y)
      specCanMulNotMixed tDyadic
      specCanMulSameType tDyadic
      specCanMul tInt tDyadic tInteger
      it "Dyadic * Rational (dyadic only)" $ do
        property $ \ (x :: Dyadic) (y :: Dyadic) ->
          x * y == x * (rational y)
      specCanPow tDyadic tInteger
