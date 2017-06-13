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

    Currently, we use hmpfr when compiling with ghc 7.10 and higher
    and haskell-mpfr when compiling with ghc 7.8.
-}

module AERN2.MP.Dyadic
(
   -- * Dyadic numbers and their basic operations
   Dyadic, HasDyadics
   -- * Dyadic constructors
   , CanBeDyadic, dyadic
   -- * tests
   , specDyadic, tDyadic
)
where

import Numeric.MixedTypes
import qualified Prelude as P

import Control.CollectErrors

import Text.Printf
import Text.Regex.TDFA

import Data.Typeable
import Data.Convertible

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

import Data.Ratio (denominator, numerator)

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import AERN2.MP.Float

{-| Exact dyadic type based on MPFloat. -}
newtype Dyadic = Dyadic { dyadicMPFloat :: MPFloat }
  deriving (P.Eq, P.Ord, CanRound, HasPrecision, HasNorm, Typeable)

instance HasAccuracy Dyadic where getAccuracy _ = Exact

instance Show Dyadic where
  show (Dyadic x)
    | e == 0 = printf "dyadic (%d)" n
    | e > 0 = printf "dyadic (%d*0.5^%d)" n e
    | otherwise = error "in show Dyadic"
    where
    xR = rational x
    NormBits e = getNormLog (denominator xR)
    n = numerator xR

instance Read Dyadic where
  readsPrec _pr dyadicS =
    tryInt $ tryWithExp []
    where
    tryInt tryNext =
      case groups of
        [nS] ->
          case reads nS of
            [(n,"")] -> [(dyadic (n :: Integer), afterS)]
            _ -> tryNext
        _ -> tryNext
      where
      (_,_,afterS,groups) =
        dyadicS =~ "\\`dyadic \\(([-0-9]*)\\)"
          :: (String, String, String, [String])
    tryWithExp tryNext =
      case groups of
        [nS,eS] ->
          case (reads nS, reads eS) of
            ([(n,"")],[(e,"")]) ->
              [(dyadic (((n :: Integer)*0.5^(e :: Integer))⚡), afterS)]
            _ -> tryNext
        _ -> tryNext
      where
      (_,_,afterS,groups) =
        dyadicS =~ "\\`dyadic \\(([-0-9]*)\\*0.5\\^([0-9]*)\\)"
          :: (String, String, String, [String])

instance (SuitableForCE es) => CanEnsureCE es Dyadic


{-- conversions --}

type HasDyadics t = ConvertibleExactly Dyadic t

instance ConvertibleExactly Dyadic MPFloat where
  safeConvertExactly = Right . dyadicMPFloat

instance ConvertibleExactly Dyadic Rational where
  safeConvertExactly = safeConvertExactly . dyadicMPFloat

type CanBeDyadic t = ConvertibleExactly t Dyadic
dyadic :: (CanBeDyadic t) => t -> Dyadic
dyadic = convertExactly

instance ConvertibleExactly MPFloat Dyadic where
  safeConvertExactly = Right . Dyadic

instance HasIntegerBounds Dyadic where
  integerBounds d = (floor d, ceiling d)

instance ConvertibleExactly Integer Dyadic where
  safeConvertExactly = fmap Dyadic . safeConvertExactly

instance ConvertibleExactly Int Dyadic where
  safeConvertExactly = fmap Dyadic . safeConvertExactly

instance ConvertibleExactly Rational Dyadic where
  safeConvertExactly q
    | isDyadic = Right $ Dyadic (fromRationalUp (prec $ max 2 (dp + np + 1)) q)
    | otherwise = convError "this number is not dyadic" q
    where
    isDyadic = d == ((2^dp)⚡)
    dp = integerLog2 d
    d = denominator q
    np = integerLog2 (max 1 $ abs $ numerator q)

instance Convertible Dyadic Double where
  safeConvert = safeConvert . dyadicMPFloat

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

-- -- TODO: finish it once we have HasEq for a pair of CatchingNumExceptions
-- instance HasEqAsymmetric t Dyadic => HasEqAsymmetric (CatchingNumExceptions t) Dyadic where
--   type EqCompareType (CatchingNumExceptions t) Dyadic = CatchingNumExceptions (EqCompareType t Dyadic)
--   equalTo = convertSecondUsing catchingNumExceptions equalTo

instance
  (HasEqAsymmetric Dyadic b
  , CanEnsureCE es (EqCompareType Dyadic b)
  , IsBool (EnsureCE es (EqCompareType Dyadic b))
  , SuitableForCE es)
  =>
  HasEqAsymmetric Dyadic (CollectErrors es  b)
  where
  type EqCompareType Dyadic (CollectErrors es  b) =
    EnsureCE es (EqCompareType Dyadic b)
  equalTo = lift2TLCE equalTo

instance
  (HasEqAsymmetric a Dyadic
  , CanEnsureCE es (EqCompareType a Dyadic)
  , IsBool (EnsureCE es (EqCompareType a Dyadic))
  , SuitableForCE es)
  =>
  HasEqAsymmetric (CollectErrors es a) Dyadic
  where
  type EqCompareType (CollectErrors es  a) Dyadic =
    EnsureCE es (EqCompareType a Dyadic)
  equalTo = lift2TCE equalTo

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

instance CanTestInteger Dyadic where
  certainlyNotInteger = certainlyNotInteger . rational
  certainlyIntegerGetIt = certainlyIntegerGetIt . rational

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
  type MinMaxType Rational Dyadic = Rational
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Dyadic Rational where
  type MinMaxType Dyadic Rational = Rational
  min = convertFirst min
  max = convertFirst max

instance
  (CanMinMaxAsymmetric Dyadic b
  , CanEnsureCE es (MinMaxType Dyadic b)
  , SuitableForCE es)
  =>
  CanMinMaxAsymmetric Dyadic (CollectErrors es  b)
  where
  type MinMaxType Dyadic (CollectErrors es  b) =
    EnsureCE es (MinMaxType Dyadic b)
  min = lift2TLCE min
  max = lift2TLCE max

instance
  (CanMinMaxAsymmetric a Dyadic
  , CanEnsureCE es (MinMaxType a Dyadic)
  , SuitableForCE es)
  =>
  CanMinMaxAsymmetric (CollectErrors es a) Dyadic
  where
  type MinMaxType (CollectErrors es  a) Dyadic =
    EnsureCE es (MinMaxType a Dyadic)
  min = lift2TCE min
  max = lift2TCE max

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
  type AddType Rational Dyadic = Rational
  add = convertSecond add
instance CanAddAsymmetric Dyadic Rational where
  type AddType Dyadic Rational = Rational
  add = convertFirst add

instance
  (CanAddAsymmetric Dyadic b
  , CanEnsureCE es (AddType Dyadic b)
  , SuitableForCE es)
  =>
  CanAddAsymmetric Dyadic (CollectErrors es  b)
  where
  type AddType Dyadic (CollectErrors es  b) =
    EnsureCE es (AddType Dyadic b)
  add = lift2TLCE add

instance
  (CanAddAsymmetric a Dyadic
  , CanEnsureCE es (AddType a Dyadic)
  , SuitableForCE es)
  =>
  CanAddAsymmetric (CollectErrors es a) Dyadic
  where
  type AddType (CollectErrors es  a) Dyadic =
    EnsureCE es (AddType a Dyadic)
  add = lift2TCE add

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
  type SubType Rational Dyadic = Rational
  sub = convertSecond sub
instance CanSub Dyadic Rational where
  type SubType Dyadic Rational = Rational
  sub = convertFirst sub

instance
  (CanSub Dyadic b
  , CanEnsureCE es (SubType Dyadic b)
  , SuitableForCE es)
  =>
  CanSub Dyadic (CollectErrors es  b)
  where
  type SubType Dyadic (CollectErrors es  b) =
    EnsureCE es (SubType Dyadic b)
  sub = lift2TLCE sub

instance
  (CanSub a Dyadic
  , CanEnsureCE es (SubType a Dyadic)
  , SuitableForCE es)
  =>
  CanSub (CollectErrors es a) Dyadic
  where
  type SubType (CollectErrors es  a) Dyadic =
    EnsureCE es (SubType a Dyadic)
  sub = lift2TCE sub


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
  type MulType Rational Dyadic = Rational
  mul = convertSecond mul
instance CanMulAsymmetric Dyadic Rational where
  type MulType Dyadic Rational = Rational
  mul = convertFirst mul

instance
  (CanMulAsymmetric Dyadic b
  , CanEnsureCE es (MulType Dyadic b)
  , SuitableForCE es)
  =>
  CanMulAsymmetric Dyadic (CollectErrors es  b)
  where
  type MulType Dyadic (CollectErrors es  b) =
    EnsureCE es (MulType Dyadic b)
  mul = lift2TLCE mul

instance
  (CanMulAsymmetric a Dyadic
  , CanEnsureCE es (MulType a Dyadic)
  , SuitableForCE es)
  =>
  CanMulAsymmetric (CollectErrors es a) Dyadic
  where
  type MulType (CollectErrors es  a) Dyadic =
    EnsureCE es (MulType a Dyadic)
  mul = lift2TCE mul

instance CanPow Dyadic Integer where
  pow = powUsingMul
instance CanPow Dyadic Int where
  pow = powUsingMul

instance
  (CanDiv a Dyadic
  , CanEnsureCE es (DivType a Dyadic)
  , SuitableForCE es)
  =>
  CanDiv (CollectErrors es a) Dyadic
  where
  type DivType (CollectErrors es  a) Dyadic =
    EnsureCE es (DivType a Dyadic)
  divide = lift2TCE divide

instance CanDiv Integer Dyadic where
  type DivType Integer Dyadic = CN Rational
  divide a b = divide a (rational b)
instance CanDiv Dyadic Integer where
  type DivType Dyadic Integer = CN Rational
  divide a b = divide (rational a) b

instance CanDiv Int Dyadic where
  type DivType Int Dyadic = CN Rational
  divide a b = divide a (rational b)
instance CanDiv Dyadic Int where
  type DivType Dyadic Int = CN Rational
  divide a b = divide (rational a) b

instance CanDiv Rational Dyadic where
  type DivType Rational Dyadic = CN Rational
  divide = convertSecond divide
instance CanDiv Dyadic Rational where
  type DivType Dyadic Rational = CN Rational
  divide = convertFirst divide

instance
  (CanDiv Dyadic b
  , CanEnsureCE es (DivType Dyadic b)
  , SuitableForCE es)
  =>
  CanDiv Dyadic (CollectErrors es  b)
  where
  type DivType Dyadic (CollectErrors es  b) =
    EnsureCE es (DivType Dyadic b)
  divide = lift2TLCE divide

instance
  (CanPow Dyadic b
  , CanEnsureCE es (PowType Dyadic b)
  , SuitableForCE es)
  =>
  CanPow Dyadic (CollectErrors es  b)
  where
  type PowType Dyadic (CollectErrors es  b) =
    EnsureCE es (PowType Dyadic b)
  pow = lift2TLCE pow

instance
  (CanPow a Dyadic
  , CanEnsureCE es (PowType a Dyadic)
  , SuitableForCE es)
  =>
  CanPow (CollectErrors es a) Dyadic
  where
  type PowType (CollectErrors es  a) Dyadic =
    EnsureCE es (PowType a Dyadic)
  pow = lift2TCE pow

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
          if isFinite x
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
