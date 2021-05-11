{-# LANGUAGE CPP #-}
-- #define DEBUG
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
import qualified Prelude as P

import Control.CollectErrors (CollectErrors(..), CanBeErrors)
import qualified Control.CollectErrors as CE
-- import qualified Numeric.CollectErrors as CN

import Text.Printf
import Text.Regex.TDFA

import Data.Typeable
-- import Data.Convertible

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

instance Ring Dyadic
instance Ring (CN Dyadic)

instance OrderedRing Dyadic
instance OrderedRing (CN Dyadic)

instance OrderedCertainlyRing Dyadic
instance OrderedCertainlyRing (CN Dyadic)

instance HasAccuracy Dyadic where getAccuracy _ = Exact
instance CanGiveUpIfVeryInaccurate Dyadic -- ie, never give up

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
              [((n :: Integer)*(dyadic 0.5)^(e :: Integer), afterS)]
            _ -> tryNext
        _ -> tryNext
      where
      (_,_,afterS,groups) =
        dyadicS =~ "\\`dyadic \\(([-0-9]*)\\*0.5\\^([0-9]*)\\)"
          :: (String, String, String, [String])


{-- conversions --}

type HasDyadics t = ConvertibleExactly Dyadic t

instance ConvertibleExactly Dyadic Dyadic where
  safeConvertExactly = Right

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
    | isDyadic = Right $ Dyadic (ceduCentre $ fromRationalCEDU (prec $ max 2 (dp + np + 1)) q)
    | otherwise = convError "this number is not dyadic" q
    where
    isDyadic = d == 2^dp
    dp = integerLog2 d
    d = denominator q
    np = integerLog2 (max 1 $ abs $ numerator q)

instance Convertible Dyadic Double where
  safeConvert = safeConvert . dyadicMPFloat

instance (ConvertibleExactly Dyadic t, Monoid es) => ConvertibleExactly Dyadic (CollectErrors es t) where
  safeConvertExactly = fmap (\v -> CollectErrors (Just v) mempty) . safeConvertExactly

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

instance
  (HasEqAsymmetric Dyadic b
  , IsBool (CollectErrors es (EqCompareType Dyadic b))
  , CanBeErrors es)
  =>
  HasEqAsymmetric Dyadic (CollectErrors es b)
  where
  type EqCompareType Dyadic (CollectErrors es b) =
    CollectErrors es (EqCompareType Dyadic b)
  equalTo = CE.liftT1 equalTo

instance
  (HasEqAsymmetric a Dyadic
  , IsBool (CollectErrors es (EqCompareType a Dyadic))
  , CanBeErrors es)
  =>
  HasEqAsymmetric (CollectErrors es a) Dyadic
  where
  type EqCompareType (CollectErrors es  a) Dyadic =
    CollectErrors es (EqCompareType a Dyadic)
  equalTo = CE.lift1T equalTo

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

instance
  (HasOrderAsymmetric Dyadic b
  , IsBool (CollectErrors es (OrderCompareType Dyadic b))
  , CanBeErrors es)
  =>
  HasOrderAsymmetric Dyadic (CollectErrors es  b)
  where
  type OrderCompareType Dyadic (CollectErrors es  b) =
    CollectErrors es (OrderCompareType Dyadic b)
  lessThan = CE.liftT1 lessThan
  leq = CE.liftT1 leq
  greaterThan = CE.liftT1 greaterThan
  geq = CE.liftT1 geq

instance
  (HasOrderAsymmetric a Dyadic
  , IsBool (CollectErrors es (OrderCompareType a Dyadic))
  , CanBeErrors es)
  =>
  HasOrderAsymmetric (CollectErrors es a) Dyadic
  where
  type OrderCompareType (CollectErrors es  a) Dyadic =
    CollectErrors es (OrderCompareType a Dyadic)
  lessThan = CE.lift1T lessThan
  leq = CE.lift1T leq
  greaterThan = CE.lift1T greaterThan
  geq = CE.lift1T geq


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
  , CanBeErrors es)
  =>
  CanMinMaxAsymmetric Dyadic (CollectErrors es  b)
  where
  type MinMaxType Dyadic (CollectErrors es  b) =
    CollectErrors es (MinMaxType Dyadic b)
  min = CE.liftT1 min
  max = CE.liftT1 max

instance
  (CanMinMaxAsymmetric a Dyadic
  , CanBeErrors es)
  =>
  CanMinMaxAsymmetric (CollectErrors es a) Dyadic
  where
  type MinMaxType (CollectErrors es  a) Dyadic =
    CollectErrors es (MinMaxType a Dyadic)
  min = CE.lift1T min
  max = CE.lift1T max

{- addition -}

instance CanAddAsymmetric Dyadic Dyadic where
  add = lift2 addCEDU

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
  , CanBeErrors es)
  =>
  CanAddAsymmetric Dyadic (CollectErrors es  b)
  where
  type AddType Dyadic (CollectErrors es  b) =
    CollectErrors es (AddType Dyadic b)
  add = CE.liftT1 add

instance
  (CanAddAsymmetric a Dyadic
  , CanBeErrors es)
  =>
  CanAddAsymmetric (CollectErrors es a) Dyadic
  where
  type AddType (CollectErrors es  a) Dyadic =
    CollectErrors es (AddType a Dyadic)
  add = CE.lift1T add

{- subtraction -}

instance CanSub Dyadic Dyadic where
  sub = lift2 subCEDU

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
  , CanBeErrors es)
  =>
  CanSub Dyadic (CollectErrors es  b)
  where
  type SubType Dyadic (CollectErrors es  b) =
    CollectErrors es (SubType Dyadic b)
  sub = CE.liftT1 sub

instance
  (CanSub a Dyadic
  , CanBeErrors es)
  =>
  CanSub (CollectErrors es a) Dyadic
  where
  type SubType (CollectErrors es  a) Dyadic =
    CollectErrors es (SubType a Dyadic)
  sub = CE.lift1T sub


{- multiplication -}

instance CanMulAsymmetric Dyadic Dyadic where
  mul = lift2 mulCEDU

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
  , CanBeErrors es)
  =>
  CanMulAsymmetric Dyadic (CollectErrors es  b)
  where
  type MulType Dyadic (CollectErrors es  b) =
    CollectErrors es (MulType Dyadic b)
  mul = CE.liftT1 mul

instance
  (CanMulAsymmetric a Dyadic
  , CanBeErrors es)
  =>
  CanMulAsymmetric (CollectErrors es a) Dyadic
  where
  type MulType (CollectErrors es  a) Dyadic =
    CollectErrors es (MulType a Dyadic)
  mul = CE.lift1T mul

instance CanPow Dyadic Integer where
  pow = powUsingMul (dyadic 1)
instance CanPow Dyadic Int where
  pow = powUsingMul (dyadic 1)

instance
  (CanDiv a Dyadic
  , CanBeErrors es)
  =>
  CanDiv (CollectErrors es a) Dyadic
  where
  type DivType (CollectErrors es  a) Dyadic =
    CollectErrors es (DivType a Dyadic)
  divide = CE.lift1T divide

instance CanDiv Integer Dyadic where
  type DivType Integer Dyadic = Rational
  divide a b = divide a (rational b)
instance CanDiv Dyadic Integer where
  type DivType Dyadic Integer = Rational
  divide a b = divide (rational a) b

instance CanDiv Int Dyadic where
  type DivType Int Dyadic = Rational
  divide a b = divide a (rational b)
instance CanDiv Dyadic Int where
  type DivType Dyadic Int = Rational
  divide a b = divide (rational a) b

instance CanDiv Rational Dyadic where
  type DivType Rational Dyadic = Rational
  divide = convertSecond divide
instance CanDiv Dyadic Rational where
  type DivType Dyadic Rational = Rational
  divide = convertFirst divide

instance
  (CanDiv Dyadic b
  , CanBeErrors es)
  =>
  CanDiv Dyadic (CollectErrors es b)
  where
  type DivType Dyadic (CollectErrors es b) =
    CollectErrors es (DivType Dyadic b)
  divide = CE.liftT1 divide

instance
  (CanPow Dyadic b
  , CanBeErrors es)
  =>
  CanPow Dyadic (CollectErrors es  b)
  where
  type PowType Dyadic (CollectErrors es b) =
    CollectErrors es (PowType Dyadic b)
  pow = CE.liftT1 pow

instance
  (CanPow a Dyadic
  , CanBeErrors es)
  =>
  CanPow (CollectErrors es a) Dyadic
  where
  type PowType (CollectErrors es  a) Dyadic =
    CollectErrors es (PowType a Dyadic)
  pow = CE.lift1T pow

instance CanTestFinite Dyadic where
  isFinite = isFinite . dyadicMPFloat
  isInfinite = isInfinite . dyadicMPFloat

lift2 ::
  (MPFloat -> MPFloat -> BoundsCEDU MPFloat) ->
  (Dyadic -> Dyadic -> Dyadic)
lift2 opCEDU (Dyadic x0) (Dyadic y0) = Dyadic (opExact x0 y0)
  where
    opExact x y
      | rE P.== zero = rC
      | otherwise =
          maybeTrace (printf "Dyadic.lift2: rC = %s; rE = %s; p = %s" (show rC) (show rE) (show $ integer p)) $
          opExact xH yH
      where
      rC = ceduCentre rCEDU
      rE = ceduErr rCEDU
      rCEDU = opCEDU x y
      xH = setPrecision pH x
      yH = setPrecision pH y
      pH = precisionTimes2 p
      p = getPrecision rC

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

instance P.Num Dyadic where
    fromInteger = convertExactly
    negate = negate
    (+) = (+)
    (*) = (*)
    abs = abs
    signum d
      | d < 0 = dyadic (-1)
      | d == 0 = dyadic 0
      | otherwise = dyadic 1

instance P.Real Dyadic where
    toRational = convertExactly
