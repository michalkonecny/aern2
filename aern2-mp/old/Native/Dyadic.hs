{-# LANGUAGE CPP #-}
-- #define DEBUG
{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  AERN2.MP.Native.Dyadic
    Description :  Dyadics with exact ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision dyadic numbers with exact ring operations.
    This module uses native Haskell Integer, not MPFR.
-}

module AERN2.MP.Native.Dyadic
(
   -- * Dyadic numbers and their basic operations
  --  Dyadic, HasDyadics
  --  -- * Dyadic constructors
  --  , CanBeDyadic, dyadic
  --  -- * tests
  --  , specDyadic, tDyadic
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

import Control.CollectErrors

import Text.Printf
import Text.Regex.TDFA

-- import Data.Typeable

import Test.Hspec
import Test.QuickCheck hiding ((.&.))
-- import qualified Test.Hspec.SmallCheck as SC

import Data.Convertible
import Data.Ratio (denominator, numerator)
import Data.Bits

import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Normalize

import AERN2.Norm
-- import AERN2.MP.Precision
import AERN2.MP.Accuracy

{-| Exact dyadic number using an exponent for the denominator -}
data Dyadic = Dyadic { dyadicDigits :: Integer, dyadicExp :: Int }
  -- deriving (P.Eq, P.Ord, CanRound, HasPrecision, HasNorm, Typeable)

instance CanNormalize Dyadic where
  normalize x@(Dyadic d e)
    | d == 0 && e == 0 = x
    | d == 0 = Dyadic d (int 0)
    | odd d = x
    | otherwise = Dyadic (shift d (-k)) (e P.+ k)
    where
    k = countZeroes 0 1
    countZeroes i b
      | b .&. d == 0 = countZeroes (i + 1) (shift b (int 1))
      | otherwise = (int i)

instance P.Eq Dyadic where
  a == b = da == db && ea == eb
    where
    (Dyadic da ea) = normalize a
    (Dyadic db eb) = normalize b

instance P.Ord Dyadic where
  compare (Dyadic da ea) (Dyadic db eb)
    | ea == eb = P.compare da db
    | ea > eb = P.compare da (db * 2^!(ea - eb))
    | otherwise = P.compare (da * 2^!(eb - ea)) db

instance HasNorm Dyadic where
  getNormLog (Dyadic d e) =
    case getNormLog d of
      NormBits n -> NormBits $ n + (integer e)
      n -> n

instance Ring Dyadic
instance Ring (CN Dyadic)

instance OrderedRing Dyadic
instance OrderedRing (CN Dyadic)

instance OrderedCertainlyRing Dyadic
instance OrderedCertainlyRing (CN Dyadic)

instance HasAccuracy Dyadic where getAccuracy _ = Exact

{-
instance Show Dyadic where
  show (Dyadic d e)
    | e >= 0 = printf "dyadic (%d)" (d * 2^!e)
    | e < 0 = printf "dyadic (%d*0.5^%d)" d (-e)
    | otherwise = error "in show Dyadic"

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
              [((n :: Integer)*(dyadic 0.5)^!(e :: Integer), afterS)]
            _ -> tryNext
        _ -> tryNext
      where
      (_,_,afterS,groups) =
        dyadicS =~ "\\`dyadic \\(([-0-9]*)\\*0.5\\^([0-9]*)\\)"
          :: (String, String, String, [String])

instance (SuitableForCE es) => CanEnsureCE es Dyadic


{-- conversions --}

type HasDyadics t = ConvertibleExactly Dyadic t

instance ConvertibleExactly Dyadic Dyadic where
  safeConvertExactly = Right

instance ConvertibleExactly Dyadic Rational where
  safeConvertExactly (Dyadic d e) = Right $ d * (0.5^!e)

type CanBeDyadic t = ConvertibleExactly t Dyadic
dyadic :: (CanBeDyadic t) => t -> Dyadic
dyadic = convertExactly

instance HasIntegerBounds Dyadic where
  integerBounds = integerBounds . rational

instance ConvertibleExactly Integer Dyadic where
  safeConvertExactly n = Right (Dyadic n (int 0))

instance ConvertibleExactly Int Dyadic where
  safeConvertExactly = safeConvertExactly . integer

instance ConvertibleExactly Rational Dyadic where
  safeConvertExactly q
    | isDyadic = Right $ Dyadic n dp
    | otherwise = convError "this number is not dyadic" q
    where
    isDyadic = d == 2^!dp
    dp = integerLog2 d
    d = denominator q
    n = numerator q

instance Convertible Dyadic Double where
  safeConvert = safeConvert . rational

instance (ConvertibleExactly Dyadic t, SuitableForCE es) => ConvertibleExactly Dyadic (CollectErrors es t) where
  safeConvertExactly = fmap (\v -> CollectErrors (Just v) mempty) . safeConvertExactly

instance CanRound Dyadic where
  properFraction (Dyadic d e)
    | e >= 0 = (d * 2^!e, dyadic 0)
    | otherwise = (d `div` (2^!(-e)), normalize $ Dyadic (d `mod` (2^!(-e))) e)
  round (Dyadic d e)
    | e >= 0 = d * 2^!e
    | otherwise = d `div` (2^!(-e))

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
  , CanEnsureCE es b
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
  , CanEnsureCE es a
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

instance
  (HasOrderAsymmetric Dyadic b
  , CanEnsureCE es b
  , CanEnsureCE es (OrderCompareType Dyadic b)
  , IsBool (EnsureCE es (OrderCompareType Dyadic b))
  , SuitableForCE es)
  =>
  HasOrderAsymmetric Dyadic (CollectErrors es  b)
  where
  type OrderCompareType Dyadic (CollectErrors es  b) =
    EnsureCE es (OrderCompareType Dyadic b)
  lessThan = lift2TLCE lessThan
  leq = lift2TLCE leq
  greaterThan = lift2TLCE greaterThan
  geq = lift2TLCE geq

instance
  (HasOrderAsymmetric a Dyadic
  , CanEnsureCE es a
  , CanEnsureCE es (OrderCompareType a Dyadic)
  , IsBool (EnsureCE es (OrderCompareType a Dyadic))
  , SuitableForCE es)
  =>
  HasOrderAsymmetric (CollectErrors es a) Dyadic
  where
  type OrderCompareType (CollectErrors es  a) Dyadic =
    EnsureCE es (OrderCompareType a Dyadic)
  lessThan = lift2TCE lessThan
  leq = lift2TCE leq
  greaterThan = lift2TCE greaterThan
  geq = lift2TCE geq


instance CanTestPosNeg Dyadic

instance CanTestInteger Dyadic where
  certainlyNotInteger x =
    e >= 0
    where
    (Dyadic _d e) = normalize x
  certainlyIntegerGetIt x
    | e >= 0 = Just (d * 2^!e)
    | otherwise = Nothing
    where
    (Dyadic d e) = normalize x

{- unary functions -}

instance CanNeg Dyadic where
  negate (Dyadic d e) = Dyadic (negate d) e

instance CanAbs Dyadic where
  abs (Dyadic d e) = Dyadic (abs d) e

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
  , CanEnsureCE es b
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
  , CanEnsureCE es a
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
  add x1 x2 =
    normalize $ Dyadic (d1 + d2) e
    where
    (d1, d2, e) = unifyEs x1 x2

unifyEs :: Dyadic -> Dyadic -> (Integer, Integer, Int)
unifyEs (Dyadic d1 e1) (Dyadic d2 e2)
  | e1 <= e2 = (d1, (shift d2 (e2 P.- e1)), e)
  | otherwise = ((shift d1 (e1 P.- e2)), d2, e)
  where
  e = e1 `min` e2

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
  , CanEnsureCE es b
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
  , CanEnsureCE es a
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
  sub x1 x2 =
    normalize $ Dyadic (d1 - d2) e
    where
    (d1, d2, e) = unifyEs x1 x2

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
  , CanEnsureCE es b
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
  , CanEnsureCE es a
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
  mul (Dyadic d1 e1) (Dyadic d2 e2) = Dyadic (d1 * d2) (e1 P.+ e2)

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
  , CanEnsureCE es b
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
  , CanEnsureCE es a
  , CanEnsureCE es (MulType a Dyadic)
  , SuitableForCE es)
  =>
  CanMulAsymmetric (CollectErrors es a) Dyadic
  where
  type MulType (CollectErrors es  a) Dyadic =
    EnsureCE es (MulType a Dyadic)
  mul = lift2TCE mul

instance CanPow Dyadic Integer where
  powNoCN = powUsingMul (dyadic 1)
  pow = integerPowCN (powUsingMul (dyadic 1))
instance CanPow Dyadic Int where
  powNoCN = powUsingMul (dyadic 1)
  pow = integerPowCN (powUsingMul (dyadic 1))

instance
  (CanDiv a Dyadic
  , CanEnsureCE es a
  , CanEnsureCE es (DivType a Dyadic)
  , CanEnsureCE es (DivTypeNoCN a Dyadic)
  , SuitableForCE es)
  =>
  CanDiv (CollectErrors es a) Dyadic
  where
  type DivType (CollectErrors es  a) Dyadic =
    EnsureCE es (DivType a Dyadic)
  divide = lift2TCE divide
  type DivTypeNoCN (CollectErrors es a) Dyadic =
    EnsureCE es (DivTypeNoCN a Dyadic)
  divideNoCN = lift2TCE divideNoCN

instance CanDiv Integer Dyadic where
  type DivTypeNoCN Integer Dyadic = Rational
  divideNoCN a b = divideNoCN a (rational b)
instance CanDiv Dyadic Integer where
  type DivTypeNoCN Dyadic Integer = Rational
  divideNoCN a b = divideNoCN (rational a) b

instance CanDiv Int Dyadic where
  type DivTypeNoCN Int Dyadic = Rational
  divideNoCN a b = divideNoCN a (rational b)
instance CanDiv Dyadic Int where
  type DivTypeNoCN Dyadic Int = Rational
  divideNoCN a b = divideNoCN (rational a) b

instance CanDiv Rational Dyadic where
  type DivTypeNoCN Rational Dyadic = Rational
  divideNoCN = convertSecond divideNoCN
instance CanDiv Dyadic Rational where
  type DivTypeNoCN Dyadic Rational = Rational
  divideNoCN = convertFirst divideNoCN

instance
  (CanDiv Dyadic b
  , CanEnsureCE es b
  , CanEnsureCE es (DivType Dyadic b)
  , CanEnsureCE es (DivTypeNoCN Dyadic b)
  , SuitableForCE es)
  =>
  CanDiv Dyadic (CollectErrors es  b)
  where
  type DivType Dyadic (CollectErrors es  b) =
    EnsureCE es (DivType Dyadic b)
  divide = lift2TLCE divide
  type DivTypeNoCN Dyadic (CollectErrors es  b) =
    EnsureCE es (DivTypeNoCN Dyadic b)
  divideNoCN = lift2TLCE divideNoCN

instance
  (CanPow Dyadic b
  , CanEnsureCE es b
  , CanEnsureCE es (PowTypeNoCN Dyadic b)
  , CanEnsureCE es (PowType Dyadic b)
  , SuitableForCE es)
  =>
  CanPow Dyadic (CollectErrors es  b)
  where
  type PowTypeNoCN Dyadic (CollectErrors es b) =
    EnsureCE es (PowTypeNoCN Dyadic b)
  powNoCN = lift2TLCE powNoCN
  type PowType Dyadic (CollectErrors es b) =
    EnsureCE es (PowType Dyadic b)
  pow = lift2TLCE pow

instance
  (CanPow a Dyadic
  , CanEnsureCE es a
  , CanEnsureCE es (PowType a Dyadic)
  , CanEnsureCE es (PowTypeNoCN a Dyadic)
  , SuitableForCE es)
  =>
  CanPow (CollectErrors es a) Dyadic
  where
  type PowTypeNoCN (CollectErrors es  a) Dyadic =
    EnsureCE es (PowTypeNoCN a Dyadic)
  powNoCN = lift2TCE powNoCN
  type PowType (CollectErrors es  a) Dyadic =
    EnsureCE es (PowType a Dyadic)
  pow = lift2TCE pow

instance Arbitrary Dyadic where
  arbitrary =
    normalize <$> (Dyadic <$> arbitrary <*> arbitrary)

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

-}

