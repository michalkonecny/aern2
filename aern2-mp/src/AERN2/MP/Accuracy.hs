{-|
    Module      :  AERN2.MP.Accuracy
    Description :  Rough accuracy of an enclosure
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A type for roughly measuring the accuracy of an enclosure.
-}
module AERN2.MP.Accuracy
    (Accuracy(NoInformation, Exact), bits, fromAccuracy,
     normLog2Accuracy,
     HasAccuracy(..), getFiniteAccuracy,
     CanReduceSizeUsingAccuracyGuide(..),
      specCanReduceSizeUsingAccuracyGuide,
     iterateUntilAccurate,
     convergentList2CauchySeq,
     seqByPrecision2CauchySeq,
     setPrecisionAtLeastAccuracy,
     HasApproximate(..))
where

import Numeric.MixedTypes
import qualified Prelude as P

import Data.Complex

import Control.Lens

import Test.Hspec
import Test.QuickCheck

import Numeric.CatchingExceptions

import AERN2.Norm
import AERN2.MP.Precision

{- example -}

_example1 :: Accuracy
_example1 = 1 + 2*(bits 100)

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
data Accuracy = NoInformation | Bits { fromAccuracy :: Integer } | Exact
  deriving (P.Eq, P.Ord)

instance Arbitrary Accuracy where
  arbitrary =
    frequency
      [(int 1, pure Exact),
       (int 1, pure NoInformation),
       (int 8, Bits <$> arbitrary)]

instance Enum Accuracy where
    fromEnum NoInformation = minBound
    fromEnum (Bits i) = int i
    fromEnum Exact = maxBound
    toEnum i = Bits (integer i)

instance Bounded Accuracy where
    minBound = NoInformation
    maxBound = Exact

instance ConvertibleExactly Integer Accuracy where
  safeConvertExactly = Right . Bits
instance ConvertibleExactly Int Accuracy where
  safeConvertExactly = Right . Bits . integer
instance ConvertibleExactly Precision Accuracy where
  safeConvertExactly = Right . Bits . integer

bits :: (ConvertibleExactly t Accuracy) => t -> Accuracy
bits = convertExactly

normLog2Accuracy :: NormLog -> Accuracy
normLog2Accuracy (NormBits b) = bits (-b)
normLog2Accuracy NormZero = Exact

instance Show Accuracy where
    show (NoInformation) = "NoInformation"
    show (Bits a) = "bits " ++ show a
    show (Exact) = "Exact"

instance HasEqAsymmetric Accuracy Accuracy
instance HasOrderAsymmetric Accuracy Accuracy
instance CanMinMaxAsymmetric Accuracy Accuracy

instance HasEqAsymmetric Accuracy Integer where
  equalTo = convertSecond equalTo
instance HasEqAsymmetric Integer Accuracy where
  equalTo = convertFirst equalTo
instance HasEqAsymmetric Accuracy Int where
  equalTo = convertSecond equalTo
instance HasEqAsymmetric Int Accuracy where
  equalTo = convertFirst equalTo

instance HasOrderAsymmetric Accuracy Integer where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric Integer Accuracy where
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric Accuracy Int where
  lessThan = convertSecond lessThan
  leq = convertSecond leq
instance HasOrderAsymmetric Int Accuracy where
  lessThan = convertFirst lessThan
  leq = convertFirst leq

instance CanMinMaxAsymmetric Accuracy Integer where
    type MinMaxType Accuracy Integer = Accuracy
    min = convertSecond min
    max = convertSecond max
instance CanMinMaxAsymmetric Integer Accuracy where
    type MinMaxType Integer Accuracy = Accuracy
    min = convertFirst min
    max = convertFirst max
instance CanMinMaxAsymmetric Accuracy Int where
    type MinMaxType Accuracy Int = Accuracy
    min = convertSecond min
    max = convertSecond max
instance CanMinMaxAsymmetric Int Accuracy where
    type MinMaxType Int Accuracy = Accuracy
    min = convertFirst min
    max = convertFirst max

instance CanNeg Accuracy where
  negate NoInformation = Exact
  negate Exact = NoInformation
  negate (Bits a) = Bits (-a)

instance CanAddAsymmetric Accuracy Accuracy where
   add NoInformation _ = NoInformation
   add _ NoInformation = NoInformation
   add (Bits a) (Bits b) = Bits $ a + b
   add Exact _ = Exact
   add _ Exact = Exact

instance CanSub Accuracy Accuracy

--instance CanMulAsymmetric Accuracy Accuracy where
--    mulA NoInformation _ = NoInformation
--    mulA _ NoInformation = NoInformation
--    mulA (Bits a) (Bits b) = Bits $ a * b
--    mulA Exact _ = Exact
--    mulA _ Exact = Exact

instance CanMulAsymmetric Accuracy Integer where
    type MulType Accuracy Integer = Accuracy
    mul NoInformation _ = NoInformation
    mul (Bits a) i = Bits $ a * i
    mul Exact _ = Exact

instance CanMulAsymmetric Integer Accuracy where
    type MulType Integer Accuracy = Accuracy
    mul i a = mul a i

instance CanAddAsymmetric Accuracy Integer where
    type AddType Accuracy Integer = Accuracy
    add NoInformation _ = NoInformation
    add (Bits a) i = Bits $ a + i
    add Exact _ = Exact

instance CanAddAsymmetric Integer Accuracy where
    type AddType Integer Accuracy = Accuracy
    add i a = add a i

instance CanSub Accuracy Integer where
    type SubType Accuracy Integer = Accuracy
    sub NoInformation _ = NoInformation
    sub (Bits a) i = Bits $ a - i
    sub Exact _ = Exact

class HasAccuracy a where
  getAccuracy :: a -> Accuracy

instance HasAccuracy a => HasAccuracy (CatchingNumExceptions a) where
  getAccuracy aCE =
    case aCE ^. numEXC_maybeValue of
      Just v -> getAccuracy v
      _ -> NoInformation

instance HasAccuracy Int where getAccuracy _ = Exact
instance HasAccuracy Integer where getAccuracy _ = Exact
instance HasAccuracy Rational where getAccuracy _ = Exact

instance HasAccuracy t => HasAccuracy (Complex t) where
  getAccuracy (a :+ i) =
    (getAccuracy a) `min` (getAccuracy i)

instance HasAccuracy t => HasAccuracy [t] where
  getAccuracy xs = foldl min Exact $ map getAccuracy xs

{-| Return accuracy, except when the element is Exact, return its nominal Precision dressed as Accuracy.
    This function is useful when we have a convergent sequence where all elements happen to be
    actually equal to the limit and we need the property that the sequence elements keep improving.
-}
getFiniteAccuracy ::
    (HasAccuracy t, HasPrecision t) =>
    t -> Accuracy
getFiniteAccuracy b =
    case getAccuracy b of
        Exact -> bits $ getPrecision b
        a -> a

iterateUntilAccurate ::
  (HasAccuracy t) =>
  Accuracy ->
  (Precision -> Maybe t) ->
  [(Precision, Maybe t)]
iterateUntilAccurate ac =
  iterateUntilOK $ \maybeResult ->
      case maybeResult of
          Just result -> getAccuracy result >= ac
          _ -> False

seqByPrecision2CauchySeq ::
    (HasAccuracy t) =>
    (Precision -> t) -> (Accuracy -> t)
seqByPrecision2CauchySeq seqByPrecision ac =
    convergentList2CauchySeq list ac
    where
    list =
      map seqByPrecision $ dropWhile (lowPrec ac) standardPrecisions
    lowPrec Exact _ = False
    lowPrec _ p = bits p < ac

convergentList2CauchySeq :: (HasAccuracy t) => [t] -> (Accuracy -> t)
convergentList2CauchySeq list ac = findAccurate list
  where
  findAccurate [] =
    error "convergentList2CauchySeq: the sequence either converges too slowly or it does not converge"
  findAccurate (b : rest)
    | getAccuracy b >= ac = b
    | otherwise = findAccurate rest

{-|
    Change the precision so that
    it is at least as high as the supplied accuracy
    (assuming the accuracy is finite).
-}
setPrecisionAtLeastAccuracy :: (CanSetPrecision t) => Accuracy -> t -> t
setPrecisionAtLeastAccuracy acc b
    | p_b < p_acc = setPrecision p_acc b
    | otherwise = b
    where
    p_acc =
        case acc of
          Exact -> error $ "setPrecisionAtLeastAccuracy: cannot match Exact accuracy"
          NoInformation -> p_b
          _ -> prec $ max 2 (fromAccuracy acc)
    p_b = getPrecision b


class CanReduceSizeUsingAccuracyGuide t where
  reduceSizeUsingAccuracyGuide :: Accuracy -> t -> t

specCanReduceSizeUsingAccuracyGuide ::
  ( CanReduceSizeUsingAccuracyGuide t
  , HasEqCertainly t t
  , Arbitrary t, Show t)
  =>
  (T t) -> Spec
specCanReduceSizeUsingAccuracyGuide (T tName :: T t) =
  describe ("CanReduceSizeUsingAccuracyGuide " ++ tName) $ do
    it "is safe" $
      property $
        \ (t :: t) (ac :: Accuracy) ->
          reduceSizeUsingAccuracyGuide ac t ?==? t

{-| An unsafe approximation of an enclosure or exact value,
    useful mainly for showing something brief and readable to humans.
-}
class HasApproximate t where
    type Approximate t
    getApproximate :: Accuracy -> t -> (Approximate t)
