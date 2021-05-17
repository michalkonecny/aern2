{-|
    Module      :  AERN2.MP.Accuracy
    Description :  Rough accuracy of an enclosure
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A type for measuring the accuracy of an enclosing set,
    roughly corresponding to the maximum absolute error in some distance metric
    approximately measured in bits.
-}
module AERN2.MP.Accuracy
    (Accuracy(NoInformation, Exact), bits, fromAccuracy,
     HasAccuracy(..),
     ac2prec,
     iterateUntilAccurate,
     convergentList2seqByAccuracy,
     seqByPrecision2seqByAccuracy,
     setPrecisionAtLeastAccuracy,
     ShowWithAccuracy(..),
     HasApproximate(..))
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Control.CollectErrors

import Data.Complex

-- import Test.Hspec
import Test.QuickCheck ( Arbitrary(arbitrary), frequency )

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
instance ConvertibleExactly NormLog Accuracy where
  safeConvertExactly (NormBits b) = Right $ bits (-b)
  safeConvertExactly NormZero = Right Exact

bits :: (ConvertibleExactly t Accuracy) => t -> Accuracy
bits = convertExactly

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
  {-| Return accuracy, except when the element is Exact, return its nominal Precision dressed as Accuracy.
      This function is useful when we have a convergent sequence where all elements happen to be
      actually equal to the limit and we need the property that the sequence elements keep improving.
  -}
  getFiniteAccuracy :: a -> Accuracy
  default getFiniteAccuracy :: (HasPrecision a) => a -> Accuracy
  getFiniteAccuracy b =
      case getAccuracy b of
          Exact -> bits $ getPrecision b
          a -> a

instance (HasAccuracy a, CanBeErrors es) => HasAccuracy (CollectErrors es a) where
  getAccuracy (CollectErrors ma es) =
    case ma of
      Just a | not (hasCertainError es) -> getAccuracy a
      _ -> NoInformation
  getFiniteAccuracy (CollectErrors ma es) = 
    case ma of
      Just a | not (hasCertainError es) -> getFiniteAccuracy a
      _ -> NoInformation

instance HasAccuracy Int where getAccuracy _ = Exact; getFiniteAccuracy _ = NoInformation
instance HasAccuracy Integer where getAccuracy _ = Exact; getFiniteAccuracy _ = NoInformation
instance HasAccuracy Rational where getAccuracy _ = Exact; getFiniteAccuracy _ = NoInformation
instance HasAccuracy Bool where getAccuracy _ = Exact; getFiniteAccuracy _ = NoInformation
instance HasAccuracy Kleenean where getAccuracy _ = Exact; getFiniteAccuracy _ = NoInformation

instance HasAccuracy t => HasAccuracy (Complex t) where
  getAccuracy (a :+ i) =
    (getAccuracy a) `min` (getAccuracy i)
  getFiniteAccuracy (a :+ i) =
    (getFiniteAccuracy a) `min` (getFiniteAccuracy i)
  
instance HasAccuracy t => HasAccuracy [t] where
  getAccuracy xs = foldl min Exact $ map getAccuracy xs
  getFiniteAccuracy xs = foldl min Exact $ map getFiniteAccuracy xs

instance HasAccuracy t => HasAccuracy (Maybe t) where
  getAccuracy (Just x) = getAccuracy x
  getAccuracy _ = NoInformation
  getFiniteAccuracy (Just x) = getFiniteAccuracy x
  getFiniteAccuracy _ = NoInformation

iterateUntilAccurate ::
  (HasAccuracy t) =>
  Accuracy ->
  (Precision -> Maybe t) ->
  [(Precision, Maybe t)]
iterateUntilAccurate ac =
  iterateUntilOK (ac2prec ac) $ \maybeResult ->
    case maybeResult of
      Just result -> getAccuracy result >= ac
      _ -> False

ac2prec :: Accuracy -> Precision
ac2prec ac =
  case ac of
    Bits b -> prec (max 2 $ b + 50)
    _ -> prec 100

seqByPrecision2seqByAccuracy ::
    (HasAccuracy t) =>
    (Precision -> t) -> (Accuracy -> t)
seqByPrecision2seqByAccuracy seqByPrecision ac =
    convergentList2seqByAccuracy list ac
    where
    list =
      map seqByPrecision $ dropWhile (lowPrec ac) (standardPrecisions (ac2prec ac))
    lowPrec Exact _ = False
    lowPrec _ p = bits p < ac

convergentList2seqByAccuracy :: (HasAccuracy t) => [t] -> (Accuracy -> t)
convergentList2seqByAccuracy list ac = findAccurate list
  where
  findAccurate [] =
    error "convergentList2seqByAccuracy: the sequence either converges too slowly or it does not converge"
  findAccurate (b : rest)
    | getAccuracy b >= ac = b
    | otherwise = findAccurate rest

{-|
    Change the precision so that
    it is at least as high as the supplied accuracy
    (assuming the accuracy is finite).
-}
setPrecisionAtLeastAccuracy :: (HasPrecision t, CanSetPrecision t) => Accuracy -> t -> t
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

class ShowWithAccuracy t where
  showWithAccuracy :: Accuracy -> t -> String

{-| An unsafe approximation of an enclosure or exact value,
    useful mainly for showing something brief and readable to humans.
-}
class HasApproximate t where
    type Approximate t
    getApproximate :: Accuracy -> t -> (Approximate t)
