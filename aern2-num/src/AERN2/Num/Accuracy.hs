module AERN2.Num.Accuracy 
    (Accuracy(NoInformation, Exact), bits, fromAccuracy,
     HasAccuracy(..), getFiniteAccuracy, 
     iterateUntilAccurateA, iterateUntilAccurate,
     seqByPrecision2CauchySeq,
     HasApproximate(..)) 
where

import AERN2.Num.Operations
import Control.Arrow

import AERN2.Num.Precision
import AERN2.Num.IntegerRational ()

{- example -}

_example1 :: Accuracy
_example1 = 1 + 2*(bits 100)


{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
data Accuracy = NoInformation | Bits { fromAccuracy :: Integer } | Exact 
    deriving (Eq, Ord)

instance Enum Accuracy where
    fromEnum NoInformation = minBound
    fromEnum (Bits i) = int i
    fromEnum Exact = maxBound
    toEnum i = Bits (integer i)

instance Bounded Accuracy where
    minBound = NoInformation
    maxBound = Exact
    
bits :: Integer -> Accuracy
bits i = Bits i

instance Show Accuracy where
    show (NoInformation) = "NoInformation"
    show (Bits a) = "Bits " ++ show a
    show (Exact) = "Exact"

instance HasEq Accuracy Accuracy where

instance HasOrder Accuracy Accuracy where

instance CanMinMax Accuracy Accuracy where

instance CanMinMaxThis Accuracy Accuracy
instance CanMinMaxSameType Accuracy

--instance CanAddA (->) Accuracy Accuracy where
--    addA (NoInformation, _) = NoInformation
--    addA (_, NoInformation) = NoInformation
--    addA (Bits a, Bits b) = Bits $ a + b
--    addA (Exact, _) = Exact
--    addA (_, Exact) = Exact
--
--instance CanAddThis Accuracy Accuracy
--instance CanAddSameType Accuracy
--
--instance CanMulA (->) Accuracy Accuracy where
--    mulA (NoInformation, _) = NoInformation
--    mulA (_, NoInformation) = NoInformation
--    mulA (Bits a, Bits b) = Bits $ a * b
--    mulA (Exact, _) = Exact
--    mulA (_, Exact) = Exact
--
--instance CanMulBy Accuracy Accuracy
--instance CanMulSameType Accuracy

instance CanMinMaxA (->) Accuracy Integer where
    type MinMaxTypeA (->) Accuracy Integer = Accuracy
    minA (NoInformation, _) = NoInformation
    minA (Bits a, b) = Bits $ min a b
    minA (Exact, a) = Bits a
    maxA (NoInformation, a) = Bits a
    maxA (Bits a, b) = Bits $ max a b
    maxA (Exact, _) = Exact

instance CanMinMaxA (->) Integer Accuracy where
    type MinMaxTypeA (->) Integer Accuracy = Accuracy
    minA (a, b) = min b a
    maxA (a, b) = max b a

instance CanMinMaxThis Accuracy Integer

instance CanMulA (->) Accuracy Integer where
    type MulTypeA (->) Accuracy Integer = Accuracy
    mulA (NoInformation, _) = NoInformation
    mulA (Bits a, i) = Bits $ a * i
    mulA (Exact, _) = Exact

instance CanMulA (->) Integer Accuracy where
    type MulTypeA (->) Integer Accuracy = Accuracy
    mulA (i, a) = mul a i

instance CanMulBy Accuracy Integer

instance CanAddA (->) Accuracy Integer where
    type AddTypeA (->) Accuracy Integer = Accuracy
    addA (NoInformation, _) = NoInformation
    addA (Bits a, i) = Bits $ a + i
    addA (Exact, _) = Exact

instance CanAddA (->) Integer Accuracy where
    type AddTypeA (->) Integer Accuracy = Accuracy
    addA (i, a) = add a i

instance CanAddThis Accuracy Integer

instance CanSub Accuracy Integer where

instance CanSubThis Accuracy Integer

class HasAccuracy a
    where
    getAccuracy :: a -> Accuracy

{-| Return accuracy, except when the element is Exact, return its nominal Precision dressed as Accuracy. 
    This function is useful when we have a convergent sequence where all elements happen to be
    actually equal to the limit and we need the property that the sequence elements keep improving.
-}
getFiniteAccuracy ::
    (HasAccuracy t, HasPrecision t) => 
    t -> Accuracy
getFiniteAccuracy b =
    case getAccuracy b of
        Exact -> bits $ prec2integer (getPrecision b)
        a -> a

iterateUntilAccurateA :: 
    (ArrowChoice to, HasAccuracy t) 
    => 
    Accuracy -> 
    (Precision `to` Maybe t) -> 
    () `to` [(Precision, Maybe t)]
iterateUntilAccurateA ac = 
    iterateUntilOKA $ \maybeResult -> 
        case maybeResult of 
            Just result -> getAccuracy result >= ac
            _ -> False 

iterateUntilAccurate ::
    (HasAccuracy t) => 
    Accuracy -> 
    (Precision -> Maybe t) -> 
    [(Precision, Maybe t)]
iterateUntilAccurate ac fn = iterateUntilAccurateA ac fn () 

seqByPrecision2CauchySeq ::
    (HasAccuracy t) => 
    (Precision -> t) -> (Accuracy -> t)
seqByPrecision2CauchySeq seqByPrecision i =
    findAccurate $ map seqByPrecision $ dropWhile lowPrec standardPrecisions
    where
    lowPrec p = 
        case i of 
            Exact -> False
            _ -> bits (prec2integer p) < i
    findAccurate [] =
        error "seqByPrecision2CauchySeq: the sequence either converges too slowly or it does not converge"
    findAccurate (b : rest)
        | getAccuracy b >= i = b
        | otherwise = findAccurate rest

class HasApproximate t where
    type Approximate t
    getApproximate :: Accuracy -> t -> (Approximate t)

