module AERN2.Real.Accuracy 
    (Accuracy(Exact), bits, fromAccuracy) 
where

import Prelude hiding
    ((==),(/=),(<),(>),(<=),(>=),
     (+),(*),(/),(-),(^),abs,min,max,
     recip,div,negate,
     fromInteger,fromRational, toRational,
     sqrt,cos,sin)

import AERN2.Real.IntegerRational ()
import AERN2.Real.Operations

{- example -}

_example1 :: Accuracy
_example1 = 1 + 2*(bits 100)

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
data Accuracy = Bits { fromAccuracy :: Integer } | Exact
    deriving (Eq, Ord)

bits :: Integer -> Accuracy
bits i = Bits i 

instance Show Accuracy where
    show (Bits a) = "Bits " ++ show a
    show (Exact) = "Exact"

instance HasEq Accuracy Accuracy where

instance HasOrder Accuracy Accuracy where

instance CanMinMax Accuracy Accuracy where

instance CanMinMaxThis Accuracy Accuracy
instance CanMinMaxSameType Accuracy

instance CanAdd Accuracy Accuracy where
    type AddType Accuracy Accuracy = Accuracy
    add (Bits a) (Bits b) = Bits $ a + b
    add Exact _ = Exact
    add _ Exact = Exact

instance CanAddThis Accuracy Accuracy
instance CanAddSameType Accuracy

instance CanMul Accuracy Accuracy where
    type MulType Accuracy Accuracy = Accuracy
    mul (Bits a) (Bits b) = Bits $ a * b
    mul Exact _ = Exact
    mul _ Exact = Exact

instance CanMulBy Accuracy Accuracy
instance CanMulSameType Accuracy

instance CanMinMax Accuracy Integer where
    type MinMaxType Accuracy Integer = Accuracy
    min (Bits a) b = Bits $ min a b
    min Exact a = Bits a
    max (Bits a) b = Bits $ max a b
    max Exact _ = Exact

instance CanMinMax Integer Accuracy where
    type MinMaxType Integer Accuracy = Accuracy
    min a b = min b a
    max a b = max b a

instance CanMinMaxThis Accuracy Integer

instance CanMul Accuracy Integer where
    type MulType Accuracy Integer = Accuracy
    mul (Bits a) i = Bits $ a * i
    mul Exact _ = Exact

instance CanMul Integer Accuracy where
    type MulType Integer Accuracy = Accuracy
    mul i a = mul a i

instance CanMulBy Accuracy Integer

instance CanAdd Accuracy Integer where
    type AddType Accuracy Integer = Accuracy
    add (Bits a) i = Bits $ a + i
    add Exact _ = Exact

instance CanAdd Integer Accuracy where
    type AddType Integer Accuracy = Accuracy
    add i a = add a i

instance CanAddThis Accuracy Integer

instance CanSub Accuracy Integer where
    type SubType Accuracy Integer = Accuracy
    sub (Bits a) i = Bits $ a - i
    sub Exact _ = Exact

instance CanSubThis Accuracy Integer
