module AERN2.Real.Accuracy 
    (Accuracy(Exact), bits, toInteger) 
where

import Prelude hiding
    ((==),(/=),(<),(>),(<=),(>=),
     (+),(*),(/),(-),(^),abs,min,max,
     recip,div,negate,
     fromInteger,fromRational, toRational, toInteger,
     sqrt,cos,sin)

import AERN2.Real.IntegerRational ()
import AERN2.Real.Operations

{- example -}

_example1 :: Accuracy
_example1 = 1 + 2*(bits 100)

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
data Accuracy = Exact | Bits { toInteger :: Integer }

bits :: Integer -> Accuracy
bits i = Bits i 

instance Show Accuracy where
    show (Bits a) = "Bits " ++ show a
    show (Exact) = "Exact"

instance HasEq Accuracy Accuracy where
    type EqCompareType Accuracy Accuracy = Bool
    equalTo (Bits a) (Bits b) = a == b
    equalTo Exact Exact = True
    equalTo _ _ = False
    notEqualTo a b = not (a == b)

instance HasOrder Accuracy Accuracy where
    type OrderCompareType Accuracy Accuracy = Bool
    lessThan (Bits a) (Bits b) = a < b
    lessThan Exact Exact = False
    lessThan _ Exact = True
    lessThan Exact _ = False
    greaterThan a b = lessThan b a
    leq a b = (a == b) || (lessThan a b)
    geq a b = leq b a

instance CanMinMax Accuracy Accuracy where
    type MinMaxType Accuracy Accuracy = Accuracy
    min (Bits a) (Bits b) = Bits $ min a b
    min Exact a = a
    min a Exact = a
    max (Bits a) (Bits b) = Bits $ max a b
    max Exact _ = Exact
    max _ Exact = Exact

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
