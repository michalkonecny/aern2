{-# LANGUAGE DefaultSignatures, UndecidableInstances, TypeSynonymInstances, FlexibleInstances  #-}

module AERN2.Real.Operations 
(
    module Prelude,
    fromInteger, fromRational, ifThenElse, fromInt, toInt,
    HasIntegers(..), HasRationals(..),
    (==), (/=), (>), (<), (<=), (>=),
    HasEq(..), HasOrder(..),
    negate, CanNeg(..), CanNegSameType,
    (+), (-), (*), (/), (^), sum, product,
    CanAbs(..), CanAbsSameType,
    CanMinMax(..), CanMinMaxThis, CanMinMaxSameType,
    CanAdd(..), CanAddThis, CanAddSameType,
    CanSub(..), CanSubThis, CanSubSameType,
    CanMul(..), CanMulBy, CanMulSameType,
    CanPow(..), CanPowBy,
    CanDiv(..), CanDivBy, CanDivSameType,
    CanRecip(..), CanRecipSameType,
    Ring, Field, CanAddMulScalar, CanAddMulDivScalar,
    CanSqrt(..), CanSqrtSameType,
    CanSineCosine(..), CanSineCosineSameType
)
where

import Prelude hiding
    ((==),(/=),(<),(>),(<=),(>=),
     (+),(*),(/),(-),(^),sum,product,abs,min,max,
     recip,div,negate,
     fromInteger,fromRational,
     pi,sqrt,cos,sin)

import qualified Prelude as P

{- 
    The following arranges that all numeric literals are monomorphic and of the type Integer or Rational.
-}

_example1 :: Integer -- inferred
_example1 = 1 -- not polymorphic 
_example2 :: Rational -- inferred
_example2 = 1.0 -- not polymorphic
_example3 :: Int
_example3 = toInt 1 -- the easiest way to get Int literals (1 :: Int does not compile)
_example4 :: Integer
_example4 = fromInt (length [])

fromInteger :: Integer -> Integer
fromInteger = id

fromRational :: Rational -> Rational
fromRational = id

-- the following is needed to restore if-then-else while using RebindableSyntax 
ifThenElse :: Bool -> t -> t -> t
ifThenElse b e1 e2
    | b = e1
    | otherwise = e2

toInt :: Integer -> Int
toInt i 
    | iInIntRange = P.fromInteger i
    | otherwise = error $ "int out of range: " ++ show i 
    where
    iInIntRange =
        i P.>= toInteger (minBound :: Int)
        &&
        i P.<= toInteger (maxBound :: Int)

fromInt :: Int -> Integer
fromInt = P.toInteger


{-| 
    This is useful for embedding integers into other types
    when using the monomorphic fromInteger.
-}
class HasIntegers a where
    integer :: Integer -> a
    default integer :: (Num a) => Integer -> a
    integer n = P.fromInteger n 

instance HasIntegers Integer
instance HasIntegers Rational

{-| 
    This is useful for embedding rationals into other types
    when using the monomorphic fromRational. 
-}
class HasRationals a where
    rational :: Rational -> a 



{- 
    The following mixed-type operators shadow the classic mono-type Prelude versions. 
-}

infixl 8 ^
infixl 7 *, /
infixl 6 +, -

negate :: CanNeg a => a -> NegType a
negate x = neg x
(+) :: CanAdd a b => a -> b -> AddType a b
(+) x y = add x y
(-) :: CanSub a b => a -> b -> SubType a b
(-) x y = sub x y
(*) :: CanMul a b => a -> b -> MulType a b
(*) x y = mul x y
(/) :: CanDiv a b => a -> b -> DivType a b
(/) x y = div x y
(^) :: CanPow a b => a -> b -> PowType a b
(^) x y = pow x y
sum :: (CanAddSameType a) => [a] -> a
sum = foldr1 (+)
product :: (CanMulSameType a) => [a] -> a
product = foldr1 (*)

(==) :: HasEq a b => a -> b -> EqCompareType a b
(==) x y = equalTo x y
(/=) :: HasEq a b => a -> b -> EqCompareType a b
(/=) x y = notEqualTo x y
(<) :: HasOrder a b => a -> b -> OrderCompareType a b
(<) x y = lessThan x y
(>) :: HasOrder a b => a -> b -> OrderCompareType a b
(>) x y = greaterThan x y
(<=) :: HasOrder a b => a -> b -> OrderCompareType a b
(<=) x y = leq x y
(>=) :: HasOrder a b => a -> b -> OrderCompareType a b
(>=) x y = geq x y

class HasEq a b where
    type EqCompareType a b
    type EqCompareType a b = Bool -- default
    equalTo :: a -> b -> EqCompareType a b
    default equalTo :: (EqCompareType a b ~ Bool, a~b, P.Eq a) => a -> b -> EqCompareType a b
    equalTo = (P.==)
    notEqualTo :: a -> b -> EqCompareType a b
    default notEqualTo :: (EqCompareType a b ~ Bool) => a -> b -> EqCompareType a b
    notEqualTo a b = not $ equalTo a b 

instance HasEq Bool Bool
instance HasEq Char Char
instance (HasEq a a, EqCompareType a a ~ Bool) => HasEq (Maybe a) (Maybe a) where
    equalTo Nothing Nothing = True
    equalTo (Just a) (Just b) = equalTo a b
    equalTo _ _ = False 
instance (HasEq a a, EqCompareType a a ~ Bool) => HasEq [a] [a] where
    equalTo [] [] = True
    equalTo (h1:t1) (h2:t2) = (equalTo h1 h2) && (equalTo t1 t2)
    equalTo _ _ = False 

class HasOrder a b where
    type OrderCompareType a b
    type OrderCompareType a b = Bool -- default
    lessThan :: a -> b -> OrderCompareType a b
    default lessThan :: (OrderCompareType a b ~ Bool, a~b, P.Ord a) => a -> b -> OrderCompareType a b
    lessThan = (P.<)
    greaterThan :: a -> b -> OrderCompareType a b
    default greaterThan :: (OrderCompareType a b ~ OrderCompareType b a, HasOrder b a) => a -> b -> OrderCompareType a b
    greaterThan a b = lessThan b a
    leq :: a -> b -> OrderCompareType a b
    default leq :: (OrderCompareType a b ~ Bool, a~b, P.Ord a) => a -> b -> OrderCompareType a b
    leq = (P.<=)
    geq :: a -> b -> OrderCompareType a b
    default geq :: (OrderCompareType a b ~ OrderCompareType b a, HasOrder b a) => a -> b -> OrderCompareType a b
    geq a b = leq b a

class CanMinMax a b where
    type MinMaxType a b :: *
    type MinMaxType a b = a -- default
    min :: a -> b -> MinMaxType a b
    max :: a -> b -> MinMaxType a b
    default min :: (MinMaxType a b ~ a, a~b, P.Ord a) => a -> a -> a
    min = P.min
    default max :: (MinMaxType a b ~ a, a~b, P.Ord a) => a -> a -> a
    max = P.max

class
    (CanMinMax a b, MinMaxType a b ~ a, CanMinMax b a, MinMaxType b a ~ a) => 
    CanMinMaxThis a b

class
    (CanMinMaxThis a a) => 
    CanMinMaxSameType a


class CanNeg a where
    type NegType a :: *
    type NegType a = a -- default
    neg :: a -> NegType a

class
    (CanNeg a, NegType a ~ a) => 
    CanNegSameType a

class CanAbs a where
    type AbsType a :: *
    type AbsType a = a -- default
    abs :: a -> AbsType a

class
    (CanAbs a, AbsType a ~ a) => 
    CanAbsSameType a

class CanRecip a where
    type RecipType a :: *
    type RecipType a = a -- default
    recip :: a -> RecipType a

class
    (CanRecip a, RecipType a ~ a) => 
    CanRecipSameType a

class CanAdd a b where
    type AddType a b :: *
    type AddType a b = a -- default
    add :: a -> b -> AddType a b

class
    (CanAdd a b, AddType a b ~ a, CanAdd b a, AddType b a ~ a) => 
    CanAddThis a b

class
    (CanAddThis a a) => 
    CanAddSameType a

class CanSub a b where
    type SubType a b :: *
    type SubType a b = AddType a (NegType b)
    sub :: a -> b -> SubType a b
    default sub :: (CanNeg b, CanAdd a c, c~NegType b) => a -> b -> AddType a (NegType b)
    sub x y = add x (neg y)

class
    (CanSub a b, SubType a b ~ a) => 
    CanSubThis a b

class
    (CanSubThis a a) => 
    CanSubSameType a

class CanMul a b where
    type MulType a b :: *
    type MulType a b = a -- default
    mul :: a -> b -> MulType a b

class
    (CanMul a b, MulType a b ~ a, CanMul b a, MulType b a ~ a) => 
    CanMulBy a b

class
    (CanMulBy a a) => 
    CanMulSameType a

class CanDiv a b where
    type DivType a b :: *
    type DivType a b = MulType a (RecipType b)
    div :: a -> b -> DivType a b
    default div :: (CanRecip b, CanMul a c, c~RecipType b) => a -> b -> MulType a (RecipType b)
    div x y = mul x (recip y)

class
    (CanDiv a b, DivType a b ~ a) => 
    CanDivBy a b

class
    (CanDivBy a a) => 
    CanDivSameType a

class CanPow a b where
    type PowType a b :: *
    type PowType a b = a -- default
    pow :: a -> b -> PowType a b

class
    (CanPow a b, PowType a b ~ a) => 
    CanPowBy a b

class
    (CanNegSameType a, CanAddSameType a, CanSubSameType a, CanMulSameType a, 
     HasEq a a, HasOrder a a, HasIntegers a)
    => 
    Ring a
    
class
    (Ring a, CanDivSameType a, CanRecipSameType a)
    =>
    Field a
    
class
    (CanAddThis a s, CanMulBy a s)
    =>
    CanAddMulScalar a s 
    
class
    (CanAddMulScalar a s, CanDivBy a s)
    =>
    CanAddMulDivScalar a s 
    


class CanSqrt a where
    type SqrtType a :: *
    type SqrtType a = a -- default
    sqrt :: a -> SqrtType a

class
    (CanSqrt a, SqrtType a ~ a) => 
    CanSqrtSameType a

class CanSineCosine a where
    type SineCosineType a :: *
    type SineCosineType a = a -- default
    sin :: a -> SineCosineType a
    cos :: a -> SineCosineType a

class
    (CanSineCosine a, SineCosineType a ~ a) => 
    CanSineCosineSameType a

    