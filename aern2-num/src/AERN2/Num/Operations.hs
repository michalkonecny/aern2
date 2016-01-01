{-# LANGUAGE Arrows, DefaultSignatures, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, TypeOperators, FlexibleContexts, ConstraintKinds  #-}

module AERN2.Num.Operations
(
    module Prelude, (.), id,
    fromInteger, fromRational, ifThenElse, 
    fromInt, toInt,
    ConvertibleA(..), Convertible, convert,
    HasIntsA, intA, HasInts, int, intDefault,
    HasIntegersA, integerA, HasIntegers, integer, integerDefault, 
    HasRationalsA, rationalA, HasRationals, rational, rationalDefault,
    HasEqA(..), HasOrderA(..),
    HasEq, HasOrder, equalTo, notEqualTo, lessThan, leq, greaterThan, geq,
    (==), (/=), (>), (<), (<=), (>=),
    CanMinMaxA(..), CanMinMaxThisA, CanMinMaxSameTypeA,
    CanMinMax, CanMinMaxThis, CanMinMaxSameType, min, max,
    CanNegA(..), CanNegSameTypeA,
    CanNeg, CanNegSameType, neg, negate, 
    CanAbsA(..), CanAbsSameTypeA,
    CanAbs, CanAbsSameType, abs,
    CanAddA(..), CanAddThisA, CanAddSameTypeA, sumA,
    CanAdd, CanAddThis, CanAddSameType, add, sum, (+),
    CanSubA(..), CanSubThisA, CanSubSameTypeA,
    CanSub, CanSubThis, CanSubSameType, sub, (-),
    CanMulA(..), CanMulByA, CanMulSameTypeA, productA, 
    CanMul, CanMulBy, CanMulSameType, mul, (*), product,
    CanPowA(..), CanPowByA,
    CanPow, CanPowBy, pow, (^),
    CanDivA(..), CanDivByA, CanDivSameTypeA,
    CanDiv, CanDivBy, CanDivSameType, div, (/),
    CanRecipA(..), CanRecipSameTypeA,
    CanRecip, CanRecipSameType, recip,
    RingA, FieldA, CanAddMulScalarA, CanAddMulDivScalarA,
    Ring, Field, CanAddMulScalar, CanAddMulDivScalar,
    CanSqrtA(..), CanSqrtSameTypeA,
    CanSqrt, CanSqrtSameType, sqrt,
    CanExpA(..), CanExpSameTypeA,
    CanExp, CanExpSameType, exp,
    CanSineCosineA(..), CanSineCosineSameTypeA,
    CanSineCosine, CanSineCosineSameType, sin, cos
)
where

import Prelude hiding
    (id, (.),
     (==),(/=),(<),(>),(<=),(>=),
     (+),(*),(/),(-),(^),sum,product,abs,min,max,
     recip,div,negate,
     fromInteger,fromRational,
     pi,sqrt,exp,cos,sin)

import qualified Prelude as P

import Control.Category
import Control.Arrow

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


class ConvertibleA to a b where
    convertA :: a `to` b

type Convertible = ConvertibleA (->)

convert :: (Convertible a b) => a -> b
convert = convertA

type HasIntegersA to = ConvertibleA to Integer
integerA :: (HasIntegersA to a) => Integer `to` a
integerA = convertA

{-| 
    This is useful for embedding integers into other types
    when using the monomorphic fromInteger.
-}
type HasIntegers = HasIntegersA (->)
integer :: (HasIntegers a) => Integer -> a
integer = integerA
integerDefault :: (Num a) => Integer -> a
integerDefault n = P.fromInteger n 

instance ConvertibleA (->) Integer Int where convertA = integerDefault
instance ConvertibleA (->) Integer Integer where convertA = id
instance ConvertibleA (->) Integer Rational where convertA = integerDefault

type HasIntsA to = ConvertibleA to Int
intA :: (HasIntsA to a) => Int `to` a
intA = convertA

type HasInts = HasIntsA (->)
int :: (HasInts a) => Int -> a
int = intA
intDefault :: (Num a) => Int -> a
intDefault = P.fromIntegral

instance ConvertibleA (->) Int Int where convertA = id
instance ConvertibleA (->) Int Integer where convertA = intDefault
instance ConvertibleA (->) Int Rational where convertA = intDefault

{-| 
    This is useful for embedding rationals into other types
    when using the monomorphic fromRational. 
-}
type HasRationalsA to = ConvertibleA to Rational
rationalA :: (HasRationalsA to a) => Rational `to` a
rationalA = convertA

type HasRationals = HasRationalsA (->)
rational :: (HasRationals a) => Rational -> a
rational = rationalA
rationalDefault :: (Fractional a) => Rational -> a
rationalDefault = P.fromRational 

instance ConvertibleA (->) Rational Rational where convertA = id

{- 
    The following mixed-type operators shadow the classic mono-type Prelude versions. 
-}

infixl 8 ^
infixl 7 *, /
infixl 6 +, -

{- equality -}

class Arrow to => HasEqA to a b where
    type EqCompareTypeA to a b
    type EqCompareTypeA to a b = Bool -- default
    equalToA :: (a,b) `to` (EqCompareTypeA to a b)
    -- default equalToA via Prelude for (->) and Bool:
    default equalToA :: (to ~ (->), EqCompareTypeA (->) a b ~ Bool, a~b, P.Eq a) => (a,b) -> Bool
    equalToA = uncurry (P.==)
    notEqualToA :: (a,b) `to` (EqCompareTypeA to a b)
    -- default notEqualToA via equalToA for Bool:
    default notEqualToA :: 
        (CanNegSameTypeA to (EqCompareTypeA to a b)) => 
        (a,b) `to` (EqCompareTypeA to a b)
    notEqualToA = negA <<< equalToA

type HasEq = HasEqA (->)
type EqCompareType a b = EqCompareTypeA (->) a b

equalTo :: (HasEq a b) => a -> b -> EqCompareType a b
equalTo = curry equalToA
notEqualTo :: (HasEq a b) => a -> b -> EqCompareType a b
notEqualTo = curry notEqualToA

(==) :: (HasEq a b) => a -> b -> EqCompareType a b
(==) = equalTo
(/=) :: (HasEq a b) => a -> b -> EqCompareType a b
(/=) = notEqualTo

instance HasEqA (->) Bool Bool
instance HasEqA (->) Char Char
instance (HasEqA (->) a a, EqCompareTypeA (->) a a ~ Bool) => HasEqA (->) (Maybe a) (Maybe a) where
    equalToA (Nothing, Nothing) = True
    equalToA (Just a, Just b) = equalToA (a, b)
    equalToA _ = False 
instance (HasEqA (->) a a, EqCompareTypeA (->) a a ~ Bool) => HasEqA (->) [a] [a] where
    equalToA ([],[]) = True
    equalToA (h1:t1, h2:t2) = (equalToA (h1, h2)) && (equalToA (t1, t2))
    equalToA _ = False 

{- order -}

class Arrow to => HasOrderA to a b where
    type OrderCompareTypeA to a b
    type OrderCompareTypeA to a b = Bool -- default
    lessThanA :: (a,b) `to` OrderCompareTypeA to a b
    default lessThanA :: 
        (to ~ (->), OrderCompareTypeA to a b ~ Bool, a~b, P.Ord a) => 
        (a,b) -> OrderCompareTypeA to a b
    lessThanA = uncurry (P.<)
    greaterThanA :: (a,b) `to` OrderCompareTypeA to a b
    default greaterThanA :: 
        (OrderCompareTypeA to a b ~ OrderCompareTypeA to b a, HasOrderA to b a) => 
        (a,b) `to` OrderCompareTypeA to a b
    greaterThanA = proc (a,b) -> lessThanA -< (b,a)
    leqA :: (a,b) `to` OrderCompareTypeA to a b
    default leqA :: 
        (to ~ (->), OrderCompareTypeA to a b ~ Bool, a~b, P.Ord a) => 
        (a,b) `to` OrderCompareTypeA to a b
    leqA = uncurry (P.<=)
    geqA :: (a,b) `to` OrderCompareTypeA to a b
    default geqA :: 
        (OrderCompareTypeA to a b ~ OrderCompareTypeA to b a, HasOrderA to b a) => 
        (a,b) `to` OrderCompareTypeA to a b
    geqA = proc (a,b) -> leqA -< (b,a)

type HasOrder = HasOrderA (->)
type OrderCompareType a b = OrderCompareTypeA (->) a b

lessThan :: (HasOrder a b) => a -> b -> OrderCompareType a b
lessThan = curry lessThanA
leq :: (HasOrder a b) => a -> b -> OrderCompareType a b
leq = curry leqA
greaterThan :: (HasOrder a b) => a -> b -> OrderCompareType a b
greaterThan = curry greaterThanA
geq :: (HasOrder a b) => a -> b -> OrderCompareType a b
geq = curry geqA

(<) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(<) = lessThan
(<=) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(<=) = leq
(>) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(>) = greaterThan
(>=) :: (HasOrder a b) => a -> b -> OrderCompareType a b
(>=) = geq

class (Arrow to) => CanMinMaxA to a b where
    type MinMaxTypeA to a b
    type MinMaxTypeA to a b = a -- default
    minA :: (a,b) `to` MinMaxTypeA to a b
    maxA :: (a,b) `to` MinMaxTypeA to a b
    default minA :: (to ~ (->), MinMaxTypeA to a b ~ a, a~b, P.Ord a) => (a,a) -> a
    minA = uncurry P.min
    default maxA :: (to ~ (->), MinMaxTypeA to a b ~ a, a~b, P.Ord a) => (a,a) -> a
    maxA = uncurry P.max

type CanMinMax = CanMinMaxA (->)
type MinMaxType a b = MinMaxTypeA (->) a b

min :: (CanMinMax a b) => a -> b -> MinMaxType a b
min = curry minA
max :: (CanMinMax a b) => a -> b -> MinMaxType a b
max = curry maxA

class
    (CanMinMaxA to a b, MinMaxTypeA to a b ~ a, CanMinMaxA to b a, MinMaxTypeA to b a ~ a) => 
    CanMinMaxThisA to a b

type CanMinMaxThis = CanMinMaxThisA (->)

class
    (CanMinMaxThisA to a a) => 
    CanMinMaxSameTypeA to a

type CanMinMaxSameType = CanMinMaxSameTypeA (->)

{- negation -}

class (Arrow to) => CanNegA to a where
    type NegTypeA to a :: *
    type NegTypeA to a = a -- default
    negA :: a `to` NegTypeA to a

type CanNeg = CanNegA (->)
type NegType a = NegTypeA (->) a

neg :: CanNeg a => a -> NegType a
neg = negA

negate :: CanNeg a => a -> NegType a
negate = neg

class
    (CanNegA to a, NegTypeA to a ~ a) => 
    CanNegSameTypeA to a

type CanNegSameType = CanNegSameTypeA (->)

instance (Arrow to) => CanNegA to Bool where
    negA = arr not

instance (Arrow to) => CanNegSameTypeA to Bool

{- abs -}

class (Arrow to) => CanAbsA to a where
    type AbsTypeA to a
    type AbsTypeA to a = a -- default
    absA :: a `to` AbsTypeA to a

type CanAbs = CanAbsA (->)
type AbsType a = AbsTypeA (->) a

abs :: (CanAbs a) => a -> AbsType a
abs = absA

class
    (CanAbsA to a, AbsTypeA to a ~ a) => 
    CanAbsSameTypeA to a

type CanAbsSameType = CanAbsSameTypeA (->)

{- recip -}

class CanRecipA to a where
    type RecipTypeA to a
    type RecipTypeA to a = a -- default
    recipA :: a `to` RecipTypeA to a

type CanRecip = CanRecipA (->)
type RecipType a = RecipTypeA (->) a

recip :: (CanRecip a) => a -> RecipType a
recip = recipA 

class
    (CanRecipA to a, RecipTypeA to a ~ a) => 
    CanRecipSameTypeA to a

type CanRecipSameType = CanRecipSameTypeA (->)

{- add -}

class (Arrow to) => CanAddA to a b where
    type AddTypeA to a b :: *
    type AddTypeA to a b = a -- default
    addA :: (a,b) `to` AddTypeA to a b

type CanAdd = CanAddA (->)
type AddType a b = AddTypeA (->) a b

add :: (CanAdd a b) => a -> b -> AddType a b
add = curry addA

(+) :: CanAdd a b => a -> b -> AddType a b
(+) = add

class
    (CanAddA to a b, AddTypeA to a b ~ a, CanAddA to b a, AddTypeA to b a ~ a) => 
    CanAddThisA to a b

type CanAddThis = CanAddThisA (->)

class
    (CanAddThisA to a a) => 
    CanAddSameTypeA to a

type CanAddSameType = CanAddSameTypeA (->)

sumA :: (ArrowChoice to, CanAddSameTypeA to a, HasIntegersA to a) => [a] `to` a
sumA = 
    proc list ->
        case list of
            [] -> integerA -< 0
            (x:xs) -> 
                do
                a <- sumA -< xs
                r <- addA -< (x, a)
                returnA -< r

sum :: (CanAddSameType a, HasIntegers a) => [a] -> a
sum = sumA

{- sub -}

class CanSubA to a b where
    type SubTypeA to a b :: *
    type SubTypeA to a b = AddTypeA to a (NegTypeA to b)
    subA :: (a,b) `to` SubTypeA to a b
    default subA :: (CanNegA to b, CanAddA to a c, c~NegTypeA to b) => (a,b) `to` AddTypeA to a (NegTypeA to b)
    subA = 
        proc (x,y) -> 
            do
            yn <- negA -< y
            r <- addA -< (x,yn)
            returnA -< r

type CanSub = CanSubA (->)
type SubType a b = SubTypeA (->) a b

sub :: (CanSub a b) => a -> b -> SubType a b
sub = curry subA

(-) :: CanSub a b => a -> b -> SubType a b
(-) = sub

class
    (CanSubA to a b, SubTypeA to a b ~ a) => 
    CanSubThisA to a b

type CanSubThis = CanSubThisA (->)

class
    (CanSubThisA to a a) => 
    CanSubSameTypeA to a

type CanSubSameType = CanSubSameTypeA (->)

{- mul -}

class (Arrow to) => CanMulA to a b where
    type MulTypeA to a b
    type MulTypeA to a b = a -- default
    mulA :: (a,b) `to` MulTypeA to a b

type CanMul = CanMulA (->)
type MulType a b = MulTypeA (->) a b

mul :: (CanMul a b) => a -> b -> MulType a b
mul = curry mulA

(*) :: CanMul a b => a -> b -> MulType a b
(*) = mul

class
    (CanMulA to a b, MulTypeA to a b ~ a, CanMulA to b a, MulTypeA to b a ~ a) => 
    CanMulByA to a b

type CanMulBy = CanMulByA (->)

class
    (CanMulByA to a a) => 
    CanMulSameTypeA to a

type CanMulSameType = CanMulSameTypeA (->)

productA :: (ArrowChoice to, CanMulSameTypeA to a, HasIntegersA to a) => [a] `to` a
productA = 
    proc list ->
        case list of
            [] -> integerA -< 1
            (x:xs) -> 
                do
                a <- productA -< xs
                r <- mulA -< (x, a)
                returnA -< r

product :: (CanMulSameType a, HasIntegers a) => [a] -> a
product = productA

{- div -}

class (Arrow to) => CanDivA to a b where
    type DivTypeA to a b :: *
    type DivTypeA to a b = MulTypeA to a (RecipTypeA to b)
    divA :: (a,b) `to` DivTypeA to a b
    default divA :: (CanRecipA to b, CanMulA to a c, c~RecipTypeA to b) => (a,b) `to` MulTypeA to a (RecipTypeA to b)
    divA =
        proc (x,y) ->
            do
            ry <- recipA -< y
            r <- mulA -< (x,ry)
            returnA -< r

type CanDiv = CanDivA (->)
type DivType a b = DivTypeA (->) a b

div :: (CanDiv a b) => a -> b -> DivType a b
div = curry divA

(/) :: CanDiv a b => a -> b -> DivType a b
(/) = div

class
    (CanDivA to a b, DivTypeA to a b ~ a) => 
    CanDivByA to a b

type CanDivBy = CanDivByA (->)

class
    (CanDivByA to a a) => 
    CanDivSameTypeA to a

type CanDivSameType = CanDivSameTypeA (->)

class CanPowA to a b where
    type PowTypeA to a b
    type PowTypeA to a b = a -- default
    powA :: (a,b) `to` PowTypeA to a b

type CanPow = CanPowA (->)
type PowType a b = PowTypeA (->) a b

pow :: (CanPow a b) => a -> b -> PowType a b
pow = curry powA

(^) :: (CanPow a b) => a -> b -> PowType a b
(^) = pow

class
    (CanPowA to a b, PowTypeA to a b ~ a) => 
    CanPowByA to a b

type CanPowBy = CanPowByA (->)

class
    (CanNegSameTypeA to a, CanAddSameTypeA to a, CanSubSameTypeA to a, CanMulSameTypeA to a, 
     HasEqA to a a, HasOrderA to a a, HasIntegersA to a)
    => 
    RingA to a

type Ring = RingA (->)

class
    (RingA to a, CanDivSameTypeA to a, CanRecipSameTypeA to a)
    =>
    FieldA to a
    
type Field = FieldA (->)

class
    (CanAddThisA to a s, CanMulByA to a s)
    =>
    CanAddMulScalarA to a s 
    
type CanAddMulScalar = CanAddMulScalarA (->)
    
class
    (CanAddMulScalarA to a s, CanDivByA to a s)
    =>
    CanAddMulDivScalarA to a s 
    
type CanAddMulDivScalar = CanAddMulDivScalarA (->)

class CanSqrtA to a where
    type SqrtTypeA to a :: *
    type SqrtTypeA to a = a -- default
    sqrtA :: a `to` SqrtTypeA to a

type CanSqrt = CanSqrtA (->)
type SqrtType a = SqrtTypeA (->) a

sqrt :: (CanSqrt a) => a -> SqrtType a
sqrt = sqrtA

class
    (CanSqrtA to a, SqrtTypeA to a ~ a) => 
    CanSqrtSameTypeA to a

type CanSqrtSameType = CanSqrtSameTypeA (->)

class CanExpA to a where
    type ExpTypeA to a :: *
    type ExpTypeA to a = a -- default
    expA :: a `to` ExpTypeA to a

type CanExp = CanExpA (->)
type ExpType a = ExpTypeA (->) a

exp :: (CanExp a) => a -> ExpType a
exp = expA

class
    (CanExpA to a, ExpTypeA to a ~ a) => 
    CanExpSameTypeA to a

type CanExpSameType = CanExpSameTypeA (->)

class CanSineCosineA to a where
    type SineCosineTypeA to a :: *
    type SineCosineTypeA to a = a -- default
    sinA :: a `to` SineCosineTypeA to a
    cosA :: a `to` SineCosineTypeA to a

type CanSineCosine = CanSineCosineA (->)
type SineCosineType a = SineCosineTypeA (->) a

sin :: (CanSineCosine a) => a -> SineCosineType a
sin = sinA
cos :: (CanSineCosine a) => a -> SineCosineType a
cos = cosA

class
    (CanSineCosineA to a, SineCosineTypeA to a ~ a) => 
    CanSineCosineSameTypeA to a

type CanSineCosineSameType = CanSineCosineSameTypeA (->)
    