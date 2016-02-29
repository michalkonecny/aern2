{-# LANGUAGE Arrows, DefaultSignatures, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, TypeOperators, FlexibleContexts, ConstraintKinds, GeneralizedNewtypeDeriving  #-}

module AERN2.Num.Operations
(
    module Prelude, (.), id,
    fromInteger, fromRational, ifThenElse,
    IsString, fromString,
    ArrowConvert(..), Fn2Arrow, fn2arrow, fn2arrowNamed, Arrow2Fn, arrow2fn,
    ConvertibleA(..), convertListNamedA, Convertible, convert, convertList,
    CanEmbedFnA, embedFnNamedA,
    HasIntsA, HasInts, fromIntADefault, 
    CanBeIntA, intA, intNamedA, intsA, intsNamedA, CanBeInt, int, intDefault, ints,
    (!!!),
    HasIntegersA, HasIntegers, fromIntegerADefault, 
    CanBeIntegerA, integerA, integerNamedA, integersA, integersNamedA, CanBeInteger, integer, integerADefault, integers, 
    HasRationalsA, HasRationals, fromRationalADefault, 
    CanBeRationalA, rationalA, rationalNamedA, rationalsA, rationalsNamedA, CanBeRational, rational, rationalDefault, rationals,
    HasBoolsA, HasBools, 
    notA, not, 
    CanAndOrA(..), CanAndOrSameTypeA, andA, orA, CanAndOr, CanAndOrSameType, AndOrType, (&&), (||), and, or,
    BoolA,
    HasEqA(..), HasOrderA(..),
    HasEq, EqCompareType, HasOrder, OrderCompareType, equalTo, notEqualTo, lessThan, leq, greaterThan, geq,
    (==), (/=), (>), (<), (<=), (>=),    
    CanMinMaxA(..), CanMinMaxThisA, CanMinMaxSameTypeA,
    CanMinMax, CanMinMaxThis, CanMinMaxSameType, min, max,
    HasParallelComparisonsA(..), HasParallelComparisons, pickNonZero,
    CanNegA(..), CanNegSameTypeA,
    CanNeg, CanNegSameType, neg, negate, 
    CanAbsA(..), CanAbsSameTypeA,
    CanAbs, CanAbsSameType, abs,
    CanAddA(..), CanAddThisA, CanAddSameTypeA, sumA,
    CanAdd, CanAddThis, CanAddSameType, add, sum, (+),
    CanSubA(..), CanSubThisA, CanSubSameTypeA,
    CanSub, SubType, CanSubThis, CanSubSameType, sub, (-),
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
    HasPiA(..), HasPi,
    CanSqrtA(..), CanSqrtSameTypeA,
    CanSqrt, CanSqrtSameType, sqrt,
    CanExpA(..), CanExpSameTypeA,
    CanExp, CanExpSameType, exp,
    CanSineCosineA(..), CanSineCosineSameTypeA,
    CanSineCosine, CanSineCosineSameType, sin, cos,
    CanPlusMinusA(..), CanPlusMinus, (+-),
    CanLimitA(..), CanLimit, LimitType, lim, iterateLim,
    VarName(..), VarMap,
    iterateA, mapA, mapAwithPos, zipWithA, zipWithAwithPos, 
    foldlA, mergeInputsA,
    convertFirstA, convertSecondA, flipA
)
where

import Prelude hiding
    (id, (.), (&&), (||), not, and, or,
     (==),(/=),(<),(>),(<=),(>=),
     (+),(*),(/),(-),(^),sum,product,abs,min,max,
     recip,div,negate,
     fromInteger,fromRational,
     pi,sqrt,exp,cos,sin)

import qualified Prelude as P
import Data.String (IsString(..),fromString)
import qualified Data.Map as Map
import qualified Data.List as List

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

class ArrowConvert a1 to1 b1 a2 to2 b2 where 
    arrow2arrow :: (a1 `to1` b1) -> (a2 `to2` b2)
    arrow2arrowNamed :: String -> (a1 `to1` b1) -> (a2 `to2` b2)
    arrow2arrowNamed _ = arrow2arrow

instance ArrowConvert a (->) b a (->) b where
    arrow2arrow = id

type Fn2Arrow to a1 b1 a2 b2 = ArrowConvert  a1 (->) b1 a2 to b2
fn2arrow :: (Fn2Arrow to a1 b1 a2 b2) => (a1 -> b1) -> (a2 `to` b2)
fn2arrow = arrow2arrow
fn2arrowNamed :: (Fn2Arrow to a1 b1 a2 b2) => String -> (a1 -> b1) -> (a2 `to` b2)
fn2arrowNamed = arrow2arrowNamed

type Arrow2Fn to a1 b1 a2 b2 = ArrowConvert  a1 to b1 a2 (->) b2
arrow2fn :: (Arrow2Fn to a1 b1 a2 b2) => (a1 `to` b1) -> (a2 -> b2)
arrow2fn = arrow2arrow

type CanEmbedFnA to r1 r2 = ArrowConvert [r1] (->) r1 [r2] to r2

{-| use a normal computation, bypassing the arrow -}
embedFnNamedA ::
    (CanEmbedFnA to r1 r2) => String -> ([r1] -> r1) -> [r2] `to` r2
embedFnNamedA = fn2arrowNamed

class (ArrowChoice to) => ConvertibleA to a b where
    convertA :: a `to` b
    convertNamedA :: String -> a `to` b
    convertNamedA _ = convertA -- the name can be useful in some Arrows, eg to name a network node
    convertListA :: [a] `to` [b]
    convertListA =
        proc list ->
            case list of
                [] -> returnA -< []
                (x:xs) ->
                    do
                    y <- convertA -< x
                    ys <- convertListA -< xs
                    returnA -< (y:ys)

convertListNamedA :: (ConvertibleA to a b) => String -> [a] `to` [b]
convertListNamedA name = aux 0
    where
    aux i =
        proc list ->
            case list of
                [] -> returnA -< []
                (x:xs) ->
                    do
                    y <- convertNamedA name_i -< x
                    ys <- aux (i P.+ 1) -< xs
                    returnA -< (y:ys)
        where
        name_i = name ++ "." ++ show i

type Convertible = ConvertibleA (->)

convert :: (Convertible a b) => a -> b
convert = convertA
convertList :: (Convertible a b) => [a] -> [b]
convertList = map convert

instance (ArrowChoice to) => ConvertibleA to Int Int where convertA = id; convertListA = id
instance (ArrowChoice to) => ConvertibleA to Integer Integer where convertA = id; convertListA = id
instance (ArrowChoice to) => ConvertibleA to Rational Rational where convertA = id; convertListA = id

{-|
    This is useful so that 'convert' can be used as a replacement 
    for 'P.fromInteger' when all integer literals are of type Integer.
    For example, we can say @cauchyReal2ball (convert 1)@.
-}
type HasIntegersA to = ConvertibleA to Integer
type HasIntegers = HasIntegersA (->)
fromIntegerADefault :: (ArrowChoice to, Num a) => Integer `to` a
fromIntegerADefault = arr P.fromInteger

-- | ie HasIntegers Int
instance (ArrowChoice to) => ConvertibleA to Integer Int where 
    convertA = arr toInt
-- | ie HasIntegers Rational, CanBeRational Integer
instance (ArrowChoice to) => ConvertibleA to Integer Rational where 
    convertA = fromIntegerADefault

type CanBeIntegerA to a = ConvertibleA to a Integer
integerA :: (CanBeIntegerA to a) => a `to` Integer
integerA = convertA
integerADefault :: (Integral a, ArrowChoice to) => a `to` Integer
integerADefault = arr P.toInteger
integerNamedA :: (CanBeIntegerA to a) => String -> a `to` Integer
integerNamedA = convertNamedA
integersA :: (CanBeIntegerA to a) => [a] `to` [Integer]
integersA = convertListA
integersNamedA :: (CanBeIntegerA to a) => String -> [a] `to` [Integer]
integersNamedA = convertListNamedA

{-|
    This is useful for converting int obtained eg by 'length' to integer,
    so that it can be easily mixed with Integers.
-}
type CanBeInteger a = CanBeIntegerA (->) a
integer :: (CanBeInteger a) => a -> Integer
integer = convert
integers :: (CanBeInteger a) => [a] -> [Integer]
integers = convertList

-- | ie CanBeInteger Int
instance (ArrowChoice to) => ConvertibleA to Int Integer where 
    convertA = integerADefault

type HasIntsA to = ConvertibleA to Int
type HasInts = HasIntsA (->)
fromIntADefault :: (ArrowChoice to, Num a) => Int `to` a
fromIntADefault = arr P.fromIntegral

-- | ie HasInts Rational, CanBeRational Int
instance (ArrowChoice to) => ConvertibleA to Int Rational where 
    convertA = fromIntADefault

type CanBeIntA to a = ConvertibleA to a Int
intA :: (CanBeIntA to a) => a `to` Int
intA = convertA
intNamedA :: (CanBeIntA to a) => String -> a `to` Int
intNamedA = convertNamedA
intsA :: (CanBeIntA to a) => [a] `to` [Int]
intsA = convertListA
intsNamedA :: (CanBeIntA to a) => String -> [a] `to` [Int]
intsNamedA = convertListNamedA

{-|
    This is useful for calls such as: @drop (int 1) list@
-}
type CanBeInt a = CanBeIntA (->) a
int :: (CanBeInt a) => a -> Int
int = convert
intDefault :: (Integral a) => a -> Int
intDefault = toInt . P.toInteger
ints :: (CanBeInt a) => [a] -> [Int]
ints = convertList

toInt :: Integer -> Int
toInt i 
    | iInIntRange = P.fromInteger i
    | otherwise = error $ "int out of range: " ++ show i 
    where
    iInIntRange =
        i P.>= toInteger (minBound :: Int)
        &&
        i P.<= toInteger (maxBound :: Int)

(!!!) :: [a] -> Integer -> a
(!!!) = List.genericIndex

{-|
    This is useful so that 'convert' can be used as a replacement 
    for 'P.fromRational' when all rational literals are of type Rational.
    For example, we can say @cauchyReal2ball (convert 0.5)@.
-}
type HasRationalsA to = ConvertibleA to Rational
type HasRationals = HasRationalsA (->)
fromRationalADefault :: (ArrowChoice to, Fractional a) => Rational `to` a
fromRationalADefault = arr P.fromRational

type CanBeRationalA to a = ConvertibleA to a Rational
rationalA :: (CanBeRationalA to a) => a `to` Rational
rationalA = convertA
rationalNamedA :: (CanBeRationalA to a) => String -> a `to` Rational
rationalNamedA = convertNamedA
rationalsA :: (CanBeRationalA to a) => [a] `to` [Rational]
rationalsA = convertListA
rationalsNamedA :: (CanBeRationalA to a) => String -> [a] `to` [Rational]
rationalsNamedA = convertListNamedA

{-|
    This is useful for calls such as: @drop (rational 1) list@
-}
type CanBeRational a = CanBeRationalA (->) a
rational :: (CanBeRational a) => a -> Rational
rational = convert
rationalDefault :: (P.Real a) => a -> Rational
rationalDefault = P.toRational
rationals :: (CanBeRational a) => [a] -> [Rational]
rationals = convertList

{- 
    The following mixed-type operators shadow the classic mono-type Prelude versions. 
-}

infixr 8  ^
infixl 7  *, /  -- , ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘  
infixl 6  +, -

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&  
infixr 2  ||  
--infixl 1  >>, >>=  
--infixr 1  =<<  
--infixr 0  $, $!, ‘seq‘ 

{- Booleans, Kleeneans -}

type HasBoolsA to = ConvertibleA to Bool
type HasBools = HasBoolsA (->)

instance (ArrowChoice to) => ConvertibleA to Bool Bool where convertA = arr id; convertListA = arr id 
instance (ArrowChoice to) => ConvertibleA to Bool (Maybe Bool) where
    convertA = arr Just

notA :: (CanNegA to a) => a `to` (NegTypeA to a)
notA = negA
not :: (CanNeg a) => a -> (NegType a)
not = neg

instance (Arrow to) => CanNegA to Bool where
    negA = arr P.not

instance (Arrow to) => CanNegA to (Maybe Bool) where
    negA = arr (fmap P.not)

instance (Arrow to) => CanNegSameTypeA to Bool
instance (Arrow to) => CanNegSameTypeA to (Maybe Bool)

class Arrow to => CanAndOrA to a b where
    type AndOrTypeA to a b
    type AndOrTypeA to a b = a
    and2A :: (a,b) `to` AndOrTypeA to a b
    or2A :: (a,b) `to` AndOrTypeA to a b

andA :: (ArrowChoice to, CanAndOrSameTypeA to a, HasBoolsA to a) => [a] `to` a
andA = foldlA and2A True
orA :: (ArrowChoice to, CanAndOrSameTypeA to a, HasBoolsA to a) => [a] `to` a
orA = foldlA or2A False


type CanAndOr = CanAndOrA (->)
type AndOrType a b = AndOrTypeA (->) a b

(&&) :: (CanAndOr a b) => a -> b -> AndOrType a b
(&&) = curry and2A
(||) :: (CanAndOr a b) => a -> b -> AndOrType a b
(||) = curry or2A

class
    (CanAndOrA to a a, AndOrTypeA to a a ~ a)
    =>
    CanAndOrSameTypeA to a

type CanAndOrSameType = CanAndOrSameTypeA (->)

and :: (CanAndOrSameType a, HasBools a) => [a] -> a
and = andA
or :: (CanAndOrSameType a, HasBools a) => [a] -> a
or = orA

class
    (ArrowChoice to, HasBoolsA to a, CanNegSameTypeA to a, CanAndOrSameTypeA to a)
    => 
    BoolA to a 

instance (ArrowChoice to) => BoolA to Bool
instance (ArrowChoice to) => BoolA to (Maybe Bool)

instance (Arrow to) => CanAndOrA to Bool Bool where
    and2A = arr (uncurry (P.&&))
    or2A = arr (uncurry (P.||))

instance (Arrow to) => CanAndOrSameTypeA to Bool

instance (Arrow to) => CanAndOrA to (Maybe Bool) (Maybe Bool) where
    and2A = arr (\(aM,bM) -> (do a <- aM; b <- bM; Just (a P.&& b)))
    or2A = arr (\(aM,bM) -> (do a <- aM; b <- bM; Just (a P.|| b)))

instance (Arrow to) => CanAndOrSameTypeA to (Maybe Bool)

instance (Arrow to) => CanAndOrA to Bool (Maybe Bool) where
    type AndOrTypeA to Bool (Maybe Bool) = Maybe Bool
    and2A = arr (\(a,bM) -> (do b <- bM; Just (a P.&& b)))
    or2A = arr (\(a,bM) -> (do b <- bM; Just (a P.|| b)))

instance (Arrow to) => CanAndOrA to (Maybe Bool) Bool where
    type AndOrTypeA to (Maybe Bool) Bool = Maybe Bool
    and2A = arr (\(aM,b) -> (do a <- aM; Just (a P.&& b)))
    or2A = arr (\(aM,b) -> (do a <- aM; Just (a P.|| b)))

{- equality -}

class (Arrow to, BoolA to (EqCompareTypeA to a b)) => HasEqA to a b where
    type EqCompareTypeA to a b
    type EqCompareTypeA to a b = Bool -- default
    equalToA :: (a,b) `to` (EqCompareTypeA to a b)
    -- default equalToA via Prelude for (->) and Bool:
    default equalToA :: (EqCompareTypeA to a b ~ Bool, a~b, P.Eq a) => (a,b) `to` Bool
    equalToA = arr $ uncurry (P.==)
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

instance (ArrowChoice to) => HasEqA to Bool Bool
instance (ArrowChoice to) => HasEqA to Char Char
instance (HasEqA to a a, BoolA to a) => HasEqA to (Maybe a) (Maybe a) where
    type EqCompareTypeA to (Maybe a) (Maybe a) = EqCompareTypeA to a a
    equalToA =
        proc (ma, mb) ->
            case (ma, mb) of 
                (Nothing, Nothing) -> convertA -< True
                (Just a, Just b) -> equalToA -< (a, b)
                _ -> convertA -< False 
instance (HasEqA to a a, BoolA to (EqCompareTypeA to a a)) => HasEqA to [a] [a] where
    type EqCompareTypeA to [a] [a] = EqCompareTypeA to a a
    equalToA =
        proc (l1, l2) ->
            case (l1,l2) of
                ([],[]) -> convertA -< True
                (h1:t1, h2:t2) ->
                    do
                    hEq <- equalToA -< (h1, h2)
                    tEq <- equalToA -< (t1, t2)
                    and2A -< (hEq, tEq)
                _ -> convertA -< False 


{- order -}

class (Arrow to, BoolA to (OrderCompareTypeA to a b)) => HasOrderA to a b where
    type OrderCompareTypeA to a b
    type OrderCompareTypeA to a b = Bool -- default
    lessThanA :: (a,b) `to` OrderCompareTypeA to a b
    default lessThanA :: 
        (OrderCompareTypeA to a b ~ Bool, a~b, P.Ord a) => 
        (a,b) `to` OrderCompareTypeA to a b
    lessThanA = arr $ uncurry (P.<)
    greaterThanA :: (a,b) `to` OrderCompareTypeA to a b
    default greaterThanA :: 
        (OrderCompareTypeA to a b ~ OrderCompareTypeA to b a, HasOrderA to b a) => 
        (a,b) `to` OrderCompareTypeA to a b
    greaterThanA = proc (a,b) -> lessThanA -< (b,a)
    leqA :: (a,b) `to` OrderCompareTypeA to a b
    default leqA :: 
        (OrderCompareTypeA to a b ~ Bool, a~b, P.Ord a) => 
        (a,b) `to` OrderCompareTypeA to a b
    leqA = arr $ uncurry (P.<=)
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
    default minA :: (MinMaxTypeA to a b ~ a, a~b, P.Ord a) => (a,a) `to` a
    minA = arr $ uncurry P.min
    default maxA :: (MinMaxTypeA to a b ~ a, a~b, P.Ord a) => (a,a) `to` a
    maxA = arr $ uncurry P.max

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


class (Arrow to) => (HasParallelComparisonsA to a) where
    pickNonZeroA :: [(a,b)] `to` (Maybe (a,b))

type HasParallelComparisons = HasParallelComparisonsA (->)
pickNonZero :: (HasParallelComparisons a) => [(a,b)] -> (Maybe (a,b))
pickNonZero = pickNonZeroA

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

class (Arrow to) => CanRecipA to a where
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
--type AddType a b = AddTypeA (->) a b

add :: (CanAdd a b) => a -> b -> AddTypeA (->) a b
add = curry addA

(+) :: CanAdd a b => a -> b -> AddTypeA (->) a b
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
            [] -> convertA -< 0
            (x:xs) -> 
                do
                a <- sumA -< xs
                r <- addA -< (x, a)
                returnA -< r

sum :: (CanAddSameType a, HasIntegers a) => [a] -> a
sum = sumA

{- sub -}

class (Arrow to) => CanSubA to a b where
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

sub :: (CanSub a b) => a -> b -> SubTypeA (->) a b
sub = curry subA

(-) :: CanSub a b => a -> b -> SubTypeA (->) a b
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
--type MulType a b = MulTypeA (->) a b

mul :: (CanMul a b) => a -> b -> MulTypeA (->) a b
mul = curry mulA

(*) :: CanMul a b => a -> b -> MulTypeA (->) a b
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
            [] -> convertA -< 1
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
--type DivType a b = DivTypeA (->) a b

div :: (CanDiv a b) => a -> b -> DivTypeA (->) a b
div = curry divA

(/) :: CanDiv a b => a -> b -> DivTypeA (->) a b
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
    default powA :: 
        (b~Integer, HasIntegersA to a, CanMulSameTypeA to a, PowTypeA to a b~a) 
        =>
        (a,b) `to` PowTypeA to a b
    powA =
        proc (a,n) ->
            productA -< (replicate (int n) a) 
            -- TODO: replace with a more efficient method based on halving n

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
     CanPowByA to a Integer,
     HasEqA to a a, HasOrderA to a a, HasIntegersA to a)
    => 
    RingA to a

type Ring = RingA (->)

class
    (RingA to a, CanDivSameTypeA to a, CanRecipSameTypeA to a, HasRationalsA to a)
    =>
    FieldA to a
    
type Field = FieldA (->)

class
    (CanAddThisA to a s, CanSubThisA to a s, CanMulByA to a s)
    =>
    CanAddMulScalarA to a s 
    
type CanAddMulScalar = CanAddMulScalarA (->)
    
class
    (CanAddMulScalarA to a s, CanDivByA to a s)
    =>
    CanAddMulDivScalarA to a s 
    
type CanAddMulDivScalar = CanAddMulDivScalarA (->)

class HasPiA to a where
    piA :: () `to` a

type HasPi = HasPiA (->)

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


{- Interval operations -}    
    
class CanPlusMinusA to a b where
    type PlusMinusTypeA to a b
    --type PlusMinusTypeA to a b = Interval a a -- default? But requires interval type
    plusMinusA :: (a,b) `to` PlusMinusTypeA to a b                

type CanPlusMinus = CanPlusMinusA (->)
type PlusMinusType a b = PlusMinusTypeA (->) a b

(+-) :: (CanPlusMinus a b) => a -> b -> PlusMinusType a b
(+-) = curry plusMinusA

{- limit -}
class (ArrowChoice to) => CanLimitA to a where
        type LimitTypeA to a
        limA :: (Integer -> (b `to` a)) -> b `to` LimitTypeA to a
        iterateLimA :: (a `to` a) -> a `to` LimitTypeA to a
        iterateLimA fnA =
            proc a -> 
                (iterateLimWithA $ proc (a,()) -> do r <- fnA -< a; returnA -< (r,())) -< (a,())
        iterateLimWithA :: ((a,b) `to` (a,b)) -> (a,b) `to` LimitTypeA to a
        --type ApproxTypeA to a
        --type ApproxTypeA to a = a
        --approx :: (LimitTypeA to a, Accuracy) `to` ApproxType to a
        

type CanLimit = CanLimitA (->)

type LimitType a = LimitTypeA (->) a

lim :: (CanLimit a) => (Integer -> a) -> LimitTypeA (->) a
lim sq = limA (\n () -> sq n) ()
    

iterateLim :: 
    (CanLimitA (->) a) => 
    a -> (a -> a) -> LimitType a
iterateLim = flip iterateLimA 

{-| Variable names for symbolic expressions, many-variate functions, etc. -}
newtype VarName = VarName String
    deriving (IsString, Eq, Ord, Show)

type VarMap = Map.Map VarName


{- Utilities for arrow programming -}

iterateA :: (Arrow to) => (a `to` a) -> a `to` [a]
iterateA fnA =
    proc a0 ->
        do
        a1 <- fnA -< a0
        rest <- iterateA fnA -< a1
        returnA -< a0 : rest

mapA :: (ArrowChoice to) => (a `to` b) -> ([a] `to` [b]) 
mapA processOne = mapAwithPos (const processOne)

mapAwithPos :: (ArrowChoice to) => (Integer -> (a `to` b)) -> ([a] `to` [b])
mapAwithPos processOne = aux 0
    where
    aux k =
        proc xs ->
            case xs of
                [] -> returnA -< []
                (x : xrest) -> 
                    do
                    y <- processOne k -< x
                    yrest <- aux (k P.+ 1) -< xrest
                    returnA -< y : yrest

zipWithA :: (ArrowChoice to) => ((a,b) `to` c) -> (([a],[b]) `to` [c])
zipWithA processOne = zipWithAwithPos (const processOne)

zipWithAwithPos :: (ArrowChoice to) => (Integer -> ((a,b) `to` c)) -> (([a],[b]) `to` [c])
zipWithAwithPos processOne = aux 0
    where
    aux k =
        proc (xs, ys) ->
            case (xs, ys) of
                ([], _) -> returnA -< []
                (_, []) -> returnA -< []
                (x : xrest, y : yrest) -> 
                    do
                    z <- processOne k -< (x,y)
                    zrest <- aux (k P.+ 1) -< (xrest, yrest)
                    returnA -< z : zrest

foldlA :: 
    (ArrowChoice to, CanAndOrSameTypeA to a, ConvertibleA to b a) 
    => 
    ((a,a) `to` a) -> b -> ([a] `to` a)
foldlA opA b =
    proc list ->
        case list of
            [] -> convertA -< b
            (x:xs) -> 
                do
                a <- foldlA opA b -< xs
                r <- opA -< (x, a)
                returnA -< r

mergeInputsA :: (ArrowChoice to) => [(a `to` b)] -> (a `to` [b])
mergeInputsA [] = proc _ -> returnA -< []
mergeInputsA (f:fs) =
    proc input ->
        do
        r <- f -< input
        rs <- mergeInputsA fs -< input
        returnA -< (r:rs)

convertSecondA ::
    (Arrow to, ConvertibleA to a r) =>
    ((r,r) `to` b) -> ((r,a) `to` b) 
convertSecondA opA =
    proc (x,yI) ->
        do
        y <- convertA -< yI
        opA -< (x,y)

convertFirstA ::
    (Arrow to, ConvertibleA to a r) =>
    ((r,r) `to` b) -> ((a,r) `to` b) 
convertFirstA opA =
    proc (xI,y) ->
        do
        x <- convertA -< xI
        opA -< (x,y)

flipA ::
    (Arrow to) =>
    ((a,b) `to` c) -> ((b,a) `to` c) 
flipA opA =
    proc (x,y) -> opA -< (y,x) 

