{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module AERN2.Real.Operations 
(
    fromInteger, fromRational, ifThenElse, int,
    negate, CanNeg(..), CanNegSameType,
    (+), (-), (*), (/), (^),
    CanAbs(..), CanAbsSameType,
    CanAdd(..), CanAddThis, CanAddSameType,
    CanSub(..), CanSubThis, CanSubSameType,
    CanMul(..), CanMulBy, CanMulSameType,
    CanPow(..), CanPowBy,
    CanDiv(..), CanDivBy, CanDivSameType,
    CanRecip(..), CanRecipSameType,
    CanSqrt(..), CanSqrtSameType,
    CanSineCosine(..), CanSineCosineSameType
)
where

import Prelude hiding ((+),(*),(/),(-),(^),abs,recip,div,negate,fromInteger,fromRational,sqrt,cos,sin)

import qualified Prelude as P (fromInteger)

{- 
    The following arranges that all numeric literals are monomorphic and of the type Integer or Rational.
-}

_example1 :: Integer -- inferred
_example1 = 1 -- not polymorphic 
_example2 :: Rational -- inferred
_example2 = 1.0 -- not polymorphic
_example3 :: Int
_example3 = int 1 -- the easiest way to get Int literals (1 :: Int does not compile)

fromInteger :: Integer -> Integer
fromInteger = id

fromRational :: Rational -> Rational
fromRational = id

-- the following is needed to restore if-then-else while using RebindableSyntax 
ifThenElse :: Bool -> t -> t -> t
ifThenElse b e1 e2
    | b = e1
    | otherwise = e2

int :: Integer -> Int
int i 
    | iInIntRange = P.fromInteger i
    | otherwise = error $ "int out of range: " ++ show i 
    where
    iInIntRange =
        i >= toInteger (minBound :: Int)
        &&
        i <= toInteger (maxBound :: Int)


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
    recip :: a -> RecipType a

class
    (CanRecip a, RecipType a ~ a) => 
    CanRecipSameType a

class CanAdd a b where
    type AddType a b :: *
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
    pow :: a -> b -> PowType a b

class
    (CanPow a b, PowType a b ~ a) => 
    CanPowBy a b

class CanSqrt a where
    type SqrtType a :: *
    sqrt :: a -> SqrtType a

class
    (CanSqrt a, SqrtType a ~ a) => 
    CanSqrtSameType a

class CanSineCosine a where
    type SineCosineType a :: *
    sin :: a -> SineCosineType a
    cos :: a -> SineCosineType a

class
    (CanSineCosine a, SineCosineType a ~ a) => 
    CanSineCosineSameType a
