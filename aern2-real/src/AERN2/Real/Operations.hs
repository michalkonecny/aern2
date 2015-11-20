{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module AERN2.Real.Operations where

import Prelude (Integer,Rational,Bool,id,otherwise)

--import qualified Prelude as P

{- 
    The following arranges that all numeric literals are monomorphic and of the type Integer or Rational.
-}

fromInteger :: Integer -> Integer
fromInteger = id

fromRational :: Rational -> Rational
fromRational = id

-- the following is needed to restore if-then-else while using RebindableSyntax 
ifThenElse :: Bool -> t -> t -> t
ifThenElse b e1 e2
    | b = e1
    | otherwise = e2

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

class CanHalve a where
    type HalfType a :: *
    type HalfType a = a -- default
    half :: a -> HalfType a

class CanRecip a where
    type RecipType a :: *
    recip :: a -> RecipType a

class CanAdd a b where
    type AddType a b :: *
    add :: a -> b -> AddType a b

class CanSub a b where
    type SubType a b :: *
    type SubType a b = AddType a (NegType b)
    sub :: a -> b -> SubType a b
    default sub :: (CanNeg b, CanAdd a c, c~NegType b) => a -> b -> AddType a (NegType b)
    sub x y = add x (neg y)

class CanMul a b where
    type MulType a b :: *
    mul :: a -> b -> MulType a b

class CanDiv a b where
    type DivType a b :: *
    type DivType a b = MulType a (RecipType b)
    div :: a -> b -> DivType a b
    default div :: (CanRecip b, CanMul a c, c~RecipType b) => a -> b -> MulType a (RecipType b)
    div x y = mul x (recip y)

class CanPow a b where
    type PowType a b :: *
    pow :: a -> b -> PowType a b

class CanSqrt a where
    type SqrtType a :: *
    sqrt :: a -> SqrtType a


