{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AERN2.Real.OperationsToBall where

import Prelude hiding ((+),(*),(/),(-),abs,fromInteger,fromRational)
--import qualified Prelude as P

import AERN2.Real.Operations

{- 
    The following mixed-type operators return a ball: an approximate result + an error bound. 
-}

infixl 8 ^: 
infixl 7 *:, /:
infixl 6 +:, -:

(+:) :: CanAddB a b => a -> b -> Ball (AddType a b)
(+:) x y = addB x y
(-:) :: CanSubB a b => a -> b -> Ball (SubType a b)
(-:) x y = subB x y
(*:) :: CanMulB a b => a -> b -> Ball (MulType a b)
(*:) x y = mulB x y
(/:) :: CanDivB a b => a -> b -> Ball (DivType a b)
(/:) x y = divB x y
(^:) :: CanPowB a b => a -> b -> Ball (PowType a b)
(^:) x y = powB x y

data Ball a = Ball { ball_value :: a, ball_error :: ErrorBoundType a }

instance (Show a, Show (ErrorBoundType a)) => Show (Ball a)
    where
    show (Ball x e) = "[" ++ show x ++ "±" ++ show e ++ "]"

type family ErrorBoundType a

class (CanNeg a) => CanNegB a where
    negB :: a -> Ball (NegType a)

class (CanRecip a) => CanRecipB a where
    recipB :: a -> Ball (RecipType a)

class (CanAdd a b) => CanAddB a b where
    addB :: a -> b -> Ball (AddType a b)

class (CanSub a b) => CanSubB a b where
    subB :: a -> b -> Ball (SubType a b)

class (CanMul a b) => CanMulB a b where
    mulB :: a -> b -> Ball (MulType a b)

class (CanDiv a b) => CanDivB a b where
    divB :: a -> b -> Ball (DivType a b)

class (CanPow a b) => CanPowB a b where
    powB :: a -> b -> Ball (PowType a b)

class (CanSqrt a) => CanSqrtB a where
    sqrtB :: a -> Ball (SqrtType a)

