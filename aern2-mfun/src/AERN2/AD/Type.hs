module AERN2.AD.Type where

import MixedTypesNumPrelude
import AERN2.MP.Precision

data Differential a =
    OrderZero  {x :: a}
    | OrderOne {x :: a, dx :: a}
    | OrderTwo {x :: a, dx :: a, dxt :: a, d2x :: a}
    deriving (Show)

order :: Differential a -> Integer
order (OrderZero _)     = 0
order (OrderOne  _ _)   = 1
order (OrderTwo  _ _ _ _) = 2

class CanBeDifferential a where
    differential :: Integer -> a -> Differential a

instance 
    (HasIntegers a) =>
    CanBeDifferential a
    where 
    differential 0 a = OrderZero a
    differential 1 a = OrderOne  a (convertExactly 0)
    differential _ a = OrderTwo  a (convertExactly 0) (convertExactly 0) (convertExactly 0)

instance Functor Differential where
    fmap f (OrderZero x)        = OrderZero (f x)
    fmap f (OrderOne x dx)      = OrderOne  (f x) (f dx)
    fmap f (OrderTwo x dx dxt d2x)  = OrderTwo  (f x) (f dx) (f dxt) (f d2x)

instance 
    (HasPrecision a) => (HasPrecision (Differential a))
    where
    getPrecision a = getPrecision (x a) -- TODO: safe?

instance 
    (CanSetPrecision a) => (CanSetPrecision (Differential a))
    where
    setPrecision p = fmap (setPrecision p)

setValue (OrderZero x)           v = OrderZero v
setValue (OrderOne x dx)         v = OrderOne  v dx
setValue (OrderTwo x dx dxt d2x) v = OrderTwo  v dx dxt d2x
