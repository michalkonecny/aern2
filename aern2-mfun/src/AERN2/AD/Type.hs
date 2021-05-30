module AERN2.AD.Type where

import MixedTypesNumPrelude
import AERN2.MP.Precision

data Differential a =
    OrderZero  {diff_x :: a}
    | OrderOne {diff_x :: a, diff_dx :: a}
    | OrderTwo {diff_x :: a, diff_dx :: a, diff_dxt :: a, diff_d2x :: a}
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
    getPrecision a = getPrecision (diff_x a) -- TODO: safe?

instance 
    (CanSetPrecision a) => (CanSetPrecision (Differential a))
    where
    setPrecision p = fmap (setPrecision p)

setValue :: Differential a -> a -> Differential a
setValue (OrderZero _x)           v = OrderZero v
setValue (OrderOne _x dx)         v = OrderOne  v dx
setValue (OrderTwo _x dx dxt d2x) v = OrderTwo  v dx dxt d2x
