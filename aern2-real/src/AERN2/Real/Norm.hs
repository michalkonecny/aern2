{-# LANGUAGE FlexibleInstances #-}
module AERN2.Real.Norm 
(
    HasNorm(..), NormLog(..)
)
where

import AERN2.Real.Operations
import qualified Prelude as P
import AERN2.Real.IntegerRational ()

import Math.NumberTheory.Logarithms (integerLog2)


class HasNorm a where
    {-|
        For a value @x@, return @NormBits j@ where $j$ is close
        to the smallest @i@ with @|x| <= 2^i@.
        If @x == 0@ then return @NormZero@.
    -}
    getNormLog :: a -> NormLog

data NormLog 
    = NormZero -- ^ ie NormBits (-infinity) 
    | NormBits Integer
    deriving (Eq, Ord, Show)

instance HasEq NormLog NormLog where
    equalTo a b = a P.== b

instance HasOrder NormLog NormLog where
    lessThan a b = a P.< b
    leq a b = a P.<= b

instance CanMinMax NormLog NormLog where
    min NormZero _ = NormZero
    min _ NormZero = NormZero
    min (NormBits a) (NormBits b) = NormBits (min a b) 
    max NormZero b = b
    max a NormZero = a
    max (NormBits a) (NormBits b) = NormBits (max a b) 

instance HasNorm Integer where
    getNormLog n 
        | n == 0 = NormZero
        | otherwise = NormBits $ toInteger $ integerLog2 $ abs n

instance HasNorm Rational where
    getNormLog x 
        | x == 0.0 = NormZero
        | abs x >= 1.0 = NormBits $ toInteger $ integerLog2 $ ceiling $ abs x
        | otherwise = NormBits $ neg $ toInteger $ integerLog2 $ ceiling (1 / (abs x))

