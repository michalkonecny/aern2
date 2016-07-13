{-|
    Module      :  AERN2.Num.Norm
    Description :  Rough logarithmic norm
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module AERN2.Num.Norm
(
    HasNorm(..), NormLog(..)
)
where

import Numeric.MixedTypes
--import qualified Prelude as P

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

instance HasEqAsymmetric NormLog NormLog
instance HasOrderAsymmetric NormLog NormLog
instance CanMinMaxAsymmetric NormLog NormLog

instance HasNorm Integer where
    getNormLog n
        | n == 0 = NormZero
        | otherwise = NormBits $ integer $ integerLog2 $ abs n

instance HasNorm Int where
    getNormLog = getNormLog . integer

instance HasNorm Rational where
    getNormLog x
        | x == 0.0 = NormZero
        | abs x >= 1.0 = NormBits $ integer $ integerLog2 $ ceiling $ abs x
        | otherwise = NormBits $ negate $ integer $ integerLog2 $ ceiling (1 / (abs x))
