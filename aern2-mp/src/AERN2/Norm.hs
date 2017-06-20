{-|
    Module      :  AERN2.Norm
    Description :  Rough logarithmic norm
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module AERN2.Norm
(
    HasNorm(..), NormLog(..), invertNormLog
)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Data.Complex

import Math.NumberTheory.Logarithms (integerLog2)


class HasNorm a where
    {-|
        For a value @x@, return @NormBits j@ where @j@ is close
        to the smallest @i@ with @|x| <= 2^i@.
        If @x == 0@ then return @NormZero@.
    -}
    getNormLog :: a -> NormLog

data NormLog
    = NormZero -- ^ ie NormBits (-infinity)
    | NormBits Integer
    deriving (P.Eq, P.Ord, Show)

instance HasEqAsymmetric NormLog NormLog
instance HasOrderAsymmetric NormLog NormLog
instance CanMinMaxAsymmetric NormLog NormLog

invertNormLog :: NormLog -> NormLog
invertNormLog NormZero = error "cannot invert NormZero"
invertNormLog (NormBits b) = NormBits (-b)

instance HasNorm Integer where
    getNormLog n
        | n == 0 = NormZero
        | abs n == 1 = NormBits 0
        | otherwise = NormBits $ 1 + (integer $ integerLog2 $ abs n - 1)

instance HasNorm Int where
    getNormLog = getNormLog . integer

instance HasNorm Rational where
    getNormLog x
        | x == 0.0 = NormZero
        | abs x >= 1.0 = getNormLog $ ceiling $ abs x
        | otherwise = NormBits $ negate $ integer $ integerLog2 $ floor $ (1 /! (abs x))

instance
  (HasNorm t)
  =>
  HasNorm (Complex t)
  where
  getNormLog (a :+ i) =
    (getNormLog a) `max` (getNormLog i)
