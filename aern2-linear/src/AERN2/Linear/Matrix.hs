{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module AERN2.Linear.Matrix
-- ()
where

import MixedTypesNumPrelude
-- import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

-- import qualified Prelude as P
-- import Text.Printf

import qualified Linear.V as LV
import qualified Linear as L
import qualified Data.Vector as Vector
import GHC.TypeLits (KnownNat, Nat)
import Data.Typeable (Typeable)
import AERN2.Real (CReal, creal)

type MatrixRC rn cn e = LV.V (rn :: Nat) (LV.V (cn :: Nat) e)

type CanBeMatrixRC rn cn e t = ConvertibleExactly t (MatrixRC rn cn e)

matrixRC :: CanBeMatrixRC rn cn e t => t -> MatrixRC rn cn e
matrixRC = convertExactly

instance (Show e, Typeable e, KnownNat rn, KnownNat cn) => ConvertibleExactly [[e]] (MatrixRC rn cn e) where
  safeConvertExactly rows =
    do
    rowsV <- mapM convertRow rows
    case LV.fromVector $ Vector.fromList rowsV of
      Just v -> return v
      _ -> convError "convertExactly to MatrixRC: incorrect number of rows" rows
    where
    convertRow :: (Show e, KnownNat cn) => [e] -> Either ConvertError (LV.V (cn :: Nat) e)
    convertRow row =
      case LV.fromVector $ Vector.fromList row of
        Just v -> return v
        _ -> convError "convertExactly to MatrixRC: row of incorrect length" row

{- mini test -}

n1 :: Integer
n1 = 100

rows1I :: [[Integer]]
rows1I = [[ item i j  | j <- [1..n1] ] | i <- [1..n1]]
  where
  item i j
    | i == j = n1 + i - 1
    | otherwise = 1

rows1D :: [[Double]]
rows1D = map (map double) rows1I

rows1R :: [[CReal]]
rows1R = map (map creal) rows1I

type VN1 = LV.V 100

m1D :: VN1 (VN1 Double)
m1D = matrixRC rows1D

m1Det :: Double
m1Det = L.luDetFinite m1D

m1R :: VN1 (VN1 CReal)
m1R = matrixRC rows1R

