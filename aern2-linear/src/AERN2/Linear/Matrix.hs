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
import Data.Foldable (Foldable(toList))
-- import qualified Data.Map as Map

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

detLaplace :: 
  (HasIntegers e, CanMulBy e Integer, CanAddSameType e, CanMulSameType e) =>
  MatrixRC rn cn e -> e
detLaplace mx = 
  -- fst $ aux submatrixResults0 mask0 0 1 rowsV
  doRows alternatingSigns (toList mx)
  where
  alternatingSigns :: [Integer]
  alternatingSigns = 1 : aux
    where
    aux = (-1) : alternatingSigns
  -- submatrixResults0 = Map.empty
  -- aux submatrixResults mask n s [] = (fromInteger_ s, submatrixResults)
  doRows _mask [] = fromInteger_ 1
  doRows mask (row:rest) =
    sum $ zipWith3 doItem (toList row) mask (submasks mask)
    where
    doItem item itemSign submask
      | itemSign == 0 = fromInteger_ 0
      | otherwise = 
        item * itemSign * (doRows submask rest)
  submasks mask = aux mask
    where
    aux [] = []
    aux (b:bs) 
      | b == 0 = 
        (0:bs) : (map (b:) (aux bs))
      | otherwise = 
        (0:(map negate bs)) : (map (b:) (aux bs))

  {-
    recurse from top row downwards, 
    going over all columns whose elements are not certainly zero, 
    each sub-matrix identified by: number of dropped top rows + a Boolean vector showing active columns
    memoizing results for all sub-matrices to be reused when the same sub-matrix is needed again
  -}

{- mini test -}

n1 :: Integer
n1 = 9

rows1I :: [[Integer]]
rows1I = [[ item i j  | j <- [1..n1] ] | i <- [1..n1]]
  where
  item i j
    | i == j = n1 + i - 1
    | j > i + 1 = 0
    | otherwise = 1

rows1D :: [[Double]]
rows1D = map (map double) rows1I

rows1R :: [[CReal]]
rows1R = map (map creal) rows1I

type VN1 = LV.V 9

m1D :: VN1 (VN1 Double)
m1D = matrixRC rows1D

m1DetLU :: Double
m1DetLU = L.luDetFinite m1D

m1DetLaplace :: Double
m1DetLaplace = detLaplace m1D

m1R :: VN1 (VN1 CReal)
m1R = matrixRC rows1R

