{-# LANGUAGE DataKinds #-}
module AERN2.Linear.MatrixTest
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

import qualified Data.Matrix as Matrix

n1 :: Integer
n1 = 100

rowsD :: [[Double]]
rowsD = map (map double) rowsI
  where
  rowsI = [[ item i j  | j <- [1..n1] ] | i <- [1..n1]]
  item i j 
    | i == j = n1 + i - 1
    | otherwise = 1

{- Using Linear -}

type VN1 = LV.V 100

vn1FromList :: [a] -> VN1 a
vn1FromList list = 
  case LV.fromVector $ Vector.fromList list of
    Just v -> v
    _ -> error $ "vn1FromList: list of incorrect length"

v1 :: VN1 Double
v1 = vn1FromList (map double [1..n1])

m1 :: VN1 (VN1 Double)
m1 = vn1FromList (map vn1FromList rowsD)

m1Det :: Double
m1Det = L.luDetFinite m1

{- Using Data.Matrix -}

m1DM :: Matrix.Matrix Double
m1DM = Matrix.fromLists rowsD

m1DetDM :: Double
m1DetDM = Matrix.detLU m1DM -- much slower than Linear
