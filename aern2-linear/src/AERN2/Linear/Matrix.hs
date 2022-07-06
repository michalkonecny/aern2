{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module AERN2.Linear.Matrix
-- ()
where

import MixedTypesNumPrelude
-- import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

-- import qualified Prelude as P

-- import qualified Debug.Trace as Debug
-- import Text.Printf (printf)

import qualified Linear.V as LV
import Linear.V (V)
import qualified Linear as L
import qualified Data.Vector as Vector
import GHC.TypeLits (KnownNat, Nat)
import Data.Typeable (Typeable)
import AERN2.Real (CReal, creal, prec, (?), bits)
import Data.Foldable (Foldable(toList))
import qualified Data.Map as Map
import AERN2.MP (MPBall (ball_value), mpBallP)
import AERN2.MP.Float (MPFloat) 
import Linear (luSolve)

type CanBeVN n e t = ConvertibleExactly t (V (n :: Nat) e)

vectorN :: CanBeVN n e t => t -> V n e
vectorN = convertExactly

instance (KnownNat n, Show e1, Typeable e1, Typeable e2, ConvertibleExactly e1 e2) => ConvertibleExactly [e1] (V n e2) 
  where
  safeConvertExactly e1s =
    do
    e2s <- mapM safeConvertExactly e1s
    case LV.fromVector $ Vector.fromList e2s of
      Just v -> return v
      _ -> convError "convertExactly to V: list of incorrect length" e1s

type MatrixRC rn cn e = (V (rn :: Nat) (V (cn :: Nat) e))

matrixRC :: (Show e1, Typeable e1, Typeable e2,
                   ConvertibleExactly e1 e2, KnownNat cn, KnownNat rn) => 
            [[e1]] -> MatrixRC rn cn e2
matrixRC rows =
  case aux of
    Right mx -> mx
    _ -> error "convertExactly to MatrixRC: incorrect number of rows"
  where 
  aux =
    do
    rowsV <- mapM safeConvertExactly rows
    case LV.fromVector $ Vector.fromList rowsV of
      Just v -> return v
      _ -> error "convertExactly to MatrixRC: incorrect number of rows"

detLaplace :: 
  (KnownNat n, HasIntegers e, CanMulBy e Integer, CanAddSameType e, CanMulSameType e, Show e) =>
  (e -> Bool) -> 
  MatrixRC n n e -> e
detLaplace isZero mx = 
  fst $ doRows submatrixResults0 mask0 (toList mx)
  where
  mask0 = take (LV.dim mx) alternatingSigns
  alternatingSigns :: [Integer]
  alternatingSigns = 1 : aux
    where
    aux = (-1) : alternatingSigns
  submatrixResults0 = Map.empty
  -- aux submatrixResults mask n s [] = (fromInteger_ s, submatrixResults)
  doRows submatrixResults _mask [] = (fromInteger_ 1, submatrixResults)
  doRows submatrixResults mask (row:restRows) =
    foldl doItem (fromInteger_ 0, submatrixResults) $ zip3 (toList row) mask (submasks mask)
    where
    doItem (value_prev, submatrixResults_prev) (item, itemSign, submask)
      | itemSign == 0 || isZero item = 
          (value_prev, submatrixResults_prev)
      | otherwise = 
          (value_prev + item * itemSign * determinantValue, submatrixResults_next)
      where
      (determinantValue, submatrixResults_next) =
        -- Debug.trace (printf "mask: %s\n" (show mask)) $
        case Map.lookup submask submatrixResults_prev of
          Just v  -> 
            -- Debug.trace (printf "LOOKED UP: submask = %s,, value = %s\n" (show submask) (show v)) $
            (v, submatrixResults_prev) -- use the memoized determinant
          _ -> 
            -- Debug.trace (printf "ADDING: submask = %s, value = %s\n" (show submask) (show v_item)) $
            (v_item, Map.insert submask v_item submatrixResults_item)
            where
            (v_item, submatrixResults_item) = doRows submatrixResults_prev submask restRows
        
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
    each sub-matrix identified by:
      a vector of signs (-1,0,1) showing inactive columns and, eventualy, the sign of the permutation
    memoizing results for all sub-matrices to be reused when the same sub-matrix is needed again
  -}

{- mini test -}

n1 :: Integer
n1 = 100

-- type VN e = forall n. KnownNat n => V n e

-- onesV :: (HasIntegers e, Typeable e) => Integer -> VN e
-- onesV n = vectorN $ replicate n 1

rows1I :: [[Rational]]
rows1I = [[ item i j  | j <- [1..n1] ] | i <- [1..n1]]
  where
  item i j
    | i == j = rational 1
    | j > i + 1 = rational 0
    | otherwise = 1/(i+j)

type VN1 = V 100

--------------------

rows1D :: [[Double]]
rows1D = map (map double) rows1I

m1D :: VN1 (VN1 Double)
m1D = matrixRC rows1D

m1D_detLU :: Double
m1D_detLU = L.luDetFinite m1D

m1D_detLaplace :: Double
m1D_detLaplace = detLaplace (== 0) m1D

--------------------

rows1MP :: [[MPFloat]]
rows1MP = map (map (ball_value . mpBallP (prec 1000))) rows1I

m1MP :: VN1 (VN1 MPFloat)
m1MP = matrixRC rows1MP

m1MP_detLU :: MPFloat
m1MP_detLU = L.luDetFinite m1MP

-- m1MP_detLaplace :: MPFloat
-- m1MP_detLaplace = detLaplace (== 0) m1MP

--------------------

rows1R :: [[CReal]]
rows1R = map (map creal) rows1I

m1R :: VN1 (VN1 CReal)
m1R = matrixRC rows1R

m1R_detLaplace :: CReal
m1R_detLaplace = detLaplace (\(e :: CReal) -> (e ? (prec 10))!==! 0) m1R

m1R_detLaplaceBits :: CN MPBall
m1R_detLaplaceBits = m1R_detLaplace ? (bits 1000)

--------------------

b1D :: VN1 Double
b1D = vectorN $ replicate n1 (double 1)

m1b1_solLU :: VN1 Double
m1b1_solLU = luSolve m1D b1D