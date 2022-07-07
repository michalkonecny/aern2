{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module AERN2.Linear.Matrix
-- ()
where

import MixedTypesNumPrelude
-- import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

import qualified Prelude as P

-- import qualified Debug.Trace as Debug
-- import Text.Printf (printf)

import qualified Linear.V as LV
import Linear.V (V)
import qualified Linear as L
import qualified Data.Vector as Vector
import GHC.TypeLits (KnownNat, SomeNat (SomeNat), someNatVal, natVal)
import Data.Typeable (Typeable, Proxy (Proxy))
import AERN2.Real (CReal, creal, prec, (?), bits)
import Data.Foldable (Foldable(toList))
import qualified Data.Map as Map
import AERN2.MP (MPBall (ball_value), mpBallP)
import AERN2.MP.Float (MPFloat) 
import GHC.Real (Fractional)
import Unsafe.Coerce (unsafeCoerce)
import Control.Applicative (Applicative(liftA2))

----------------------
-- hiding type Nat type parameters 
----------------------

data VN e = forall n. (KnownNat n) => VN (V n e)
  
deriving instance (Show e) => (Show (VN e))

vNFromList :: (Typeable e, Show e) => [e] -> VN e
vNFromList (es :: [e]) = 
  case someNatVal (length es) of
    Nothing -> error "internal error in vNFromList"
    Just (SomeNat (_ :: Proxy n)) ->
      VN (vectorN es :: V n e)

vectorN :: (KnownNat n) => [e] -> V n e
vectorN es =
    case LV.fromVector $ Vector.fromList es of
      Just v -> v
      _ -> error "convertExactly to V: list of incorrect length"

liftVN1 :: (e1 -> e2) -> (VN e1) -> (VN e2)
liftVN1 f (VN v) = VN (fmap f v)

liftVN2 :: (e1 -> e2 -> e3) -> (VN e1) -> (VN e2) -> (VN e3)
liftVN2 f (VN (v1 :: V n1_t e1)) (VN (v2 :: V n2_t e2)) 
  | n1_v == n2_v = VN $ liftA2 f v1 v2_n1
  | otherwise = error "liftVN2: the vectors have different lengths"
  where
  n1_v = natVal (Proxy :: Proxy n1_t)
  n2_v = natVal (Proxy :: Proxy n2_t)
  v2_n1 = (unsafeCoerce v2) :: V n1_t e2

data MatrixRC e = forall rn cn. (KnownNat rn, KnownNat cn) => MatrixRC (V rn (V cn e))

deriving instance (Show e) => (Show (MatrixRC e))

matrixRCFromList :: (Typeable e, Show e) => [[e]] -> MatrixRC e
matrixRCFromList [] = error "matrixRCFromList called with the empty list"
matrixRCFromList rows@((row1 :: [e]):_) =
  case (someNatVal (length rows), someNatVal (length row1)) of
    (Just (SomeNat (_ :: Proxy rn)), Just (SomeNat (_ :: Proxy cn))) ->
      MatrixRC (matrixRC rows :: V rn (V cn e))
    _ -> error "internal error in matrixRCFromList"
  where
  matrixRC :: (Typeable e, KnownNat cn, KnownNat rn) => 
              [[e]] -> V rn (V cn e)
  matrixRC rows2 =
    case LV.fromVector $ Vector.fromList (map vectorN rows2) of
      Just v -> v
      _ -> error "convertExactly to MatrixRC: incorrect number of rows"

liftMatrixRC1 :: (e1 -> e2) -> (MatrixRC e1) -> (MatrixRC e2)
liftMatrixRC1 f (MatrixRC mx) = MatrixRC (fmap (fmap f) mx)

liftMatrixRC2 :: (e1 -> e2 -> e3) -> (MatrixRC e1) -> (MatrixRC e2) -> (MatrixRC e3)
liftMatrixRC2 f (MatrixRC (mx1 :: V rn1_t (V cn1_t e1))) (MatrixRC (mx2 :: V rn2_t (V cn2_t e2)))  
  | (rn1_v, cn1_v) == (rn2_v, cn2_v) = MatrixRC $ liftA2 (liftA2 f) mx1 mx2_cast
  | otherwise = error "liftMatrixRC2: the matrices have different dimensions"
  where
  rn1_v = natVal (Proxy :: Proxy rn1_t)
  cn1_v = natVal (Proxy :: Proxy cn1_t)
  rn2_v = natVal (Proxy :: Proxy rn2_t)
  cn2_v = natVal (Proxy :: Proxy cn2_t)
  mx2_cast = (unsafeCoerce mx2) :: V rn1_t (V cn1_t e2)

luDetFinite :: (Fractional e) => MatrixRC e -> e
luDetFinite (MatrixRC (mx :: V rn_t (V cn_t e))) 
  | rn_v == cn_v = L.luDetFinite mxRR
  | otherwise = error "luDetFinite called for a non-square matrix"
  where
  rn_v = natVal (Proxy :: Proxy rn_t)
  cn_v = natVal (Proxy :: Proxy cn_t)
  mxRR = unsafeCoerce mx :: V rn_t (V rn_t e)

luSolveFinite :: (Fractional e) => MatrixRC e -> VN e -> VN e
luSolveFinite (MatrixRC (a :: V rn_t (V cn_t e))) (VN (b :: V n_t e))
  | rn_v == cn_v = VN $ L.luSolveFinite aNN b
  | otherwise = error "luDetFinite called for a non-square matrix"
  where
  rn_v = natVal (Proxy :: Proxy rn_t)
  cn_v = natVal (Proxy :: Proxy cn_t)
  aNN = unsafeCoerce a :: V n_t (V n_t e)

trace :: (P.Num e)=> MatrixRC e -> e
trace (MatrixRC (a :: V rn_t (V cn_t e)))
  | rn_v == cn_v = L.trace aNN
  | otherwise = error "trace called for a non-square matrix"
  where
  rn_v = natVal (Proxy :: Proxy rn_t)
  cn_v = natVal (Proxy :: Proxy cn_t)
  aNN = unsafeCoerce a :: V rn_t (V rn_t e)

mulMatrixRC :: (P.Num e) => (MatrixRC e) -> (MatrixRC e) -> (MatrixRC e)
mulMatrixRC (MatrixRC (rows1 :: V rn1_t (V cn1_t e))) (MatrixRC (rows2 :: V rn2_t (V cn2_t e)))
  | cn1_v == rn2_v = MatrixRC $ rows1 L.!*! rows2_cn1
  | otherwise = error "mulMatrixRC: the matrices have incompatible sizes"
  where
  cn1_v = natVal (Proxy :: Proxy cn1_t)
  rn2_v = natVal (Proxy :: Proxy rn2_t)
  rows2_cn1 = (unsafeCoerce rows2) :: V cn1_t (V cn2_t e)

{-
  Basic vector and matrix operations
-}

instance (CanAddAsymmetric e1 e2) => CanAddAsymmetric (VN e1) (VN e2) where
  type AddType (VN e1) (VN e2) = VN (AddType e1 e2)
  add = liftVN2 add

instance (CanSub e1 e2) => CanSub (VN e1) (VN e2) where
  type SubType (VN e1) (VN e2) = VN (SubType e1 e2)
  sub = liftVN2 sub

instance (CanNeg e1) => CanNeg (VN e1) where
  type NegType (VN e1) = VN (NegType e1)
  negate = liftVN1 negate



instance (CanAddAsymmetric e1 e2) => CanAddAsymmetric (MatrixRC e1) (MatrixRC e2) where
  type AddType (MatrixRC e1) (MatrixRC e2) = MatrixRC (AddType e1 e2)
  add = liftMatrixRC2 add

instance (CanSub e1 e2) => CanSub (MatrixRC e1) (MatrixRC e2) where
  type SubType (MatrixRC e1) (MatrixRC e2) = MatrixRC (SubType e1 e2)
  sub = liftMatrixRC2 sub

instance (CanNeg e1) => CanNeg (MatrixRC e1) where
  type NegType (MatrixRC e1)= MatrixRC (NegType e1)
  negate = liftMatrixRC1 negate

instance (P.Num e1, e1~e2) => CanMulAsymmetric (MatrixRC e1) (MatrixRC e2) where
  type MulType (MatrixRC e1) (MatrixRC e2) = MatrixRC e1
  mul = mulMatrixRC

{-
  Determinant using the Laplace method.
  
  This works OK for sparse matrices and signular matrices.
-}

detLaplace :: 
  (HasIntegers e, CanMulBy e Integer, CanAddSameType e, CanMulSameType e, Show e) =>
  (e -> Bool) -> MatrixRC e -> e
detLaplace isZero (MatrixRC mx) = 
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

{- mini tests -}

n1 :: Integer
n1 = 100

rows1I :: [[Rational]]
rows1I = [[ item i j  | j <- [1..n1] ] | i <- [1..n1]]
  where
  item i j
    | i == j = rational 1
    | j > i + 1 = rational 0
    | otherwise = 1/(i+j)

--------------------

rows1D :: [[Double]]
rows1D = map (map double) rows1I

m1D :: MatrixRC Double
m1D = matrixRCFromList rows1D

m1D_detLU :: Double
m1D_detLU = luDetFinite m1D

m1D_detLaplace :: Double
m1D_detLaplace = detLaplace (== 0) m1D

b1D :: VN Double
b1D = vNFromList $ replicate n1 (double 1)

m1b1D_solLU :: VN Double
m1b1D_solLU = luSolveFinite m1D b1D

--------------------

rows1MP :: [[MPFloat]]
rows1MP = map (map (ball_value . mpBallP (prec 1000))) rows1I

m1MP :: MatrixRC MPFloat
m1MP = matrixRCFromList rows1MP

m1MP_detLU :: MPFloat
m1MP_detLU = luDetFinite m1MP

-- The following needs (Ring MPFloat)
-- m1MP_detLaplace :: MPFloat
-- m1MP_detLaplace = detLaplace (== 0) m1MP

b1MP :: VN MPFloat
b1MP = vNFromList $ replicate n1 (ball_value $ mpBallP (prec 1000) 1)

m1b1MP_solLU :: VN MPFloat
m1b1MP_solLU = luSolveFinite m1MP b1MP

--------------------

rows1B :: [[MPBall]]
rows1B = map (map (mpBallP (prec 1000))) rows1I

m1B :: MatrixRC MPBall
m1B = matrixRCFromList rows1B

m1B_detLaplace :: MPBall
m1B_detLaplace = detLaplace (!==! 0) m1B

--------------------

rows1R :: [[CReal]]
rows1R = map (map creal) rows1I

m1R :: MatrixRC CReal
m1R = matrixRCFromList rows1R

m1R_detLaplace :: CReal
m1R_detLaplace = detLaplace (\e -> (e ? (prec 10))!==! 0) m1R

m1R_detLaplaceBits :: CN MPBall
m1R_detLaplaceBits = m1R_detLaplace ? (bits 1000)

