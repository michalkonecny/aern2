{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module AERN2.Linear.Matrix
-- (VN(..), MatrixRC(..), identity)
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
import GHC.TypeLits (KnownNat, SomeNat (SomeNat), someNatVal)
import Data.Typeable (Typeable, Proxy)
import Data.Foldable (Foldable(toList))
import qualified Data.Map as Map
import AERN2.MP
import AERN2.MP.Float (MPFloat, mpFloat)
import Control.Applicative (Applicative(liftA2))
import Control.Lens hiding (Contains(..))
-- import qualified Debug.Trace as D
-- import Text.Printf (printf)
import qualified Numeric.CollectErrors as CN
import Numeric.CollectErrors (NumError(NumError))

import qualified AERN2.Linear.Vector as VN
import AERN2.Linear.Vector (VN(..), checkVVSquare, checkVSameSizeAs, vvFromList, checkVVSameSizeAs)


----------------------
-- matrices of all sizes (hiding size type parameters)
----------------------

data MatrixRC e = forall rn cn. (KnownNat rn, KnownNat cn) => MatrixRC (V rn (V cn e))

deriving instance (Show e) => (Show (MatrixRC e))

identity :: (P.Num e) => Integer -> MatrixRC e
identity n =
  case someNatVal n of
    Just (SomeNat (_ :: Proxy n)) -> MatrixRC (L.identity :: V n (V n _))
    _ -> error "identity: internal error"

fromList :: (Typeable e, Show e) => [[e]] -> MatrixRC e
fromList [] = error "fromList called with the empty list"
fromList rows@((row1 :: [e]):_) =
  case (someNatVal (length rows), someNatVal (length row1)) of
    (Just (SomeNat (_ :: Proxy rn)), Just (SomeNat (_ :: Proxy cn))) ->
      MatrixRC (vvFromList rows :: V rn (V cn e))
    _ -> error "internal error in Matrix.fromList"

instance Functor MatrixRC where
  fmap f (MatrixRC mx) = MatrixRC (fmap (fmap f) mx)
  {-# INLINE fmap #-}

lift2 :: (e1 -> e2 -> e3) -> (MatrixRC e1) -> (MatrixRC e2) -> (MatrixRC e3)
lift2 f (MatrixRC mx1) (MatrixRC mx2) =
  MatrixRC $ liftA2 (liftA2 f) mx1 ((checkVVSameSizeAs mx1) mx2)

instance Foldable MatrixRC where
  foldl f z (MatrixRC as) = foldl (foldl f) z as
  {-# INLINE foldl #-}
  foldr f z (MatrixRC as) = foldr (flip $ foldr f) z as
  {-# INLINE foldr #-}

instance Traversable MatrixRC where
  traverse f (MatrixRC rows) = MatrixRC <$> traverse (traverse f) rows
  {-# INLINE traverse #-}

instance CanGiveUpIfVeryInaccurate e => CanGiveUpIfVeryInaccurate (MatrixRC e) where
  giveUpIfVeryInaccurate mrcCN = do
    (MatrixRC m) <- mrcCN
    fmap MatrixRC $ mapM (mapM (giveUpIfVeryInaccurate . cn)) m

instance HasPrecision e => HasPrecision (MatrixRC e) where
  getPrecision (MatrixRC a) = 
    foldl (foldl min) maximumPrecision $ fmap (fmap getPrecision) a

instance (HasAccuracy e, HasPrecision e) => HasAccuracy (MatrixRC e) where
  getAccuracy (MatrixRC a) = foldl (foldl min) Exact $ fmap (fmap getAccuracy) a 


-- wrappers for Linear functions

diagonal :: (P.Num e) => MatrixRC e -> VN e
diagonal (MatrixRC a) = VN $ L.diagonal (checkVVSquare a)

trace :: (P.Num e) => MatrixRC e -> e
trace (MatrixRC a) = L.trace (checkVVSquare a)

inftyNorm :: _ => MatrixRC e -> e
inftyNorm (MatrixRC (a :: V _ (V _ e))) =
  foldl max (fromInteger_ 0 :: e) $ fmap maxnormV a
  where
  maxnormV v = foldl max (fromInteger_ 0 :: e) $ fmap abs v

luSolve :: (P.Fractional e) => MatrixRC e -> VN e -> VN e
luSolve (MatrixRC a) (VN b) =
  VN $ L.luSolveFinite (checkVVSquare a) ((checkVSameSizeAs a) b)

luInv :: (P.Fractional e) => MatrixRC e -> MatrixRC e
luInv (MatrixRC a) = MatrixRC $ L.luInvFinite (checkVVSquare a)

luDet :: (P.Fractional e) => MatrixRC e -> e
luDet (MatrixRC a) = L.luDetFinite (checkVVSquare a)

{- Basic operations -}

instance (CanAddAsymmetric e1 e2) => CanAddAsymmetric (MatrixRC e1) (MatrixRC e2) where
  type AddType (MatrixRC e1) (MatrixRC e2) = MatrixRC (AddType e1 e2)
  add = lift2 add

instance (CanSub e1 e2) => CanSub (MatrixRC e1) (MatrixRC e2) where
  type SubType (MatrixRC e1) (MatrixRC e2) = MatrixRC (SubType e1 e2)
  sub = lift2 sub

instance (CanNeg e1) => CanNeg (MatrixRC e1) where
  type NegType (MatrixRC e1)= MatrixRC (NegType e1)
  negate = fmap negate

instance (P.Num e1, e1~e2) => CanMulAsymmetric (MatrixRC e1) (MatrixRC e2) where
  type MulType (MatrixRC e1) (MatrixRC e2) = MatrixRC e1
  mul (MatrixRC a) (MatrixRC b) = MatrixRC (a L.!*! (checkVSameSizeAs (L.transpose a) b))

instance (P.Num e1, e1~e2) => CanMulAsymmetric (MatrixRC e1) (VN e2) where
  type MulType (MatrixRC e1) (VN e2) = VN e1
  mul (MatrixRC a) (VN b) = VN (a L.!* (checkVSameSizeAs (L.transpose a) b))

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

{-
  Following Algorithm 10.7 from:
  S. Rump 2010: Verification methods: Rigorous results using floating-point arithmetic
-}
solveBViaFP :: MatrixRC MPBall -> VN MPBall -> CN (VN MPBall)
solveBViaFP aB bB@(VN bv) =
  iterateUntilInclusion 15 zB
  where
  n = VN.dimension bB

  -- approximate inverse of a:
  rF = luInv aF
  rFB = sequence $ fmap mpF2mpB rF

  -- solve approximately in FP arithmetic:
  aF = fmap centreMPF aB
  bF = fmap centreMPF bB
  xF = luSolve aF bF
  xFB = sequence $ fmap mpF2mpB xF

  -- C = I - R*A
  iB = cn $ (identity n) :: CN (MatrixRC MPBall)
  cB = iB - rFB*(cn aB)

  zB = rFB *((cn bB) - (cn aB)*xFB)

  -- attempt to bound the residual:
  iterateUntilInclusion i xBprev
    | i <= 0 = CN.noValueNumErrorPotential $ NumError "interval linear solver: failed to bound residual"
    | yB `contains` xB = xFB + xB
    | otherwise = 
      -- D.trace (printf "i = %d\n xB = %s\n yB = %s\n" i (show xB) (show yB)) $
      iterateUntilInclusion (i-1) xB
    where
    yB = fmap (fmap widenCentre) xBprev
    widenCentre = (* (mpBallP p (0.9,1.1))) . updateRadius (+ eps)
    eps = errorBound $ 0.5^((integer p))
    xB = zB + cB*yB

  -- conversion functions:
  centreMPF :: MPBall -> MPFloat
  centreMPF x = mpFloat $ centre x
  mpF2mpB :: MPFloat  -> CN MPBall
  mpF2mpB x 
    | isFinite x = cn $ MPBall x (errorBound 0)
    | otherwise = CN.noValueNumErrorCertain $ NumError "NumError"
  p = getPrecision b0
  Just b0 = (bv ^? ix (int 0))
