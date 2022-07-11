{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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
import AERN2.Real
import qualified AERN2.Real.Type as Real
import Data.Foldable (Foldable(toList))
import qualified Data.Map as Map
import AERN2.MP
import AERN2.MP.Float (MPFloat, mpFloat)
import Unsafe.Coerce (unsafeCoerce)
import Control.Applicative (Applicative(liftA2))
import Control.Lens hiding (Contains(..))
-- import qualified Debug.Trace as D
-- import Text.Printf (printf)
import qualified Numeric.CollectErrors as CN
import Numeric.CollectErrors (NumError(NumError))

----------------------
-- checking sizes of vectors and matrices
----------------------

checkVVSquare :: (KnownNat rn, KnownNat cn) => V rn (V cn e) -> V rn (V rn e)
checkVVSquare (mx :: V rn_t (V cn_t e))
  | rn_v == cn_v = unsafeCoerce mx
  | otherwise = error "expecting a square matrix, but got a non-square matrix"
  where
  rn_v = natVal (Proxy :: Proxy rn_t)
  cn_v = natVal (Proxy :: Proxy cn_t)

checkVSameSizeAs :: (KnownNat n1, KnownNat n2) => (V n1 e1) -> (V n2 e2) -> (V n1 e2)
checkVSameSizeAs (_ :: V n1_t e1) (v2 :: V n2_t e2)
  | n1_v == n2_v = unsafeCoerce v2
  | otherwise = error "expecting a vector of matching size, but got one of a different size"
  where
  n1_v = natVal (Proxy :: Proxy n1_t)
  n2_v = natVal (Proxy :: Proxy n2_t)

checkVVSameSizeAs :: 
  (KnownNat rn1, KnownNat cn1, KnownNat rn2, KnownNat cn2) => 
  (V rn1 (V cn1 e1)) -> (V rn2 (V cn2 e2)) -> (V rn1 (V cn1 e2))
checkVVSameSizeAs (_ :: V rn1_t (V cn1_t e1)) (mx2 :: V rn2_t (V cn2_t e2))
  | (rn1_v, cn1_v) == (rn2_v, cn2_v) = unsafeCoerce mx2
  | otherwise = error "expecting a matrix of matching size, but got one of a different size"
  where
  rn1_v = natVal (Proxy :: Proxy rn1_t)
  cn1_v = natVal (Proxy :: Proxy cn1_t)
  rn2_v = natVal (Proxy :: Proxy rn2_t)
  cn2_v = natVal (Proxy :: Proxy cn2_t)

vFromList :: (KnownNat n) => [e] -> V n e
vFromList es =
    case LV.fromVector $ Vector.fromList es of
      Just v -> v
      _ -> error "convertExactly to V: list of incorrect length"

vvFromList :: (Typeable e, KnownNat cn, KnownNat rn) => 
            [[e]] -> V rn (V cn e)
vvFromList rows2 =
  case LV.fromVector $ Vector.fromList (map vFromList rows2) of
    Just v -> v
    _ -> error "convertExactly to MatrixRC: incorrect number of rows"

----------------------
-- vectors of all sizes (hiding size type parameters)
----------------------

data VN e = forall n. (KnownNat n) => VN (V n e)
  
deriving instance (Show e) => (Show (VN e))

vNFromList :: (Typeable e, Show e) => [e] -> VN e
vNFromList (es :: [e]) = 
  case someNatVal (length es) of
    Nothing -> error "internal error in vNFromList"
    Just (SomeNat (_ :: Proxy n)) ->
      VN (vFromList es :: V n e)
  
instance Functor VN where
  fmap f (VN v) = VN (fmap f v)

liftVN2 :: (e1 -> e2 -> e3) -> (VN e1) -> (VN e2) -> (VN e3)
liftVN2 f (VN (v1 :: V n1_t e1)) (VN (v2 :: V n2_t e2)) =
  VN $ liftA2 f v1 (checkVSameSizeAs v1 v2)

instance Foldable VN where
  foldl f z (VN as) = foldl f z as
  {-# INLINE foldl #-}
  foldr f z (VN as) = foldr f z as
  {-# INLINE foldr #-}

instance Traversable VN where
  traverse f (VN as) = VN <$> traverse f as
  {-# INLINE traverse #-}

instance CanTestContains e1 e2 => CanTestContains (VN e1) (VN e2) where
  contains (VN v1) (VN v2) = 
    foldl (&&) True $ liftA2 contains v1 (checkVSameSizeAs v1 v2)
    
instance CanGiveUpIfVeryInaccurate b => CanGiveUpIfVeryInaccurate (VN b) where
  giveUpIfVeryInaccurate vnCN = do
    (VN v) <- vnCN
    fmap VN $ mapM (giveUpIfVeryInaccurate . cn) v

instance HasPrecision e => HasPrecision (VN e) where
  getPrecision (VN v) = foldl min maximumPrecision $ fmap getPrecision v 

instance (HasAccuracy e, HasPrecision e) => HasAccuracy (VN e) where
  getAccuracy (VN v) = foldl min Exact $ fmap getAccuracy v 


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

matrixRCFromList :: (Typeable e, Show e) => [[e]] -> MatrixRC e
matrixRCFromList [] = error "matrixRCFromList called with the empty list"
matrixRCFromList rows@((row1 :: [e]):_) =
  case (someNatVal (length rows), someNatVal (length row1)) of
    (Just (SomeNat (_ :: Proxy rn)), Just (SomeNat (_ :: Proxy cn))) ->
      MatrixRC (vvFromList rows :: V rn (V cn e))
    _ -> error "internal error in matrixRCFromList"

instance Functor MatrixRC where
  fmap f (MatrixRC mx) = MatrixRC (fmap (fmap f) mx)

instance Foldable MatrixRC where
  foldl f z (MatrixRC as) = foldl (foldl f) z as
  {-# INLINE foldl #-}
  foldr f z (MatrixRC as) = foldr (flip $ foldr f) z as
  {-# INLINE foldr #-}

instance Traversable MatrixRC where
  traverse f (MatrixRC rows) = MatrixRC <$> traverse (traverse f) rows
  {-# INLINE traverse #-}

liftMatrixRC2 :: (e1 -> e2 -> e3) -> (MatrixRC e1) -> (MatrixRC e2) -> (MatrixRC e3)
liftMatrixRC2 f (MatrixRC mx1) (MatrixRC mx2) =
  MatrixRC $ liftA2 (liftA2 f) mx1 ((checkVVSameSizeAs mx1) mx2)

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

luSolve :: (P.Fractional e) => MatrixRC e -> VN e -> VN e
luSolve (MatrixRC a) (VN b) =
  VN $ L.luSolveFinite (checkVVSquare a) ((checkVSameSizeAs a) b)

luInv :: (P.Fractional e) => MatrixRC e -> MatrixRC e
luInv (MatrixRC a) = MatrixRC $ L.luInvFinite (checkVVSquare a)

luDet :: (P.Fractional e) => MatrixRC e -> e
luDet (MatrixRC a) = L.luDetFinite (checkVVSquare a)

trace :: (P.Num e) => MatrixRC e -> e
trace (MatrixRC a) = L.trace (checkVVSquare a)

diagonal :: (P.Num e) => MatrixRC e -> VN e
diagonal (MatrixRC a) = VN $ L.diagonal (checkVVSquare a)

maxnormVN :: _ => VN e -> e
maxnormVN (VN (v :: V _ e)) = 
  foldl max (fromInteger_ 0 :: e) $ fmap abs v

maxnorm :: _ => MatrixRC e -> e
maxnorm (MatrixRC (a :: V _ (V _ e))) = 
  foldl max (fromInteger_ 0 :: e) $ fmap maxnormV a
  where
  maxnormV v = foldl max (fromInteger_ 0 :: e) $ fmap abs v

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
  negate = fmap negate



instance (CanAddAsymmetric e1 e2) => CanAddAsymmetric (MatrixRC e1) (MatrixRC e2) where
  type AddType (MatrixRC e1) (MatrixRC e2) = MatrixRC (AddType e1 e2)
  add = liftMatrixRC2 add

instance (CanSub e1 e2) => CanSub (MatrixRC e1) (MatrixRC e2) where
  type SubType (MatrixRC e1) (MatrixRC e2) = MatrixRC (SubType e1 e2)
  sub = liftMatrixRC2 sub

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

{- mini tests -}

n1 :: Integer
n1 = 100

rows1Q :: [[Rational]]
rows1Q = [[ item i j  | j <- [1..n1] ] | i <- [1..n1]]
  where
  item i j
    -- | i == j = rational 1
    -- | j > i + 1 = rational 0
    | otherwise = 1/(n1*(i-1)+j)

--------------------

rows1D :: [[Double]]
rows1D = map (map double) rows1Q

m1D :: MatrixRC Double
m1D = matrixRCFromList rows1D

m1D_detLU :: Double
m1D_detLU = luDet m1D

m1D_detLaplace :: Double
m1D_detLaplace = detLaplace (== 0) m1D

x1D :: VN Double
x1D = vNFromList $ replicate n1 (double 1)

b1D :: VN Double
b1D = m1D * x1D

m1b1D_solLU :: VN Double
m1b1D_solLU = luSolve m1D b1D

--------------------

p1 :: Precision
p1 = prec 1000

rows1MP :: [[MPFloat]]
rows1MP = map (map (ball_value . mpBallP p1)) rows1Q

m1MP :: MatrixRC MPFloat
m1MP = matrixRCFromList rows1MP

m1MP_detLU :: MPFloat
m1MP_detLU = luDet m1MP

-- The following needs (Ring MPFloat)
-- m1MP_detLaplace :: MPFloat
-- m1MP_detLaplace = detLaplace (== 0) m1MP

x1MP :: VN MPFloat
x1MP = vNFromList $ replicate n1 (ball_value $ mpBallP p1 1)

b1MP :: VN MPFloat
b1MP = m1MP * x1MP

m1b1MP_solLU :: VN MPFloat
m1b1MP_solLU = luSolve m1MP b1MP

--------------------

{-
  Following Algorithm 10.7 from:
  S. Rump 2010: Verification methods: Rigorous results using floating-point arithmetic
-}
solveBViaFP :: MatrixRC MPBall -> VN MPBall -> CN (VN MPBall)
solveBViaFP aB bB@(VN bv) =
  iterateUntilInclusion 15 zB
  where
  -- approximate inverse of a:
  rF = luInv aF
  rFB = sequence $ fmap mpF2mpB rF

  -- solve approximately in FP arithmetic:
  aF = fmap centreMPF aB
  bF = fmap centreMPF bB
  xF = luSolve aF bF
  xFB = sequence $ fmap mpF2mpB xF

  -- C = I - R*A
  iB = cn $ (identity n1) :: CN (MatrixRC MPBall)
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

rows1B :: [[MPBall]]
rows1B = map (map (mpBallP p1)) rows1Q

m1B :: MatrixRC MPBall
m1B = matrixRCFromList rows1B

m1B_detLaplace :: MPBall
m1B_detLaplace = detLaplace (!==! 0) m1B

x1B :: VN MPBall
x1B = vNFromList $ replicate n1 (mpBallP p1 1)

b1B :: VN MPBall
b1B = m1B * x1B

m1b1B_solveViaFP :: CN (VN MPBall)
m1b1B_solveViaFP = solveBViaFP m1B b1B

--------------------

type CVN = CSequence (VN MPBall)
type CMatrixRC = CSequence (MatrixRC MPBall)

solveRViaFP :: CMatrixRC -> CVN -> CVN
solveRViaFP aR bR =
  Real.lift2 solve aR bR
  where
  solve aCN bCN = do
    a <- aCN
    b <- bCN
    solveBViaFP a b

m1R :: CMatrixRC
m1R = cseqFromPrecFunction $ \p -> 
  cn $ matrixRCFromList $ 
    map (map (mpBallP p)) rows1Q

m1R_detLaplace :: CReal
m1R_detLaplace = 
  Real.lift1 (fmap $ detLaplace (!==! 0)) m1R

m1R_detLaplaceBits :: CN MPBall
m1R_detLaplaceBits = m1R_detLaplace ? (bits 1000)

x1R :: CVN
x1R = cseqFromPrecFunction $ \p ->
  cn $ vNFromList $ 
    replicate n1 (mpBallP p 1)

b1R :: CVN
b1R = m1R * x1R

m1b1R_solveViaFP :: CVN
m1b1R_solveViaFP = solveRViaFP m1R b1R

m1b1R_solveViaFPBits :: CN (VN MPBall)
m1b1R_solveViaFPBits = m1b1R_solveViaFP ? (bits 100)
