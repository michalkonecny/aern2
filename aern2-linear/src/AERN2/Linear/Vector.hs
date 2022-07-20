{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module AERN2.Linear.Vector
-- (VN(..), MatrixRC(..), identity)
where

import MixedTypesNumPrelude
-- import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

-- import qualified Prelude as P

-- import qualified Debug.Trace as Debug
-- import Text.Printf (printf)

import qualified Linear.V as LV
import Linear.V (V)
import qualified Data.Vector as Vector
import GHC.TypeLits (KnownNat, SomeNat (SomeNat), someNatVal, natVal)
import Data.Typeable (Typeable, Proxy (Proxy))
import AERN2.MP
import Unsafe.Coerce (unsafeCoerce)
import Control.Applicative (Applicative(liftA2))
-- import qualified Debug.Trace as D
-- import Text.Printf (printf)

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

fromList :: (Typeable e, Show e) => [e] -> VN e
fromList (es :: [e]) = 
  case someNatVal (length es) of
    Nothing -> error "internal error in VN.fromList"
    Just (SomeNat (_ :: Proxy n)) ->
      VN (vFromList es :: V n e)
  
instance Functor VN where
  fmap f (VN v) = VN (fmap f v)

lift2 :: (e1 -> e2 -> e3) -> (VN e1) -> (VN e2) -> (VN e3)
lift2 f (VN (v1 :: V n1_t e1)) (VN (v2 :: V n2_t e2)) =
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

inftyNorm :: _ => VN e -> e
inftyNorm (VN (v :: V _ e)) = 
  foldl max (fromInteger_ 0 :: e) $ fmap abs v

-- wrappers for Linear functions

dimension :: VN e -> Integer
dimension (VN v) = integer $ LV.dim v

{-
  Basic vector and matrix operations
-}

instance (CanAddAsymmetric e1 e2) => CanAddAsymmetric (VN e1) (VN e2) where
  type AddType (VN e1) (VN e2) = VN (AddType e1 e2)
  add = lift2 add

instance (CanSub e1 e2) => CanSub (VN e1) (VN e2) where
  type SubType (VN e1) (VN e2) = VN (SubType e1 e2)
  sub = lift2 sub

instance (CanNeg e1) => CanNeg (VN e1) where
  type NegType (VN e1) = VN (NegType e1)
  negate = fmap negate

