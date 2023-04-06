{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.Real.CKleenean
    Description :  lazy Kleenean
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Lazy Kleenean, ie a sequence of Kleeneans, usually indexed by increasing precisions.
-}
module AERN2.Real.CKleenean
(
  CKleenean, CanBeCKleenean, ckleenean, CanAndOrCountable(..)
)
where

import MixedTypesNumPrelude

import qualified Numeric.CollectErrors as CN

-- import Data.Complex

import qualified Data.List as List

import AERN2.Select

import AERN2.MP

import AERN2.Real.Type

type CKleenean = CSequence Kleenean

type CanBeCKleenean t = ConvertibleExactly t CKleenean

ckleenean :: (CanBeCKleenean t) => t -> CKleenean
ckleenean = convertExactly

-- IsBool CKleenean:

instance (ConvertibleExactly t Kleenean) => ConvertibleExactly t CKleenean where
  safeConvertExactly b = Right $ CSequence $ List.repeat $ cn $ kleenean b

instance (CanNeg t) => CanNeg (CSequence t) where
  type NegType (CSequence t) = CSequence (NegType t)
  negate = lift1 negate

instance (CanAndOrAsymmetric t1 t2, CanTestCertainly t1, HasBools t2) => 
  CanAndOrAsymmetric (CSequence t1) (CSequence t2) 
  where
  type AndOrType (CSequence t1)  (CSequence t2) = CSequence (AndOrType t1 t2)
  and2 = lift2LeftFirst and2
  or2 = lift2LeftFirst or2

instance (CanAndOrAsymmetric Bool t2) => CanAndOrAsymmetric Bool (CSequence t2) where
  type AndOrType Bool  (CSequence t2) = CSequence (AndOrType Bool t2)
  and2 = liftT1 and2
  or2 = liftT1 or2

instance (CanAndOrAsymmetric Kleenean t2) => CanAndOrAsymmetric Kleenean (CSequence t2) where
  type AndOrType Kleenean  (CSequence t2) = CSequence (AndOrType Kleenean t2)
  and2 = liftT1 and2
  or2 = liftT1 or2

instance (CanAndOrAsymmetric t1 Bool) => CanAndOrAsymmetric (CSequence t1) Bool where
  type AndOrType (CSequence t1)  Bool = CSequence (AndOrType t1 Bool)
  and2 = lift1T and2
  or2 = lift1T or2

instance (CanAndOrAsymmetric t1 Kleenean) => CanAndOrAsymmetric (CSequence t1) Kleenean where
  type AndOrType (CSequence t1)  Kleenean = CSequence (AndOrType t1 Kleenean)
  and2 = lift1T and2
  or2 = lift1T or2

class CanAndOrCountable t where
  or_countable :: (Integer -> t) -> t
  and_countable :: (Integer -> t) -> t

instance 
  CanAndOrCountable CKleenean
  where
  or_countable = lift_countable or2
  and_countable = lift_countable and2

lift_countable :: (CN Kleenean -> CN Kleenean -> CN Kleenean) -> (Integer -> CKleenean) -> CKleenean
lift_countable op s = CSequence $ map withFuel [0..]
    where
    withFuel n = 
      -- try the n'th result of the first n CKleenean's
      -- s00  s01 ... *s0n*
      -- s10  s11 ... *s1n*
      -- ...  ...     ...
      -- sn0  sn1 ... *snn*
      (foldl op (cn TrueOrFalse) (map ((!! n) . unCSequence . s) [0..(n-1)]))
      `op`
      -- try first n results of the n'th CKleenean
      -- .  s00    s01  ...  s0n
      -- .  s10    s11  ...  s1n
      -- .  ...    ...       ...
      -- . *sn0*  *sn1* ... *snn*
      (foldl op (cn TrueOrFalse) (take (n+1) (unCSequence $ s n)))

instance CanSelect CKleenean where
  type SelectType CKleenean = CN Bool
  select (CSequence s1) (CSequence s2) = aux s1 s2
    where
    aux (k1 : rest1) (k2 : rest2) =
      case (CN.toEither k1, CN.toEither k2) of
        (Right CertainTrue, _) -> cn True 
        (_, Right CertainTrue) -> cn False
        (Right CertainFalse, Right CertainFalse) -> 
          CN.noValueNumErrorCertain $ CN.NumError "select: Both branches failed!"
        _ -> aux rest1 rest2
    aux _ _ = CN.noValueNumErrorCertain $ CN.NumError "select: internal error"

instance (CanUnionCNSameType t) =>
  HasIfThenElse CKleenean (CSequence t)
  where
  type IfThenElseType CKleenean (CSequence t) = (CSequence t)
  ifThenElse (CSequence sc) (CSequence s1) (CSequence s2) = (CSequence r)
    where
    r = zipWith3 ifThenElse sc s1 s2
