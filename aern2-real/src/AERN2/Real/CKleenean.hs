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
  CKleenean, CanBeCKleenean, ckleenean
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

instance (CanAndOrAsymmetric t1 t2) => CanAndOrAsymmetric (CSequence t1) (CSequence t2) where
  type AndOrType (CSequence t1)  (CSequence t2) = CSequence (AndOrType t1 t2)
  and2 = lift2 and2
  or2 = lift2 or2

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

instance CanSelect CKleenean where
  type SelectType CKleenean = Bool
  select (CSequence s1) (CSequence s2) = aux s1 s2
    where
    aux (k1 : rest1) (k2 : rest2) =
      case (CN.toEither k1, CN.toEither k2) of
        (Right CertainTrue, _) -> True 
        (_, Right CertainTrue) -> False
        (Right CertainFalse, Right CertainFalse) -> error "select: Both branches failed!"
        _ -> aux rest1 rest2
    aux _ _ = error "select: internal error"

instance (CanUnionCNSameType t) =>
  HasIfThenElse CKleenean (CSequence t)
  where
  type IfThenElseType CKleenean (CSequence t) = (CSequence t)
  ifThenElse (CSequence sc) (CSequence s1) (CSequence s2) = (CSequence r)
    where
    r = zipWith3 ifThenElse sc s1 s2

instance (HasIfThenElse CKleenean t1, HasIfThenElse CKleenean t2) =>
  HasIfThenElse CKleenean (t1, t2)
  where
  type IfThenElseType CKleenean (t1, t2) = (IfThenElseType CKleenean t1, IfThenElseType CKleenean t2)
  ifThenElse s (a1, b1) (a2, b2) =
    (ifThenElse s a1 a2, ifThenElse s b1 b2)

instance (HasIfThenElse CKleenean t) =>
  HasIfThenElse CKleenean (Maybe t)
  where
  type IfThenElseType CKleenean (Maybe t) = Maybe (IfThenElseType CKleenean t)
  ifThenElse _s Nothing Nothing = Nothing
  ifThenElse s (Just v1) (Just v2) = Just (ifThenElse s v1 v2)
  ifThenElse _ _ _ = 
    error "ifThenElse with a sequence of Kleeneans and Maybe: branches clash: Just vs Nothing"

instance (HasIfThenElse CKleenean t) =>
  HasIfThenElse CKleenean [t]
  where
  type IfThenElseType CKleenean [t] = [IfThenElseType CKleenean t]
  ifThenElse _s [] [] = []
  ifThenElse s (h1:t1) (h2:t2) = (ifThenElse s h1 h2) : (ifThenElse s t1 t2)
  ifThenElse _ _ _ = 
    error "ifThenElse with a sequence of Kleeneans and lists: branches clash: lists of different lengths"

instance (HasIfThenElse CKleenean v) =>
  HasIfThenElse CKleenean (k -> v)
  where
  type IfThenElseType CKleenean (k -> v) = k -> (IfThenElseType CKleenean v)
  ifThenElse s f1 f2 = \k -> ifThenElse s (f1 k) (f2 k)
