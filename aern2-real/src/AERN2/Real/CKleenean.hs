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
import Data.List (uncons)

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

instance CanSelectCountable CKleenean where
  type SelectCountableType CKleenean = Integer
  selectCountable ckleeneans = 
    findTrue $ concatInfiniteLists (map (unCSequence . ckleeneans) [0..])
    where
    findTrue ((ki, i) : rest) =
      case (CN.toEither ki) of
        Right CertainTrue -> i
        _ -> findTrue rest
    findTrue [] = error "selectCountable: internal error"

{-|
  Take an infinite list of infinite lists and concatenate all the elements
  in a single infinite list in such a way that no element is lost.
  Moreoved, each element has the number of the original list added to it
  so that it is possible to work out which of the lists it came from.

  This function orders the elements as follows (where each item corresponds
  to one of the lists and the numbers show the positions in the result list):

  * 1 3 6 10 ... 
  * 2 5 9 ... 
  * 4 8 ...
  * 7 ...
  * ...

-}
concatInfiniteLists :: [[t]] -> [(t, Integer)]
concatInfiniteLists lists = 
  aux [] listsWithNumbers 
  where
  aux openedLists (nextList:remainingLists) =
    heads ++ (aux tails remainingLists)
    where
    (heads, tails) = unzip (map (removeJust . uncons) (nextList:openedLists))
  aux _ [] = e
  removeJust (Just x) = x
  removeJust Nothing = e
  e = error "concatInfiniteLists can be applied only to a list of infinite lists"
  listsWithNumbers = 
    [zip list (repeat listNumber) 
      | (list, listNumber) <- zip lists [0..]]

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
