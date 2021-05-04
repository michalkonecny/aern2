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
  CKleenean, select
)
where

import MixedTypesNumPrelude

import Numeric.CollectErrors ( CN, cn )
import qualified Numeric.CollectErrors as CN

-- import Data.Complex

import qualified Data.List as List

import AERN2.Real.Type

type CKleenean = CSequence Kleenean

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

-- select:

class (IsBool (SelectType k)) => CanSelect k where
  {-| Must be Bool or similar -}
  type SelectType k 
  {-|
    Execute two lazy computations "in parallel" until one of them succeeds. 
  -}
  select :: k -> k -> (SelectType k) {-^ True means that the first computation succeeded. -}

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

instance CanSelect Kleenean where
  type SelectType Kleenean = CN Bool
  select CertainTrue _ = cn True
  select _ CertainTrue = cn False
  select CertainFalse CertainFalse =
    CN.noValueNumErrorPotential $ 
      CN.NumError "select (Kleenean): Both branches failed!"
  select _ _ = 
    CN.noValueNumErrorPotential $ 
      CN.NumError "select (Kleenean): Insufficient information to determine selection."

instance CanSelect (CN Kleenean) where
  type SelectType (CN Kleenean) = CN Bool
  select cnk1 cnk2 =
    do
    k1 <- cnk1
    k2 <- cnk2
    select k1 k2
