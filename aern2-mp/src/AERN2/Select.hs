{-|
    Module      :  AERN2.Select
    Description :  multivalued select operation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Generic multivalued select operation
-}
module AERN2.Select where

import MixedTypesNumPrelude

import qualified Numeric.CollectErrors as CN

import AERN2.Kleenean

class (IsBool (SelectType k)) => CanSelect k where
  {-| Must be Bool or similar -}
  type SelectType k 
  {-|
    Execute two lazy computations "in parallel" until one of them succeeds. 
  -}
  select :: k -> k -> (SelectType k) {-^ True means that the first computation succeeded. -}

type CanSelectBool k = (CanSelect k, SelectType k ~ Bool)
type CanSelectCNBool k = (CanSelect k, SelectType k ~ CN Bool)

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

class CanSelectCountable k where
  {-| Must be Integer or similar -}
  type SelectCountableType k 
  {-|
    Execute a sequence of lazy computations "in parallel" until one of them succeeds
    and return the index of a succeeding computation. 
  -}
  selectCountable :: (Integer -> k) -> SelectCountableType k

type CanSelectCountableInteger k = (CanSelectCountable k, SelectCountableType k ~ Integer)
type CanSelectCountableCNInteger k = (CanSelectCountable k, SelectCountableType k ~ CN Integer)

instance CanSelectCountable Kleenean where
  type SelectCountableType Kleenean = Integer
  selectCountable kleeneans = aux 0
    where
    aux i = case kleeneans i of
      CertainTrue -> i
      _ -> aux (i + 1)

instance CanSelectCountable (CN Kleenean) where
  type SelectCountableType (CN Kleenean) = CN Integer
  selectCountable cnkleeneans = aux 0
    where
    aux i = do
      ki <- cnkleeneans i
      case ki of
        CertainTrue -> cn i
        _ -> aux (i + 1)
