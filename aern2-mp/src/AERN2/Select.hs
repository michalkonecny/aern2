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


