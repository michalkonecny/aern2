module AERN2.BoxFunMinMax.Expressions.Translators.Tree where

import MixedTypesNumPrelude

import AERN2.AD.Differential
import AERN2.BoxFun.Type
import AERN2.BoxFun.TestFunctions (fromListDomain)
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.MP.Ball

import qualified AERN2.BoxFunMinMax.Type as T 
import qualified AERN2.Linear.Vector.Type as V

import Data.List

qualifiedEsToTree :: [([E], E)] -> [(String, (Rational, Rational))] -> T.MinMaxTree
qualifiedEsToTree l varMap =
  T.Min
    (map 
      (\(ps, q) -> 
        T.Max 
          (T.Leaf (simplifyE q) varMap : 
          map (\p -> T.Leaf ((simplifyE (EUnOp Negate p))) varMap) ps) varMap) 
      l)
    varMap
