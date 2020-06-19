module AERN2.BoxFunMinMax.Expressions.Eliminator where

import MixedTypesNumPrelude

import Data.List
import AERN2.BoxFunMinMax.Expressions.Type

minMaxAbsEliminator :: E -> [([E],E)]
minMaxAbsEliminator (EBinOp op e1 e2) =
  case op of
    Min ->
      concat 
      [
        [
          (nub ((p1 ++ p2) ++ [EBinOp Sub e2' e1']), e1'), -- e2' >= e1'
          (nub ((p2 ++ p1) ++ [EBinOp Sub e1' e2']), e2')  -- e1' >= e2'
        ] 
        | 
        (p1, e1') <- branch1, (p2, e2') <- branch2
      ]
    Max ->
      concat 
      [
        [
          (nub ((p1 ++ p2) ++ [EBinOp Sub e1' e2']), e1'), -- e1' >= e2'
          (nub ((p2 ++ p1) ++ [EBinOp Sub e2' e1']), e2')  -- e2' >= e1'
        ] 
        | 
        (p1, e1') <- branch1, (p2, e2') <- branch2
      ]
    op' ->
      [(nub (p1 ++ p2), EBinOp op' e1' e2') | (p1, e1') <- branch1, (p2, e2') <- branch2]
  where
    branch1 = minMaxAbsEliminator e1
    branch2 = minMaxAbsEliminator e2
minMaxAbsEliminator (EUnOp op e) =
  case op of
    Abs -> 
      minMaxAbsEliminator (EBinOp Max e (EUnOp Negate e))
    op' ->
      [(p, EUnOp op' e') | (p, e') <- minMaxAbsEliminator e]
minMaxAbsEliminator (PowI e i)            =
  [(p, PowI e' i) | (p, e') <- minMaxAbsEliminator e]
minMaxAbsEliminator e@(Lit _)             = [([],e)]
minMaxAbsEliminator e@(Var _)             = [([],e)]

qualifiedEsToCNF :: [([E],E)] -> E
qualifiedEsToCNF []               = undefined
qualifiedEsToCNF [([], q)]        = q
qualifiedEsToCNF [(ps, q)]        = EBinOp Max (buildPs ps) q
  where
    buildPs :: [E] -> E
    buildPs []  = undefined
    buildPs [p] = (EUnOp Negate p)
    buildPs (p : ps) = EBinOp Max (EUnOp Negate p) (buildPs ps) 
qualifiedEsToCNF ((ps, q) : es) = EBinOp Min (qualifiedEsToCNF [(ps, q)]) (qualifiedEsToCNF es)

