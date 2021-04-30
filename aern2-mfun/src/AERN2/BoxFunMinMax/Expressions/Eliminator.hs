module AERN2.BoxFunMinMax.Expressions.Eliminator where

import MixedTypesNumPrelude

import Data.List
import AERN2.BoxFunMinMax.Expressions.Type

-- | Given an expression, eliminate all Min, Max, and Abs
-- occurences. This is done by:
-- 
-- When we come across a Min e1 e2:
-- 1) We have two cases
-- 1a) if e2 >= e1, then choose e1
-- 1b) if e1 >= e2, then choose e2
-- 2) So, we eliminate min and add two elements to the qualified list
-- 2a) Add e2 - e1 to the list of premises, call the eliminiator on e1 and e2
-- recursively, add any new premises from the recursive call to the list of premises,
-- set the qualified value of e1 from the recursive call to be the qualified value
-- in this case
-- 2b) similar to 2a
-- 
-- Max e1 e2 is similar to Min e1 e2
-- Abs is treated as Max e (-e)
-- 
-- If we come across any other operator, recursively call the eliminator on any
-- expressions, add any resulting premises, and set the qualified value to be
-- the operator called on the resulting Es 
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
minMaxAbsEliminator (Float mode e)        =
  [(p, Float mode e') | (p, e') <- minMaxAbsEliminator e]
minMaxAbsEliminator (Float32 mode e)        =
  [(p, Float32 mode e') | (p, e') <- minMaxAbsEliminator e]
minMaxAbsEliminator (Float64 mode e)        =
  [(p, Float64 mode e') | (p, e') <- minMaxAbsEliminator e]
minMaxAbsEliminator e@(Lit _)             = [([],e)]
minMaxAbsEliminator e@(Var _)             = [([],e)]

-- [[[[E]]]] where [[E]] = [e1 /\ (e2 \/ e3) /\ e4]
-- [[[e1 /\ (e2 \/ e3) /\ e4]] \/ [e1 /\ (e2 \/ e3) /\ e4]]
-- [[[[e1 /\ (e2 \/ e3) /\ e4]] \/ [e1 /\ (e2 \/ e3) /\ e4]] /\ [[[e1 /\ (e2 \/ e3) /\ e4]] \/ [e1 /\ (e2 \/ e3) /\ e4]]]
minMaxAbsEliminatorECNF :: [[E]] -> [[E]]
minMaxAbsEliminatorECNF ecnf = and $ map or (map (map (qualifiedEsToCNF2 . minMaxAbsEliminator)) ecnf)
  where
    and2 = (++)
    or2 ecnf1 ecnf2 = [d1 ++ d2 | d1 <- ecnf1, d2 <- ecnf2]
    and :: [[[E]]] -> [[E]]
    and = foldl and2 []
    or :: [[[E]]] -> [[E]]
    or = foldl or2 [[]]

-- | Translate the qualified Es list to a single expression
-- The qualified Es list is basically the following formula:
-- e >= 0 == (p1 >= 0 /\ p2 >= 0 /\ p3 >=0 -> q1 >= 0) /\ repeat...
-- where e is the expression passed to minMaxAbsEliminator
-- 
-- This can be rewritten to
-- (-p1 >= 0 \/ - p2 >= 0 \/ -p3 >= 0 \/ q1 >= 0)
-- ???
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

-- | Convert a list of qualified Es to a list of lists where
-- the outer list is a conjunction and the inner list is a disjunction,
-- AKA a CNF
qualifiedEsToCNF2 :: [([E],E)] -> [[E]]
qualifiedEsToCNF2 = map (\(ps,q) -> q : map (EUnOp Negate) ps)


-- TODO:

-- Translate to this type
-- Vector (Differential (CN MPBall)) -> [[Differential (CN MPBall)]]
-- We'd give this type some domain

-- type EvalE = Vector (Differential (CN MPBall)) -> Differential (CN MPBall)
-- type EvalECNF = Vector (Differential (CN MPBall)) -> [[Differential (CN MPBall)]]
