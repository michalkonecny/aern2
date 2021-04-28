module AERN2.BoxFunMinMax.Expressions.Translators.MetiTarski where

import MixedTypesNumPrelude

import AERN2.BoxFunMinMax.Expressions.Type

import Data.List
import Data.Ratio

expressionToTptp :: E -> String
expressionToTptp (EBinOp op e1 e2) =
  case op of
    Add -> "(" ++ expressionToTptp e1 ++ " + " ++ expressionToTptp e2 ++ ")"
    Sub -> "(" ++ expressionToTptp e1 ++ " - " ++ expressionToTptp e2 ++ ")"
    Mul -> "(" ++ expressionToTptp e1 ++ " * " ++ expressionToTptp e2 ++ ")"
    Div -> "(" ++ expressionToTptp e1 ++ " / " ++ expressionToTptp e2 ++ ")"
    Min -> undefined
    Max -> undefined
    Pow -> undefined
expressionToTptp (EUnOp op e) =
  case op of
    Sqrt -> "(sqrt(" ++ expressionToTptp e ++ "))"
    Negate -> "(-1 * " ++ expressionToTptp e ++ ")"
    Abs -> "(abs(" ++ expressionToTptp e ++ "))"
    Sin -> "(sin(" ++ expressionToTptp e ++ "))"
    Cos -> "(cos(" ++ expressionToTptp e ++ "))"
    
expressionToTptp (PowI e i) = "(" ++ expressionToTptp e ++ " ^ " ++ show i ++ ")"
expressionToTptp (Var e) = e
expressionToTptp (Lit e) = 
  case denominator e of
    1 -> show (numerator e)
    _ ->
      "(" ++ show (numerator e) ++ " / " ++ show (denominator e) ++ ")"
expressionToTptp (Float _ _)   = "MetiTarski translator does not support Floats"
expressionToTptp (Float32 _ _) = "MetiTarski translator does not support Floats"
expressionToTptp (Float64 _ _) = "MetiTarski translator does not support Floats"

-- disjunctionExpressionsToSMT :: [E] -> String
-- disjunctionExpressionsToSMT []        = ""
-- disjunctionExpressionsToSMT [e]       = expressionToSMT e
-- disjunctionExpressionsToSMT (e : es)  = "(max " ++ expressionToSMT e ++ disjunctionExpressionsToSMT es ++ ")"

-- cnfExpressionsToSMT :: [[E]] -> String
-- cnfExpressionsToSMT []        = ""
-- cnfExpressionsToSMT [e]       = disjunctionExpressionsToSMT e
-- cnfExpressionsToSMT (e : es)  = "(min " ++ disjunctionExpressionsToSMT e ++ cnfExpressionsToSMT es ++ ")"

disjunctionExpressionsToTptp :: [E] -> String
disjunctionExpressionsToTptp []        = ""
disjunctionExpressionsToTptp [e]       = "(" ++ expressionToTptp e ++ " >= 0.0)"
disjunctionExpressionsToTptp (e : es)  = disjunctionExpressionsToTptp [e] ++ " | " ++ disjunctionExpressionsToTptp es

cnfExpressionsToTptp :: [[E]] -> String
cnfExpressionsToTptp []        = ""
cnfExpressionsToTptp [e]       = "(" ++ disjunctionExpressionsToTptp e ++ ")"
cnfExpressionsToTptp (e : es)  = cnfExpressionsToTptp [e] ++ " & " ++ cnfExpressionsToTptp es

cnfExpressionAndDomainsToMetiTarski :: [[E]] -> [(String, (Rational, Rational))] -> Rational -> String
cnfExpressionAndDomainsToMetiTarski cnf realDomains epsilon =
  "cnf(eps, axiom, (eps=" ++ show (numerator epsilon) ++ "/" ++ show (denominator epsilon) ++ "))." ++
  "fof(vc,conjecture, ! [" ++ (intercalate "," (map fst realDomains)) ++ "] : " ++
  "(" ++
  case Data.List.length realDomains of
    0 ->       
      cnfExpressionsToTptp cnf ++
      "))."
    _ ->
      "(" ++ 
       (intercalate " & " (map (\(x,(l,u))->show (numerator l) ++ "/" ++ show (denominator l) ++ " <= " ++ x ++ " & " ++ x ++ "<=" ++ show (numerator u) ++ "/" ++ show (denominator u)) realDomains)) ++
      ") =>" ++
      cnfExpressionsToTptp cnf ++
      "))."
