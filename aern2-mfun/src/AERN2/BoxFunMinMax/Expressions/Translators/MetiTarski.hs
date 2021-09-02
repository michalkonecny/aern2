module AERN2.BoxFunMinMax.Expressions.Translators.MetiTarski where

import MixedTypesNumPrelude

import AERN2.BoxFunMinMax.Expressions.Type

import Data.List
import Data.Ratio
import Data.Char (toUpper)

expressionToTptp :: E -> String
expressionToTptp (EBinOp op e1 e2) =
  case op of
    Add -> "(" ++ expressionToTptp e1 ++ " + " ++ expressionToTptp e2 ++ ")"
    Sub -> "(" ++ expressionToTptp e1 ++ " - " ++ expressionToTptp e2 ++ ")"
    Mul -> "(" ++ expressionToTptp e1 ++ " * " ++ expressionToTptp e2 ++ ")"
    Div -> "(" ++ expressionToTptp e1 ++ " / " ++ expressionToTptp e2 ++ ")"
    Min -> "(min(" ++ expressionToTptp e1 ++ "," ++ expressionToTptp e2 ++ "))"
    Max -> "(max(" ++ expressionToTptp e1 ++ "," ++ expressionToTptp e2 ++ "))"
    Pow -> undefined
expressionToTptp (EUnOp op e) =
  case op of
    Sqrt -> "(sqrt(" ++ expressionToTptp e ++ "))"
    Negate -> "(-1 * " ++ expressionToTptp e ++ ")"
    Abs -> "(abs(" ++ expressionToTptp e ++ "))"
    Sin -> "(sin(" ++ expressionToTptp e ++ "))"
    Cos -> "(cos(" ++ expressionToTptp e ++ "))"
expressionToTptp (RoundToInteger m e) = 
  case m of
    RNE -> "round(" ++ expressionToTptp e ++ ")"
    RTP -> "ceiling(" ++ expressionToTptp e ++ ")"
    RTN -> "floor(" ++ expressionToTptp e ++ ")"
    RTZ -> error "Round Towards Zero not supported in TPTP"
    RNA -> error "Round Nearest Away not supported in TPTP"
expressionToTptp (PowI e i) = "(" ++ expressionToTptp e ++ " ^ " ++ show i ++ ")"
expressionToTptp (Var e) = e
expressionToTptp (Lit e) = 
  case denominator e of
    1 -> show (numerator e)
    _ ->
      "(" ++ show (numerator e) ++ " / " ++ show (denominator e) ++ ")"
expressionToTptp Pi = "pi"
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

disjunctionExpressionsToTptp :: [ESafe] -> String
disjunctionExpressionsToTptp []        = ""
disjunctionExpressionsToTptp [eSafe]       = 
  case eSafe of
    EStrict e    -> "\n\t\t\t" ++ expressionToTptp e ++ " > 0.0"
    ENonStrict e -> "\n\t\t\t" ++ expressionToTptp e ++ " >= 0.0"
disjunctionExpressionsToTptp (e : es)  = disjunctionExpressionsToTptp [e] ++ "\n\t\t\t|" ++ disjunctionExpressionsToTptp es

cnfExpressionsToTptp :: [[ESafe]] -> String
cnfExpressionsToTptp []        = ""
cnfExpressionsToTptp [e]       = "\n\t\t(" ++ disjunctionExpressionsToTptp e ++ "\n\t\t)"
cnfExpressionsToTptp (e : es)  = "\n\t\t(" ++ disjunctionExpressionsToTptp e ++ "\n\t\t)" ++ "\n\t\t&" ++ cnfExpressionsToTptp es

cnfExpressionAndDomainsToMetiTarski :: [[ESafe]] -> [(String, (Rational, Rational))] -> String
cnfExpressionAndDomainsToMetiTarski cnf realDomains =
  "fof(vc,conjecture, " ++
  "\n\t! [" ++ intercalate "," (map (\(v,_) -> map toUpper v) realDomains) ++ "] : " ++
  "\n\t(" ++
  case Data.List.length realDomains of
    0 ->       
      cnfExpressionsToTptp cnf ++
      "\n\t))."
    _ ->
      "\n\t(" ++ 
       intercalate "\n\t& " (map (\(x',(l,u)) -> let x = map toUpper x' in show (numerator l) ++ "/" ++ show (denominator l) ++ " <= " ++ x ++ " & " ++ x ++ " <= " ++ show (numerator u) ++ "/" ++ show (denominator u)) realDomains) ++
      "\n\t) =>" ++
      "\n\t(" ++ cnfExpressionsToTptp cnf ++
      "\n\t)" ++
      "\n\t))."

runMetiTarskiTranslatorCNFWithVarMap :: [[ESafe]] -> [(String, (Rational, Rational))] -> IO ()
runMetiTarskiTranslatorCNFWithVarMap cnf realVarMap =
  do
  putStrLn "Running Haskell to dReal translator for Expressions"
  putStr "Enter target file name: "
  fileName <- getLine
  writeFile fileName $ cnfExpressionAndDomainsToMetiTarski cnf realVarMap
