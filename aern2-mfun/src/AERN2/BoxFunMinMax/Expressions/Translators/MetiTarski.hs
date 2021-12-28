module AERN2.BoxFunMinMax.Expressions.Translators.MetiTarski where

import MixedTypesNumPrelude

import AERN2.BoxFunMinMax.Expressions.Type

-- import Data.List
import Data.Ratio
import Data.Char (toUpper)
import AERN2.BoxFunMinMax.VarMap
import Data.List (intercalate)

formulaToTPTP :: F -> Integer -> String
formulaToTPTP (FConn op f1 f2) numTabs = 
  case op of
    And     -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ formulaToTPTP f1 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ "&"  ++ formulaToTPTP f2 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
    Or      -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ formulaToTPTP f1 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ "|"  ++ formulaToTPTP f2 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
    Impl    -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ formulaToTPTP f1 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ "=>" ++ formulaToTPTP f2 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
    Equiv   -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ formulaToTPTP f1 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ "="  ++ formulaToTPTP f2 (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
formulaToTPTP (FComp op e1 e2) numTabs =
  case op of
    Ge -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e1 ++ "\n" ++ concat (replicate numTabs "\t") ++ ">=" ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e2 ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
    Gt -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e1 ++ "\n" ++ concat (replicate numTabs "\t") ++ ">"  ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e2 ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
    Le -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e1 ++ "\n" ++ concat (replicate numTabs "\t") ++ "<=" ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e2 ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
    Lt -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e1 ++ "\n" ++ concat (replicate numTabs "\t") ++ "<"  ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e2 ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
    Eq -> "\n" ++ concat (replicate numTabs "\t") ++ "(" ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e1 ++ "\n" ++ concat (replicate numTabs "\t") ++ "="  ++ "\n" ++ concat (replicate (numTabs + 1) "\t") ++ expressionToTPTP e2 ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
formulaToTPTP (FNot f) numTabs = "\n" ++ concat (replicate numTabs "\t") ++ "~(" ++ formulaToTPTP f (numTabs + 1) ++ "\n" ++ concat (replicate numTabs "\t") ++ ")"
formulaToTPTP FTrue numTabs = "\n" ++ concat (replicate numTabs "\t") ++ "(0 = 0)"
formulaToTPTP FFalse numTabs = "\n" ++ concat (replicate numTabs "\t") ++ "(0 = 1)"

expressionToTPTP :: E -> String
expressionToTPTP (EBinOp op e1 e2) =
  case op of
    Add -> "(" ++ expressionToTPTP e1 ++ " + " ++ expressionToTPTP e2 ++ ")"
    Sub -> "(" ++ expressionToTPTP e1 ++ " - " ++ expressionToTPTP e2 ++ ")"
    Mul -> "(" ++ expressionToTPTP e1 ++ " * " ++ expressionToTPTP e2 ++ ")"
    Div -> "(" ++ expressionToTPTP e1 ++ " / " ++ expressionToTPTP e2 ++ ")"
    Min -> "(min(" ++ expressionToTPTP e1 ++ ", " ++ expressionToTPTP e2 ++ "))"
    Max -> "(max(" ++ expressionToTPTP e1 ++ ", " ++ expressionToTPTP e2 ++ "))"
    Pow -> "(" ++ expressionToTPTP e1 ++ " ^ " ++ expressionToTPTP e2 ++ ")"
    Mod -> error "Modulo is not supported in tptp"
expressionToTPTP (EUnOp op e) =
  case op of
    Sqrt -> "(sqrt(" ++ expressionToTPTP e ++ "))"
    Negate -> "(-1 * " ++ expressionToTPTP e ++ ")"
    Abs -> "(abs(" ++ expressionToTPTP e ++ "))"
    Sin -> "(sin(" ++ expressionToTPTP e ++ "))"
    Cos -> "(cos(" ++ expressionToTPTP e ++ "))"
expressionToTPTP (RoundToInteger m e) = 
  case m of
    RNE -> "round(" ++ expressionToTPTP e ++ ")"
    RTP -> "ceiling(" ++ expressionToTPTP e ++ ")"
    RTN -> "floor(" ++ expressionToTPTP e ++ ")"
    RTZ -> error "Round Towards Zero not supported in TPTP"
    RNA -> error "Round Nearest Away not supported in TPTP"
expressionToTPTP (PowI e i) = "(" ++ expressionToTPTP e ++ " ^ " ++ show i ++ ")"
expressionToTPTP (Var e) = map toUpper e
expressionToTPTP (Lit e) = 
  case denominator e of
    1 -> show (numerator e)
    _ ->
      "(" ++ show (numerator e) ++ " / " ++ show (denominator e) ++ ")"
expressionToTPTP Pi = "pi"
expressionToTPTP (Float _ _)   = "MetiTarski translator does not support Floats"
expressionToTPTP (Float32 _ _) = "MetiTarski translator does not support Floats"
expressionToTPTP (Float64 _ _) = "MetiTarski translator does not support Floats"

formulaAndVarMapToMetiTarski :: F -> TypedVarMap -> String
formulaAndVarMapToMetiTarski f typedVarMap =
  "fof(vc,conjecture," ++ 
  case typedVarMap of
    []  -> "\n\t(" ++ formulaToTPTP f 2 ++ "\n\t)" ++ "\n)."
    _   -> "\n\t! [" ++ intercalate "," (map (\(TypedVar (v,_) _) -> map toUpper v) typedVarMap) ++ "] : (" ++
           "\n\t(" ++ variablesAsString typedVarMap ++ "\n\t) =>" ++
           "\n\t(" ++ formulaToTPTP f 2 ++ "\n\t))" ++ "\n)."
  where
    variablesAsString [] = ""
    variablesAsString ((TypedVar (varName, (leftBound, rightBound)) _) : typedVarIntervals) =
      let 
        varNameUpper = map toUpper varName
        leftBoundString = show (numerator leftBound) ++ " / " ++ show (denominator leftBound)
        rightBoundString = show (numerator rightBound) ++ " / " ++ show (denominator rightBound)
      in
        "\n\t\t" ++ leftBoundString ++ " <= " ++ varNameUpper ++ " & " ++ varNameUpper ++ " <= " ++ rightBoundString ++ (if null typedVarIntervals then "" else " &" ++ variablesAsString typedVarIntervals)

disjunctionExpressionsToSMT :: [E] -> String
disjunctionExpressionsToSMT []        = ""
disjunctionExpressionsToSMT [e]       = expressionToTPTP e
disjunctionExpressionsToSMT (e : es)  = "(max " ++ expressionToTPTP e ++ disjunctionExpressionsToSMT es ++ ")"

cnfExpressionsToSMT :: [[E]] -> String
cnfExpressionsToSMT []        = ""
cnfExpressionsToSMT [e]       = disjunctionExpressionsToSMT e
cnfExpressionsToSMT (e : es)  = "(min " ++ disjunctionExpressionsToSMT e ++ cnfExpressionsToSMT es ++ ")"

disjunctionExpressionsToTptp :: [ESafe] -> String
disjunctionExpressionsToTptp []        = ""
disjunctionExpressionsToTptp [eSafe]       = 
  case eSafe of
    EStrict e    -> "\n\t\t\t" ++ expressionToTPTP e ++ " > 0.0"
    ENonStrict e -> "\n\t\t\t" ++ expressionToTPTP e ++ " >= 0.0"
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
  case realDomains of
    [] ->       
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
