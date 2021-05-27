module AERN2.BoxFunMinMax.Expressions.Translators.DReal where

import MixedTypesNumPrelude

import AERN2.BoxFunMinMax.Expressions.Type

import Data.List
import Data.Ratio
import System.IO.Unsafe (unsafePerformIO)

expressionToSMT :: E -> String
expressionToSMT (EBinOp op e1 e2) =
  case op of
    Add -> "(+ " ++ expressionToSMT e1 ++ " " ++ expressionToSMT e2 ++ ")"
    Sub -> "(- " ++ expressionToSMT e1 ++ " " ++ expressionToSMT e2 ++ ")"
    Mul -> "(* " ++ expressionToSMT e1 ++ " " ++ expressionToSMT e2 ++ ")"
    Div -> "(/ " ++ expressionToSMT e1 ++ " " ++ expressionToSMT e2 ++ ")"
    Min -> "(min " ++ expressionToSMT e1 ++ " " ++ expressionToSMT e2 ++ ")"
    Max -> "(max " ++ expressionToSMT e1 ++ " " ++ expressionToSMT e2 ++ ")"
    Pow -> "(^ "  ++ expressionToSMT e1 ++ " " ++ expressionToSMT e2 ++ ")"
expressionToSMT (EUnOp op e) =
  case op of
    Sqrt -> "(sqrt " ++ expressionToSMT e ++ ")"
    Negate -> "(* -1 " ++ expressionToSMT e ++ ")"
    Abs -> "(abs " ++ expressionToSMT e ++ ")"
    Sin -> "(sin " ++ expressionToSMT e ++ ")"
    Cos -> "(cos " ++ expressionToSMT e ++ ")"
expressionToSMT (PowI e i) = "(^ " ++ expressionToSMT e ++ " " ++ show i ++ ")"
expressionToSMT (Var e) = e
expressionToSMT (Lit e) = 
  case denominator e of
    1 -> show (numerator e)
    _ ->
      "(/ " ++ show (numerator e) ++ " " ++ show (denominator e) ++ ")"
expressionToSMT (Float _ _)   = error "dReal translator does not support Floats"
expressionToSMT (Float32 _ _) = error "dReal translator does not support Floats"
expressionToSMT (Float64 _ _) = error "dReal translator does not support Floats"

disjunctionExpressionsToSMT :: [E] -> String
disjunctionExpressionsToSMT es = "(or " ++ concatMap (\e -> "(>= " ++ expressionToSMT e ++ "(+ 0 1e-300))") es ++ ")"

cnfExpressionsToSMT :: [[E]] -> String
cnfExpressionsToSMT disjunctions = "(and " ++ concatMap disjunctionExpressionsToSMT disjunctions ++ ")"

cnfExpressionAndDomainsToDreal :: [[E]] -> [(String, (Rational, Rational))] -> [(String, (Rational, Rational))] -> String
cnfExpressionAndDomainsToDreal cnf realDomains intDomains =
  "(set-option :precision 1e-300)" ++
  "(assert " ++ forAll (cnfExpressionsToSMT cnf) ++ ")(check-sat)(exit)"
  where
    forAll vc =
      "(forall (" ++ concatMap (\(v, (_, _)) -> "(" ++ v ++ " Real)") realDomains ++ concatMap (\(v, (_, _)) -> "(" ++ v ++ " Int)") intDomains ++ ")" ++ 
      "(=>" ++ 
      "(and " ++ concatMap (\(v, (vL, vR)) -> "(>= " ++ v ++ " " ++ expressionToSMT (Lit vL) ++ ")(<= " ++ v ++ " " ++ expressionToSMT (Lit vR) ++ ")") (realDomains ++ intDomains) ++ ")" ++
      vc ++ "))"

runDRealTranslatorCNF :: [[E]] -> IO ()
runDRealTranslatorCNF cnf = do
  putStrLn "Running Haskell to dReal translator for Expressions"
  -- PutStr "Enter tool: "
  putStr "Enter target file name: "
  fileName <- getLine
  putStr "How many Real vars in expression? "
  numReals <- getLine
  putStr "How many Int vars in expression? "
  numInts <- getLine
  writeFile fileName (cnfExpressionAndDomainsToDreal cnf (parseDomains "real var name? " (read numReals)) (parseDomains "integer var name? " (read numInts)))
  where
    parseDomains :: String -> Integer -> [(String, (Rational, Rational))]
    parseDomains _ 0 = []
    parseDomains msg n =

      (unsafePerformIO (getVar msg), (unsafePerformIO (parseRational "lower bound") :: Rational, unsafePerformIO (parseRational "upper bound") :: Rational))
      : parseDomains msg (n - 1)

    getVar message = do
      putStr message
      getLine

    parseRational message = do
      putStr (message ++ " numerator? ")
      num <- getLine
      putStr (message ++ " denominator? ")
      den <- getLine
      return ((read num :: Integer) /! (read den :: Integer))
