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
expressionToSMT (PowI e i) = "(^ " ++ expressionToSMT e ++ " " ++ show i ++ ")"
expressionToSMT (Var e) = e
expressionToSMT (Lit e) = 
  case denominator e of
    1 -> show (numerator e)
    _ ->
      "(/ " ++ show (numerator e) ++ " " ++ show (denominator e) ++ ")"

-- disjunctionExpressionsToSMT :: [E] -> String
-- disjunctionExpressionsToSMT []        = ""
-- disjunctionExpressionsToSMT [e]       = expressionToSMT e
-- disjunctionExpressionsToSMT (e : es)  = "(max " ++ expressionToSMT e ++ disjunctionExpressionsToSMT es ++ ")"

-- cnfExpressionsToSMT :: [[E]] -> String
-- cnfExpressionsToSMT []        = ""
-- cnfExpressionsToSMT [e]       = disjunctionExpressionsToSMT e
-- cnfExpressionsToSMT (e : es)  = "(min " ++ disjunctionExpressionsToSMT e ++ cnfExpressionsToSMT es ++ ")"

disjunctionExpressionsToSMT :: [E] -> String
disjunctionExpressionsToSMT []        = ""
disjunctionExpressionsToSMT [e]       = "(>=" ++ expressionToSMT e ++ " (+ 0 1e-300))"
disjunctionExpressionsToSMT (e : es)  = "(or " ++ "(>= " ++ expressionToSMT e ++ " (+ 0 1e-300))" ++ disjunctionExpressionsToSMT es ++ ")"

cnfExpressionsToSMT :: [[E]] -> String
cnfExpressionsToSMT []        = ""
cnfExpressionsToSMT [e]       = disjunctionExpressionsToSMT e
cnfExpressionsToSMT (e : es)  = "(and " ++ disjunctionExpressionsToSMT e ++ cnfExpressionsToSMT es ++ ")"

cnfExpressionAndDomainsToDreal :: [[E]] -> [(String, (Rational, Rational))] -> [(String, (Rational, Rational))] -> Rational -> String
cnfExpressionAndDomainsToDreal cnf realDomains intDomains epsilon =
  "(set-option :precision 1e-300)" ++
  "(declare-const eps Real)" ++
  "(assert (= eps (/ " ++ show (numerator epsilon) ++ " " ++ show (denominator epsilon) ++ "))) " ++
  "(assert " ++
  case Data.List.length realDomains of
    0 ->       
      case Data.List.length intDomains of
        0 ->
          cnfExpressionsToSMT cnf ++
          ")" ++
          commonEnd
        _ ->
          "(forall (" ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Int " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") intDomains ++
          ")" ++       
          cnfExpressionsToSMT cnf ++
          "))" ++
          commonEnd
    _ ->
      case Data.List.length intDomains of
        0 ->
          "(forall (" ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Real " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") realDomains ++
          ")" ++       
          cnfExpressionsToSMT cnf ++
          "))" ++
          commonEnd
        _ ->
          "(forall (" ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Real " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") realDomains ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Int " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") intDomains ++
          ")" ++       
          cnfExpressionsToSMT cnf ++
          "))" ++
          commonEnd
  where
    commonEnd =
      "(check-sat)" ++
      "(exit)"

expressionAndDomainsToDreal :: E -> [(String, (Rational, Rational))] -> [(String, (Rational, Rational))] -> Rational -> String
expressionAndDomainsToDreal e realDomains intDomains epsilon =
  "(set-option :precision 1e-300)" ++
  "(declare-const eps Real)" ++
  "(assert (= eps (/ " ++ show (numerator epsilon) ++ " " ++ show (denominator epsilon) ++ "))) " ++
  "(assert " ++
  case Data.List.length realDomains of
    0 ->       
      case Data.List.length intDomains of
        0 ->
          "(>= " ++
          expressionToSMT e ++
          "(+ 0 1e-300))" ++
          ")" ++
          commonEnd
        _ ->
          "(forall (" ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Int " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") intDomains ++
          ")" ++       
          "(>= " ++
          expressionToSMT e ++
          "(+ 0 1e-300))" ++
          "))" ++
          commonEnd
    _ ->
      case Data.List.length intDomains of
        0 ->
          "(forall (" ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Real " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") realDomains ++
          ")" ++       
          "(>= " ++
          expressionToSMT e ++
          "(+ 0 1e-300))" ++
          "))" ++
          commonEnd
        _ ->
          "(forall (" ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Real " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") realDomains ++
          concatMap (\(x, (xL, xR)) -> "(" ++ x ++ " Int " ++ "[" ++ expressionToSMT (Lit xL) ++ ", " ++ expressionToSMT (Lit xR) ++ "]" ++ ")") intDomains ++
          ")" ++       
          "(>= " ++
          expressionToSMT e ++
          "(+ 0 1e-300))" ++
          "))" ++
          commonEnd
  where
    commonEnd =
      "(check-sat)" ++
      "(exit)"

runDRealTranslatorCNF :: [[E]] -> IO ()
runDRealTranslatorCNF cnf = do
  putStrLn "Running Haskell to dReal translator for Expressions"
  -- PutStr "Enter tool: "
  putStr "Enter target file name: "
  fileName <- getLine
  epsilon <- parseRational "epsilon "
  putStr "How many Real vars in expression? " -- BUG: dReal does not work as expected when a var has the same lower and upper bound
  numReals <- getLine
  putStr "How many Int vars in expression? "
  numInts <- getLine
  writeFile fileName (cnfExpressionAndDomainsToDreal cnf (parseDomains "real var name? " (read numReals)) (parseDomains "integer var name? " (read numInts)) epsilon)
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


runDRealTranslator :: E -> IO ()
runDRealTranslator e = do
  putStrLn "Running Haskell to dReal translator for Expressions"
  -- PutStr "Enter tool: "
  putStr "Enter target file name: "
  fileName <- getLine
  epsilon <- parseRational "epsilon "
  putStr "How many Real vars in expression? " -- BUG: dReal does not work as expected when a var has the same lower and upper bound
  numReals <- getLine
  putStr "How many Int vars in expression? "
  numInts <- getLine
  writeFile fileName (expressionAndDomainsToDreal e (parseDomains "real var name? " (read numReals)) (parseDomains "integer var name? " (read numInts)) epsilon)
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
