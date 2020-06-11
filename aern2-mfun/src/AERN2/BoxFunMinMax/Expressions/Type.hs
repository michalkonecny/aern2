module AERN2.BoxFunMinMax.Expressions.Type where

import MixedTypesNumPrelude
import Data.Ratio
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import qualified Prelude as P

import Data.List

-- TODO: Implement symbolic expressions

-- data E = Add E E | Sub E E | Mul E E | Div E E | Sqrt E | Abs E | Var String | Lit Rational | Min E E | Max E E | Negate E

-- minMaxTransformer :: E -> E
-- minMaxTransformer (Add e1 e2) = minMaxTransformer e1

-- The right E does not contain any Min/Max/Abs
-- This is done by dropping one branch for Min/Max
-- For abs, we can rewrite as Max(x,-x), and apply previous rule
-- The left E is a list of expressions where all expressions must be >= 0
--  If this is true, then the right E is equivalent to the original E
-- minMaxAbsEliminator :: E -> [([E],E)]
-- minMaxAbsEliminator (Min e1 e2) = ([Sub e2 e1], e1) : ([Sub e1 e2], e2) : minMaxAbsEliminator e1 : minMaxAbsEliminator e2
-- minMaxAbsEliminator (Max e1 e2) = ([Sub e1 e2], e1) : ([Sub e2 e1], e2) : minMaxAbsEliminator e1 : minMaxAbsEliminator e2
-- minMaxAbsEliminator (Abs e)     = minMaxAbsEliminator (Max (e, Negate e))

-- say we have Min (e1, e2)
-- function would return [([Sub e2 e1], e1), ([Sub e1 e2], e2)]
-- This is only the case if e1 e2 are without Abs/Min/Max 

-- Next step, E -> MinMaxTree

-- In particular, we can take an E value and put all the abs on the top

-- If we abandon symbolic expressions...
-- We want abs on trees
-- We need to implement mathematical operations on trees
-- TODO: Implement addition on trees
--  - Easy case, one tree is a leaf. Add result of leaf to all leaves in the other tree
--  - Another easy case, tree is the same shape. Simply add matching leaves with eachother
--  - Other cases will need thinking
--  -  Probably can just add a dummy Min/Max node and then recurse


-- BoxFunMinMax will be
-- MinMaxTree, dimension, and domain

-- TODO: Refactor E to this


data BinOp = Add | Sub | Mul | Div | Min | Max | Pow
  deriving (Show, P.Eq)
data UnOp  = Sqrt | Negate | Abs
  deriving (Show, P.Eq)
data E = EBinOp BinOp E E | EUnOp UnOp E | Lit Rational | Var String  -- TODO: Make Var a pair, (String, (Rational, Rational)), where (Rational, Rational) is domain
  deriving (Show, P.Eq)

data Comp = Gt | Ge | Lt | Le
  deriving (Show, P.Eq)

data Conn = And | Or | Impl
  deriving (Show, P.Eq)

data F = FComp Comp E E | FConn Conn F F
  deriving (Show, P.Eq)

simpleMax = EBinOp Max (Lit 1.0) (EUnOp Negate (Lit 1.0))
simpleMin = EBinOp Min (Lit 1.0) (EUnOp Negate (Lit 1.0))

simpleMixed = EBinOp Add (EBinOp Min (Lit 4.0) (Lit (-8.0))) (EBinOp Max (Lit 7.9) (Lit 4.0))

heronPreservationM =
    FConn
      Impl -- ->
      (FComp 
        Le -- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) -- |sqrt x - y|
        (EBinOp Add (EBinOp Pow (Lit 0.5) (EBinOp Pow (Lit 2.0) (EBinOp Sub (Var "i") (Lit 1.0)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Var "eps")) (EBinOp Sub (Var "i") (Lit 1.0)))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le -- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) -- |sqrt x - (y+x/y)/2|
        (EBinOp Add (EBinOp Pow (Lit 0.5) (EBinOp Pow (Lit 2.0) (Var "i"))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Var "eps")) (EBinOp Sub (Var "i") (Lit 1.0)))))  -- 0.5^(2^i) + 6 eps * (i-1)

-- Translate F to a single expression
-- Removes implications, logical connectives
fToE :: F -> E
fToE (FComp op e1 e2)   = case op of
  Le ->
    EBinOp Add (EUnOp Negate e1) e2 -- f1 <= f2 == f1 - f2 <= 0 == -f1 + f2 >= 0
  Lt ->
    EBinOp Add (EUnOp Negate e1) e2
  Ge ->
    EBinOp Sub e1 e2 -- f1 >= f2 == f1 - f2 >= 0 == 
  Gt ->
    EBinOp Sub e1 e2
fToE (FConn op e1 e2)   = case op of
  And ->
    EBinOp Min (fToE e1) (fToE e2)
  Or ->
    EBinOp Max (fToE e1) (fToE e2)
  Impl -> 
    EBinOp Max (EUnOp Negate (fToE e1)) (fToE e2) -- !f1 \/ f2 = max(!f1, f2)

minMaxAbsEliminator :: E -> [([E],E)]
minMaxAbsEliminator (EBinOp op e1 e2) =
  case op of
    Min ->
      concat 
      [
        [
          (p1 ++ [EBinOp Sub e2' e1'], e1'), -- e2' >= e1'
          (p2 ++ [EBinOp Sub e1' e2'], e2')  -- e1' >= e2'
        ] 
        | 
        (p1, e1') <- branch1, (p2, e2') <- branch2
      ]
    Max ->
      concat 
      [
        [
          (p1 ++ [EBinOp Sub e1' e2'], e1'), -- e1' >= e2'
          (p2 ++ [EBinOp Sub e2' e1'], e2')  -- e2' >= e1'
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

runTranslator :: E -> IO ()
runTranslator e = do
  putStrLn "Running Haskell to SMT translator for Expressions"
  -- PutStr "Enter tool: "
  putStr "Enter target file name: "
  fileName <- getLine
  putStr "Enter epsilon exponent (integer): "
  epsilonExponent <- getLine
  putStr "How many Real vars in expression? " -- BUG: dReal does not work as expected when a var has the same lower and upper bound
  numReals <- getLine
  putStr "How many Int vars in expression? "
  numInts <- getLine
  writeFile fileName (expressionAndDomainsToDreal e (parseDomains "real var name? " (read numReals)) (parseDomains "integer var name? " (read numInts)) (read epsilonExponent))
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

-- Various rules to simplify expressions
-- TODO: simplifyE :: E -> E
simplifyE :: E -> E
simplifyE (EBinOp Div e (Lit 1.0)) = e
simplifyE (EBinOp Div (Lit 0.0) _) = Lit 0.0
simplifyE (EBinOp Mul (Lit 0.0) _) = Lit 0.0
simplifyE (EBinOp Mul _ (Lit 0.0)) = Lit 0.0
simplifyE (EBinOp Mul (Lit 1.0) e) = e
simplifyE (EBinOp Mul e (Lit 1.0)) = e
simplifyE (EBinOp Add (Lit 0.0) e) = e
simplifyE (EBinOp Add e (Lit 0.0)) = e
simplifyE (EBinOp Sub e (Lit 0.0)) = e
simplifyE (EBinOp Pow _ (Lit 0.0)) = Lit 1.0
simplifyE (EBinOp Pow e (Lit 1.0)) = e
simplifyE (EUnOp Negate (Lit 0.0)) = Lit 0.0
simplifyE (EUnOp Sqrt (Lit 0.0))   = Lit 0.0
simplifyE (EUnOp Sqrt (Lit 1.0))   = Lit 1.0
simplifyE (EBinOp op e1 e2)        = EBinOp op (simplifyE e1) (simplifyE e2)
simplifyE (EUnOp op e)             = EUnOp op (simplifyE e)
simplifyE e                        = e

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
expressionToSMT (Var e) = e
expressionToSMT (Lit e) = 
  case denominator e of
    1 -> show (numerator e)
    _ ->
      "(/ " ++ show (numerator e) ++ " " ++ show (denominator e) ++ ")"

expressionToGappa :: E -> String --FIXME
expressionToGappa (EBinOp op e1 e2) =
  case op of
    Add -> expressionToGappa e1 ++ "+" ++ expressionToGappa e2
    Sub -> expressionToGappa e1 ++ "-" ++ expressionToGappa e2
    Mul -> expressionToGappa e1 ++ "*" ++ expressionToGappa e2
    Div -> expressionToGappa e1 ++ "/" ++ expressionToGappa e2
    Min -> expressionToGappa e1 ++ ">= 0 /\\ " ++ expressionToGappa e2 ++ " >= 0"
    Max -> expressionToGappa e1 ++ ">= 0 \\/ " ++ expressionToGappa e2 ++ " >= 0"
    Pow -> undefined
expressionToGappa (EUnOp op e) =
  case op of
    Sqrt -> "sqrt (" ++ expressionToSMT e ++ ")"
    Negate -> "(-1 * " ++ expressionToSMT e ++ ")"
    Abs -> "|" ++ expressionToSMT e ++ "|"
expressionToGappa (Var e) = e
expressionToGappa (Lit e) = 
  case (denominator e) of
    1 -> show (numerator e)
    _ ->
      show (numerator e) ++ "/" ++ show (denominator e)

-- Once Var is updated, search through expression to find any Vars, and then specify domains
-- Will need two searches, one will place constants before "(assert ", and the other will place foralls after "(assert "
expressionAndDomainsToDreal :: E -> [(String, (Rational, Rational))] -> [(String, (Rational, Rational))] -> Int -> String
expressionAndDomainsToDreal e realDomains intDomains epsilonExponent =
  "(set-option :precision 1e-300)" ++
  "(declare-const eps Real)" ++
  "(assert (= eps (^ 2 " ++ show epsilonExponent ++ "))) " ++
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
