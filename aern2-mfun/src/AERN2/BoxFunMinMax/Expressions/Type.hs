module AERN2.BoxFunMinMax.Expressions.Type where

import MixedTypesNumPrelude
import Data.Ratio
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import AERN2.MP.Ball
import AERN2.AD.Type
import AERN2.BoxFun.Type

import qualified AERN2.Linear.Vector.Type as V
import qualified AERN2.BoxFunMinMax.Type as T
import qualified Prelude as P

import AERN2.BoxFun.TestFunctions (fromListDomain) -- TODO: Move this to Util?
import AERN2.BoxFun.Optimisation (SearchBox)

import Data.List
import qualified Data.Map as Map

import Debug.Trace (trace)

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
data E = EBinOp BinOp E E | EUnOp UnOp E | Lit Rational | Var String | PowI E Integer  -- TODO: Make Var a pair, (String, (Rational, Rational)), where (Rational, Rational) is domain
  deriving (Show, P.Eq)

data Comp = Gt | Ge | Lt | Le
  deriving (Show, P.Eq)

data Conn = And | Or | Impl
  deriving (Show, P.Eq)

data F = FComp Comp E E | FConn Conn F F
  deriving (Show, P.Eq)

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
          (nub (p1 ++ p2) ++ [EBinOp Sub e2' e1'], e1'), -- e2' >= e1'
          (nub (p2 ++ p1) ++ [EBinOp Sub e1' e2'], e2')  -- e1' >= e2'
        ] 
        | 
        (p1, e1') <- branch1, (p2, e2') <- branch2
      ]
    Max ->
      concat 
      [
        [
          (nub (p1 ++ p2) ++ [EBinOp Sub e1' e2'], e1'), -- e1' >= e2'
          (nub (p2 ++ p1) ++ [EBinOp Sub e2' e1'], e2')  -- e2' >= e1'
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

qualifiedEsToCNFList :: [([E],E)] -> [E]
qualifiedEsToCNFList []               = undefined
qualifiedEsToCNFList [([], q)]        = [q]
qualifiedEsToCNFList [(ps, q)]        = [EBinOp Max (buildPs ps) q]
  where
    buildPs :: [E] -> E
    buildPs []  = undefined
    buildPs [p] = (EUnOp Negate p)
    buildPs (p : ps) = EBinOp Max (EUnOp Negate p) (buildPs ps) 
qualifiedEsToCNFList ((ps, q) : es) = (qualifiedEsToCNFList [(ps, q)]) ++ (qualifiedEsToCNFList es)


-- [ps >= 0 -> e >= 0]
-- [-ps >= 0 \/ e >= 0]
-- [max (-ps, e) >= 0]
-- min [max (-ps, e) >= 0]
qualifiedEsToTree :: [([E], E)] -> [(String, (Rational, Rational))] -> T.MinMaxTree
qualifiedEsToTree l varDomains =
  T.Min $ 
    map 
      (\(ps, q) -> 
        T.Max 
          (T.Leaf (expressionToBoxFun (simplifyE q) varDomains) : 
          map (\p -> T.Leaf (expressionToBoxFun (simplifyE (EUnOp Negate p)) varDomains)) ps)) 
      l

-- expressionToTree :: E -> [(String, (Rational, Rational))] -> T.MinMaxTree
-- expressionToTree e@(EBinOp op e1 e2) varDomains = 
--   case op of
--     Max -> T.Max $ [expressionToTree e1 varDomains] ++ [expressionToTree e2 varDomains] 
--     Min -> T.Min $ [expressionToTree e1 varDomains] ++ [expressionToTree e2 varDomains] 
--     _ -> T.Leaf (expressionToBoxFun e varDomains)
-- expressionToTree e varDomains = T.Leaf $ expressionToBoxFun e varDomains

runTranslator :: E -> IO ()
runTranslator e = do
  putStrLn "Running Haskell to SMT translator for Expressions"
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
simplifyE (PowI e 0)               = Lit 1.0
simplifyE (PowI e 1)               = e
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
expressionToSMT (PowI e i) = "(^ " ++ expressionToSMT e ++ " " ++ show i ++ ")"
expressionToSMT (Var e) = e
expressionToSMT (Lit e) = 
  case denominator e of
    1 -> show (numerator e)
    _ ->
      "(/ " ++ show (numerator e) ++ " " ++ show (denominator e) ++ ")"

-- data BoxFun =
--     BoxFun
--     {
--             dimension :: Integer
--         ,   bf_eval   :: Vector (Differential (CN MPBall)) -> Differential (CN MPBall)
--         ,   domain    :: Vector (CN MPBall)
--     }

expressionToBoxFun :: E -> [(String, (Rational, Rational))] -> BoxFun
expressionToBoxFun e domain =
  BoxFun
    (fromIntegral (Data.List.length domain))
    (expressionToDifferential e)
    vectorDomain
  where

    expressionToDifferential :: E -> V.Vector (Differential (CN MPBall)) -> Differential (CN MPBall)
    expressionToDifferential (EBinOp op e1 e2) v = 
      case op of
        Min -> undefined
        Max -> undefined
        Pow -> undefined
        Add -> expressionToDifferential e1 v + expressionToDifferential e2 v
        Sub -> expressionToDifferential e1 v - expressionToDifferential e2 v
        Mul -> expressionToDifferential e1 v * expressionToDifferential e2 v
        Div -> expressionToDifferential e1 v / expressionToDifferential e2 v
    expressionToDifferential (EUnOp op e) v = 
      case op of
        Abs -> undefined
        Sqrt -> sqrt (expressionToDifferential e v)
        Negate -> negate (expressionToDifferential e v)
    expressionToDifferential (Lit e) _ = differential 2 $ cn (mpBallP (prec 1000) e) -- TODO: paramaterise precision
    expressionToDifferential (Var e) v = 
      case elemIndex e variableOrder of
        Nothing -> undefined
        Just i -> v V.! (fromIntegral i)
    expressionToDifferential (PowI e i) v = expressionToDifferential e v ^! i


    variableOrder = map fst domain
    vectorDomain  = fromListDomain (map snd domain)

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
-- expressionToGappa (PowI e i) = TODO: translate a recursive Pow? 
expressionToGappa (Var e) = e
expressionToGappa (Lit e) = 
  case (denominator e) of
    1 -> show (numerator e)
    _ ->
      show (numerator e) ++ "/" ++ show (denominator e)

-- Once Var is updated, search through expression to find any Vars, and then specify domains
-- Will need two searches, one will place constants before "(assert ", and the other will place foralls after "(assert "
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



-- data E = EBinOp BinOp E E | EUnOp UnOp E | Lit Rational | Var String | PowI E Integer  -- TODO: Make Var a pair, (String, (Rational, Rational)), where (Rational, Rational) is domain

-- computeE using haskell with variables at specified points
computeE :: E -> [(String, Double)] -> CN Double
computeE (EBinOp op e1 e2) varMap = 
  case op of
    Min -> computeE e1 varMap `min` computeE e2 varMap
    Max -> computeE e1 varMap `max` computeE e2 varMap
    Add -> computeE e1 varMap + computeE e2 varMap
    Sub -> computeE e1 varMap - computeE e2 varMap
    Mul -> computeE e1 varMap * computeE e2 varMap
    Div -> computeE e1 varMap / computeE e2 varMap
    Pow -> computeE e1 varMap ^ computeE e2 varMap 
computeE (EUnOp op e) varMap =
  case op of
    Abs -> abs (computeE e varMap)
    Sqrt -> sqrt (computeE e varMap)
    Negate -> negate (computeE e varMap)
computeE (Var v) varMap = 
  case Map.lookup v (Map.fromList varMap) of
    Nothing -> 
      trace ("map does not contain variable " ++ show v)
      undefined
    Just r -> cn r
computeE (Lit i) _ = cn (double i)
computeE (PowI e i) varMap = computeE e varMap  ^ i

computeQualifiedEs :: [([E], E)] -> [(String, Double)] -> [CN Double]
computeQualifiedEs [] _ = []
computeQualifiedEs ((ps, q) : es) varMap =
  if all (\p -> computeE p varMap !>=! 0) ps
    then computeE q varMap : computeQualifiedEs es varMap
    else computeQualifiedEs es varMap