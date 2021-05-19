module AERN2.BoxFunMinMax.Expressions.Type where

import MixedTypesNumPrelude

import qualified Prelude as P

import qualified Data.Map as Map

import Debug.Trace (trace)

data BinOp = Add | Sub | Mul | Div | Min | Max | Pow
  deriving (Show, P.Eq, P.Ord)
data UnOp  = Sqrt | Negate | Abs | Sin | Cos
  deriving (Show, P.Eq, P.Ord)

data RoundingMode = RNE | RTP | RTN | RTZ deriving (Show, P.Eq, P.Ord)
-- | The E type represents the inequality: expression :: E >= 0
-- TODO: Add rounding operator with certain epsilon/floating-point type
data E = EBinOp BinOp E E | EUnOp UnOp E | Lit Rational | Var String | PowI E Integer | Float32 RoundingMode E | Float64 RoundingMode E | Float RoundingMode E -- Float Expression Significand
  deriving (Show, P.Eq, P.Ord)

data Comp = Gt | Ge | Lt | Le | Eq
  deriving (Show, P.Eq)

data Conn = And | Or | Impl
  deriving (Show, P.Eq)

-- | The F type is used to specify comparisons between E types
-- and logical connectives between F types
data F = FComp Comp E E | FConn Conn F F | FNot F
  deriving (Show, P.Eq)

-- | Translate F to a single expression (E)
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
  Eq -> fToE $ FConn And (FComp Ge e1 e2) (FComp Le e1 e2) -- f1 = f2 == f1 >= f2 /\ f1 <= f2
fToE (FConn op e1 e2)   = case op of
  And ->
    EBinOp Min (fToE e1) (fToE e2)
  Or ->
    EBinOp Max (fToE e1) (fToE e2)
  Impl -> 
    EBinOp Max (EUnOp Negate (fToE e1)) (fToE e2) -- !f1 \/ f2 = max(!f1, f2)
fToE (FNot f) = EUnOp Negate (fToE f)

fToECNF :: F -> [[E]]
fToECNF (FComp op e1 e2)   = case op of
  Le ->
    [[EBinOp Add (EUnOp Negate e1) e2]] -- f1 <= f2 == f1 - f2 <= 0 == -f1 + f2 >= 0
  Lt ->
    [[EBinOp Add (EUnOp Negate e1) e2]]
  Ge ->
    [[EBinOp Sub e1 e2]] -- f1 >= f2 == f1 - f2 >= 0 == 
  Gt ->
    [[EBinOp Sub e1 e2]]
  Eq -> fToECNF $ FConn And (FComp Ge e1 e2) (FComp Le e1 e2) -- f1 = f2 == f1 >= f2 /\ f1 <= f2
fToECNF (FConn op f1 f2)   = case op of
  And -> fToECNF f1 ++ fToECNF f2 -- [e1 /\ e2 /\ (e3 \/ e4)] ++ [p1 /\ (p2 \/ p3) /\ p4] = [e1 /\ e2 /\ (e3 \/ e4) /\ p1 /\ (p2 \/ p3) /\ p4]
  Or ->  [d1 ++ d2 | d1 <- fToECNF f1, d2 <- fToECNF f2] -- [e1 /\ e2 /\ (e3 \/ e4)] \/ [p1 /\ (p2 \/ p3) /\ p4] 
  Impl -> [d1 ++ d2 | d1 <- map (map (EUnOp Negate)) (fToECNF f1), d2 <- fToECNF f2]
fToECNF (FNot f) = map (map (EUnOp Negate)) (fToECNF f)

-- | Add bounds for any Float expressions
-- addRoundingBounds :: E -> [[E]]
-- addRoundingBounds (Float e significand) = [[exactExpression - machineEpsilon], [exactExpression + machineEpsilon]]
--   where
--     exactExpression = addRoundingBounds e
--     machineEpsilon = 2^(-23)
-- addRoundingBounds e = e

-- | Various rules to simplify expressions
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

simplifyF :: F -> F
simplifyF f@(FConn Or (FComp Lt f1l f1r) (FComp Eq f2l f2r)) = if f1l P.== f2l P.&& f1r P.== f2r then FComp Le f1l f1r else f
simplifyF (FConn Or (FComp Eq f1l f1r) (FComp Lt f2l f2r))   = simplifyF $ FConn Or (FComp Lt f2l f2r) (FComp Eq f1l f1r)
simplifyF f@(FConn Or (FComp Gt f1l f1r) (FComp Eq f2l f2r)) = if f1l P.== f2l P.&& f1r P.== f2r then FComp Ge f1l f1r else f
simplifyF (FConn Or (FComp Eq f1l f1r) (FComp Gt f2l f2r))   = simplifyF $ FConn Or (FComp Gt f2l f2r) (FComp Eq f1l f1r)
simplifyF (FConn op f1 f2)                                   = FConn op (simplifyF f1) (simplifyF f2)
simplifyF (FComp op e1 e2)                                   = FComp op (simplifyE e1) (simplifyE e2)
simplifyF (FNot (FNot f))                                    = simplifyF f
simplifyF (FNot f)                                           = FNot (simplifyF f)

simplifyECNF :: [[E]] -> [[E]]
simplifyECNF = map (map simplifyE) 

-- | compute the value of E with Vars at specified points
computeE :: E -> [(String, Rational)] -> CN Double
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
    Sin -> sin (computeE e varMap)
    Cos -> cos (computeE e varMap)
computeE (Var v) varMap = 
  case Map.lookup v (Map.fromList varMap) of
    Nothing -> 
      trace ("map does not contain variable " ++ show v)
      undefined
    Just r -> cn (double r)
computeE (Lit i) _ = cn (double i)
computeE (PowI e i) varMap = computeE e varMap  ^ i
computeE (Float _ _) _   = error "computeE for Floats not supported"
computeE (Float32 _ _) _ = error "computeE for Floats not supported"
computeE (Float64 _ _) _ = error "computeE for Floats not supported"

-- | Given a list of qualified Es and points for all Vars,
-- compute a list of valid values. 
-- 
-- A value is the computed result of the second element of 
-- the tuple and is valid if all the expressions in the list 
-- at the first element of the tuple compute to be above 0.
computeQualifiedEs :: [([E], E)] -> [(String, Rational)] -> [CN Double]
computeQualifiedEs [] _ = []
computeQualifiedEs ((ps, q) : es) varMap =
  if all (\p -> computeE p varMap !>=! 0) ps
    then computeE q varMap : computeQualifiedEs es varMap
    else computeQualifiedEs es varMap

computeEDisjunction :: [E] -> [(String, Rational)] -> [CN Double]
computeEDisjunction es varMap = map (`computeE` varMap) es

computeECNF :: [[E]] -> [(String, Rational)] -> [[CN Double]]
computeECNF cnf varMap = map (`computeEDisjunction` varMap) cnf

-- |Show an expression in a human-readable format
-- Rationals are converted into doubles
prettyShowE :: E -> String
prettyShowE (EBinOp op e1 e2) =
  case op of
    Add -> "(" ++ prettyShowE e1 ++ " + " ++ prettyShowE e2 ++ ")"
    Sub -> "(" ++ prettyShowE e1 ++ " - " ++ prettyShowE e2 ++ ")"
    Div -> "(" ++ prettyShowE e1 ++ " / " ++ prettyShowE e2 ++ ")"
    Mul -> "(" ++ prettyShowE e1 ++ " * " ++ prettyShowE e2 ++ ")"
    Pow -> "(" ++ prettyShowE e1 ++ " ^ " ++ prettyShowE e2 ++ ")"
    Min -> "min(" ++ prettyShowE e1 ++ ", " ++ prettyShowE e2 ++ ")"
    Max -> "max(" ++ prettyShowE e1 ++ ", " ++ prettyShowE e2 ++ ")"
prettyShowE (EUnOp op e) =
  case op of
    Abs    -> "|" ++ prettyShowE e ++ "|"
    Sqrt   -> "sqrt(" ++ prettyShowE e ++ ")"
    Negate -> "(-1 * " ++ prettyShowE e ++ ")"
    Sin    -> "sin(" ++ prettyShowE e ++ ")"
    Cos    -> "cos(" ++ prettyShowE e ++ ")"
prettyShowE (PowI e i) = "(" ++ prettyShowE e ++ " ^ " ++ show i ++ ")"
prettyShowE (Var v) = v
prettyShowE (Lit v) = show (double v)
prettyShowE (Float32 m e) = 
  case m of
    RNE -> "rnd32_ne(" ++ prettyShowE e ++ ")"
    RTP -> "rnd32_tp(" ++ prettyShowE e ++ ")"
    RTN -> "rnd32_tn(" ++ prettyShowE e ++ ")"
    RTZ -> "rnd32_tz(" ++ prettyShowE e ++ ")"
prettyShowE (Float64 m e) = 
  case m of
    RNE -> "rnd64_ne(" ++ prettyShowE e ++ ")"
    RTP -> "rnd64_tp(" ++ prettyShowE e ++ ")"
    RTN -> "rnd64_tn(" ++ prettyShowE e ++ ")"
    RTZ -> "rnd64_tz(" ++ prettyShowE e ++ ")"
prettyShowE (Float m e) = 
  case m of
    RNE -> "rnd_ne(" ++ prettyShowE e ++ ")"
    RTP -> "rnd_tp(" ++ prettyShowE e ++ ")"
    RTN -> "rnd_tn(" ++ prettyShowE e ++ ")"
    RTZ -> "rnd_tz(" ++ prettyShowE e ++ ")"

-- |Show a conjunction of expressions in a human-readable format
-- This is shown as an AND with each disjunction tabbed in with an OR
-- If there is only one term in a disjunction, the expression is shown without an OR 
prettyShowECNF :: [[E]] -> String
prettyShowECNF cnf =
  "AND" ++ concatMap (\d -> "\n\t" ++ prettyShowDisjunction d) cnf
  where
    -- |Show a disjunction of expressions > 0 in a human-readable format
    -- This is shown as an OR with each term tabbed in
    -- If there is only one term, the expression is shown without an OR 
    prettyShowDisjunction :: [E] -> String
    prettyShowDisjunction []  = []
    prettyShowDisjunction [e] = prettyShowE e
    prettyShowDisjunction es  = 
      "OR" ++ concatMap (\e -> "\n\t\t" ++ prettyShowE e ++ " > 0") es

