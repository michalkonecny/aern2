module AERN2.BoxFunMinMax.Expressions.Type where

import MixedTypesNumPrelude
import Data.Ratio
import System.IO.Unsafe (unsafePerformIO)

import AERN2.MP.Ball
import AERN2.AD.Type
import AERN2.BoxFun.Type

import qualified AERN2.Linear.Vector.Type as V
import qualified AERN2.BoxFunMinMax.Type as T
import qualified Prelude as P

import AERN2.BoxFun.TestFunctions (fromListDomain) -- TODO: Move this to Util?

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
data E = EBinOp BinOp E E | EUnOp UnOp E | Lit Rational | Var String | PowI E Integer
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