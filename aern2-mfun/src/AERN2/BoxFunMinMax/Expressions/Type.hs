module AERN2.BoxFunMinMax.Expressions.Type where

import MixedTypesNumPrelude

import qualified Prelude as P

import qualified Data.Map as Map
import Data.List (nub)

import Test.QuickCheck

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

data Conn = And | Or | Impl | Equiv
  deriving (Show, P.Eq)

-- | The F type is used to specify comparisons between E types
-- and logical connectives between F types
data F = FComp Comp E E | FConn Conn F F | FNot F | FTrue | FFalse
  deriving (Show, P.Eq)

newtype Name = Name String deriving Show

instance Arbitrary Name where
  arbitrary = 
    oneof 
    [
      return (Name "a"),
      return (Name "b"),
      return (Name "c"),
      return (Name "d"),
      return (Name "e"),
      return (Name "f"),
      return (Name "g"),
      return (Name "h"),
      return (Name "i"),
      return (Name "j"),
      return (Name "k"),
      return (Name "l")
    ]

instance Arbitrary UnOp where
  arbitrary =
    oneof [return Negate, return Abs, return Sin, return Cos]

instance Arbitrary BinOp where
  arbitrary =
    oneof [return Add, return Sub, return Mul, return Div, return Min, return Max]

instance Arbitrary RoundingMode where
  arbitrary =
    oneof [return RNE, return RTP, return RTN, return RTN]
instance Arbitrary E where
  arbitrary = sized eGenerator
    where
      varName :: Gen Name
      varName = arbitrary

      eGenerator :: Int -> Gen E
      eGenerator n | n>0 =
        oneof 
        [
          Lit     <$> fmap toRational (arbitrary :: Gen Integer), 
          Var     <$> show <$> varName,
          EUnOp   <$> arbitrary <*> subE,
          EUnOp Sqrt . Lit      <$> fmap getPositive (arbitrary :: Gen (Positive Rational)),
          EBinOp  <$> arbitrary <*> subE <*> subE
          -- PowI    <$> subE <*> fmap getPositive (arbitrary :: Gen (Positive Integer)) -- We do not allow Floats here
        ]
        where
          subE = eGenerator (int (floor (n / 20)))
          sqrtG x = EUnOp Sqrt (Lit x)
      eGenerator _        = oneof [Lit <$> (fmap toRational (arbitrary :: Gen Integer)), Var <$> show <$> varName]
          -- subE = eGenerator (pred n)

-- data Comp = Gt | Ge | Lt | Le | Eq
--   deriving (Show, P.Eq)

-- data Conn = And | Or | Impl | Equiv
--   deriving (Show, P.Eq)

-- -- | The F type is used to specify comparisons between E types
-- -- and logical connectives between F types
-- data F = FComp Comp E E | FConn Conn F F | FNot F | FTrue | FFalse
--   deriving (Show, P.Eq)

instance Arbitrary Comp where
  arbitrary = oneof [return Gt, return Ge, return Lt, return Le, return Eq]

instance Arbitrary Conn where
  arbitrary = oneof [return And, return Or, return Impl, return Equiv]

instance Arbitrary F where
  arbitrary = sized fGenerator
    where
      fGenerator :: Int -> Gen F
      fGenerator 0 = oneof [FComp <$> arbitrary <*> arbitrary <*> arbitrary]
      fGenerator n =
        oneof
        [
          FComp <$> arbitrary <*> arbitrary <*> arbitrary,
          FConn <$> arbitrary <*> subF <*> subF,
          FNot  <$> subF
        ]
        where
          subF = fGenerator (int (floor (n / 20)))
-- Note, does not generate FTrue, FFalse

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
fToE (FConn op f1 f2)   = case op of
  And ->
    EBinOp Min (fToE f1) (fToE f2)
  Or ->
    EBinOp Max (fToE f1) (fToE f2)
  Impl -> 
    EBinOp Max (EUnOp Negate (fToE f1)) (fToE f2) -- !f1 \/ f2 = max(!f1, f2)
  Equiv -> fToE $ FComp Eq (fToE f1) (fToE f2)
fToE (FNot f) = EUnOp Negate (fToE f)
fToE FTrue    = error "fToE for FTrue undefined"  $ Lit 1.0
fToE FFalse   = error "fToE for FFalse undefined" $ Lit $ -1.0


fToECNF :: F -> [[E]]
fToECNF = fToECNFB False 
  where
    fToECNFB :: Bool -> F -> [[E]]
    fToECNFB isNegated (FNot f) = fToECNFB (not isNegated) f
    fToECNFB True (FComp op e1 e2)   = case op of
      Le -> fToECNFB False (FComp Gt e1 e2) -- !(f1 <= f2) -> (f1 > f2)
      Lt -> fToECNFB False (FComp Ge e1 e2)
      Ge -> fToECNFB False (FComp Lt e1 e2)
      Gt -> fToECNFB False (FComp Le e1 e2)
      Eq -> fToECNFB True $ FConn And (FComp Ge e1 e2) (FComp Le e1 e2) -- !(f1 = f2)
    fToECNFB False (FComp op e1 e2)   = case op of
      Le ->
        [[EBinOp Add (EUnOp Negate e1) e2]] -- f1 <= f2 == f1 - f2 <= 0 == -f1 + f2 >= 0
      Lt ->
        [[EBinOp Add (EUnOp Negate e1) e2]]
      Ge ->
        [[EBinOp Sub e1 e2]] -- f1 >= f2 == f1 - f2 >= 0 == 
      Gt ->
        [[EBinOp Sub e1 e2]]
      Eq -> fToECNFB False $ FConn And (FComp Ge e1 e2) (FComp Le e1 e2) -- f1 = f2 == f1 >= f2 /\ f1 <= f2
    fToECNFB True (FConn op f1 f2)   = case op of
      And     -> [d1 ++ d2 | d1 <- fToECNFB True f1, d2 <- fToECNFB True f2] 
      Or      -> fToECNFB True f1 ++ fToECNFB True f2
      Impl    -> fToECNFB False f1 ++ fToECNFB True f2 -- !(!p \/ q) == p /\ !q
      Equiv   -> fToECNFB True $ FConn And (FConn Impl f1 f2) (FConn Impl f2 f1)
    fToECNFB False (FConn op f1 f2)   = case op of
      And     -> fToECNFB False f1 ++ fToECNFB False f2 -- [e1 /\ e2 /\ (e3 \/ e4)] ++ [p1 /\ (p2 \/ p3) /\ p4] = [e1 /\ e2 /\ (e3 \/ e4) /\ p1 /\ (p2 \/ p3) /\ p4]
      Or      -> [d1 ++ d2 | d1 <- fToECNFB False f1, d2 <- fToECNFB False f2] -- [e1 /\ e2 /\ (e3 \/ e4)] \/ [p1 /\ (p2 \/ p3) /\ p4] 
      Impl    -> [d1 ++ d2 | d1 <- fToECNFB True f1, d2 <- fToECNFB False f2]
      Equiv   -> fToECNFB False $ FConn And (FConn Impl f1 f2) (FConn Impl f2 f1)
    fToECNFB isNegated FTrue    = error "fToECNFB for FTrue undefined"  $ Lit 1.0
    fToECNFB isNegated FFalse   = error "fToECNFB for FFalse undefined" $ Lit $ -1.0

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
simplifyE (PowI _e 0)              = Lit 1.0
simplifyE (PowI e 1)               = e
simplifyE (EUnOp Negate (Lit 0.0)) = Lit 0.0
simplifyE (EUnOp Sqrt (Lit 0.0))   = Lit 0.0
simplifyE (EUnOp Sqrt (Lit 1.0))   = Lit 1.0
simplifyE (EBinOp op e1 e2)        = EBinOp op (simplifyE e1) (simplifyE e2)
simplifyE (EUnOp op e)             = EUnOp op (simplifyE e)
simplifyE e                        = e

simplifyF :: F -> F
-- Simplify Or
simplifyF f@(FConn Or (FComp Lt f1l f1r) (FComp Eq f2l f2r)) = if f1l P.== f2l P.&& f1r P.== f2r then FComp Le f1l f1r else f
simplifyF (FConn Or (FComp Eq f1l f1r) (FComp Lt f2l f2r))   = simplifyF $ FConn Or (FComp Lt f2l f2r) (FComp Eq f1l f1r)
simplifyF f@(FConn Or (FComp Gt f1l f1r) (FComp Eq f2l f2r)) = if f1l P.== f2l P.&& f1r P.== f2r then FComp Ge f1l f1r else f
simplifyF (FConn Or (FComp Eq f1l f1r) (FComp Gt f2l f2r))   = simplifyF $ FConn Or (FComp Gt f2l f2r) (FComp Eq f1l f1r)

-- Boolean Rules
-- Equiv
simplifyF (FConn Equiv FTrue FFalse)                         = FFalse
simplifyF (FConn Equiv FFalse FTrue)                         = FFalse
simplifyF (FConn Equiv FFalse FFalse)                        = FTrue
simplifyF (FConn Equiv FTrue FTrue)                          = FTrue
simplifyF (FConn Equiv f FTrue)                              = simplifyF f
simplifyF (FConn Equiv FTrue f)                              = simplifyF f
simplifyF (FConn Equiv f FFalse)                             = simplifyF $ FNot f
simplifyF (FConn Equiv FFalse f)                             = simplifyF $ FNot f
-- And
simplifyF (FConn And _ FFalse)                               = FFalse
simplifyF (FConn And FFalse _)                               = FFalse
simplifyF (FConn And f FTrue)                                = simplifyF f
simplifyF (FConn And FTrue f)                                = simplifyF f
-- Or
simplifyF (FConn Or _ FTrue)                                 = FTrue
simplifyF (FConn Or FTrue _)                                 = FTrue
simplifyF (FConn Or f FFalse)                                = simplifyF f
simplifyF (FConn Or FFalse f)                                = simplifyF f
-- Impl
simplifyF (FConn Impl FFalse _)                              = FTrue
simplifyF (FConn Impl _ FTrue)                               = FTrue
simplifyF (FConn Impl f FFalse)                              = simplifyF (FNot f)
simplifyF (FConn Impl FTrue f)                               = simplifyF f

-- Eliminate double not
simplifyF (FNot (FNot f))                                    = simplifyF f

simplifyF (FConn op f1 f2)                                   = FConn op (simplifyF f1) (simplifyF f2)
simplifyF (FComp op e1 e2)                                   = FComp op (simplifyE e1) (simplifyE e2)
simplifyF FTrue                                              = FTrue
simplifyF FFalse                                             = FFalse
simplifyF (FNot f)                                           = FNot (simplifyF f)
-- simplifyF FTrue = error "FTrue was not eliminated"
-- simplifyF FFalse = error "FFalse was not eliminated"

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

prettyShowF :: F -> String
prettyShowF (FComp op e1 e2) = "(" ++ prettyShowE e1 ++ ") " ++ prettyShowComp op ++ " (" ++ prettyShowE e2 ++ ")"
prettyShowF (FConn op f1 f2) = prettyShowConn op ++ " ((" ++ prettyShowF f1 ++ "), (" ++ prettyShowF f2 ++ "))"
prettyShowF (FNot f)         = "Not(" ++ prettyShowF f ++ ")"
prettyShowF FTrue            = "True"
prettyShowF FFalse           = "False"

prettyShowComp :: Comp -> String
prettyShowComp Gt = ">"
prettyShowComp Ge = ">="
prettyShowComp Lt = "<"
prettyShowComp Le = "<="
prettyShowComp Eq = "=="

prettyShowConn :: Conn -> String
prettyShowConn And   = "AND"
prettyShowConn Or    = "OR"
prettyShowConn Impl  = "IMPL"
prettyShowConn Equiv = "EQUIV"

-- |Extract all variables in an expression
-- Will not return duplicationes
extractVariablesE :: E -> [String]
extractVariablesE = nub . findAllVars
  where
    findAllVars (Lit _)          = []
    findAllVars (Var v)          = [v]
    findAllVars (EUnOp _ e)      = findAllVars e
    findAllVars (EBinOp _ e1 e2) = findAllVars e1 ++ findAllVars e2
    findAllVars (PowI e _)       = findAllVars e
    findAllVars (Float32 _ e)    = findAllVars e
    findAllVars (Float64 _ e)    = findAllVars e
    findAllVars (Float _ e)      = findAllVars e

-- |Extract all variables in an expression
-- Will not return duplicationes
extractVariablesF :: F -> [String]
extractVariablesF = nub . findAllVars
  where
    findAllVars (FComp _ e1 e2) = extractVariablesE e1 ++ extractVariablesE e2
    findAllVars (FConn _ f1 f2) = findAllVars f1 ++ findAllVars f2
    findAllVars (FNot f)        = findAllVars f
    findAllVars FTrue           = []
    findAllVars FFalse          = []

extractVariablesECNF :: [[E]] -> [String]
extractVariablesECNF = nub . concatMap (concatMap extractVariablesE) 
