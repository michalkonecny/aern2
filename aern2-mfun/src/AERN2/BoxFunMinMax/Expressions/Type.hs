{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module AERN2.BoxFunMinMax.Expressions.Type where

import MixedTypesNumPrelude

import qualified Prelude as P

import qualified Data.Map as Map
import Data.List (nub)

import Test.QuickCheck

import Debug.Trace (trace)

data BinOp = Add | Sub | Mul | Div | Min | Max | Pow | Mod
  deriving (Show, P.Eq, P.Ord)
data UnOp  = Sqrt | Negate | Abs | Sin | Cos
  deriving (Show, P.Eq, P.Ord)

data RoundingMode = RNE | RTP | RTN | RTZ | RNA deriving (Show, P.Eq, P.Ord)
-- | The E type represents the inequality: expression :: E >= 0
-- TODO: Add rounding operator with certain epsilon/floating-point type
data E = EBinOp BinOp E E | EUnOp UnOp E | Lit Rational | Var String | PowI E Integer | Float32 RoundingMode E | Float64 RoundingMode E | Float RoundingMode E | Pi | RoundToInteger RoundingMode E
  deriving (Show, P.Eq, P.Ord)

data ESafe = EStrict E | ENonStrict E
  deriving (Show, P.Eq, P.Ord)
data Comp = Gt | Ge | Lt | Le | Eq
  deriving (Show, P.Eq)

data Conn = And | Or | Impl | Equiv
  deriving (Show, P.Eq)

-- TODO: Could make prover work on 'Comp E E' (call it EComp)
-- Other method, add flag to E, whether or not it is strict

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

flipStrictness :: ESafe -> ESafe
flipStrictness (EStrict e)    = ENonStrict e
flipStrictness (ENonStrict e) = EStrict e

-- | Equivalent to E * -1
-- Example: ENonStrict e == e >= 0. negateSafeE (ENonStrict e) == e < 0 == -e > 0 == (EStrict (EUnOp Negate e))
negateSafeE :: ESafe -> ESafe
negateSafeE (EStrict e)     = ENonStrict (EUnOp Negate e)
negateSafeE (ENonStrict e)  = EStrict    (EUnOp Negate e)

extractSafeE :: ESafe -> E
extractSafeE (EStrict e)    = e
extractSafeE (ENonStrict e) = e

fmapESafe :: (E -> E) -> ESafe -> ESafe
fmapESafe f (EStrict e)    = EStrict $ f e
fmapESafe f (ENonStrict e) = ENonStrict $ f e

fToECNF :: F -> [[ESafe]]
fToECNF = fToECNFB False
  where
    fToECNFB :: Bool -> F -> [[ESafe]]
    fToECNFB isNegated (FNot f) = fToECNFB (not isNegated) f
    fToECNFB True (FComp op e1 e2)  = case op of
      Le -> fToECNFB False (FComp Gt e1 e2) -- !(f1 <= f2) -> (f1 > f2)
      Lt -> fToECNFB False (FComp Ge e1 e2)
      Ge -> fToECNFB False (FComp Lt e1 e2)
      Gt -> fToECNFB False (FComp Le e1 e2)
      Eq -> fToECNFB True (FConn And (FComp Ge e1 e2) (FComp Le e1 e2)) -- !(f1 = f2)
    fToECNFB False (FComp op e1 e2) = case op of
      Le -> fToECNFB False (FComp Ge e2 e1) -- f1 <  f2 == f1 - f2 <  0 == -f1 + f2 >= 0
      Lt -> fToECNFB False (FComp Gt e2 e1) -- f1 <= f2 == f1 - f2 <= 0 == -f1 + f2 >  0
      Ge -> [[ENonStrict (EBinOp Sub e1 e2)]]                -- f1 >= f2 == f1 - f2 >= 0 
      Gt -> [[EStrict (EBinOp Sub e1 e2)]]      -- f1 >  f2 == f1 - f2 >  0 
      Eq -> fToECNFB False (FConn And (FComp Ge e1 e2) (FComp Le e1 e2)) -- f1 = f2 == f1 >= f2 /\ f1 <= f2
    fToECNFB True (FConn op f1 f2)  = case op of
      And     -> [d1 ++ d2 | d1 <- fToECNFB True f1, d2 <- fToECNFB True f2] 
      Or      -> fToECNFB True f1 ++ fToECNFB True f2
      Impl    -> fToECNFB False f1 ++ fToECNFB True f2 -- !(!p \/ q) == p /\ !q
      Equiv   -> fToECNFB True (FConn And (FConn Impl f1 f2) (FConn Impl f2 f1))
    fToECNFB False (FConn op f1 f2)  = case op of
      And     -> fToECNFB False f1 ++ fToECNFB False f2 -- [e1 /\ e2 /\ (e3 \/ e4)] ++ [p1 /\ (p2 \/ p3) /\ p4] = [e1 /\ e2 /\ (e3 \/ e4) /\ p1 /\ (p2 \/ p3) /\ p4]
      Or      -> [d1 ++ d2 | d1 <- fToECNFB False f1, d2 <- fToECNFB False f2] -- [e1 /\ e2 /\ (e3 \/ e4)] \/ [p1 /\ (p2 \/ p3) /\ p4] 
      Impl    -> [d1 ++ d2 | d1 <- fToECNFB True f1, d2 <- fToECNFB False f2]
      Equiv   -> fToECNFB False (FConn And (FConn Impl f1 f2) (FConn Impl f2 f1))
    -- fToECNFB isNegated FTrue  _  = error "fToECNFB for FTrue undefined"  $ Lit 1.0
    -- fToECNFB isNegated FFalse _  = error "fToECNFB for FFalse undefined" $ Lit $ -1.0
    fToECNFB True  FTrue   = fToECNFB False FFalse 
    fToECNFB True  FFalse  = fToECNFB False FTrue 
    fToECNFB False FTrue     = [[ENonStrict (Lit 1.0)]]
    fToECNFB False FFalse    = [[ENonStrict (Lit (-1.0))]]

-- | Add bounds for any Float expressions
-- addRoundingBounds :: E -> [[E]]
-- addRoundingBounds (Float e significand) = [[exactExpression - machineEpsilon], [exactExpression + machineEpsilon]]
--   where
--     exactExpression = addRoundingBounds e
--     machineEpsilon = 2^(-23)
-- addRoundingBounds e = e

-- | Various rules to simplify expressions
simplifyE :: E -> E
simplifyE unsimplifiedE = if unsimplifiedE P.== simplifiedE then simplifiedE else simplifyE simplifiedE
  where
    simplifiedE = simplify unsimplifiedE

    simplify (EBinOp Div e (Lit 1.0)) = e
    simplify (EBinOp Div (Lit 0.0) _) = Lit 0.0
    simplify (EBinOp Mul (Lit 0.0) _) = Lit 0.0
    simplify (EBinOp Mul _ (Lit 0.0)) = Lit 0.0
    simplify (EBinOp Mul (Lit 1.0) e) = e
    simplify (EBinOp Mul e (Lit 1.0)) = e
    simplify (EBinOp Add (Lit 0.0) e) = e
    simplify (EBinOp Add e (Lit 0.0)) = e
    simplify (EBinOp Sub e (Lit 0.0)) = e
    simplify (EBinOp Pow _ (Lit 0.0)) = Lit 1.0
    simplify (EBinOp Pow e (Lit 1.0)) = e
    simplify (PowI _e 0)              = Lit 1.0
    simplify (PowI e 1)               = e
    simplify (EUnOp Negate (Lit 0.0)) = Lit 0.0
    simplify (EUnOp Sqrt (Lit 0.0))   = Lit 0.0
    simplify (EUnOp Sqrt (Lit 1.0))   = Lit 1.0
    simplify (EUnOp Abs (Lit v))      = Lit (abs v)
    simplify (EBinOp Min e1 e2)       = if e1 P.== e2 then e1 else EBinOp Min (simplifyE e1) (simplifyE e2)
    simplify (EBinOp Max e1 e2)       = if e1 P.== e2 then e1 else EBinOp Max (simplifyE e1) (simplifyE e2)
    simplify (EBinOp op e1 e2)        = EBinOp op (simplify e1) (simplify e2)
    simplify (EUnOp op e)             = EUnOp op (simplify e)
    simplify e                        = e

simplifyF :: F -> F
-- Simplify Or
simplifyF unsimplifiedF = if unsimplifiedF P.== simplifiedF then simplifiedF else simplifyF simplifiedF
  where
    simplifiedF = simplify unsimplifiedF

    simplify f@(FConn Or (FComp Lt f1l f1r) (FComp Eq f2l f2r)) = if f1l P.== f2l P.&& f1r P.== f2r then FComp Le f1l f1r else f
    simplify (FConn Or (FComp Eq f1l f1r) (FComp Lt f2l f2r))   = simplify $ FConn Or (FComp Lt f2l f2r) (FComp Eq f1l f1r)
    simplify f@(FConn Or (FComp Gt f1l f1r) (FComp Eq f2l f2r)) = if f1l P.== f2l P.&& f1r P.== f2r then FComp Ge f1l f1r else f
    simplify (FConn Or (FComp Eq f1l f1r) (FComp Gt f2l f2r))   = simplify $ FConn Or (FComp Gt f2l f2r) (FComp Eq f1l f1r)

    -- Boolean Rules
    -- Equiv
    simplify (FConn Equiv FTrue FFalse)                         = FFalse
    simplify (FConn Equiv FFalse FTrue)                         = FFalse
    simplify (FConn Equiv FFalse FFalse)                        = FTrue
    simplify (FConn Equiv FTrue FTrue)                          = FTrue
    simplify (FConn Equiv f FTrue)                              = simplify f
    simplify (FConn Equiv FTrue f)                              = simplify f
    simplify (FConn Equiv f FFalse)                             = simplify $ FNot f
    simplify (FConn Equiv FFalse f)                             = simplify $ FNot f
    -- Equiv contradictions and tautologies
    simplify (FConn Equiv f1 fn2@(FNot f2))                     = if f1 P.== f2 then FFalse else FConn Equiv (simplify f1) (simplify fn2)
    simplify (FConn Equiv fn1@(FNot f1) f2)                     = if f1 P.== f2 then FFalse else FConn Equiv (simplify fn1) (simplify f2)
    simplify (FConn Equiv f1 f2)                                = if f1 P.== f2 then FTrue else FConn Equiv (simplify f1) (simplify f2)
    -- And
    simplify (FConn And _ FFalse)                               = FFalse
    simplify (FConn And FFalse _)                               = FFalse
    simplify (FConn And f FTrue)                                = simplify f
    simplify (FConn And FTrue f)                                = simplify f
    -- And contradictions and eliminations
    simplify (FConn And f1 fn2@(FNot f2))                       = if f1 P.== f2 then FFalse else FConn And (simplify f1) (simplify fn2)
    simplify (FConn And fn1@(FNot f1) f2)                       = if f1 P.== f2 then FFalse else FConn And (simplify fn1) (simplify f2)
    simplify (FConn And f1 f2)                                  = if f1 P.== f2 then simplify f1 else FConn And (simplify f1) (simplify f2)
    -- Or
    simplify (FConn Or _ FTrue)                                 = FTrue
    simplify (FConn Or FTrue _)                                 = FTrue
    simplify (FConn Or f FFalse)                                = simplify f
    simplify (FConn Or FFalse f)                                = simplify f
    -- Or tautologies and eliminations

    simplify (FConn Or f1 (FConn And (FNot f2) f3))
      | f1 P.== f2 = simplify $ FConn Or f1 f3
      | otherwise = FConn Or (simplify f1) (FConn And (simplify (FNot f2)) (simplify f3))
    simplify (FConn Or f1 (FConn And f3 (FNot f2)))             = simplify (FConn Or f1 (FConn And (FNot f2) f3)) -- Less efficient but easier to maintain
    simplify (FConn Or (FConn And (FNot f2) f3) f1)             = simplify (FConn Or f1 (FConn And (FNot f2) f3))
    simplify (FConn Or (FConn And f3 (FNot f2)) f1)             = simplify (FConn Or f1 (FConn And (FNot f2) f3))

    simplify (FConn Or f1 (FConn And f2 f3))
      | f1 P.== f2 = simplify $ FConn And f1 f3
      | f1 P.== f3 = simplify $ FConn And f1 f2
      | otherwise = FConn Or (simplify f1) (FConn And (simplify f2) (simplify f3))  
    simplify (FConn Or (FConn And f2 f3) f1)                    = simplify (FConn Or f1 (FConn And f2 f3)) 
    
    simplify (FConn Or f1 fn2@(FNot f2))                        = if f1 P.== f2 then FTrue else FConn Or (simplify f1) (simplify fn2)
    simplify (FConn Or fn1@(FNot f1) f2)                        = if f1 P.== f2 then FTrue else FConn Or (simplify fn1) (simplify f2)
    simplify (FConn Or f1 f2)                                   = if f1 P.== f2 then simplify f1 else FConn Or (simplify f1) (simplify f2)
    -- Impl
    simplify (FConn Impl FFalse _)                              = FTrue
    simplify (FConn Impl _ FTrue)                               = FTrue
    simplify (FConn Impl f FFalse)                              = simplify (FNot f)
    simplify (FConn Impl FTrue f)                               = simplify f
    -- Impl tautologies and eliminations
    simplify (FConn Impl f1 fn2@(FNot f2))                      = if f1 P.== f2 then simplify (FNot f1) else FConn Impl (simplify f1) (simplify fn2)
    simplify (FConn Impl fn1@(FNot f1) f2)                      = if f1 P.== f2 then simplify f1 else FConn Impl (simplify fn1) (simplify f2)
    simplify (FConn Impl f1 f2)                                 = if f1 P.== f2 then FTrue else FConn Impl (simplify f1) (simplify f2)

    -- Comp tautologies and eliminations
    -- Eliminate double not
    simplify (FNot (FNot f))                                    = simplify f
    simplify (FComp op e1 e2)                                   = FComp op (simplifyE e1) (simplifyE e2)
    simplify FTrue                                              = FTrue
    simplify FFalse                                             = FFalse
    simplify (FNot FTrue)                                       = FFalse
    simplify (FNot FFalse)                                      = FTrue
    simplify (FNot f)                                           = FNot (simplify f)
    -- simplifyF FTrue = error "FTrue was not eliminated"
    -- simplifyF FFalse = error "FFalse was not eliminated"

simplifyECNF :: [[E]] -> [[E]]
simplifyECNF = map (map simplifyE)

simplifyESafeCNF :: [[ESafe]] -> [[ESafe]]
simplifyESafeCNF = map (map (fmapESafe simplifyE))

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

prettyShowESafeCNF :: [[ESafe]] -> String
prettyShowESafeCNF cnf = "AND" ++ concatMap (\d -> "\n\t" ++ prettyShowDisjunction d) cnf
  where
    -- |Show a disjunction of expressions > 0 in a human-readable format
    -- This is shown as an OR with each term tabbed in
    -- If there is only one term, the expression is shown without an OR 
    prettyShowDisjunction :: [ESafe] -> String
    prettyShowDisjunction []  = []
    prettyShowDisjunction [e'] = 
      case e' of
        EStrict e -> prettyShowE e ++ " > 0"
        ENonStrict e -> prettyShowE e ++ " >= 0"
    prettyShowDisjunction es  =
      "OR" ++ 
      concatMap 
      (\case
        EStrict e -> "\n\t\t" ++ prettyShowE e ++ " > 0" 
        ENonStrict e -> "\n\t\t" ++ prettyShowE e ++ " >= 0")
      es


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
    Min -> "(min " ++ prettyShowE e1 ++ ", " ++ prettyShowE e2 ++ ")"
    Max -> "(max " ++ prettyShowE e1 ++ ", " ++ prettyShowE e2 ++ ")"
    Mod -> "(mod " ++ prettyShowE e1 ++ ", " ++ prettyShowE e2 ++ ")"
prettyShowE (EUnOp op e) =
  case op of
    Abs    -> "|" ++ prettyShowE e ++ "|"
    Sqrt   -> "(sqrt " ++ prettyShowE e ++ ")"
    Negate -> "(-1 * " ++ prettyShowE e ++ ")"
    Sin    -> "(sin " ++ prettyShowE e ++ ")"
    Cos    -> "(cos " ++ prettyShowE e ++ ")"
prettyShowE (PowI e i) = "(" ++ prettyShowE e ++ " ^ " ++ show i ++ ")"
prettyShowE (Var v) = v
prettyShowE (Lit v) = show (double v)
prettyShowE (Float32 m e) =
  case m of
    RNE -> "(rnd32_ne " ++ prettyShowE e ++ ")"
    RTP -> "(rnd32_tp " ++ prettyShowE e ++ ")"
    RTN -> "(rnd32_tn " ++ prettyShowE e ++ ")"
    RTZ -> "(rnd32_tz " ++ prettyShowE e ++ ")"
    RNA -> "(rnd32_na " ++ prettyShowE e ++ ")"
prettyShowE (Float64 m e) =
  case m of
    RNE -> "(rnd64_ne " ++ prettyShowE e ++ ")"
    RTP -> "(rnd64_tp " ++ prettyShowE e ++ ")"
    RTN -> "(rnd64_tn " ++ prettyShowE e ++ ")"
    RTZ -> "(rnd64_tz " ++ prettyShowE e ++ ")"
    RNA -> "(rnd64_na " ++ prettyShowE e ++ ")"
prettyShowE (Float m e) =
  case m of
    RNE -> "(rnd_ne " ++ prettyShowE e ++ ")"
    RTP -> "(rnd_tp " ++ prettyShowE e ++ ")"
    RTN -> "(rnd_tn " ++ prettyShowE e ++ ")"
    RTZ -> "(rnd_tz " ++ prettyShowE e ++ ")"
    RNA -> "(rnd_na " ++ prettyShowE e ++ ")"
prettyShowE Pi = "Pi"
prettyShowE (RoundToInteger m e) = 
  case m of
    RNE -> "(rndToInt_ne " ++ prettyShowE e ++ ")"
    RTP -> "(rndToInt_tp " ++ prettyShowE e ++ ")"
    RTN -> "(rndToInt_tn " ++ prettyShowE e ++ ")"
    RTZ -> "(rndToInt_tz " ++ prettyShowE e ++ ")"
    RNA -> "(rndToInt_ta " ++ prettyShowE e ++ ")"

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

prettyShowF :: F -> Integer -> String
prettyShowF (FComp op e1 e2) numTabs = "\n" ++ concat (replicate numTabs "\t") ++ prettyShowE e1 ++ " " ++ prettyShowComp op ++ " " ++ prettyShowE e2
prettyShowF (FConn op f1 f2) numTabs = "\n" ++ concat (replicate numTabs "\t") ++ prettyShowConn op ++ prettyShowF f1 (numTabs + 1) ++ prettyShowF f2 (numTabs + 1)
prettyShowF (FNot f)         numTabs = "\n" ++ concat (replicate numTabs "\t") ++ "NOT" ++ prettyShowF f (numTabs + 1)
prettyShowF FTrue            numTabs = "\n" ++ concat (replicate numTabs "\t") ++ "True"
prettyShowF FFalse           numTabs = "\n" ++ concat (replicate numTabs "\t") ++ "False"

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
    findAllVars Pi               = []
    findAllVars (Var v)          = [v]
    findAllVars (EUnOp _ e)      = findAllVars e
    findAllVars (EBinOp _ e1 e2) = findAllVars e1 ++ findAllVars e2
    findAllVars (PowI e _)       = findAllVars e
    findAllVars (Float32 _ e)    = findAllVars e
    findAllVars (Float64 _ e)    = findAllVars e
    findAllVars (Float _ e)      = findAllVars e
    findAllVars (RoundToInteger _ e) = findAllVars e

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

hasFloatE :: E -> Bool
hasFloatE (Float _ _)      = True
hasFloatE (Float32 _ _)    = True
hasFloatE (Float64 _ _)    = True
hasFloatE (EBinOp _ e1 e2) = hasFloatE e1 || hasFloatE e2
hasFloatE (EUnOp _ e)      = hasFloatE e
hasFloatE (PowI e _)       = hasFloatE e
hasFloatE (Lit _)          = False
hasFloatE (Var _)          = False
hasFloatE Pi               = False
hasFloatE (RoundToInteger _ e) = hasFloatE e

hasFloatF :: F -> Bool
hasFloatF (FConn _ f1 f2) = hasFloatF f1 || hasFloatF f2
hasFloatF (FComp _ e1 e2) = hasFloatE e1 || hasFloatE e2
hasFloatF (FNot f)        = hasFloatF f
hasFloatF FTrue           = False
hasFloatF FFalse          = False

substituteVarE :: E -> String -> Rational -> E
substituteVarE (Var x) varToSubstitue valToSubstitute = if x == varToSubstitue then Lit valToSubstitute else Var x
substituteVarE (RoundToInteger m e) var val = RoundToInteger m (substituteVarE e var val)
substituteVarE Pi _ _ = Pi
substituteVarE l@(Lit _) _ _ = l
substituteVarE (EBinOp op e1 e2) var val = EBinOp op (substituteVarE e1 var val) (substituteVarE e2 var val)
substituteVarE (EUnOp op e) var val = EUnOp op (substituteVarE e var val)
substituteVarE (PowI e i) var val = PowI (substituteVarE e var val) i
substituteVarE (Float m e) var val = Float m (substituteVarE e var val)
substituteVarE (Float32 m e) var val = Float32 m (substituteVarE e var val)
substituteVarE (Float64 m e) var val = Float64 m (substituteVarE e var val)

substituteVarF :: F -> String -> Rational -> F
substituteVarF (FConn op f1 f2) var val = FConn op (substituteVarF f1 var val) (substituteVarF f2 var val)
substituteVarF (FComp op e1 e2) var val = FComp op (substituteVarE e1 var val) (substituteVarE e2 var val)
substituteVarF (FNot f)         var val = FNot (substituteVarF f var val)
substituteVarF FTrue  _ _ = FTrue
substituteVarF FFalse _ _ = FFalse