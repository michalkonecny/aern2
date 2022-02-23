{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module AERN2.BoxFunMinMax.Expressions.Parsers.DRealSmt where

import MixedTypesNumPrelude
import AERN2.BoxFunMinMax.Expressions.Type
import qualified AERN2.BoxFunMinMax.Expressions.Parsers.Lisp.DataTypes as LD
import AERN2.BoxFunMinMax.VarMap

-- | Parser for SMT files produced by the dReal Translator.
-- 
-- The DReal SMT file will have the first line declaring the SMT logic, which we ignore.
-- The next few lines declare variables (if any)
-- A variable is declared in the following format
-- (declare-fun varName () varType) where varType can be Int or Real
-- (assert (<= lowerBound varName))
-- (assert (<= varName upperBound))
-- 
-- Both lowerBound and upperBound will be rationals, though can be safely converted to Integer for Int vars
-- 
-- Once the declare-fun lines end, we have an assert which contains the entire VC as an argument
-- The rest of the file can be ignored (it will be (check-sat), (get-model), and (exit))
parseDRealSmtToF :: [LD.Expression] -> (Maybe F, TypedVarMap)
parseDRealSmtToF parsedExpressions =
  (parseDRealVC (head parsedVC), typedVarMap)
  where
   (_smtLogic : varsWithParsedVC) = parsedExpressions
   (parsedVC, typedVarMap) = parseDRealVariables varsWithParsedVC

parseDRealVC :: LD.Expression -> Maybe F
parseDRealVC (LD.Application (LD.Variable "assert") [parsedVC]) = termDRealToF parsedVC
parseDRealVC _ = Nothing
-- | Parses variables from a parsed file.
-- First item must be a variable declaration
-- Continues parsing variables until it reaches a non-variable assertion
-- Returns a TypedVarMap and the rest of the parsed file.
parseDRealVariables :: [LD.Expression] -> ([LD.Expression], TypedVarMap)
parseDRealVariables 
  (
    LD.Application (LD.Variable "declare-fun") [LD.Variable varName, LD.Null, LD.Variable varType] :
    LD.Application (LD.Variable "assert") [LD.Application (LD.Variable "<=") [LD.Application (LD.Variable "/") [LD.Number lowerNumerator, LD.Number lowerDenominator], LD.Variable _]] :
    LD.Application (LD.Variable "assert") [LD.Application (LD.Variable "<=") [LD.Variable _, LD.Application (LD.Variable "/") [LD.Number upperNumerator, LD.Number upperDenominator]]] :
    parsedExpressions
  ) =
    let
      (remainingExpressions, parsedVarMap) = parseDRealVariables parsedExpressions
      parsedVarType = 
        case varType of
          "Real" -> Real
          "Int" -> Integer
          _ -> error "Unrecognized varType in given dReal SMT file"
    in
      (remainingExpressions, TypedVar (varName, (lowerNumerator / lowerDenominator, upperNumerator / upperDenominator)) parsedVarType : parsedVarMap)
parseDRealVariables parsedExpressions = (parsedExpressions, [])

termDRealToF :: LD.Expression -> Maybe F
-- FConn
termDRealToF (LD.Application (LD.Variable "and") [p1, p2]) = FConn And <$> termDRealToF p1 <*> termDRealToF p2
-- termDRealToF (LD.Application (LD.Variable "or") [LD.Application (LD.Variable "not") [p1], p2])  = FConn Impl <$> termDRealToF p1 <*> termDRealToF p2
termDRealToF (LD.Application (LD.Variable "or") [p1, p2])  = FConn Or <$> termDRealToF p1 <*> termDRealToF p2 --TODO: parse OR (NOT p1) p2 as impl? possible but is there any benefit?
-- Special case for =
termDRealToF (LD.Application (LD.Variable "=") [p1, p2])   = 
  case (termDRealToF p1, termDRealToF p2) of
    (Just f1, Just f2) -> Just $ FConn Equiv f1 f2
    (Nothing, Nothing) ->
      case (termDRealToE p1, termDRealToE p2) of
        (Just e1, Just e2) -> Just $ FComp Eq e1 e2
        (_, _) -> Nothing
    (_, _) -> Nothing
-- FComp
termDRealToF (LD.Application (LD.Variable ">=") [p1, p2])  = FComp Ge <$> termDRealToE p1 <*> termDRealToE p2
termDRealToF (LD.Application (LD.Variable ">") [p1, p2])   = FComp Gt <$> termDRealToE p1 <*> termDRealToE p2
termDRealToF (LD.Application (LD.Variable "<=") [p1, p2])  = FComp Le <$> termDRealToE p1 <*> termDRealToE p2
termDRealToF (LD.Application (LD.Variable "<") [p1, p2])   = FComp Lt <$> termDRealToE p1 <*> termDRealToE p2
termDRealToF (LD.Application (LD.Variable "not") [p])      = FNot <$> termDRealToF p
-- Bools
termDRealToF (LD.Variable "true")  = Just FTrue
termDRealToF (LD.Variable "false") = Just FFalse
-- Unknown
termDRealToF _ = Nothing

termDRealToE :: LD.Expression -> Maybe E
-- Need to deal with Pi first
termDRealToE (LD.Application (LD.Variable "*") [LD.Number 4, LD.Application (LD.Variable "atan") [LD.Number 1]]) = Just $ Pi
-- EBinOp
termDRealToE (LD.Application (LD.Variable "+") [p1, p2])   = EBinOp Add <$> termDRealToE p1 <*> termDRealToE p2
termDRealToE (LD.Application (LD.Variable "-") [p1, p2])   = EBinOp Sub <$> termDRealToE p1 <*> termDRealToE p2
termDRealToE (LD.Application (LD.Variable "*") [p1, p2])   = EBinOp Mul <$> termDRealToE p1 <*> termDRealToE p2
termDRealToE (LD.Application (LD.Variable "/") [p1, p2])   = EBinOp Div <$> termDRealToE p1 <*> termDRealToE p2
termDRealToE (LD.Application (LD.Variable "min") [p1, p2]) = EBinOp Min <$> termDRealToE p1 <*> termDRealToE p2
termDRealToE (LD.Application (LD.Variable "max") [p1, p2]) = EBinOp Max <$> termDRealToE p1 <*> termDRealToE p2
termDRealToE (LD.Application (LD.Variable "^") [p1, p2])   = EBinOp Pow <$> termDRealToE p1 <*> termDRealToE p2
termDRealToE (LD.Application (LD.Variable "mod") [p1, p2]) = EBinOp Mod <$> termDRealToE p1 <*> termDRealToE p2
-- EUnOp
termDRealToE (LD.Application (LD.Variable "sqrt") [p]) = EUnOp Sqrt <$> termDRealToE p
-- TODO: understand negate?
termDRealToE (LD.Application (LD.Variable "abs") [p])  = EUnOp Abs <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "sin") [p])  = EUnOp Sin <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "cos") [p])  = EUnOp Cos <$> termDRealToE p
-- TODO: understand PowI? probably easier to add simplification rule
-- Variables
termDRealToE (LD.Variable v) = Just $ Var v
-- Literals
termDRealToE (LD.Number n) = Just $ Lit n
-- RoundToInt
termDRealToE (LD.Application (LD.Variable "to_int_rne") [p])  = RoundToInteger RNE <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "to_int_rtp") [p])  = RoundToInteger RTP <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "to_int_rtn") [p])  = RoundToInteger RTN <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "to_int_rtz") [p])  = RoundToInteger RTZ <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "to_int_rna") [p])  = RoundToInteger RNA <$> termDRealToE p
-- Float32
termDRealToE (LD.Application (LD.Variable "float32_rne") [p])  = Float32 RNE <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float32_rtp") [p])  = Float32 RTP <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float32_rtn") [p])  = Float32 RTN <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float32_rtz") [p])  = Float32 RTZ <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float32_rna") [p])  = Float32 RNA <$> termDRealToE p
-- Float64
termDRealToE (LD.Application (LD.Variable "float64_rne") [p])  = Float64 RNE <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float64_rtp") [p])  = Float64 RTP <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float64_rtn") [p])  = Float64 RTN <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float64_rtz") [p])  = Float64 RTZ <$> termDRealToE p
termDRealToE (LD.Application (LD.Variable "float64_rna") [p])  = Float64 RNA <$> termDRealToE p
-- Unknown
termDRealToE _ = Nothing
