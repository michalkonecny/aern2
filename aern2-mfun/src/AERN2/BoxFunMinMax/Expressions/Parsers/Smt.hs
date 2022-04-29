{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module AERN2.BoxFunMinMax.Expressions.Parsers.Smt where

import MixedTypesNumPrelude hiding (unzip)
import qualified Prelude as P
import System.IO.Unsafe

import AERN2.BoxFunMinMax.Expressions.Type

import qualified AERN2.BoxFunMinMax.Expressions.Parsers.Lisp.Parser as LP
import qualified AERN2.BoxFunMinMax.Expressions.Parsers.Lisp.DataTypes as LD

import Data.Char (digitToInt)

import Data.Word
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Maybe (mapMaybe)

import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.DeriveBounds
import AERN2.BoxFunMinMax.Expressions.EliminateFloats
import AERN2.BoxFunMinMax.Expressions.Eliminator (minMaxAbsEliminatorECNF, minMaxAbsEliminator)
import Data.List (nub, sort, isPrefixOf, sortBy, partition, foldl')
import Data.List.NonEmpty (unzip)
import Control.Arrow ((&&&))
import Debug.Trace
import AERN2.BoxFunMinMax.Expressions.Translators.DReal (formulaAndVarMapToDReal)
import Text.Regex.TDFA ( (=~) )
import Data.Ratio
import AERN2.BoxFunMinMax.Expressions.DeriveBounds
import qualified Data.Map as M
import AERN2.MP (endpoints, mpBallP, prec)

data ParsingMode = Why3 | CNF
parser :: String -> [LD.Expression]
parser = LP.analyzeExpressionSequence . LP.parseSequence . LP.tokenize

parseSMT2 :: FilePath -> IO [LD.Expression]
parseSMT2 filePath = fmap parser $ P.readFile filePath

-- |Find assertions in a parsed expression
-- Assertions are Application types with the operator being a Variable equal to "assert"
-- Assertions only have one 'operands'
findAssertions :: [LD.Expression] -> [LD.Expression]
findAssertions [] = []
findAssertions ((LD.Application (LD.Variable "assert") [operands]) : expressions) = operands : findAssertions expressions
findAssertions (_ : expressions) = findAssertions expressions

findFunctionInputsAndOutputs :: [LD.Expression] -> [(String, ([String], String))]
findFunctionInputsAndOutputs [] = []
findFunctionInputsAndOutputs ((LD.Application (LD.Variable "declare-fun") [LD.Variable fName, fInputsAsExpressions, LD.Variable fOutputs]) : expressions) =
  (fName, (expressionInputsAsString fInputsAsExpressions, fOutputs)) : findFunctionInputsAndOutputs expressions
  where
    expressionInputsAsString :: LD.Expression -> [String]
    expressionInputsAsString (LD.Application (LD.Variable inputTypeAsString) remainingInputsAsExpression) = inputTypeAsString : concatMap expressionInputsAsString remainingInputsAsExpression
    expressionInputsAsString (LD.Variable inputTypeAsString) = [inputTypeAsString]
    expressionInputsAsString _ = []
findFunctionInputsAndOutputs (_ : expressions) = findFunctionInputsAndOutputs expressions

-- |Find function declarations in a parsed expression
-- Function declarations are Application types with the operator being a Variable equal to "declare-fun"
-- Function declarations contain 3 operands
--   - Operand 1 is the name of the function
--   - Operand 2 is an Application type which can be thought of as the parameters of the functions
--     If the function has no paramters, this operand is LD.Null 
--   - Operand 3 is the type of the function
findDeclarations :: [LD.Expression] -> [LD.Expression]
findDeclarations [] = []
findDeclarations (declaration@(LD.Application (LD.Variable "declare-fun") _) : expressions) = declaration : findDeclarations expressions
findDeclarations (_ : expressions) = findDeclarations expressions

findVariables :: [LD.Expression] -> [(String, String)]
findVariables [] = []
findVariables (LD.Application (LD.Variable "declare-const") [LD.Variable varName, LD.Variable varType] : expressions)
  = (varName, varType) : findVariables expressions
findVariables (LD.Application (LD.Variable "declare-fun") [LD.Variable varName, LD.Null, LD.Variable varType] : expressions)
  = (varName, varType) : findVariables expressions
findVariables (_ : expressions) = findVariables expressions

findIntegerVariables :: [(String, String)] -> [(String, VarType)]
findIntegerVariables []           = []
findIntegerVariables ((v,t) : vs) =
  if "Int" `isPrefixOf` t || "int" `isPrefixOf` t
    then (v, Integer) : findIntegerVariables vs
    else findIntegerVariables vs

-- |Finds goals in assertion operands
-- Goals are S-Expressions with a top level 'not'
findGoalsInAssertions :: [LD.Expression] -> [LD.Expression]
findGoalsInAssertions [] = []
findGoalsInAssertions ((LD.Application (LD.Variable operator) operands) : assertions) =
  if operator == "not"
    then head operands : findGoalsInAssertions assertions -- Take head of operands since not has only one operand
    else findGoalsInAssertions assertions
findGoalsInAssertions (_ : assertions) = findGoalsInAssertions assertions

-- |Takes the last element from a list of assertions
-- We assume that the last element is the goal
takeGoalFromAssertions :: [LD.Expression] -> (LD.Expression, [LD.Expression])
takeGoalFromAssertions asserts = (goal, assertsWithoutGoal)
  where
    numberOfAssertions = length asserts
    goal = last asserts -- FIXME: Unsafe. If asserts is empty, this will fail
    assertsWithoutGoal = take (numberOfAssertions - 1) asserts

-- safelyTypeExpression :: String -> [(String, ([String], String))] -> E -> E
-- safelyTypeExpression smtFunction functionsWithInputsAndOutputs exactExpression =
--   case lookup smtFunction functionsWithInputsAndOutputs of
--     Just (inputs, output)

termToF :: LD.Expression -> [(String, ([String], String))] -> Maybe F
termToF (LD.Application (LD.Variable operator) [op]) functionsWithInputsAndOutputs = -- Single param operators
  case termToE op functionsWithInputsAndOutputs of -- Ops with E params
    Just e ->
      case operator of
        "fp.isFinite32" ->
          let maxFloat = (2.0 - (1/(2^23))) * (2^127)
              minFloat = negate maxFloat
          in
            Just $ FConn And (FComp Le (Lit minFloat) e)  (FComp Le e (Lit maxFloat))
        "fp.isFinite64" ->
          let maxFloat = (2.0 - (1/(2^52))) * (2^1023)
              minFloat = negate maxFloat
          in
            Just $ FConn And (FComp Le (Lit minFloat) e)  (FComp Le e (Lit maxFloat))
        _ -> Nothing
    Nothing ->
      case termToF op functionsWithInputsAndOutputs of
        Just f ->
          case operator of
            "not" -> Just $ FNot f
            _ -> Nothing
        _ -> Nothing
termToF (LD.Application (LD.Variable operator) [op1, op2]) functionsWithInputsAndOutputs = -- Two param operations
  case (termToE op1 functionsWithInputsAndOutputs, termToE op2 functionsWithInputsAndOutputs) of
    (Just e1, Just e2) ->
      case operator of
        n
          | n `elem` [">=", "fp.geq", "oge", "oge__logic"] -> Just $ FComp Ge e1 e2
          | n `elem` [">",  "fp.gt", "ogt", "ogt__logic"]  -> Just $ FComp Gt e1 e2
          | n `elem` ["<=", "fp.leq", "ole", "ole__logic"] -> Just $ FComp Le e1 e2
          | n `elem` ["<",  "fp.lt", "olt", "olt__logic"]  -> Just $ FComp Lt e1 e2
          | n `elem` ["=",  "fp.eq"]  -> Just $ FComp Eq e1 e2
          | "bool_eq" `isPrefixOf` n ->  Just $ FComp Eq e1 e2
          | "user_eq" `isPrefixOf` n ->  Just $ FComp Eq e1 e2
        _ -> Nothing
    (_, _) ->
      case (termToF op1 functionsWithInputsAndOutputs, termToF op2 functionsWithInputsAndOutputs) of
        (Just f1, Just f2) ->
          case operator of
            "and" -> Just $ FConn And f1 f2
            "or"  -> Just $ FConn Or f1 f2
            "=>"  -> Just $ FConn Impl f1 f2
            "="   -> Just $ FConn And (FConn Impl f1 f2) (FConn Impl f2 f1)
            n
              | "bool_eq" `isPrefixOf` n ->  Just $ FConn And (FConn Impl f1 f2) (FConn Impl f2 f1)
              | "user_eq" `isPrefixOf` n ->  Just $ FConn And (FConn Impl f1 f2) (FConn Impl f2 f1)
            _ -> Nothing
        -- Parse ite where it is used as an expression
        (_, _) ->
          case (op1, termToE op2 functionsWithInputsAndOutputs) of
            (LD.Application (LD.Variable "ite") [cond, thenTerm, elseTerm], Just e2) ->
              case (termToF cond functionsWithInputsAndOutputs, termToE thenTerm functionsWithInputsAndOutputs, termToE elseTerm functionsWithInputsAndOutputs) of
                (Just condF, Just thenTermE, Just elseTermE) ->
                  case operator of
                    n
                      | n `elem` [">=", "fp.geq", "oge", "oge__logic"]  -> Just $ FConn And (FConn Impl condF (FComp Ge thenTermE e2))
                                                                                            (FConn Impl (FNot condF) (FComp Ge elseTermE e2))
                      | n `elem` [">",  "fp.gt", "ogt", "ogt__logic"]   -> Just $ FConn And (FConn Impl condF (FComp Gt thenTermE e2))
                                                                                            (FConn Impl (FNot condF) (FComp Gt elseTermE e2))
                      | n `elem` ["<=", "fp.leq", "ole", "ole__logic"]  -> Just $ FConn And (FConn Impl condF (FComp Le thenTermE e2))
                                                                                            (FConn Impl (FNot condF) (FComp Le elseTermE e2))
                      | n `elem` ["<",  "fp.lt", "olt", "olt__logic"]   -> Just $ FConn And (FConn Impl condF (FComp Lt thenTermE e2))
                                                                                            (FConn Impl (FNot condF) (FComp Lt elseTermE e2))
                      | n `elem` ["=",  "fp.eq"]                        -> Just $ FConn And (FConn Impl condF (FComp Eq thenTermE e2))
                                                                                            (FConn Impl (FNot condF) (FComp Eq elseTermE e2))
                      | "bool_eq" `isPrefixOf` n                        ->  Just $ FConn And (FConn Impl condF (FComp Eq thenTermE e2))
                                                                                            (FConn Impl (FNot condF) (FComp Eq elseTermE e2))
                      | "user_eq" `isPrefixOf` n                        ->  Just $ FConn And (FConn Impl condF (FComp Eq thenTermE e2))
                                                                                            (FConn Impl (FNot condF) (FComp Eq elseTermE e2))
                    _ -> Nothing
                (_, _, _) -> Nothing
            (_, _) ->
              case (termToE op1 functionsWithInputsAndOutputs, op2) of
                (Just e1, LD.Application (LD.Variable "ite") [cond, thenTerm, elseTerm]) ->
                  case (termToF cond functionsWithInputsAndOutputs, termToE thenTerm functionsWithInputsAndOutputs, termToE elseTerm functionsWithInputsAndOutputs) of
                    (Just condF, Just thenTermE, Just elseTermE) ->
                      case operator of
                        n -- TODO: Change these to AND
                          | n `elem` [">=", "fp.geq", "oge", "oge__logic"]  -> Just $ FConn And (FConn Impl condF (FComp Ge e1 thenTermE))
                                                                                                (FConn Impl (FNot condF) (FComp Ge e1 elseTermE))
                          | n `elem` [">",  "fp.gt", "ogt", "ogt__logic"]   -> Just $ FConn And (FConn Impl condF (FComp Gt e1 thenTermE))
                                                                                                (FConn Impl (FNot condF) (FComp Gt e1 elseTermE))
                          | n `elem` ["<=", "fp.leq", "ole", "ole__logic"]  -> Just $ FConn And (FConn Impl condF (FComp Le e1 thenTermE))
                                                                                                (FConn Impl (FNot condF) (FComp Le e1 elseTermE))
                          | n `elem` ["<",  "fp.lt", "olt", "olt__logic"]   -> Just $ FConn And (FConn Impl condF (FComp Lt e1 thenTermE))
                                                                                                (FConn Impl (FNot condF) (FComp Lt e1 elseTermE))
                          | n `elem` ["=",  "fp.eq"]                        -> Just $ FConn And (FConn Impl condF (FComp Eq e1 thenTermE))
                                                                                                (FConn Impl (FNot condF) (FComp Eq e1 elseTermE))
                          | "bool_eq" `isPrefixOf` n                        -> Just $ FConn And (FConn Impl condF (FComp Eq e1 thenTermE))
                                                                                                (FConn Impl (FNot condF) (FComp Eq e1 elseTermE))
                          | "user_eq" `isPrefixOf` n                        -> Just $ FConn And (FConn Impl condF (FComp Eq e1 thenTermE))
                                                                                                (FConn Impl (FNot condF) (FComp Eq e1 elseTermE))
                        _ -> Nothing
                    (_, _, _) -> Nothing
                (_, _) -> Nothing

termToF (LD.Application (LD.Variable "ite") [condition, thenTerm, elseTerm]) functionsWithInputsAndOutputs = -- if-then-else operator with F types
  case (termToF condition functionsWithInputsAndOutputs, termToF thenTerm functionsWithInputsAndOutputs, termToF elseTerm functionsWithInputsAndOutputs) of
    (Just conditionF, Just thenTermF, Just elseTermF) -> Just $ FConn And (FConn Impl conditionF thenTermF) (FConn Impl (FNot conditionF) elseTermF)
    (_, _, _) -> Nothing
termToF (LD.Variable "true") functionsWithInputsAndOutputs  = Just FTrue
termToF (LD.Variable "false") functionsWithInputsAndOutputs = Just FFalse
termToF _ _ = Nothing

termToE :: LD.Expression -> [(String, ([String], String))] -> Maybe E
-- Symbols/Literals
termToE (LD.Variable "true")  functionsWithInputsAndOutputs = Nothing -- These should be parsed to F
termToE (LD.Variable "false") functionsWithInputsAndOutputs = Nothing -- These should be parsed to F
termToE (LD.Variable var) functionsWithInputsAndOutputs = Just $ Var var
termToE (LD.Number   num) functionsWithInputsAndOutputs    = Just $ Lit num
-- one param functions
termToE (LD.Application (LD.Variable operator) [op]) functionsWithInputsAndOutputs =
  case termToE op functionsWithInputsAndOutputs of
    Nothing -> Nothing
    Just e -> case operator of
      n
        | (n =~ "^abs$|^abs[0-9]+$" :: Bool)   -> deriveTypeForOneArgFunctions n (EUnOp Abs) e
        | (n =~ "^sin$|^sin[0-9]+$" :: Bool)   -> deriveTypeForOneArgFunctions n (EUnOp Sin) e
        | (n =~ "^cos$|^cos[0-9]+$" :: Bool)   -> deriveTypeForOneArgFunctions n (EUnOp Cos) e
        | (n =~ "^sqrt$|^sqrt[0-9]+$" :: Bool) -> deriveTypeForOneArgFunctions n (EUnOp Sqrt) e
      "-"               -> Just $ EUnOp Negate e
      -- SPARK Reals functions
      "from_int"        -> Just e
      -- Some to_int functions. different suffixes (1, 2, etc.)
      -- e.g. In one file, to_int1 :: Float -> Int
      --                   to_int2 :: Bool  -> Int
      -- Are these suffixes consistent?
      -- Float functions
      "fp.abs"          -> Just $ EUnOp Abs e
      "fp.neg"          -> Just $ EUnOp Negate e
      "fp.to_real"      -> Just e
      "to_real"         -> Just e
      "value"           -> Just e
      -- Undefined functions
      "fp.isNormal"     -> Nothing
      "fp.isSubnormal"  -> Nothing
      "fp.isZero"       -> Nothing
      "fp.isNaN"        -> Nothing
      "fp.isPositive"   -> Nothing
      "fp.isNegative"   -> Nothing
      "fp.isIntegral32" -> Nothing
      "fp.isIntegral64" -> Nothing
      _                 -> Nothing
  where
    deriveTypeForOneArgFunctions :: String -> (E -> E) -> E -> Maybe E
    deriveTypeForOneArgFunctions functionName functionAsExpression functionArg =
      -- case lookup functionName functionsWithInputsAndOutputs of
        -- Just ([inputType], outputType) ->
      let
        (mInputTypes, mOutputType) = unzip $ lookup functionName functionsWithInputsAndOutputs
          -- case lookup functionName functionsWithInputsAndOutputs of
          --   Just (inputTypes, outputType) -> (Just inputTypes, Just outputType)
          --   Nothing -> (Nothing, Nothing)

        newFunctionArg = functionArg
          -- case mInputTypes of
          --   Just [inputType] ->
          --     case inputType of
          --       -- "Float32" -> Float32 RNE functionArg
          --       -- "Float64" -> Float64 RNE functionArg
          --   _ -> functionArg -- Do not deal with multiple param args for one param functions. TODO: Check if these can occur, i.e. something like RoundingMode Float32
            
        mNewFunctionAsExpression =
          case mOutputType of
            Just outputType ->
              case outputType of
                -- "Float32" -> Float32 RNE . functionAsExpression -- TODO: Make these Nothing if we can't deal with them
                -- "Float64" -> Float64 RNE . functionAsExpression -- TODO: Make these Nothing if we can't deal with them
                "Real" -> Just functionAsExpression --FIXME: Should match on other possible names. Real/real Int/int/integer, etc. I've only seen these alt names in function definitions/axioms, not assertions, but would still be more safe.
                "Int" -> Just functionAsExpression -- FIXME: Round here?
                _ -> Nothing -- These should be floats, which we cannot deal with for now
            Nothing -> Nothing
      in
        case mNewFunctionAsExpression of
          Just newFunctionAsExpression -> Just $ newFunctionAsExpression newFunctionArg
          Nothing -> Nothing
        -- Nothing -> functionAsExpression functionArg

-- two param functions 
-- params are either two different E types, or one param is a rounding mode and another is the arg
termToE (LD.Application (LD.Variable operator) [op1, op2]) functionsWithInputsAndOutputs =
  case (parseRoundingMode op1, termToE op2 functionsWithInputsAndOutputs) of
    (Just roundingMode, Just e) ->
      case operator of
        n
          | (n =~ "^round$|^round[0-9]+$" :: Bool)   -> Just $ Float roundingMode e --FIXME: remove this? not used with cvc4 driver?
          | (n =~ "^to_int$|^to_int[0-9]+$" :: Bool) -> Just $ RoundToInteger roundingMode e
          | (n =~ "^of_int$|^of_int[0-9]+$" :: Bool) -> 
            case lookup n functionsWithInputsAndOutputs of
              Just (_, outputType) ->
                case outputType of
                  o
                    | o `elem` ["Float32", "single"] -> Just $ Float32 roundingMode e
                    | o `elem` ["Float64", "double"] -> Just $ Float64 roundingMode e
                  _ -> Nothing
              _ -> Nothing
        "fp.roundToIntegral" -> Just $ RoundToInteger roundingMode e
        _ -> Nothing
    _ ->
      case (termToE op1 functionsWithInputsAndOutputs, termToE op2 functionsWithInputsAndOutputs) of
        (Just e1, Just e2) ->
          case operator of
            n
              -- "o..." functions are from SPARK Reals
              | n `elem` ["+", "oadd", "oadd__logic"]                    -> Just $ EBinOp Add e1 e2
              | n `elem` ["-", "osubtract", "osubtract__logic"]          -> Just $ EBinOp Sub e1 e2
              | n `elem` ["*", "omultiply", "omultiply__logic"]          -> Just $ EBinOp Mul e1 e2
              | n `elem` ["/", "odivide", "odivide__logic"]              -> Just $ EBinOp Div e1 e2
              | (n =~ "^pow$|^pow[0-9]+$|^power$|^power[0-9]+$" :: Bool) ->
                case lookup n functionsWithInputsAndOutputs of
                  Just (["Real", "Real"], "Real") -> Just $ EBinOp Pow e1 e2
                  Just ([input1, "Int"], output) ->
                    let 
                      mExactPowExpression =
                        if input1 == "Int" || input1 == "Real" 
                          then case e2 of
                            Lit l2 -> if denominator l2 == 1.0 then Just $ PowI e1 (numerator l2) else Just $ EBinOp Pow e1 e2
                            _      -> Just $ EBinOp Pow e1 e2
                          else Nothing
                    in case mExactPowExpression of
                      Just exactPowExpression -> case output of
                        "Real" -> Just exactPowExpression
                        "Int"  -> Just $ RoundToInteger RNE exactPowExpression
                        _      -> Nothing
                      Nothing -> Nothing
                  _ -> Nothing
              | (n =~ "^mod$|^mod[0-9]+$" :: Bool)                       ->
                case lookup n functionsWithInputsAndOutputs of
                  Just (["Real", "Real"], "Real") -> Just $ EBinOp Mod e1 e2
                  Just (["Int", "Int"], "Int") -> Just $ RoundToInteger RNE $ EBinOp Mod e1 e2 --TODO: might be worth implementing Int Mod
                  _ -> Nothing
            _                                                            -> Nothing
        (_, _) -> Nothing

-- Float bits to Rational
termToE (LD.Application (LD.Variable "fp") [LD.Variable sSign, LD.Variable sExponent, LD.Variable sMantissa]) functionsWithInputsAndOutputs =
  let
    bSign     = drop 2 sSign
    bExponent = drop 2 sExponent
    bMantissa = drop 2 sMantissa

    bFull = bSign ++ bExponent ++ bMantissa

    -- Read a string of Bits ('1' or '0') where the first digit is the most significant
    -- The digit parameter denotes the current digit, should be equal to length of the first param at all times
    readBits :: String -> Integer -> Integer
    readBits [] _ = 0
    readBits (bit : bits) digit = digitToInt bit * (2 ^ (digit - 1)) + readBits bits (digit - 1)

    bitsToWord8 :: String -> [Word8]
    bitsToWord8 bits =
      let wordS = take 8 bits
          rem   = drop 8 bits
          wordV = readBits wordS 8
      in
        P.fromInteger wordV : bitsToWord8 rem

    bsFloat    = B.pack $ bitsToWord8 bFull
  in
    if all (`elem` "01") bFull
      then
        case length bFull of
          32 -> Just $ Lit $ toRational $ runGet getFloatbe bsFloat 
          64 -> Just $ Lit $ toRational $ runGet getDoublebe bsFloat
          -- 32 -> Just $ Float32 RNE $ Lit $ toRational $ runGet getFloatbe bsFloat 
          -- 64 -> Just $ Float64 RNE $ Lit $ toRational $ runGet getDoublebe bsFloat
          _  -> Nothing
      else Nothing

-- Float functions, three params. Other three param functions should be placed before here
termToE (LD.Application (LD.Variable operator) [roundingMode, op1, op2]) functionsWithInputsAndOutputs =
  -- case operator of
  --   -- SPARK Reals
  --   "fp.to_real" -> Nothing 
  --   _ -> -- Known ops
  case (termToE op1 functionsWithInputsAndOutputs, termToE op2 functionsWithInputsAndOutputs) of
    (Just e1, Just e2) ->
      case parseRoundingMode roundingMode of -- Floating-point ops
        Just mode ->
          case operator of
            "fp.add" -> Just $ Float mode $ EBinOp Add e1 e2
            "fp.sub" -> Just $ Float mode $ EBinOp Sub e1 e2
            "fp.mul" -> Just $ Float mode $ EBinOp Mul e1 e2
            "fp.div" -> Just $ Float mode $ EBinOp Div e1 e2
            _        -> Nothing
        Nothing -> Nothing
    (_, _) -> Nothing
termToE _ _ = Nothing

termsToF :: [LD.Expression] -> [(String, ([String], String))] -> [F]
termsToF es fs = mapMaybe (`termToF` fs) es

determineFloatTypeE :: E -> [(String, String)] -> Maybe E
determineFloatTypeE (EBinOp op e1 e2) varTypeMap  = case determineFloatTypeE e1 varTypeMap of
                                                      Just p1 ->
                                                        case determineFloatTypeE e2 varTypeMap of
                                                          Just p2 -> Just $ EBinOp op p1 p2
                                                          Nothing -> Nothing
                                                      Nothing -> Nothing
determineFloatTypeE (EUnOp op e)      varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ EUnOp op p
                                                      Nothing -> Nothing
determineFloatTypeE (PowI e i)        varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ PowI p i
                                                      Nothing -> Nothing
determineFloatTypeE (Float r e)       varTypeMap  = case mVariableType of
                                                      Just variableType ->
                                                        case variableType of
                                                          t
                                                            | t `elem` ["Float32", "single"] ->
                                                                case determineFloatTypeE e varTypeMap of
                                                                  Just p -> Just $ Float32 r p
                                                                  Nothing -> Nothing
                                                            | t `elem` ["Float64", "double"] ->
                                                                case determineFloatTypeE e varTypeMap of
                                                                  Just p -> Just $ Float64 r p
                                                                  Nothing -> Nothing
                                                          _ -> Nothing
                                                      Nothing -> Nothing
                                                    where
                                                      allVars = findVariablesInExpressions e
                                                      knownVarsWithPrecision = knownFloatVars e
                                                      knownVars = map fst knownVarsWithPrecision
                                                      unknownVars = filter (`notElem` knownVars) allVars
                                                      mVariableType = findVariableType unknownVars varTypeMap knownVarsWithPrecision Nothing
determineFloatTypeE (Float32 r e)     varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ Float32 r p
                                                      Nothing -> Nothing
determineFloatTypeE (Float64 r e)     varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ Float64 r p
                                                      Nothing -> Nothing
determineFloatTypeE (RoundToInteger r e) varTypeMap = case determineFloatTypeE e varTypeMap of
                                                        Just p -> Just $ RoundToInteger r p
                                                        Nothing -> Nothing
determineFloatTypeE Pi                _           = Just Pi
determineFloatTypeE (Var v)           varTypeMap  = case lookup v varTypeMap of 
                                                      Just variableType ->
                                                        case variableType of
                                                          -- t
                                                          --   | t `elem` ["Float32", "single"] -> Just $ Float32 RNE $ Var v
                                                          --   | t `elem` ["Float64", "double"] -> Just $ Float64 RNE $ Var v
                                                          _ -> Just $ Var v -- It is safe to treat integers and rationals as reals
                                                      _ -> Just $ Var v
determineFloatTypeE (Lit n)           _           = Just $ Lit n

-- |Tries to determine whether a Float operation is single or double precision
-- by searching for the type of all variables appearing in the function. If the
-- types match and are all either Float32/Float64, we can determine the type.
determineFloatTypeF :: F -> [(String, String)] -> Maybe F
determineFloatTypeF (FComp op e1 e2) varTypeMap = case (determineFloatTypeE e1 varTypeMap, determineFloatTypeE e2 varTypeMap) of
                                                    (Just p1, Just p2)  -> Just $ FComp op p1 p2
                                                    (_, _)              -> Nothing
determineFloatTypeF (FConn op f1 f2) varTypeMap = case (determineFloatTypeF f1 varTypeMap, determineFloatTypeF f2 varTypeMap) of
                                                    (Just p1, Just p2)  -> Just $ FConn op p1 p2
                                                    (_, _)              -> Nothing
determineFloatTypeF (FNot f)         varTypeMap = case determineFloatTypeF f varTypeMap of
                                                    Just p  -> Just $ FNot p
                                                    Nothing -> Nothing
determineFloatTypeF FTrue  _ = Just FTrue
determineFloatTypeF FFalse _ = Just FFalse

-- |Find the type for the given variables
-- Type is looked for in the supplied map
-- If all found types match, return this type
findVariableType :: [String] -> [(String, String)] -> [(String, Integer)] -> Maybe String -> Maybe String
findVariableType [] _ [] mFoundType  = mFoundType
findVariableType [] _ ((_, precision) : vars) mFoundType =
  case mFoundType of
    Just t ->
      if (t `elem` ["Float32", "single"] && precision == 32) || ((t `elem` ["Float64", "double"]) && (precision == 64))
        then findVariableType [] [] vars mFoundType
        else Nothing
    Nothing ->
      case precision of
        32 -> findVariableType [] [] vars (Just "Float32")
        64 -> findVariableType [] [] vars (Just "Float64")
        _ -> Nothing

findVariableType (v: vs) varTypeMap knownVarsWithPrecision mFoundType =
  case lookup v varTypeMap of
    Just t  ->
      if "Int" `isPrefixOf` t then
        findVariableType vs varTypeMap knownVarsWithPrecision mFoundType
        else
          case mFoundType of
            Just f -> if f == t then findVariableType vs varTypeMap knownVarsWithPrecision (Just t) else Nothing
            Nothing -> findVariableType vs varTypeMap knownVarsWithPrecision (Just t)
    Nothing -> Nothing

knownFloatVars :: E -> [(String, Integer)]
knownFloatVars e = removeConflictingVars . nub $ findAllFloatVars e
  where
    removeConflictingVars :: [(String, Integer)] -> [(String, Integer)]
    removeConflictingVars [] = []
    removeConflictingVars ((v, t) : vs) =
      if v `elem` map fst vs
        then removeConflictingVars $ filter (\(v', _) -> v /= v') vs
        else (v, t) : removeConflictingVars vs

    findAllFloatVars (EBinOp _ e1 e2) = knownFloatVars e1 ++ knownFloatVars e2
    findAllFloatVars (EUnOp _ e) = knownFloatVars e
    findAllFloatVars (PowI e _) = knownFloatVars e
    findAllFloatVars (Float _ e) = knownFloatVars e
    findAllFloatVars (Float32 _ (Var v)) = [(v, 32)]
    findAllFloatVars (Float64 _ (Var v)) = [(v, 64)]
    findAllFloatVars (Float32 _ e) = knownFloatVars e
    findAllFloatVars (Float64 _ e) = knownFloatVars e
    findAllFloatVars (RoundToInteger _ e) = knownFloatVars e
    findAllFloatVars (Var _) = []
    findAllFloatVars (Lit _) = []
    findAllFloatVars Pi = []

findVariablesInFormula :: F -> [String]
findVariablesInFormula f = nub $ findVars f
  where
    findVars (FConn _ f1 f2) = findVars f1 ++ findVars f2
    findVars (FComp _ e1 e2) = findVariablesInExpressions e1 ++ findVariablesInExpressions e2
    findVars (FNot f1)       = findVars f1
    findVars FTrue           = []
    findVars FFalse          = []

findVariablesInExpressions :: E -> [String]
findVariablesInExpressions (EBinOp _ e1 e2) = findVariablesInExpressions e1 ++ findVariablesInExpressions e2
findVariablesInExpressions (EUnOp _ e) = findVariablesInExpressions e
findVariablesInExpressions (PowI e _) = findVariablesInExpressions e
findVariablesInExpressions (Float _ e) = findVariablesInExpressions e
findVariablesInExpressions (Float32 _ e) = findVariablesInExpressions e
findVariablesInExpressions (Float64 _ e) = findVariablesInExpressions e
findVariablesInExpressions (RoundToInteger _ e) = findVariablesInExpressions e
findVariablesInExpressions (Var v) = [v]
findVariablesInExpressions (Lit _) = []
findVariablesInExpressions Pi      = []

parseRoundingMode :: LD.Expression -> Maybe RoundingMode
parseRoundingMode (LD.Variable mode) =
  case mode of
    m
      | m `elem` ["RNE", "NearestTiesToEven"] -> Just RNE
      | m `elem` ["RTP", "Up"]                -> Just RTP
      | m `elem` ["RTN", "Down"]              -> Just RTN
      | m `elem` ["RTZ", "ToZero"]            -> Just RTZ
      | m `elem` ["RNA"]                      -> Just RNA
    _                                         -> Nothing
parseRoundingMode _ = Nothing

-- |Process a parsed list of expressions to a VC. 
-- 
-- If the parsing mode is Why3, everything in the context implies the goal (empty context means we only have a goal). 
-- If the goal cannot be parsed, we return Nothing.
-- 
-- If the parsing mode is CNF, parse all assertions into a CNF. If any assertion cannot be parsed, return Nothing.
-- If any assertion contains Floats, return Nothing.
processVC :: [LD.Expression] -> Maybe (F, [(String, String)])
processVC parsedExpressions =
  Just (foldAssertionsF assertionsF, variablesWithTypes)
  where
    assertions  = findAssertions parsedExpressions
    assertionsF = mapMaybe (`determineFloatTypeF` variablesWithTypes) $ termsToF assertions functionsWithInputsAndOutputs

    variablesWithTypes  = findVariables parsedExpressions
    functionsWithInputsAndOutputs = findFunctionInputsAndOutputs parsedExpressions



    foldAssertionsF :: [F] -> F
    foldAssertionsF []       = error "processVC - foldAssertionsF: Empty list given"
    foldAssertionsF [f]      = f
    foldAssertionsF (f : fs) = FConn And f (foldAssertionsF fs)

-- |Derive ranges for a VC (Implication where a CNF implies a goal)
-- Remove anything which refers to a variable for which we cannot derive ranges
-- If the goal contains underivable variables, return Nothing
deriveVCRanges :: F -> [(String, String)] -> Maybe (F, TypedVarMap)
deriveVCRanges vc varsWithTypes =
  case filterOutVars simplifiedF underivableVariables False of
    Just filteredF -> Just (filteredF, safelyRoundTypedVarMap typedDerivedVarMap)
    Nothing -> Nothing
  where
    integerVariables = findIntegerVariables varsWithTypes

    (simplifiedFUnchecked, derivedVarMapUnchecked, underivableVariables) = deriveBoundsAndSimplify vc

    (piVars, derivedVarMap) = findRealPiVars derivedVarMapUnchecked

    typedDerivedVarMap = unsafeVarMapToTypedVarMap derivedVarMap integerVariables

    safelyRoundTypedVarMap [] = []
    safelyRoundTypedVarMap ((TypedVar (varName, (leftBound, rightBound)) Real) : vars)    = 
      let
        leftBoundRoundedDown = rational . fst . endpoints $ mpBallP (prec 23) leftBound
        rightBoundRoundedUp = rational . snd . endpoints $ mpBallP (prec 23) rightBound
        newBound = TypedVar (varName, (leftBoundRoundedDown, rightBoundRoundedUp)) Real
      in
        newBound : safelyRoundTypedVarMap vars
    safelyRoundTypedVarMap (vi@(TypedVar _                               Integer) : vars) = vi : safelyRoundTypedVarMap vars
 
    simplifiedF = substVarsWithPi piVars simplifiedFUnchecked

    -- TODO: Would be good to include a warning when this happens
    -- Could also make this an option
    -- First elem are the variables which can be assumed to be real pi
    -- Second elem is the varMap without the real pi vars
    findRealPiVars :: VarMap -> ([String], VarMap)
    findRealPiVars [] = ([], [])
    findRealPiVars (varWithBounds@(var, (l, r)) : vars) =
      if 
        -- If variable is pi or pi# where # is a number
        (var =~ "^pi$|^pi[0-9]+$" :: Bool) &&
        -- And bounds are equal or better than the bounds given by Why3 for Pi
        l >= (7074237752028440.0 / 2251799813685248.0) && r <= (7074237752028441.0 / 2251799813685248.0) &&
        -- And the type of the variable is Real
        (lookup var varsWithTypes == Just "Real") 
          then (\(foundPiVars, varMapWithoutPi) -> ((var : foundPiVars), varMapWithoutPi))           $ findRealPiVars vars
          else (\(foundPiVars, varMapWithoutPi) -> (foundPiVars, (varWithBounds : varMapWithoutPi))) $ findRealPiVars vars
    
    substVarsWithPi :: [String] -> F -> F
    substVarsWithPi [] f = f
    substVarsWithPi (v : _) f = substVarFWithE v f Pi


    -- |Safely filter our terms that contain underivable variables.
    -- Need to preserve unsat terms, so we can safely remove x in FConn And x y if x contains underivable variables.
    -- We cannot safely remove x from FConn Or x y if x contains underivable variables
    -- (since x may be sat and y may be unsat, filtering out x would give an incorrect unsat result), so we remove the whole term
    -- Reverse logic as appropriate when a term is negated
    filterOutVars :: F -> [String] -> Bool -> Maybe F
    filterOutVars (FConn And f1 f2) vars False =
      case (filterOutVars f1 vars False, filterOutVars f2 vars False) of
        (Just ff1, Just ff2) -> Just $ FConn And ff1 ff2
        (Just ff1, _)        -> Just ff1
        (_, Just ff2)        -> Just ff2
        (_, _)               -> Nothing
    filterOutVars (FConn Or f1 f2) vars False =
      case (filterOutVars f1 vars False, filterOutVars f2 vars False) of
        (Just ff1, Just ff2) -> Just $ FConn Or ff1 ff2
        (_, _)               -> Nothing
    filterOutVars (FConn Impl f1 f2) vars False =
      case (filterOutVars f1 vars False, filterOutVars f2 vars False) of
        (Just ff1, Just ff2) -> Just $ FConn Impl ff1 ff2
        (_, _)               -> Nothing
    filterOutVars (FConn And f1 f2) vars True =
      case (filterOutVars f1 vars True, filterOutVars f2 vars True) of
        (Just ff1, Just ff2) -> Just $ FConn And ff1 ff2
        (_, _)               -> Nothing
    filterOutVars (FConn Or f1 f2) vars True =
      case (filterOutVars f1 vars True, filterOutVars f2 vars True) of
        (Just ff1, Just ff2) -> Just $ FConn Or ff1 ff2
        (Just ff1, _)        -> Just ff1
        (_, Just ff2)        -> Just ff2
        (_, _)               -> Nothing
    filterOutVars (FConn Impl f1 f2) vars True =
      case (filterOutVars f1 vars True, filterOutVars f2 vars True) of
        (Just ff1, Just ff2) -> Just $ FConn Impl ff1 ff2
        (Just ff1, _)        -> Just ff1
        (_, Just ff2)        -> Just ff2
        (_, _)               -> Nothing
    filterOutVars (FNot f) vars isNegated = FNot <$> filterOutVars f vars (not isNegated)

    filterOutVars (FComp op e1 e2) vars _isNegated =
      if eContainsVars vars e1 || eContainsVars vars e2
        then Nothing
        else Just (FComp op e1 e2)
    
    filterOutVars FTrue  _ _ = Just FTrue
    filterOutVars FFalse _ _ = Just FFalse

eContainsVars :: [String] -> E -> Bool
eContainsVars vars (Var var)        = var `elem` vars
eContainsVars _ (Lit _)             = False
eContainsVars _ Pi                  = False

eContainsVars vars (EBinOp _ e1 e2) = eContainsVars vars e1 || eContainsVars vars e2
eContainsVars vars (EUnOp _ e)      = eContainsVars vars e
eContainsVars vars (PowI e _)       = eContainsVars vars e
eContainsVars vars (Float32 _ e)    = eContainsVars vars e
eContainsVars vars (Float64 _ e)    = eContainsVars vars e
eContainsVars vars (Float _ e)      = eContainsVars vars e
eContainsVars vars (RoundToInteger _ e) = eContainsVars vars e

fContainsVars :: [String] -> F -> Bool
fContainsVars vars (FConn _ f1 f2)  = fContainsVars vars f1 || fContainsVars vars f2
fContainsVars vars (FComp _ e1 e2)  = eContainsVars vars e1 || eContainsVars vars e2
fContainsVars vars (FNot f)         = fContainsVars vars f
fContainsVars _ FTrue               = False
fContainsVars _ FFalse              = False

inequalityEpsilon :: Rational
inequalityEpsilon = 0.000000001
-- inequalityEpsilon = 1/(2^23)

findVarEqualities :: F -> [(String, E)]
findVarEqualities (FConn And f1 f2)     = findVarEqualities f1 ++ findVarEqualities f2
findVarEqualities FConn {}              = []
findVarEqualities (FComp Eq (Var v) e2) = [(v, e2)]
findVarEqualities (FComp Eq e1 (Var v)) = [(v, e1)]
findVarEqualities FComp {}              = []
findVarEqualities (FNot _)              = [] -- Not EQ, means we can't do anything here?
findVarEqualities FTrue                 = []
findVarEqualities FFalse                = []

-- |Filter out var equalities which rely on themselves
filterOutCircularVarEqualities :: [(String, E)] -> [(String, E)]
filterOutCircularVarEqualities = filter (\(v, e) -> v `notElem` findVariablesInExpressions e)

-- |Filter out var equalities which occur multiple times by choosing the var equality with the smallest length
filterOutDuplicateVarEqualities :: [(String, E)] -> [(String, E)]
filterOutDuplicateVarEqualities [] = []
filterOutDuplicateVarEqualities ((v, e) : vs) =
  case partition (\(v',_) -> v == v')  vs of
    ([], _) -> (v, e) : filterOutDuplicateVarEqualities vs
    (matchingEqualities, otherEqualities) ->
      let shortestVarEquality = head $ sortBy (\(_, e1) (_, e2) -> P.compare (lengthE e1) (lengthE e2)) $ (v, e) : matchingEqualities
      in shortestVarEquality : filterOutDuplicateVarEqualities otherEqualities

substAllEqualities :: F -> F
substAllEqualities = recursivelySubstVars
  where
    recursivelySubstVars f@(FConn Impl context _) =
      case filteredVarEqualities of
        [] -> f
        _  -> if f P.== substitutedF then f else recursivelySubstVars . simplifyF $ substitutedF -- TODO: make this a var
      where
        substitutedF = substVars filteredVarEqualities f
        filteredVarEqualities = (filterOutDuplicateVarEqualities . filterOutCircularVarEqualities) varEqualities
        varEqualities = findVarEqualities context

    recursivelySubstVars f =
      case filteredVarEqualities of
        [] -> f
        _  -> if f P.== substitutedF then f else  recursivelySubstVars . simplifyF $ substitutedF
      where
        substitutedF = substVars filteredVarEqualities f
        filteredVarEqualities = (filterOutDuplicateVarEqualities . filterOutCircularVarEqualities) varEqualities
        varEqualities = findVarEqualities f

    substVars [] f = f
    substVars ((v, e) : _) f = substVarFWithE v f e

eliminateFloatsAndSimplifyVC :: F -> TypedVarMap -> Bool -> FilePath -> IO F
eliminateFloatsAndSimplifyVC vc typedVarMap strengthenVC fptaylorPath = 
  do
    vcWithoutFloats <- eliminateFloatsF vc (typedVarMapToVarMap typedVarMap) strengthenVC fptaylorPath
    let typedVarMapAsMap = M.fromList $ map (\(TypedVar (varName, (leftBound, rightBound)) _) -> (varName, (Just leftBound, Just rightBound))) typedVarMap
    let simplifiedVCWithoutFloats = (simplifyF . evalF_comparisons typedVarMapAsMap) vcWithoutFloats
    return simplifiedVCWithoutFloats

parseVCToF :: FilePath -> FilePath -> IO (Maybe (F, TypedVarMap))
parseVCToF filePath fptaylorPath =
  do
    parsedFile  <- parseSMT2 filePath

    case processVC parsedFile of
      Just (vc, varTypes) -> 
        let
          simplifiedVC = substAllEqualities (simplifyF vc)
          mDerivedVCWithRanges = deriveVCRanges simplifiedVC varTypes
        in
        case mDerivedVCWithRanges of
          Just (derivedVC, derivedRanges) ->
            do
              -- The file we are given is assumed to be a contradiction, so weaken the VC
              let strengthenVC = False
              simplifiedVCWithoutFloats <- eliminateFloatsAndSimplifyVC derivedVC derivedRanges strengthenVC fptaylorPath
              return $ Just (simplifiedVCWithoutFloats, derivedRanges)
          Nothing -> return Nothing
      Nothing -> return Nothing

parseVCToSolver :: FilePath -> FilePath -> (F -> TypedVarMap -> String) -> Bool -> IO (Maybe String)
parseVCToSolver filePath fptaylorPath proverTranslator negateVC =
  do
    parsedFile <- parseSMT2 filePath
    
    case processVC parsedFile of
      Just (vc, varTypes) ->
        let
          simplifiedVC = substAllEqualities (simplifyF vc)
          mDerivedVCWithRanges = deriveVCRanges simplifiedVC varTypes
        in
          case mDerivedVCWithRanges of
            Just (derivedVC, derivedRanges) ->
              do
                let strengthenVC = False
                simplifiedVCWithoutFloats <- eliminateFloatsAndSimplifyVC derivedVC derivedRanges strengthenVC fptaylorPath
                return $ Just (proverTranslator (if negateVC then simplifyF (FNot simplifiedVCWithoutFloats) else simplifiedVCWithoutFloats) derivedRanges)
            Nothing -> return Nothing
      Nothing -> return Nothing
