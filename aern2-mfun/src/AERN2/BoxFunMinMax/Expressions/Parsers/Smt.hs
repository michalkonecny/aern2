{-# LANGUAGE TupleSections #-}
module AERN2.BoxFunMinMax.Expressions.Parsers.Smt where

import MixedTypesNumPrelude
import qualified Prelude as P
import System.IO.Unsafe

import AERN2.BoxFunMinMax.Expressions.Type

import Debug.Trace

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
import AERN2.BoxFunMinMax.Expressions.Eliminator (minMaxAbsEliminatorECNF)
import Data.List (nub, sort, isPrefixOf, sortBy, partition)
data ParsingMode = Why3 | CNF

parser :: String -> [LD.Expression]
parser = LP.analyzeExpressionSequence . LP.parseSequence . LP.tokenize

parseSMT2 :: FilePath -> [LD.Expression]
parseSMT2 filePath = parser . unsafePerformIO $ P.readFile filePath

-- |Find assertions in a parsed expression
-- Assertions are Application types with the operator being a Variable equal to "assert"
-- Assertions only have one 'operands'
findAssertions :: [LD.Expression] -> [LD.Expression]
findAssertions [] = []
findAssertions ((LD.Application (LD.Variable "assert") [operands]) : expressions) = operands : findAssertions expressions
findAssertions (_ : expressions) = findAssertions expressions

findFunctionOutputs :: [LD.Expression] -> [(String, String)]
findFunctionOutputs [] = []
findFunctionOutputs ((LD.Application (LD.Variable "declare-fun") [LD.Variable fName, _fInputs, LD.Variable fOutputs]) : expressions) =
  (fName, fOutputs) : findFunctionOutputs expressions
findFunctionOutputs (_ : expressions) = findFunctionOutputs expressions

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
    goal = last asserts -- FIXME: Unsafe. If asserts is emoty, this will fail
    assertsWithoutGoal = take (numberOfAssertions - 1) asserts

termToF :: LD.Expression -> [(String, String)] -> Maybe F
termToF (LD.Application (LD.Variable operator) [op]) functionsWithOutputs = -- Single param operators
  case termToE op functionsWithOutputs of -- Ops with E params
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
      case termToF op functionsWithOutputs of
        Just f ->
          case operator of
            "not" -> Just $ FNot f -- TODO: Do we need an FNot for E as well? Answer: We could simply Negate, but we don't need it.
            _ -> Nothing
        _ -> Nothing
termToF (LD.Application (LD.Variable operator) [op1, op2]) functionsWithOutputs = -- Two param operations
  case (termToE op1 functionsWithOutputs, termToE op2 functionsWithOutputs) of
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
      case (termToF op1 functionsWithOutputs, termToF op2 functionsWithOutputs) of
        (Just f1, Just f2) ->
          case operator of
            "and" -> Just $ FConn And f1 f2
            "or"  -> Just $ FConn Or f1 f2
            "=>"  -> Just $ FConn Impl f1 f2
            "="   -> Just $ FConn Equiv f1 f2
            n
              | "bool_eq" `isPrefixOf` n ->  Just $ FConn Equiv f1 f2
              | "user_eq" `isPrefixOf` n ->  Just $ FConn Equiv f1 f2
            _ -> Nothing
        -- Parse ite where it is used as an expression
        (_, _) ->
          case (op1, termToE op2 functionsWithOutputs) of
            (LD.Application (LD.Variable "ite") [cond, thenTerm, elseTerm], Just e2) ->
              case (termToF cond functionsWithOutputs, termToE thenTerm functionsWithOutputs, termToE elseTerm functionsWithOutputs) of
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
              case (termToE op1 functionsWithOutputs, op2) of
                (Just e1, LD.Application (LD.Variable "ite") [cond, thenTerm, elseTerm]) ->
                  case (termToF cond functionsWithOutputs, termToE thenTerm functionsWithOutputs, termToE elseTerm functionsWithOutputs) of
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

termToF (LD.Application (LD.Variable "ite") [condition, thenTerm, elseTerm]) functionsWithOutputs = -- if-then-else operator with F types
  case (termToF condition functionsWithOutputs, termToF thenTerm functionsWithOutputs, termToF elseTerm functionsWithOutputs) of
    (Just conditionF, Just thenTermF, Just elseTermF) -> Just $ FConn And (FConn Impl conditionF thenTermF) (FConn Impl (FNot conditionF) elseTermF)
    (_, _, _) -> Nothing
termToF (LD.Variable "true") functionsWithOutputs  = Just FTrue
termToF (LD.Variable "false") functionsWithOutputs = Just FFalse
termToF _ _ = Nothing

termToE :: LD.Expression -> [(String, String)] -> Maybe E
-- Symbols/Literals
termToE (LD.Variable "true")  functionsWithOutputs = Nothing -- These should be parsed to F
termToE (LD.Variable "false") functionsWithOutputs = Nothing -- These should be parsed to F
termToE (LD.Variable var) functionsWithOutputs    =
  if length var <= 3 && "pi" `isPrefixOf` var
    then Just Pi
    else Just $ Var var
termToE (LD.Number   num) functionsWithOutputs    = Just $ Lit num
-- one param functions
termToE (LD.Application (LD.Variable operator) [op]) functionsWithOutputs =
  case termToE op functionsWithOutputs of
    Nothing -> Nothing
    Just e -> case operator of
      "abs"             -> Just $ EUnOp Abs e -- Haven't seen this, but added
      "abs1"            -> Just $ EUnOp Abs e -- Seems to be real version of Abs
      "sin"             -> Just $ EUnOp Sin e
      "sin1"            -> Just $ EUnOp Sin e
      "cos"             -> Just $ EUnOp Cos e
      "cos1"            -> Just $ EUnOp Cos e
      "sqrt"            -> Just $ EUnOp Sqrt e
      "sqrt1"           -> Just $ EUnOp Sqrt e
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
-- Why3 round function
termToE (LD.Application (LD.Variable "round") [mode, operand]) functionsWithOutputs =
  case (parseRoundingMode mode, termToE operand functionsWithOutputs) of
    (Just roundingMode, Just e) -> Just $ Float roundingMode e
    (_, _) -> Nothing
-- to_int/from_int float functions
termToE (LD.Application (LD.Variable "fp.roundToIntegral") [mode, operand]) functionsWithOutputs =
  case (parseRoundingMode mode, termToE operand functionsWithOutputs) of
    (Just roundingMode, Just e) -> Just $ RoundToInteger roundingMode e
    (_, _) -> Nothing
termToE (LD.Application (LD.Variable "to_int") [mode, operand]) functionsWithOutputs = termToE (LD.Application (LD.Variable "fp.roundToIntegral") [mode, operand]) functionsWithOutputs
termToE (LD.Application (LD.Variable "to_int1") [mode, operand]) functionsWithOutputs = termToE (LD.Application (LD.Variable "fp.roundToIntegral") [mode, operand]) functionsWithOutputs
termToE (LD.Application (LD.Variable "of_int") [mode, operand]) functionsWithOutputs = -- FIXME: Change these to match with prefix
  case (parseRoundingMode mode, termToE operand functionsWithOutputs) of
    (Just roundingMode, Just e) ->
      case lookup "of_int" functionsWithOutputs of
        Just outputType ->
          if outputType `elem` ["Float32", "single"] then Just $ Float32 roundingMode e else
          if outputType `elem` ["Float64", "double"] then Just $ Float64 roundingMode e else Nothing
        Nothing -> Nothing
    (_, _) -> Nothing
termToE (LD.Application (LD.Variable "of_int1") [mode, operand]) functionsWithOutputs =
  case (parseRoundingMode mode, termToE operand functionsWithOutputs) of
    (Just roundingMode, Just e) ->
      case lookup "of_int" functionsWithOutputs of
        Just outputType ->
          if outputType `elem` ["Float32", "single"] then Just $ Float32 roundingMode e else
          if outputType `elem` ["Float64", "double"] then Just $ Float64 roundingMode e else Nothing
        Nothing -> Nothing
    (_, _) -> Nothing
-- two param functions where op1 and op2 can be parsed to the E type.
-- Functions with two params which need special handling (i.e. round) should be
-- placed before here
termToE (LD.Application (LD.Variable operator) [op1, op2]) functionsWithOutputs =
  case (termToE op1 functionsWithOutputs, termToE op2 functionsWithOutputs) of
    (Just e1, Just e2) ->
      case operator of
        n
          -- "o..." functions are from SPARK Reals
          | n `elem` ["+", "oadd", "oadd__logic"]           -> Just $ EBinOp Add e1 e2
          | n `elem` ["-", "osubtract", "osubtract__logic"] -> Just $ EBinOp Sub e1 e2
          | n `elem` ["*", "omultiply", "omultiply__logic"] -> Just $ EBinOp Mul e1 e2
          | n `elem` ["/", "odivide", "odivide__logic"]     -> Just $ EBinOp Div e1 e2
          | "pow" `isPrefixOf` n                            -> Just $ EBinOp Pow e1 e2
          | "mod" `isPrefixOf` n                            -> Just $ EBinOp Mod e1 e2
        _                                                   -> Nothing
    (_, _) -> Nothing

-- Float bits to Rational
termToE (LD.Application (LD.Variable "fp") [LD.Variable sSign, LD.Variable sExponent, LD.Variable sMantissa]) functionsWithOutputs =
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
          32 -> Just $ Lit $ toRational $ runGet getFloatbe bsFloat  -- Check if finite
          64 -> Just $ Lit $ toRational $ runGet getDoublebe bsFloat -- Check if finite
          _  -> Nothing
      else Nothing

-- Float functions, three params. Other three param functions should be placed before here
termToE (LD.Application (LD.Variable operator) [roundingMode, op1, op2]) functionsWithOutputs =
  -- case operator of
  --   -- SPARK Reals
  --   "fp.to_real" -> Nothing 
  --   _ -> -- Known ops
  case (termToE op1 functionsWithOutputs, termToE op2 functionsWithOutputs) of
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

termsToF :: [LD.Expression] -> [(String, String)] -> [F]
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
determineFloatTypeE (Var v)           _           = Just (Var v)
determineFloatTypeE (Lit n)           _           = Just (Lit n)

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
processVC  :: [LD.Expression] -> ParsingMode -> Maybe (F, [(String, String)])
processVC parsedExpressions Why3 =
  trace "pi" $
  trace (show mGoal) $
  case mGoalF of
    Just goalF  -> trace "hi" $
      if null contextF
        then Just (goalF, variablesWithTypes)
        else
          Just (FConn Impl (foldContextF filteredContextF) goalF, variablesWithTypes)
          where
            filteredContextF =
              filter
              (fContainsVars relevantVars)
              contextF

            relevantVars = findDependentVars contextF goalVars

            goalVars = sort $ findVariablesInFormula goalF

            findDependentVars :: [F] -> [String] -> [String]
            findDependentVars [] varsToKeep = varsToKeep
            findDependentVars fs varsToKeep =
              if varsToKeep == foundVars
                then varsToKeep
                else findDependentVars fs foundVars
              where
                foundVars = sort . nub $ findVars fs varsToKeep

                findVars :: [F] -> [String] -> [String]
                findVars [] vs = vs
                findVars (x : xs) vs =
                  if fContainsVars vs x
                    then findVars xs $ vs ++ findVariablesInFormula x
                    else findVars xs vs
    Nothing     -> trace "li" Nothing
  where
    (goalWithNot, context) = (takeGoalFromAssertions . findAssertions) parsedExpressions

    mGoal =
      case goalWithNot of
        LD.Application (LD.Variable "not") [operand] -> Just operand -- Goals in SMT look like this
        _ -> Nothing

    contextF            = mapMaybe (`determineFloatTypeF` variablesWithTypes) $ termsToF context functionsWithOutputs
    mGoalF              = maybe Nothing (`determineFloatTypeF` variablesWithTypes) $ maybe Nothing (`termToF` functionsWithOutputs) mGoal
    variablesWithTypes  = findVariables parsedExpressions
    functionsWithOutputs = findFunctionOutputs parsedExpressions

    foldContextF :: [F] -> F
    foldContextF []       = error "processVC - foldContextF: Empty list given"
    foldContextF [f]      = f
    foldContextF (f : fs) = FConn And f (foldContextF fs)
processVC parsedExpressions CNF = trace (show mAssertionsF) $ if any hasFloatF assertionsF then Nothing else fmap (`pair` variablesWithTypes) assertionsF
  where
    pair a b = (a, b)

    assertions = findAssertions parsedExpressions
    mAssertionsF = map (`termToF` variablesWithTypes) assertions

    assertionsF = foldAssertionsF mAssertionsF

    variablesWithTypes = findVariables parsedExpressions

    foldAssertionsF :: [Maybe F] -> Maybe F
    foldAssertionsF []             = Nothing
    foldAssertionsF [f]            = f
    foldAssertionsF (Nothing : _)  = Nothing
    foldAssertionsF (Just f : mfs) =
      case foldAssertionsF mfs of
        Just fs -> Just $ FConn And f fs
        Nothing -> Nothing
    -- contextFAnd = foldl


-- |Derive ranges for a VC (Implication where a CNF implies a goal)
-- Remove anything which refers to a variable for which we cannot derive ranges
-- If the goal contains underivable variables, return Nothing
deriveVCRanges :: F -> [(String, String)] -> ParsingMode -> Maybe (F, TypedVarMap)
deriveVCRanges vc varsWithTypes mode =
  if isModeCNF && not (null underivableVariables)
    then Nothing -- We cannot deal with a CNF if any variable is underivable
    else
      case simplifiedF of
        FConn Impl contextCNF goal ->
          if fContainsVars underivableVariables goal
            then Nothing
            else
              let
                varsInGoal = sort $ findVariablesInFormula goal

                findDependentVars :: F -> [String] -> [String]
                findDependentVars f dependingVars =
                  if dependingVars == foundVarsWithDependingVars
                    then foundVarsWithDependingVars
                    else findDependentVars f foundVarsWithDependingVars
                  where
                    foundVarsWithDependingVars = sort . nub $ dependingVars ++ foundVars
                    foundVars = aux f dependingVars

                    aux (FConn And f1 f2) vars = findDependentVars f1 vars ++ findDependentVars f2 vars
                    aux f1 vars = if fContainsVars vars f1 then findVariablesInFormula f1 else []

                relevantVars   = findDependentVars contextCNF varsInGoal
                irrelevantVars = map fst $ filter (\(v, _) -> v `notElem` relevantVars) derivedVarMap

                relevantDerivedVarMap = filter (\(v, _) -> v `elem` relevantVars) derivedVarMap

                filterCNF (FConn And f1 f2) varsToKeep =
                  case (filterCNF f1 varsToKeep, filterCNF f2 varsToKeep) of
                    (Just filteredF1, Just filteredF2) -> Just (FConn And filteredF1 filteredF2)
                    (Just filteredF1, _)               -> Just filteredF1
                    (_, Just filteredF2)               -> Just filteredF2
                    (Nothing, Nothing)                 -> Nothing
                filterCNF f varsToKeep = if fContainsVars varsToKeep f then Just f else Nothing
              in trace (show irrelevantVars) $
              case filterCNF contextCNF (map fst relevantDerivedVarMap) of
                Just filteredContext  -> Just (FConn Impl filteredContext goal, unsafeVarMapToTypedVarMap relevantDerivedVarMap integerVariables)
                Nothing               -> Just                            (goal, unsafeVarMapToTypedVarMap relevantDerivedVarMap integerVariables)
        goal ->
          if fContainsVars underivableVariables goal
            then Nothing
            else Just (goal, unsafeVarMapToTypedVarMap derivedVarMap integerVariables)
  where
    integerVariables = findIntegerVariables varsWithTypes

    (simplifiedF, derivedVarMap, underivableVariables) = deriveBoundsAndSimplify vc isModeCNF

    isModeCNF =
      case mode of
        Why3 -> False
        CNF  -> True

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

-- |Convert a VC to ECNF, eliminating any floats. 
eliminateFloatsAndConvertVCToECNF :: F -> TypedVarMap -> [[ESafe]]
eliminateFloatsAndConvertVCToECNF (FConn Impl context goal) varMap = -- TODO: Save results from FPTaylor, then lookup
  minMaxAbsEliminatorECNF $
  [
    contextEs ++ goalEs
    |
    contextEs <- map (map (fmapESafe (\e -> eliminateFloats e (typedVarMapToVarMap varMap) True))) (fToECNF (FNot context)),
    goalEs    <- map (map (fmapESafe (\e -> eliminateFloats e (typedVarMapToVarMap varMap) False))) (fToECNF goal)
  ]
-- |If there is no implication, we have a goal with no context or a CNF. We deal with these in the same way
eliminateFloatsAndConvertVCToECNF goal varMap =
  minMaxAbsEliminatorECNF $
  [
    goalEs
    |
    goalEs <- map (map (fmapESafe (\e -> eliminateFloats e (typedVarMapToVarMap varMap) False))) (fToECNF goal)
  ]

parseVCToECNF :: FilePath -> ParsingMode -> Maybe ([[ESafe]], TypedVarMap)
parseVCToECNF filePath mode =
  (\(f, vm) ->
    (simplifyESafeCNF (eliminateFloatsAndConvertVCToECNF (simplifyF f) vm), vm))
    <$> -- VC as F with derived ranges
    ((\(f, vt) -> deriveVCRanges (substAllEqualities (simplifyF f)) vt mode) =<< (processVC . parseSMT2) filePath mode)
