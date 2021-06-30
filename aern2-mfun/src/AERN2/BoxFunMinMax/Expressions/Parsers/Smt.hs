module AERN2.BoxFunMinMax.Expressions.Parsers.Smt where

import MixedTypesNumPrelude
import qualified Prelude as P
import qualified Data.Text.IO
import qualified Data.Text.Internal
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

import AERN2.BoxFunMinMax.VarMap ( VarMap )
import AERN2.BoxFunMinMax.Expressions.DeriveBounds
import AERN2.BoxFunMinMax.Expressions.EliminateFloats
import AERN2.BoxFunMinMax.Expressions.Eliminator (minMaxAbsEliminatorECNF)

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

termToF :: LD.Expression -> Maybe F
termToF (LD.Application (LD.Variable operator) [op]) = -- Single param operators
  case termToE op of -- Ops with E params
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
      case termToF op of
        Just f ->
          case operator of
            "not" -> Just $ FNot f -- TODO: Do we need an FNot for E as well? Answer: We could simply Negate, but we don't need it.
            _ -> Nothing 
        _ -> Nothing
termToF (LD.Application (LD.Variable operator) [op1, op2]) = -- Two param operations
  case (termToE op1, termToE op2) of
    (Just e1, Just e2) ->
      case operator of
        n
          | n `elem` [">=", "fp.geq", "oge", "oge__logic"] -> Just $ FComp Ge e1 e2
          | n `elem` [">",  "fp.gt", "ogt", "ogt__logic"]  -> Just $ FComp Gt e1 e2
          | n `elem` ["<=", "fp.leq", "ole", "ole__logic"] -> Just $ FComp Le e1 e2
          | n `elem` ["<",  "fp.lt", "olt", "olt__logic"]  -> Just $ FComp Lt e1 e2
          | n `elem` ["=",  "fp.eq"]  -> Just $ FComp Eq e1 e2
        _ -> Nothing
    (_, _) -> 
      case (termToF op1, termToF op2) of
        (Just f1, Just f2) ->
          case operator of
            "and" -> Just $ FConn And f1 f2
            "or"  -> Just $ FConn Or f1 f2
            "=>"  -> Just $ FConn Impl f1 f2
            "="   -> Just $ FConn Equiv f1 f2
            _ -> Nothing
        -- Parse ite where it is used as an expression
        (_, _) ->
          case (op1, termToE op2) of
            (LD.Application (LD.Variable "ite") [cond, thenTerm, elseTerm], Just e2) ->
              case (termToF cond, termToE thenTerm, termToE elseTerm) of
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
                    _ -> Nothing
                (_, _, _) -> Nothing
            (_, _) ->
              case (termToE op1, op2) of
                (Just e1, LD.Application (LD.Variable "ite") [cond, thenTerm, elseTerm]) ->
                  case (termToF cond, termToE thenTerm, termToE elseTerm) of
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
                        _ -> Nothing
                    (_, _, _) -> Nothing
                (_, _) -> Nothing
                      
termToF (LD.Application (LD.Variable "ite") [condition, thenTerm, elseTerm]) = -- if-then-else operator with F types
  case (termToF condition, termToF thenTerm, termToF elseTerm) of
    (Just conditionF, Just thenTermF, Just elseTermF) -> Just $ FConn And (FConn Impl conditionF thenTermF) (FConn Impl (FNot conditionF) elseTermF)
    (_, _, _) -> Nothing
termToF (LD.Variable "true")  = Just FTrue
termToF (LD.Variable "false") = Just FFalse
termToF _ = Nothing

termToE :: LD.Expression -> Maybe E
-- Symbols/Literals
termToE (LD.Variable "true")  = Nothing -- These should be parsed to F
termToE (LD.Variable "false") = Nothing -- These should be parsed to F
termToE (LD.Variable var)     =  Just $ Var var
termToE (LD.Number   num)     = Just $ Lit num
-- one param functions
termToE (LD.Application (LD.Variable operator) [op]) =
  case termToE op of
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
termToE (LD.Application (LD.Variable "round") [mode, operand]) = 
  case (parseRoundingMode mode, termToE operand) of
    (Just roundingMode, Just e) -> Just $ Float roundingMode e
    (_, _) -> Nothing
    
-- two param functions where op1 and op2 can be parsed to the E type.
-- Functions with two params which need special handling (i.e. round) should be
-- placed before here
termToE (LD.Application (LD.Variable operator) [op1, op2]) =
  case (termToE op1, termToE op2) of
    (Just e1, Just e2) -> 
      case operator of
        n
          -- "o..." functions are from SPARK Reals
          | n `elem` ["+", "oadd", "oadd__logic"]           -> Just $ EBinOp Add e1 e2
          | n `elem` ["-", "osubtract", "osubtract__logic"] -> Just $ EBinOp Sub e1 e2
          | n `elem` ["*", "omultiply", "omultiply__logic"] -> Just $ EBinOp Mul e1 e2
          | n `elem` ["/", "odivide", "odivide__logic"]     -> Just $ EBinOp Div e1 e2
          | n `elem` ["power", "pow"]                       -> Just $ EBinOp Pow e1 e2
        _                                                   -> Nothing
    (_, _) -> Nothing

-- Float bits to Rational
termToE (LD.Application (LD.Variable "fp") [LD.Variable sSign, LD.Variable sExponent, LD.Variable sMantissa]) =
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
          _       -> Nothing
      else Nothing

-- Float functions, three params. Other three param functions should be placed before here
termToE (LD.Application (LD.Variable operator) [roundingMode, op1, op2]) =
  -- case operator of
  --   -- SPARK Reals
  --   "fp.to_real" -> Nothing 
  --   _ -> -- Known ops
  case (termToE op1, termToE op2) of
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

termToE _ = Nothing

termsToF :: [LD.Expression] -> [F]
termsToF = mapMaybe termToF

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
                                                      vars = findVariablesInExpressions e
                                                      mVariableType = findVariableType vars varTypeMap Nothing
determineFloatTypeE (Float32 r e)     varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ Float32 r p
                                                      Nothing -> Nothing
determineFloatTypeE (Float64 r e)     varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ Float64 r p
                                                      Nothing -> Nothing
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
findVariableType :: [String] -> [(String, String)] -> Maybe String -> Maybe String
findVariableType [] _ mFoundType = mFoundType
findVariableType (v: vs) varTypeMap mFoundType = 
  case lookup v varTypeMap of
    Just t  -> 
      case mFoundType of
        Just f -> if f == t then findVariableType vs varTypeMap (Just t) else Nothing
        Nothing -> findVariableType vs varTypeMap (Just t)
    Nothing -> Nothing
    
findVariablesInExpressions :: E -> [String]
findVariablesInExpressions (EBinOp _ e1 e2) = findVariablesInExpressions e1 ++ findVariablesInExpressions e2
findVariablesInExpressions (EUnOp _ e) = findVariablesInExpressions e
findVariablesInExpressions (PowI e _) = findVariablesInExpressions e
findVariablesInExpressions (Float _ e) = findVariablesInExpressions e
findVariablesInExpressions (Float32 _ e) = findVariablesInExpressions e
findVariablesInExpressions (Float64 _ e) = findVariablesInExpressions e
findVariablesInExpressions (Var v) = [v]
findVariablesInExpressions (Lit _) = []

parseRoundingMode :: LD.Expression -> Maybe RoundingMode 
parseRoundingMode (LD.Variable mode) = 
  case mode of
    m 
      | m `elem` ["RNE", "NearestTiesToEven"] -> Just RNE
      | m `elem` ["RTP", "Up"]                -> Just RTP
      | m `elem` ["RTN", "Down"]              -> Just RTN
      | m `elem` ["RTZ", "ToZero"]            -> Just RTZ
    _                                         -> Nothing
parseRoundingMode _ = Nothing

-- |Process a parsed list of expressions to a VC. Everything in the context implies the goal.
-- If the goal cannot be determined, we return Nothing
processVC  :: [LD.Expression] -> Maybe F
processVC parsedExpressions = 
  trace (show (maybe Nothing termToF mGoal)) $
  trace (show variablesWithTypes) $
  case mGoalF of
    Just goalF  -> if null contextF then Just goalF else Just $ FConn Impl (foldContextF contextF) goalF
    Nothing     -> Nothing
  where
    (goalWithNot, context) = (takeGoalFromAssertions . findAssertions) parsedExpressions
    
    mGoal = 
      case goalWithNot of
        LD.Application (LD.Variable "not") [operand] -> Just operand -- Goals in SMT look like this
        _ -> Nothing

    contextF            = mapMaybe (`determineFloatTypeF` variablesWithTypes) $ termsToF context
    mGoalF              = maybe Nothing (`determineFloatTypeF` variablesWithTypes) $ maybe Nothing termToF mGoal
    variablesWithTypes  = findVariables parsedExpressions

    foldContextF :: [F] -> F
    foldContextF []       = error "processVC - foldContextF: Empty list given"
    foldContextF [f]      = f
    foldContextF (f : fs) = FConn And f (foldContextF fs)

    -- contextFAnd = foldl

-- |Derive ranges for a VC (Implication where a CNF implies a goal)
-- Remove anything which refers to a variable for which we cannot derive ranges
-- If the goal contains underivable variables, return Nothing
deriveVCRanges :: F -> Maybe (F, VarMap)
deriveVCRanges vc =
  case simplifiedF of
    FConn Impl contextCNF goal ->
      if fContainsVars underivableVariables goal
        then Nothing
        else
          case filterCNF contextCNF of
            Just filteredContext  -> Just (FConn Impl filteredContext goal, derivedVarMap)
            Nothing               -> Just                            (goal, derivedVarMap)
    goal ->
      if fContainsVars underivableVariables goal
        then Nothing
        else Just (goal, derivedVarMap)
  where
    (simplifiedF, derivedVarMap, underivableVariables) = deriveBoundsAndSimplify vc

    filterCNF :: F -> Maybe F
    filterCNF (FConn And f1 f2) = 
      if fContainsVars underivableVariables f1 
        then filterCNF f2 
        else 
          case filterCNF f2 of
            Just goodF2 -> Just $ FConn And f1 goodF2
            Nothing     -> Just f1
    filterCNF f = if fContainsVars underivableVariables f then Nothing else Just f

    eContainsVars :: [String] -> E -> Bool
    eContainsVars vars (Var var)        = var `elem` vars
    eContainsVars _ (Lit _)             = False
    eContainsVars vars (EBinOp _ e1 e2) = eContainsVars vars e1 || eContainsVars vars e2  
    eContainsVars vars (EUnOp _ e)      = eContainsVars vars e  
    eContainsVars vars (PowI e _)       = eContainsVars vars e  
    eContainsVars vars (Float32 _ e)    = eContainsVars vars e  
    eContainsVars vars (Float64 _ e)    = eContainsVars vars e  
    eContainsVars vars (Float _ e)      = eContainsVars vars e  

    fContainsVars :: [String] -> F -> Bool
    fContainsVars vars (FConn _ f1 f2)  = fContainsVars vars f1 || fContainsVars vars f2
    fContainsVars vars (FComp _ e1 e2)  = eContainsVars vars e1 || eContainsVars vars e2
    fContainsVars vars (FNot f)         = fContainsVars vars f
    fContainsVars _ FTrue               = False
    fContainsVars _ FFalse              = False

inequalityEpsilon :: Rational
inequalityEpsilon = 1/(2^52)  -- Epsilon for double precision floats

-- |Convert a VC to ECNF, eliminating any floats. 
eliminateFloatsAndConvertVCToECNF :: F -> VarMap -> [[E]]
eliminateFloatsAndConvertVCToECNF (FConn Impl context goal) varMap =
  minMaxAbsEliminatorECNF inequalityEpsilon $
  [
    contextEs ++ goalEs 
    | 
    contextEs <- map (map (\e -> eliminateFloats e varMap True)) (fToECNF (FNot context) inequalityEpsilon), 
    goalEs    <- map (map (\e -> eliminateFloats e varMap False)) (fToECNF goal inequalityEpsilon)
  ]
-- |If there is no implication, we have a goal with no context
eliminateFloatsAndConvertVCToECNF goal varMap = 
  minMaxAbsEliminatorECNF inequalityEpsilon $
  [
    goalEs
    |
    goalEs <- map (map (\e -> eliminateFloats e varMap False)) (fToECNF goal inequalityEpsilon)
  ]
