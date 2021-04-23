module AERN2.BoxFunMinMax.Expressions.Parsers.Smt where

import MixedTypesNumPrelude
import qualified Prelude as P
import Language.SMT2.Parser
import Language.SMT2.Syntax
import qualified Data.Text as T
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
import Data.Maybe (mapMaybe, isJust, maybe)

import qualified Data.List as L

import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.DeriveBounds
import AERN2.BoxFunMinMax.Expressions.EliminateFloats
import AERN2.BoxFunMinMax.Expressions.Eliminator (minMaxAbsEliminatorECNF)

parser :: String -> [LD.Expression]
parser = LP.analyzeExpressionSequence . LP.parseSequence . LP.tokenize

parseSMT2 :: FilePath -> [LD.Expression]
parseSMT2 filePath = parser . unsafePerformIO $ P.readFile filePath

{-
parsedtmp = parseSMT "/home/junaid/Research/git/aern2-base/aern2/aern2-mfun/src/AERN2/BoxFunMinMax/Expressions/Parsers/heron__heron-Heron__heron__subprogram_def-VC_def1.smt2"
parsed = Data.List.head (rights [parsedtmp])
goals = findGoals parsed
fGoal = goalToF (Data.List.head goals) 
-}
parseSMT :: FilePath -> Either Data.Text.Internal.Text Script
parseSMT filePath = parseFileMsg script fileAsText
  where
    fileAsText = unsafePerformIO $ Data.Text.IO.readFile filePath

-- -- |Finds assertions from [LD.Expression]
-- -- We keep whatever is within the assertions (operands)
-- -- If the assertion contains a 'not' at the top level, it is a goal
-- -- Goals are stored without the top level 'not'
-- processAssertions :: [LD.Expression] -> [LD.Expression] 
-- processAssertions [] = []
-- processAssertions ((LD.Application (LD.Variable "assert") operands) : expressions) =
--   let 
--     assertion = head operands
--   in 
--     case assertion of
--       ((LD.Application (LD.Variable "not") goal)) -> goal : processAssertions expressions
--       _ -> assertion : processAssertions expressions -- Take head of operands since assert has only one operand
-- processAssertions (_ : expressions) = processAssertions expressions

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
    goal = last asserts
    assertsWithoutGoal = take (numberOfAssertions - 1) asserts

-- goalToF :: LD.Expression -> F
-- goalToF goal = FNot (termToF goal)

termToF :: LD.Expression -> Maybe F
termToF (LD.Application (LD.Variable operator) [op]) = -- Single param operators
  case termToE op of -- Ops with E params
    Just e ->
      case operator of
        "fp.isFinite32" ->
          let maxFloat = (2.0-(1/!2^!23))*2^!127
              minFloat = negate maxFloat
          in
            Just $ FConn And (FComp Le (Lit minFloat) e)  (FComp Le e (Lit maxFloat))
        "fp.isFinite64" ->
          let maxFloat = (2.0-(1/!2^!53))*2^!1023
              minFloat = negate maxFloat
          in
            Just $ FConn And (FComp Le (Lit minFloat) e)  (FComp Le e (Lit maxFloat))
        _ -> Nothing
    Nothing -> 
      case termToF op of
        Just f ->
          case operator of
            "not" -> Just $ FNot f -- TODO: Do we need an FNot for E as well?
            _ -> Nothing 
        _ -> Nothing
termToF (LD.Application (LD.Variable operator) [op1, op2]) = -- Two param operations
  case termToE op1 of
    Just e1 ->
      case termToE op2 of
        Just e2 ->
          case operator of
            n
              | n `elem` [">=", "fp.geq"] -> Just $ FComp Ge e1 e2
              | n `elem` [">",  "fp.gt"]  -> Just $ FComp Gt e1 e2
              | n `elem` ["<=", "fp.leq"] -> Just $ FComp Le e1 e2
              | n `elem` ["<",  "fp.lt"]  -> Just $ FComp Lt e1 e2
              | n `elem` ["=",  "fp.eq"]  -> Just $ FComp Eq e1 e2
            _ -> Nothing
        Nothing -> Nothing
    Nothing -> 
      case termToF op1 of
        Just f1 ->
          case termToF op2 of
            Just f2 ->
              case operator of
                "and" -> Just $ FConn And f1 f2
                "or"  -> Just $ FConn Or f1 f2
                "=>"  -> Just $ FConn Impl f1 f2
            Nothing -> Nothing
        Nothing -> Nothing
termToF _ = Nothing

termToE :: LD.Expression -> Maybe E
-- Symbols/Literals
termToE (LD.Variable var) = Just $ Var var
termToE (LD.Number   num) = Just $ Lit num
-- one param functions
termToE (LD.Application (LD.Variable operator) [op]) =
  case termToE op of
    Nothing -> Nothing
    Just e -> case operator of
      -- Float functions
      "fp.abs"          -> Just $ EUnOp Abs e
      "fp.neg"          -> Just $ EUnOp Negate e
      "fp.to_real"      -> Just e
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
-- Float functions, two params
-- Float bits to Rational
termToE (LD.Application (LD.Variable "fp") o@[LD.Variable sSign, LD.Variable sExponent, LD.Variable sMantissa]) =
  let
    bSign     = drop 2 sSign
    bExponent = drop 2 sExponent
    bMantissa = drop 2 sMantissa

    bFull = bSign ++ bExponent ++ bMantissa

    -- Read a string of Bits ('1' or '0') where the first digit is the most significant
    -- The digit parameter denotes the current digit, should be equal to length of the first param at all times
    readBits :: String -> Integer -> Integer
    readBits [] _ = 0
    readBits (bit : bits) digit = digitToInt bit * (2 ^! (digit - 1)) + readBits bits (digit - 1) 
    
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
    -- if all (`elem` "01") (bSign ++ bExponent ++ bMantissa) 
    --   then
    --     let pSign = if bSign == "1" then 1 else -1
    --         pMantissa = readBits bMantissa (length bMantissa)
    --     in
    --       case length bExponent of
    --         8 -> -- Single precision
    --           let exponentBias = 127
    --               pExponent = readBits bExponent 8
    --               pExponentWithBias = pExponent - exponentBias
    --           in  Just $ Lit $ rational $ pSign * 2^!pExponent * pMantissa
    --         11 -> undefined
    --   else Nothing

  -- case bSign of
-- Float functions, three params
termToE (LD.Application (LD.Variable operator) [roundingMode, op1, op2]) =
  case operator of
    -- Undefined functions ops
    "fp.to_real" -> Nothing 
    _ -> -- Known ops
      case termToE op1 of
        Just e1 ->
          case termToE op2 of
            Just e2 -> 
              case parseRoundingMode roundingMode of -- Floating-point ops
                Just mode ->
                  case operator of
                    "fp.add" -> Just $ Float mode $ EBinOp Add e1 e2
                    "fp.sub" -> Just $ Float mode $ EBinOp Sub e1 e2
                    "fp.mul" -> Just $ Float mode $ EBinOp Mul e1 e2
                    "fp.div" -> Just $ Float mode $ EBinOp Div e1 e2
                    _        -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing

termToE (LD.Variable var) = Just $ Var var
termToE _ = Nothing

termsToF :: [LD.Expression] -> [F]
termsToF = mapMaybe termToF

determineFloatTypeE :: E -> [(String, String)] -> Maybe E
determineFloatTypeE (EBinOp op e1 e2) varTypeMap  = case determineFloatTypeE e1 varTypeMap of
                                                      Just p1 ->
                                                        case determineFloatTypeE e2 varTypeMap of
                                                          Just p2 -> Just $ EBinOp op p1 p2                       
determineFloatTypeE (EUnOp op e)      varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ EUnOp op p 
determineFloatTypeE (PowI e i)        varTypeMap  = case determineFloatTypeE e varTypeMap of
                                                      Just p -> Just $ PowI p i 
determineFloatTypeE (Float r e)       varTypeMap  = case mVariableType of
                                                      Just "Float32" ->
                                                        case determineFloatTypeE e varTypeMap of
                                                          Just p -> Just $ Float32 r p
                                                          Nothing -> Nothing
                                                      Just "Float64" ->
                                                        case determineFloatTypeE e varTypeMap of
                                                          Just p -> Just $ Float64 r p
                                                          Nothing -> Nothing
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
determineFloatTypeE (Var v)           varTypeMap  = Just (Var v)
determineFloatTypeE (Lit n)           varTypeMap  = Just (Lit n)

-- |Tries to determine whether a Float operation is single or double precision
-- by searching for the type of all variables appearing in the function. If the
-- types match and are all either Float32/Float64, we can determine the type.
determineFloatTypeF :: F -> [(String, String)] -> Maybe F
determineFloatTypeF (FComp op e1 e2) varTypeMap = case determineFloatTypeE e1 varTypeMap of
                                                  Just p1 ->
                                                    case determineFloatTypeE e2 varTypeMap of
                                                      Just p2 -> Just $ FComp op p1 p2
                                                      Nothing -> Nothing
                                                  Nothing -> Nothing
determineFloatTypeF (FConn op f1 f2) varTypeMap = case determineFloatTypeF f1 varTypeMap of
                                                  Just p1 ->
                                                    case determineFloatTypeF f2 varTypeMap of
                                                      Just p2 -> Just $ FConn op p1 p2
                                                      Nothing -> Nothing
                                                  Nothing -> Nothing
determineFloatTypeF (FNot f)         varTypeMap = case determineFloatTypeF f varTypeMap of
                                                  Just p -> Just $ FNot p
                                                  Nothing -> Nothing
        
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
parseRoundingMode (LD.Variable "RNE") = Just RNE
parseRoundingMode (LD.Variable "RTP") = Just RTP
parseRoundingMode (LD.Variable "RTN") = Just RTN
parseRoundingMode (LD.Variable "RTZ") = Just RTZ
parseRoundingMode _ = Nothing

-- |Process a parsed list of expressions to a VC. Everything in the context implies the goal.
-- If the goal cannot be determined, we return Nothing
processVC  :: [LD.Expression] -> Maybe F
processVC parsedExpressions = 
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
deriveVCRanges vc@(FConn Impl contextCNF goal) =
  if fContainsVars underivableVariables goal
    then Nothing
    else
      case filterCNF contextCNF of
        Just filteredContext  -> Just (FConn Impl filteredContext goal, derivedVarMap)
        Nothing               -> Just                            (goal, derivedVarMap)
  where
    (derivedVarMap, underivableVariables) = deriveBounds vc

    -- FConn And good1 (FConn And good2 bad)
    -- FConn And good1 (FConn And good2 good3)
    -- FConn And good1 (FConn And bad good2)
    -- FConn And bad (FConn And good2 good3)
    -- FConn And bad (FConn And bad bad)
    -- FConn And bad (FConn And bad good)

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
    eContainsVars vars (PowI e _)       = eContainsVars vars e  
    eContainsVars vars (Float32 _ e)    = eContainsVars vars e  
    eContainsVars vars (Float64 _ e)    = eContainsVars vars e  
    eContainsVars vars (Float _ e)      = eContainsVars vars e  

    fContainsVars :: [String] -> F -> Bool
    fContainsVars vars (FConn _ f1 f2)  = fContainsVars vars f1 || fContainsVars vars f2
    fContainsVars vars (FComp _ e1 e2)  = eContainsVars vars e1 || eContainsVars vars e2
    fContainsVars vars (FNot f)         = fContainsVars vars f

-- |Convert a VC to ECNF, eliminating any floats. 
eliminateFloatsAndConvertVCToECNF :: F -> VarMap -> [[E]]
eliminateFloatsAndConvertVCToECNF (FConn Impl context goal) varMap =
  minMaxAbsEliminatorECNF $
  [
    contextEs ++ goalEs 
    | 
    contextEs <- map (map (\e -> EUnOp Negate (eliminateFloats e varMap True))) (fToECNF context), 
    goalEs    <- map (map (\e -> eliminateFloats e varMap False)) (fToECNF goal)
  ]


-- findAssertions :: Script -> [Command]
-- findAssertions [] = []
-- findAssertions (p : ps) =
--   case p of
--     Assert _ -> p : findAssertions ps
--     _ -> findAssertions ps

-- findGoalsInAssertions :: [Command] -> [Command]
-- findGoalsInAssertions [] = []
-- findGoalsInAssertions (p : ps) = 
--   case p of
--     Assert (TermApplication (Unqualified (IdSymbol s)) _) -> 
--       if s P.== T.pack "not"
--         then p : findGoalsInAssertions ps
--         else findGoalsInAssertions ps
--     _ -> findGoalsInAssertions ps

-- findGoals :: Script -> [Command]
-- findGoals [] = []
-- findGoals (p : ps) =
--   case p of
--     Assert (TermApplication (Unqualified s@(IdSymbol _)) _) -> 
--       if s P.== IdSymbol (T.pack "not")
--         then p : findGoalsInAssertions ps
--         else findGoalsInAssertions ps
--     _ -> findGoalsInAssertions ps

-- goalToF :: Command -> F
-- goalToF (Assert goal) = termApplicationToF goal
-- goalToF _ = undefined

-- termApplicationToF :: Term -> F
-- termApplicationToF (TermApplication (Unqualified (IdSymbol f)) terms) =
--   case T.unpack f of
--     "not" ->
--       case terms of
--         (term :| []) -> FNot (termApplicationToF term)
--         _ -> undefined -- Left errorMsg
--     "tqtisFinite" ->
--       case terms of
--         (term :| []) -> 
--           let
--             maxFloat = (2.0-(1/!2^!23))*2^!127
--             minFloat = negate maxFloat
--             e = termApplicationToE term
--           in
--             FConn And (FComp Ge (Lit minFloat) e)  (FComp Le e (Lit maxFloat))
--         _ -> undefined
--       -- undefined --FNot (termsToExpression terms)
--     _ -> trace (show f) undefined -- Left errorMsg
-- termApplicationToF _ = undefined

-- termApplicationToE :: Term -> E
-- termApplicationToE (TermApplication (Unqualified (IdSymbol f)) terms) =
--   case T.unpack f of
--     "div1" -> 
--       case terms of
--         (TermQualIdentifier (Unqualified (IdSymbol roundingMode)) :| (var1 : [var2])) ->
--           EBinOp Div (termQualToEVars var1) (termQualToEVars var2)
--         _ -> undefined
--     _ -> undefined
-- termApplicationToE _ = undefined

-- termQualToEVars :: Term -> E
-- termQualToEVars (TermQualIdentifier (Unqualified (IdSymbol v))) = Var (T.unpack v)
-- termQualToEVars _ = undefined

-- roundedOpsToE :: Term -> E
-- roundedOpsToE = (TermApplication (Unqualified (IdSymbol f)) terms)

-- termsToExpression :: NonEmpty Term -> F
-- termsToExpression (t :| []) = undefined
-- termsToExpression (t :| ts) = 
--   case t of
--     t@(TermApplication _ _) 




{-
  Assert 
    (TermApplication (Unqualified (IdSymbol "not")) 
    (TermApplication (Unqualified (IdSymbol "tqtisFinite")) 
    (TermApplication (Unqualified (IdSymbol "div1")) (TermQualIdentifier (Unqualified (IdSymbol "RNE1")) 
    :| [TermQualIdentifier (Unqualified (IdSymbol "x")),TermQualIdentifier (Unqualified (IdSymbol "y"))]) 
    :| []) 
    :| []))

  Parsing this
    identify top level not (done)
    Translating tqtIsFinite to the expression MinFloat <= secondParam of TermApplication (recurse here) <= MaxFloat
    When finding div1 the next term application indicates rounding mode
      The next two term applications indicates variables
-}