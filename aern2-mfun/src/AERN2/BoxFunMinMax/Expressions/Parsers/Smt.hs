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

import Data.List.NonEmpty
import Debug.Trace

import qualified AERN2.BoxFunMinMax.Expressions.Parsers.Lisp.Parser as LispParser
import qualified AERN2.BoxFunMinMax.Expressions.Parsers.Lisp.DataTypes as LispDataTypes

parser :: String -> [LispDataTypes.Expression]
parser = LispParser.analyzeExpressionSequence . LispParser.parseSequence . LispParser.tokenize

parseSMT2 :: FilePath -> [LispDataTypes.Expression]
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

findAssertions :: Script -> [Command]
findAssertions [] = []
findAssertions (p : ps) =
  case p of
    Assert _ -> p : findAssertions ps
    _ -> findAssertions ps

findGoalsInAssertions :: [Command] -> [Command]
findGoalsInAssertions [] = []
findGoalsInAssertions (p : ps) = 
  case p of
    Assert (TermApplication (Unqualified (IdSymbol s)) _) -> 
      if s P.== T.pack "not"
        then p : findGoalsInAssertions ps
        else findGoalsInAssertions ps
    _ -> findGoalsInAssertions ps

findGoals :: Script -> [Command]
findGoals [] = []
findGoals (p : ps) =
  case p of
    Assert (TermApplication (Unqualified s@(IdSymbol _)) _) -> 
      if s P.== IdSymbol (T.pack "not")
        then p : findGoalsInAssertions ps
        else findGoalsInAssertions ps
    _ -> findGoalsInAssertions ps

goalToF :: Command -> F
goalToF (Assert goal) = termApplicationToF goal
goalToF _ = undefined

termApplicationToF :: Term -> F
termApplicationToF (TermApplication (Unqualified (IdSymbol f)) terms) =
  case T.unpack f of
    "not" ->
      case terms of
        (term :| []) -> FNot (termApplicationToF term)
        _ -> undefined -- Left errorMsg
    "tqtisFinite" ->
      case terms of
        (term :| []) -> 
          let
            maxFloat = (2.0-(1/!2^!23))*2^!127
            minFloat = negate maxFloat
            e = termApplicationToE term
          in
            FConn And (FComp Ge (Lit minFloat) e)  (FComp Le e (Lit maxFloat))
        _ -> undefined
      -- undefined --FNot (termsToExpression terms)
    _ -> trace (show f) undefined -- Left errorMsg
termApplicationToF _ = undefined

termApplicationToE :: Term -> E
termApplicationToE (TermApplication (Unqualified (IdSymbol f)) terms) =
  case T.unpack f of
    "div1" -> 
      case terms of
        (TermQualIdentifier (Unqualified (IdSymbol roundingMode)) :| (var1 : [var2])) ->
          EBinOp Div (termQualToEVars var1) (termQualToEVars var2)
        _ -> undefined
    _ -> undefined
termApplicationToE _ = undefined

termQualToEVars :: Term -> E
termQualToEVars (TermQualIdentifier (Unqualified (IdSymbol v))) = Var (T.unpack v)
termQualToEVars _ = undefined

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