module AERN2.BoxFunMinMax.Expressions.EliminateFloats where

import MixedTypesNumPrelude
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Expressions.Translators.FPTaylor
import System.Process
import System.IO.Unsafe
import AERN2.BoxFunMinMax.VarMap (VarMap)
import System.Exit 

fpTaylorPath :: FilePath 
fpTaylorPath = "/home/junaid/Research/git/FPTaylor/fptaylor"

removeFloats :: E -> E
removeFloats (Float _ e)       = removeFloats e
removeFloats (Float32 _ e)     = removeFloats e
removeFloats (Float64 _ e)     = removeFloats e
removeFloats (EBinOp op e1 e2) = EBinOp op (removeFloats e1) (removeFloats e2)
removeFloats (EUnOp op e)      = EUnOp op (removeFloats e)
removeFloats (PowI e i)        = PowI (removeFloats e) i
removeFloats (Lit v)           = Lit v
removeFloats (Var v)           = Var v
removeFloats Pi                = Pi
removeFloats (RoundToInteger m e) = RoundToInteger m (removeFloats e)

-- Bool True means 'strengthen' the formula, i.e. rnd(x) >= rnd(y) becomes x - rndErrorX >= y + rndErrorY.
-- Bool False means 'weaken' the formula , i.e. rnd(x) >= rnd(y) becomes x + rndErrorX >= y - rndErrorY
-- Eqs are turned into <= and >=, and then floats are eliminated
-- TODO: Test Max,Min
eliminateFloatsF :: F -> VarMap -> Bool -> F
eliminateFloatsF (FConn op f1 f2) varMap strengthenFormula = FConn op (eliminateFloatsF f1 varMap strengthenFormula) (eliminateFloatsF f2 varMap strengthenFormula)
eliminateFloatsF (FNot f) varMap strengthenFormula = FNot (eliminateFloatsF f varMap strengthenFormula)
eliminateFloatsF (FComp op e1 e2) varMap True = 
  case op of
    Gt -> FComp op (eliminateFloats e1 varMap False) (eliminateFloats e2 varMap True)
    Ge -> FComp op (eliminateFloats e1 varMap False) (eliminateFloats e2 varMap True)
    Lt -> FComp op (eliminateFloats e1 varMap True) (eliminateFloats e2 varMap False)
    Le -> FComp op (eliminateFloats e1 varMap True) (eliminateFloats e2 varMap False)
    Eq -> 
      eliminateFloatsF
      (FConn And
        (FComp Ge e1 e2)
        (FComp Le e1 e2))
      varMap
      True
eliminateFloatsF (FComp op e1 e2) varMap False = 
  case op of
    Gt -> FComp op (eliminateFloats e1 varMap True) (eliminateFloats e2 varMap False)
    Ge -> FComp op (eliminateFloats e1 varMap True) (eliminateFloats e2 varMap False)
    Lt -> FComp op (eliminateFloats e1 varMap False) (eliminateFloats e2 varMap True)
    Le -> FComp op (eliminateFloats e1 varMap False) (eliminateFloats e2 varMap True)
    Eq -> 
      eliminateFloatsF
      (FConn And
        (FComp Ge e1 e2)
        (FComp Le e1 e2))
      varMap
      False
eliminateFloatsF FTrue _ _ = FTrue
eliminateFloatsF FFalse _ _ = FFalse 

eliminateFloats :: E -> VarMap -> Bool -> E
eliminateFloats e@(Float _ _) _ _ = error $ "Cannot eliminate Float, precision unknown. Expression: " ++ show e
eliminateFloats floatE@(Float32 _ e) varMap addError = 
  if addError 
    then EBinOp Add eWithotFloats (Lit absoluteError) 
    else EBinOp Sub eWithotFloats (Lit absoluteError) 
  where
    absoluteError = unsafePerformIO $ findAbsoluteErrorUsingFPTaylor floatE varMap
    eWithotFloats = removeFloats e
eliminateFloats floatE@(Float64 _ e) varMap addError = 
  if addError 
    then EBinOp Add eWithotFloats (Lit absoluteError) 
    else EBinOp Sub eWithotFloats (Lit absoluteError) 
  where
    absoluteError = unsafePerformIO $ findAbsoluteErrorUsingFPTaylor floatE varMap
    eWithotFloats = removeFloats e
eliminateFloats (EBinOp op e1 e2) varMap addError = EBinOp op (eliminateFloats e1 varMap addError) (eliminateFloats e2 varMap addError)
eliminateFloats (EUnOp op e) varMap addError = EUnOp op (eliminateFloats e varMap addError)
eliminateFloats (Lit v) _ _ = Lit v
eliminateFloats (Var v) _ _ = Var v
eliminateFloats Pi      _ _ = Pi
eliminateFloats (PowI e i) _ _ = PowI e i
eliminateFloats (RoundToInteger m e) varMap addError = RoundToInteger m (eliminateFloats e varMap addError)

-- |Made for Float32/64 expressions
findAbsoluteErrorUsingFPTaylor :: E -> VarMap -> IO Rational
findAbsoluteErrorUsingFPTaylor e varMap =
  do
    writeFile fptaylorFile fptaylorInput --TODO: Make this safer by either writing to different files on repeated calls, or by sending the input directly to FPTaylor
    (exitCode, output, errDetails) <- readProcessWithExitCode fpTaylorPath [fptaylorFile] []
    -- removeFile fptaylorFile
    case exitCode of
      ExitSuccess -> 
        case parseFPTaylorRational output of
          Just result -> return result
          Nothing     -> error "Could not parse FPTaylor output"
      ExitFailure _   -> error $ "Error when running FPTaylor on generated fptaylor.txt. Error message: " ++ show errDetails
  where
    fptaylorInput = expressionWithVarMapToFPTaylor e varMap
    fptaylorFile = "fptaylor.txt"
