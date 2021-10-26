module AERN2.BoxFunMinMax.Expressions.EliminateFloats where

import MixedTypesNumPrelude
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Expressions.Translators.FPTaylor
import System.Process
import System.IO.Unsafe
import AERN2.BoxFunMinMax.VarMap (VarMap)
import System.Exit 
import System.IO.Temp
import GHC.IO.Handle

-- FIXME: Use system var which points to fptaylor bin
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
eliminateFloatsF :: F -> VarMap -> Bool -> IO F
eliminateFloatsF (FConn op f1 f2) varMap strengthenFormula = FConn op <$> eliminateFloatsF f1 varMap strengthenFormula <*> eliminateFloatsF f2 varMap strengthenFormula
eliminateFloatsF (FNot f) varMap strengthenFormula = FNot <$> eliminateFloatsF f varMap strengthenFormula
eliminateFloatsF (FComp op e1 e2) varMap True = 
  case op of
    Gt -> FComp op <$> eliminateFloats e1 varMap False <*> eliminateFloats e2 varMap True
    Ge -> FComp op <$> eliminateFloats e1 varMap False <*> eliminateFloats e2 varMap True
    Lt -> FComp op <$> eliminateFloats e1 varMap True <*> eliminateFloats e2 varMap False
    Le -> FComp op <$> eliminateFloats e1 varMap True <*> eliminateFloats e2 varMap False
    Eq -> 
      eliminateFloatsF
      (FConn And
        (FComp Ge e1 e2)
        (FComp Le e1 e2))
      varMap
      True
eliminateFloatsF (FComp op e1 e2) varMap False = 
  case op of
    Gt -> FComp op <$> eliminateFloats e1 varMap True <*> eliminateFloats e2 varMap False
    Ge -> FComp op <$> eliminateFloats e1 varMap True <*> eliminateFloats e2 varMap False
    Lt -> FComp op <$> eliminateFloats e1 varMap False <*> eliminateFloats e2 varMap True
    Le -> FComp op <$> eliminateFloats e1 varMap False <*> eliminateFloats e2 varMap True
    Eq -> 
      eliminateFloatsF
      (FConn And
        (FComp Ge e1 e2)
        (FComp Le e1 e2))
      varMap
      False
eliminateFloatsF FTrue _ _ = return FTrue
eliminateFloatsF FFalse _ _ = return FFalse 

eliminateFloats :: E -> VarMap -> Bool -> IO E
eliminateFloats e@(Float _ _) _ _ = error $ "Cannot eliminate Float, precision unknown. Expression: " ++ show e
eliminateFloats floatE@(Float32 _ e) varMap addError = do
  absoluteError <- findAbsoluteErrorUsingFPTaylor floatE varMap
  if addError 
    then return $ EBinOp Add eWithotFloats (Lit absoluteError) 
    else return $ EBinOp Sub eWithotFloats (Lit absoluteError) 
  where
    eWithotFloats = removeFloats e
eliminateFloats floatE@(Float64 _ e) varMap addError = do
  absoluteError <- findAbsoluteErrorUsingFPTaylor floatE varMap
  if addError 
    then return $ EBinOp Add eWithotFloats (Lit absoluteError)
    else return $ EBinOp Sub eWithotFloats (Lit absoluteError) 
  where
    eWithotFloats = removeFloats e
eliminateFloats (EBinOp op e1 e2) varMap addError = EBinOp op <$> eliminateFloats e1 varMap addError <*> eliminateFloats e2 varMap addError
eliminateFloats (EUnOp op e) varMap addError = EUnOp op <$> eliminateFloats e varMap addError
eliminateFloats (Lit v) _ _ = return $ Lit v
eliminateFloats (Var v) _ _ = return $ Var v
eliminateFloats Pi      _ _ = return Pi
eliminateFloats (PowI e i) _ _ = return $ PowI e i
eliminateFloats (RoundToInteger m e) varMap addError = RoundToInteger m <$> eliminateFloats e varMap addError

-- |Made for Float32/64 expressions
findAbsoluteErrorUsingFPTaylor :: E -> VarMap -> IO Rational
findAbsoluteErrorUsingFPTaylor e varMap =
  do
    (exitCode, output, errDetails) <- withSystemTempFile "fptaylor" handleFPTaylorFile 
    case exitCode of
      ExitSuccess -> 
        case parseFPTaylorRational output of
          Just result -> return result
          Nothing     -> error $ "Could not parse FPTaylor output" ++ show output
      ExitFailure _   -> error $ "Error when running FPTaylor on generated fptaylor.txt. Error message: " ++ show errDetails
  where
    fptaylorInput = expressionWithVarMapToFPTaylor e varMap
    -- fptaylorFile = "fptaylor.txt"
    handleFPTaylorFile filePath fileHandle = 
      do 
        hPutStr fileHandle fptaylorInput
        _ <- hGetContents fileHandle -- Ensure handler is finished writing before calling FPTaylor
        readProcessWithExitCode fpTaylorPath [filePath] []


