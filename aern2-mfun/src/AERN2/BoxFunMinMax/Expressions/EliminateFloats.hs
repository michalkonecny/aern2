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
eliminateFloatsF :: F -> VarMap -> Bool -> FilePath -> IO F
eliminateFloatsF (FConn op f1 f2) varMap strengthenFormula fptaylorPath = FConn op <$> eliminateFloatsF f1 varMap strengthenFormula fptaylorPath <*> eliminateFloatsF f2 varMap strengthenFormula fptaylorPath
eliminateFloatsF (FNot f) varMap strengthenFormula fptaylorPath = FNot <$> eliminateFloatsF f varMap strengthenFormula fptaylorPath
eliminateFloatsF (FComp op e1 e2) varMap True fptaylorPath = 
  case op of
    Gt -> FComp op <$> eliminateFloats e1 varMap False fptaylorPath <*> eliminateFloats e2 varMap True fptaylorPath
    Ge -> FComp op <$> eliminateFloats e1 varMap False fptaylorPath <*> eliminateFloats e2 varMap True fptaylorPath
    Lt -> FComp op <$> eliminateFloats e1 varMap True fptaylorPath <*> eliminateFloats e2 varMap False fptaylorPath
    Le -> FComp op <$> eliminateFloats e1 varMap True fptaylorPath <*> eliminateFloats e2 varMap False fptaylorPath
    Eq -> 
      eliminateFloatsF
      (FConn And
        (FComp Ge e1 e2)
        (FComp Le e1 e2))
      varMap
      True
      fptaylorPath
eliminateFloatsF (FComp op e1 e2) varMap False fptaylorPath = 
  case op of
    Gt -> FComp op <$> eliminateFloats e1 varMap True fptaylorPath <*> eliminateFloats e2 varMap False fptaylorPath
    Ge -> FComp op <$> eliminateFloats e1 varMap True fptaylorPath <*> eliminateFloats e2 varMap False fptaylorPath
    Lt -> FComp op <$> eliminateFloats e1 varMap False fptaylorPath <*> eliminateFloats e2 varMap True fptaylorPath
    Le -> FComp op <$> eliminateFloats e1 varMap False fptaylorPath <*> eliminateFloats e2 varMap True fptaylorPath
    Eq -> 
      eliminateFloatsF
      (FConn And
        (FComp Ge e1 e2)
        (FComp Le e1 e2))
      varMap
      False
      fptaylorPath
eliminateFloatsF FTrue _ _ _ = return FTrue
eliminateFloatsF FFalse _ _ _ = return FFalse 

eliminateFloats :: E -> VarMap -> Bool -> FilePath -> IO E
eliminateFloats e@(Float _ _) _ _ _ = error $ "Cannot eliminate Float, precision unknown. Expression: " ++ show e
eliminateFloats floatE@(Float32 _ e) varMap addError fptaylorPath = do
  absoluteError <- findAbsoluteErrorUsingFPTaylor floatE varMap fptaylorPath
  if addError 
    then return $ EBinOp Add eWithotFloats (Lit absoluteError) 
    else return $ EBinOp Sub eWithotFloats (Lit absoluteError) 
  where
    eWithotFloats = removeFloats e
eliminateFloats floatE@(Float64 _ e) varMap addError fptaylorPath = do
  absoluteError <- findAbsoluteErrorUsingFPTaylor floatE varMap fptaylorPath
  if addError 
    then return $ EBinOp Add eWithotFloats (Lit absoluteError)
    else return $ EBinOp Sub eWithotFloats (Lit absoluteError) 
  where
    eWithotFloats = removeFloats e
eliminateFloats (EBinOp op e1 e2) varMap addError fptaylorPath = EBinOp op <$> eliminateFloats e1 varMap addError fptaylorPath <*> eliminateFloats e2 varMap addError fptaylorPath
eliminateFloats (EUnOp op e) varMap addError fptaylorPath = EUnOp op <$> eliminateFloats e varMap addError fptaylorPath
eliminateFloats (Lit v) _ _ _ = return $ Lit v
eliminateFloats (Var v) _ _ _ = return $ Var v
eliminateFloats Pi      _ _ _ = return Pi
eliminateFloats (PowI e i) _ _ _ = return $ PowI e i
eliminateFloats (RoundToInteger m e) varMap addError fptaylorPath = RoundToInteger m <$> eliminateFloats e varMap addError fptaylorPath

-- |Made for Float32/64 expressions
findAbsoluteErrorUsingFPTaylor :: E -> VarMap -> FilePath -> IO Rational
findAbsoluteErrorUsingFPTaylor e varMap fptaylorPath =
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
        readProcessWithExitCode fptaylorPath [filePath] []


