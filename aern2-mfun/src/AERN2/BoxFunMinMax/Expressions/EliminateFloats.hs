module AERN2.BoxFunMinMax.Expressions.EliminateFloats where

import MixedTypesNumPrelude
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Expressions.Translators.FPTaylor
import System.Process
import System.IO.Unsafe
import AERN2.BoxFunMinMax.VarMap (VarMap, prettyShowVarMap)
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
eliminateFloatsF f varMap strengthenFormula fptaylorPath =
  aux f strengthenFormula
  where
    aux :: F -> Bool -> IO F
    aux (FConn Impl context goal) strengthenFormula =
      FConn Impl
      <$> aux context (not strengthenFormula)
      <*> aux goal    strengthenFormula
    aux (FNot f) strengthenFormula = FNot <$> aux f (not strengthenFormula)
    aux (FComp op e1 e2) strengthenFormula =
      case op of
        Gt -> FComp op <$> eliminateFloatsFromExpression e1 strengthenFormula <*> eliminateFloatsFromExpression e2 (not strengthenFormula)
        Ge -> FComp op <$> eliminateFloatsFromExpression e1 strengthenFormula <*> eliminateFloatsFromExpression e2 (not strengthenFormula)
        Lt -> FComp op <$> eliminateFloatsFromExpression e1 (not strengthenFormula) <*> eliminateFloatsFromExpression e2 strengthenFormula
        Le -> FComp op <$> eliminateFloatsFromExpression e1 (not strengthenFormula) <*> eliminateFloatsFromExpression e2 strengthenFormula
        Eq -> aux (FConn And (FComp Ge e1 e2) (FComp Le e1 e2)) strengthenFormula
    aux (FConn op e1 e2) strengthenFormula = FConn op <$> aux e1 strengthenFormula <*> aux e2 strengthenFormula
    aux FTrue  _ = return FTrue
    aux FFalse _ = return FFalse

    eliminateFloatsFromExpression e strengthenExpression = 
      if hasFloatE e 
        then do
          absError <- findAbsoluteErrorUsingFPTaylor e varMap fptaylorPath
          let eWithoutFloats = removeFloats e
          if strengthenExpression 
            then return $ EBinOp Sub eWithoutFloats $ Lit absError
            else return $ EBinOp Add eWithoutFloats $ Lit absError
        else
          return e

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
