module Main where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import qualified Data.List as List

import AERN2.AD.Differential hiding (x)
-- import AERN2.Util.Util
import AERN2.BoxFun.Type
import AERN2.Linear.Vector.Type ((!), Vector)
import qualified AERN2.Linear.Vector.Type as V
import AERN2.BoxFunMinMax.Type
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Expressions.Eliminator
import AERN2.BoxFunMinMax.Expressions.TestFunctions
import System.Environment
import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.Parsers.Smt (parseSMT2, parseVCToF, ParsingMode (Why3, CNF))
import AERN2.BoxFunMinMax.Expressions.Parsers.DRealSmt
import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory
import Data.Ratio
import Debug.Trace
import AERN2.BoxFunMinMax.Expressions.Type (eSafeCNFToDNF)
import AERN2.BoxFunMinMax.Expressions.DeriveBounds (evalF_comparisons)
data ProverOptions = ProverOptions
  {
    provingProcessDone :: Bool,
    ceMode :: Bool,
    depthCutoff :: Integer,
    bestFirstSearchCutoff :: Integer,
    precision :: Integer,
    -- relativeImprovementCutoff :: Rational, make this a flag, as a double is probably easier
    fileName :: String
  }

-- data DRealOptions = DRealOptions
--   {
--     dRealFileName :: String,
--     dRealTargetName :: String
--   }

proverOptions :: Parser ProverOptions
proverOptions = ProverOptions
  <$> switch
    (
      long "proving-process-done"
      <> short 'a'
      <> help "Add this option if the target file was generated by our proving process"
    )
  <*> switch
    (
      long "ce-mode"
      <> short 'c'
      <> help "Add this option the prover should focus on finding a counterexample"
    )
  <*> option auto
    (
      long "depth-cutoff"
      <> short 'd'
      <> help "How hard the prover 'works' until giving up. This option is ignored when ce-mode is on"
      <> showDefault
      <> value 50
      <> metavar "INT"
    )
  <*> option auto
    (
      long "best-first-search-cutoff"
      <> short 'b'
      <> help "How hard the prover 'works' until giving up when searching for a counterexample. This option is ignored when ce-mode is off"
      <> showDefault
      <> value 1000
      <> metavar "INT"
    )
  <*> option auto
    (
      long "precision"
      <> short 'p'
      <> help "precision of floating-point numbers used within the prover. Higher precision slows down the prover but may be needed for more difficult problems"
      <> showDefault
      <> value 100
      <> metavar "INT"
    )
  <*> strOption
    (
      long "file-path"
      <> short 'f'
      <> help "path to smt2 file to be checked"
      <> metavar "filePath"
    )
main :: IO ()
main = 
  do 
    runProver =<< execParser opts
    where
      opts = info (proverOptions <**> helper)
        ( fullDesc
        <> progDesc "fixme"
        <> header "LPPaver - prover" )

runProver :: ProverOptions -> IO ()
runProver proverOptions@(ProverOptions provingProcessDone ceMode depthCutoff bestFirstSearchCutoff p filePath) =
  do 
    if provingProcessDone
      then do
        parsedFile <- parseSMT2 filePath
        case parseDRealSmtToF parsedFile of
          (Just vc, typedVarMap) ->
            let 
              -- If there are variable free comparisons here, we could not deal with them earlier in the proving process.
              -- LPPaver cannot perform any better with these so we safely remove them.
              ednf = simplifyESafeDoubleList . minMaxAbsEliminatorEDNF . fToEDNF . simplifyF . removeVariableFreeComparisons $ vc
            in do
              decideEDNFWithVarMap ednf typedVarMap proverOptions
          (_, _) -> error "Error - Issue parsing given SMT file"
      else do
        -- PATH needs to include folder containing FPTaylor binary after make
        -- symlink to the binary in somewhere like ~/.local/bin will NOT work reliably
        mFptaylorPath <- findExecutable "fptaylor"
        case mFptaylorPath of
          Nothing -> putStrLn "Error - fptaylor executable not in path"
          Just fptaylorPath -> do
            mParsedVC <- parseVCToF filePath fptaylorPath
            case mParsedVC of
              Just (vc, typedVarMap) ->
                let 
                  -- If there are variable free comparisons here, we could not deal with them earlier in the proving process.
                  -- LPPaver cannot perform any better with these so we safely remove them.
                  ednf = simplifyESafeDoubleList . minMaxAbsEliminatorEDNF . fToEDNF . simplifyF . removeVariableFreeComparisons $ vc
                in do
                  decideEDNFWithVarMap ednf typedVarMap proverOptions
              Nothing -> do
                putStrLn "unknown"
                putStrLn "Issue parsing file"

decideEDNFWithVarMap :: [[ESafe]] -> TypedVarMap -> ProverOptions -> IO ()
decideEDNFWithVarMap ednf typedVarMap (ProverOptions provingProcessDone ceMode depthCutoff bestFirstSearchCutoff p filePath) = do
  let result =
        if ceMode
          then checkEDNFBestFirstWithSimplexCE ednf typedVarMap bestFirstSearchCutoff 1.2 (prec p)
          else checkEDNFDepthFirstWithSimplex  ednf typedVarMap depthCutoff 0         1.2 (prec p)
  -- Since we prove a negation of the VC, present results as appropriate
  case result of
    (Just True, Just model) -> do
      putStrLn "sat"
      printSMTModel model
      prettyPrintCounterExample model
    (Just False, _) -> do
      putStrLn "unsat"
    r@(_, Just indeterminateExample) -> do
      putStrLn "unknown"
      printSMTModel indeterminateExample
      prettyPrintCounterExample indeterminateExample
    r@(_, _) -> do
      putStrLn "unknown"

prettyPrintCounterExample :: TypedVarMap -> IO ()
prettyPrintCounterExample [] = return ()
prettyPrintCounterExample ((TypedVar (v, (l, r)) t) : vs) = 
  if l == r
    then do 
      putStrLn (v ++ " = " ++ show (double l))
      prettyPrintCounterExample vs
    else do
      putStrLn (v ++ " = [" ++ show (double l) ++ ", " ++ show (double r) ++ "]")
      prettyPrintCounterExample vs

printSMTModel :: TypedVarMap -> IO ()
printSMTModel typedVarMap =
  do
    putStrLn "(model"
    printModels typedVarMap
    putStrLn ")"
  where
    printModels [] = return ()
    printModels ((TypedVar (v, (l, r)) t) : vs) = do
      putStrLn $ "(define-fun " ++ v ++ " () " ++ show t ++ " " ++ showNum (l) ++ ")"
      putStrLn $ "(define-fun " ++ v ++ "_vc_constant" ++ " () " ++ show t ++ " " ++ showNum (l) ++ ")" --FIXME: Only do this with a Why3 or similar flag?
      printModels vs

    showNum :: Rational -> String
    showNum num =
      if num < 0
        then "(/ " ++ "(" ++ show (numerator num) ++ ") " ++ show (denominator num) ++ ")"
        else "(/ " ++ show (numerator num) ++ " " ++ show (denominator num) ++ ")"
