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
import AERN2.BoxFunMinMax.Expressions.Parsers.Smt (parseVCToECNF, ParsingMode (Why3, CNF))
import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory
data ProverOptions = ProverOptions
  {
    why3Mode :: Bool,
    ceMode :: Bool,
    depthCutoff :: Integer,
    bestFirstSearchCutoff :: Integer,
    precision :: Integer,
    -- relativeImprovementCutoff :: Rational, make this a flag, as a double is probably easier
    fileName :: String
  }

proverOptions :: Parser ProverOptions
proverOptions = ProverOptions
  <$> switch
    (
      long "why3-mode"
      <> short 'w'
      <> help "Add this option if the target file was generated by Why3"
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
runProver (ProverOptions why3Mode ceMode depthCutoff bestFirstSearchCutoff p filePath) =
  do 
    mFptaylorPath <- findExecutable "fptaylor"
    case mFptaylorPath of
      Nothing -> putStrLn "error - fptaylor executable not in path"
      Just fptaylorPath -> do
        mParsedVC <- parseVCToECNF filePath (if why3Mode then Why3 else CNF) fptaylorPath
        case mParsedVC of
          Just (ecnf, typedVarMap) -> do
            result <- 
              if ceMode
                then
                  return $ checkECNFBestFirstWithSimplexCE ecnf typedVarMap bestFirstSearchCutoff 1.2 (prec p)
                else
                  return $ checkECNFDepthFirstWithSimplex ecnf typedVarMap depthCutoff 0 1.2 (prec p)
            if why3Mode
              then do
                case result of
                  (Just True, _) -> putStrLn "unsat"
                  (Just False, Just counterExample) -> do
                    putStrLn "sat"
                    print counterExample
                  (_, indeterminateExample) -> do
                    putStrLn "unknown"
                    print indeterminateExample
              else do
                case result of
                  (Just True, _) -> putStrLn "sat"
                  (Just False, Just counterExample) -> do
                    putStrLn "unsat"
                    print counterExample
                  (_, indeterminateExample) -> do
                    putStrLn "unknown"
                    print indeterminateExample
          Nothing -> do
            putStrLn "Issue parsing file"
            putStrLn "unknown"
