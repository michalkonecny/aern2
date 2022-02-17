module Main where

import MixedTypesNumPrelude
import Options.Applicative
import System.Directory
import AERN2.BoxFunMinMax.Expressions.Parsers.Smt
import AERN2.BoxFunMinMax.Expressions.Translators.DReal

data ProverOptions = ProverOptions
  {
    fileName :: String,
    targetName :: String
  }

proverOptions :: Parser ProverOptions
proverOptions = ProverOptions
  <$> strOption
    (
      long "file-path"
      <> short 'f'
      <> help "path to smt2 file to be checked"
      <> metavar "filePath"
    )
  <*> strOption
    (
      long "target-path"
      <> short 't'
      <> help "path to write dReal file"
      <> metavar "targetPath"
    )

runDRealTranslator :: ProverOptions -> IO ()
runDRealTranslator (ProverOptions filePath targetPath) =
  do
    -- PATH needs to include folder containing FPTaylor binary after make
    -- symlink to the binary in somewhere like ~/.local/bin will NOT work reliably
    mFptaylorPath <- findExecutable "fptaylor"
    case mFptaylorPath of
      Nothing -> putStrLn "error - fptaylor executable not in path"
      Just fptaylorPath -> do
        mDRealInput <- parseVCToSolver filePath fptaylorPath formulaAndVarMapToDReal False
        case mDRealInput of
          Just dRealInput -> writeFile targetPath dRealInput
          Nothing         -> error "Issue generating input for dReal"

main :: IO ()
main = 
  do 
    runDRealTranslator =<< execParser opts
    where
      opts = info (proverOptions <**> helper)
        ( fullDesc
        <> progDesc "fixme"
        <> header "DReal - translator" )
