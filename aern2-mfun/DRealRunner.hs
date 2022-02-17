module Main where

import MixedTypesNumPrelude
import Options.Applicative
import System.Directory
import AERN2.BoxFunMinMax.Expressions.Parsers.Smt
import AERN2.BoxFunMinMax.Expressions.Translators.DReal
import System.Process
import System.IO.Temp
import System.Exit
import GHC.IO.Handle

data ProverOptions = ProverOptions
  {
    fileName :: String,
    dRealPath :: String
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
      long "dreal-path"
      <> short 'p'
      <> help "path to dReal executable"
      <> metavar "filePath"
    )

runDReal :: ProverOptions -> IO ()
runDReal (ProverOptions filePath dRealPath) =
  do
    -- PATH needs to include folder containing FPTaylor binary after make
    -- symlink to the binary in somewhere like ~/.local/bin will NOT work reliably
    mFptaylorPath <- findExecutable "fptaylor"
    case mFptaylorPath of
      Nothing -> error "fptaylor executable not in path"
      Just fptaylorPath -> do
        mdRealInput <- parseVCToSolver filePath fptaylorPath formulaAndVarMapToDReal False
        case mdRealInput of
          Just dRealInput -> do
            (exitCode, output, errDetails) <- withSystemTempFile "dreal.smt2" handleDRealFile
            putStrLn output
            return ()
            where
              handleDRealFile fPath fHandle =
                do
                  hPutStr fHandle dRealInput
                  _ <- hGetContents fHandle -- Ensure handler has finished writing before calling DReal
                  readProcessWithExitCode dRealPath [fPath] []
          Nothing         -> error "Issue generating input for DReal"

main :: IO ()
main = 
  do 
    runDReal =<< execParser opts
    where
      opts = info (proverOptions <**> helper)
        ( fullDesc
        <> progDesc "fixme"
        <> header "DReal - runner" )
