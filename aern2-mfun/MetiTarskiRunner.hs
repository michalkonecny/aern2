module Main where

import MixedTypesNumPrelude
import Options.Applicative
import System.Directory
import AERN2.BoxFunMinMax.Expressions.Parsers.Smt
import AERN2.BoxFunMinMax.Expressions.Translators.MetiTarski
import System.Process
import System.IO.Temp
import System.Exit
import GHC.IO.Handle

data ProverOptions = ProverOptions
  {
    fileName :: String,
    metiTarskiPath :: String,
    metiTarskiLibraries :: String
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
      long "metitarski-path"
      <> short 'p'
      <> help "path to MetiTarski executable"
      <> metavar "filePath"
    )
  <*> strOption
    (
      long "metitarski-libraries"
      <> short 'l'
      <> help "path to MetiTarski executable"
      <> metavar "filePath"
    )

runMetiTarski :: ProverOptions -> IO ()
runMetiTarski (ProverOptions filePath metiTarskiPath metiTarskiLibraries) =
  do
    -- PATH needs to include folder containing FPTaylor binary after make
    -- symlink to the binary in somewhere like ~/.local/bin will NOT work reliably
    mFptaylorPath <- findExecutable "fptaylor"
    case mFptaylorPath of
      Nothing -> error "fptaylor executable not in path"
      Just fptaylorPath -> do
        mMetiTarskiInput <- parseVCToSolver filePath fptaylorPath formulaAndVarMapToMetiTarski True
        case mMetiTarskiInput of
          Just metiTarskiInput -> do
            (exitCode, output, errDetails) <- withSystemTempFile "metitarski.tptp" handleMetiTarskiFile
            putStrLn output
            return ()
            where
              handleMetiTarskiFile fPath fHandle =
                do
                  hPutStr fHandle metiTarskiInput
                  _ <- hGetContents fHandle -- Ensure handler has finished writing before calling MetiTarski
                  readProcessWithExitCode metiTarskiPath ["--tptp", metiTarskiLibraries, "--autoInclude", fPath] []
          Nothing         -> error "Issue generating input for MetiTarski"

main :: IO ()
main = 
  do 
    runMetiTarski =<< execParser opts
    where
      opts = info (proverOptions <**> helper)
        ( fullDesc
        <> progDesc "fixme"
        <> header "MetiTarski - runner" )
