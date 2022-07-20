{-# LANGUAGE DataKinds, Arrows, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import MixedTypesNumPrelude
import qualified Prelude as P
-- import Data.String (fromString)

-- import Control.Arrow hiding (loop)

import System.Environment (getArgs)
import AERN2.MP (Accuracy, bits, ac2prec, mpBallP, MPBall (ball_value), HasAccuracy (getAccuracy))

import Text.Printf (printf)
import Data.Typeable (Typeable)

import qualified AERN2.Linear.Vector as VN
import qualified AERN2.Linear.Matrix as MRC
import AERN2.Linear.Matrix (MatrixRC)
import AERN2.Linear.Vector (VN)

main :: IO ()
main =
    do
    [benchS,  benchParamsS, implS, acS] <- getArgs
    let benchParams = read ("[" ++ benchParamsS ++ "]") :: [Integer]
    let ac = bits (read acS :: Integer)
    let (resultDecription, benchDecription) = bench benchS benchParams implS ac
    putStrLn benchDecription
    putStrLn resultDecription

bench :: String -> [Integer] -> String -> Accuracy -> (String, String)
bench benchS benchParams implS ac =
    (implS ++ ": " ++ resultDecription, benchDecription)
    where
    benchDecription =
        case (benchS, benchParams) of
            ("matrix", [n]) -> 
              printf "|M| where M is the %dx%d matrix [1,1/2,1/3,...] (ac = %s)" n n (show ac)
            ("product", [n]) -> 
              printf "|M*M| where M is the %dx%d matrix [1,1/2,1/3,...] (ac = %s)" n n (show ac)
            ("inverse", [n]) -> 
              printf "|I/M| where M is the %dx%d matrix [1,1/2,1/3,...] (ac = %s)" n n (show ac)
            ("systemRHS", [n]) ->
              printf "|M*1| where M is the %dx%d matrix [1,1/2,1/3,...] (ac = %s)" n n (show ac)
            ("solve", [n]) ->
              printf "|M\\b| where M is the %dx%d matrix [1,1/2,1/3,...] (ac = %s)" n n (show ac)
            _ ->
              error $ "unknown benchmark or incorrect number of parameters: " ++ benchS ++ show benchParams
    resultDecription =
        case (benchS, benchParams) of
            ("matrix", [n]) ->
                case implS of
                    "MPFloat" -> show (MRC.inftyNorm $ taskMatrix mpFloat_AC n)
                    "MPBall" -> showB (MRC.inftyNorm $ taskMatrix cnMPBall_AC n)
                    _ -> error $ "unknown implementation: " ++ implS
            ("product", [n]) ->
                case implS of
                    "MPFloat" -> show (MRC.inftyNorm $ taskProduct mpFloat_AC n)
                    "MPBall" -> showB (MRC.inftyNorm $ taskProduct cnMPBall_AC n)
                    _ -> error $ "unknown implementation: " ++ implS
            ("inverse", [n]) ->
                case implS of
                    "MPFloat" -> show (MRC.inftyNorm $ taskInverse mpFloat_AC n)
                    "MPBall" -> showB (MRC.inftyNorm $ taskInverse cnMPBall_AC n)
                    _ -> error $ "unknown implementation: " ++ implS
            ("systemRHS", [n]) ->
                case implS of
                    "MPFloat" -> show (VN.inftyNorm $ taskSystemRHS mpFloat_AC n)
                    "MPBall" -> showB (VN.inftyNorm $ taskSystemRHS cnMPBall_AC n)
                    _ -> error $ "unknown implementation: " ++ implS
            ("solve", [n]) ->
                case implS of
                    "MPFloat" -> show (VN.inftyNorm $ taskSolve mpFloat_AC n)
                    "MPBall" -> showB (VN.inftyNorm $ taskSolve cnMPBall_AC n)
                    "MPBallViaFP" -> showB (fmap VN.inftyNorm $ taskSolveViaFP mpBall_AC n)
                    _ -> error $ "unknown implementation: " ++ implS
            _ -> error ""
    mpFloat_AC = ball_value . mpBall_AC
    cnMPBall_AC = cn . mpBall_AC
    mpBall_AC = mpBallP (ac2prec ac)
    showB :: CN MPBall -> String
    showB b = printf "value = %s (accuracy = %s)" (show b) (show $ getAccuracy b)

taskMatrix :: 
  (Typeable t, Show t) => 
  (Rational -> t) -> Integer -> MatrixRC t
taskMatrix fromQ n = 
  m
  where
  rowsQ = [[ item i j  | j <- [1..n] ] | i <- [1..n]]
    where
    item i j
      -- | i == j = rational 1
      -- | j > i + 1 = rational 0
      | otherwise = 1/(n*(i-1)+j)
  m = MRC.fromList $ map (map fromQ) rowsQ

taskProduct :: 
  (P.Num t, Typeable t, Show t) => 
  (Rational -> t) -> Integer -> MatrixRC t
taskProduct fromQ n = m * m
  where
  m = taskMatrix fromQ n

taskInverse :: 
  (P.Fractional t, Typeable t, Show t) => 
  (Rational -> t) -> Integer -> MatrixRC t
taskInverse fromQ n = 
  MRC.luInv (taskMatrix fromQ n)

-- | Compute b such that the solution of M*x = b is x = 1.
taskSystemRHS :: 
  (P.Fractional t, Typeable t, Show t) => 
  (Rational -> t) -> Integer -> VN t
taskSystemRHS fromQ n = 
  (taskMatrix fromQ n) * ones 
  where
  ones = VN.fromList $ replicate n (fromQ 1.0)

taskSolve :: 
  (P.Fractional t, Typeable t, Show t) => 
  (Rational -> t) -> Integer -> VN t
taskSolve fromQ n = 
  MRC.luSolve (taskMatrix fromQ n) (taskSystemRHS fromQ n)
  
taskSolveViaFP :: (Rational -> MPBall) -> Integer -> CN (VN MPBall)
taskSolveViaFP fromQ n = 
  MRC.solveBViaFP (taskMatrix fromQ n) (taskSystemRHS fromQ n)
