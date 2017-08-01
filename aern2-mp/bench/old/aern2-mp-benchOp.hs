{-|
    Module      :  Main (file aern2-mp-benchOp)
    Description :  execute an MPBall operation for benchmarking
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import MixedTypesNumPrelude
-- import Prelude

import Text.Printf

import System.Environment

import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import Test.QuickCheck

import AERN2.Utils.Bench

import AERN2.MP.UseMPFR.Float
import AERN2.MP.UseMPFR.Ball
import AERN2.MP.Ball.Tests () -- instance Arbitrary MPBall

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, results) <- processArgs args
    putStrLn $ computationDescription
    putStrLn $ "accuracies = " ++ show (map getAccuracy results)

processArgs ::
    [String] ->
    IO (String, [MPBall])
processArgs [op, countS, precisionS] =
    return (computationDescription, results)
    where
    computationDescription =
        printf "computing %s (%d times) using precision %d" op count p
    p :: Integer
    p = read precisionS
    count :: Integer
    count = read countS

    results =
      case op of
        "exp" -> map exp $ unsafePerformIO $ pickBalls ballsSmallExact p count
        _ -> error $ "unknown op " ++ op
processArgs _ =
    error "expecting arguments: <operation> <count> <precision>"

pickBalls :: [MPBall] -> Integer -> Integer -> IO [MPBall]
pickBalls vals p count =
  sequence $
  [
    do
    i1 <- randomRIO (1,maxIndex)
    let x = setPrecision (prec p) $ vals !! i1
    -- printf "bgroupOp: %s: p = %d, j = %d, i1 = %d, x = %s\n" name p _j i1 (show x)
    return x
  | _j <- [1..count]
  ]

maxIndex :: Integer
maxIndex = 1000

ballsExactPositive :: [MPBall]
ballsExactPositive = filter (!>! 0) ballsExact

ballsSmallExact :: [MPBall]
ballsSmallExact = map centreAsBall ballsSmall

ballsExact :: [MPBall]
ballsExact = map centreAsBall balls

ballsSmall :: [MPBall]
ballsSmall = map makeSmall balls
  where
  makeSmall :: MPBall -> MPBall
  makeSmall b
    | (abs b) !<! 1000000 = b
    | otherwise = 100000 * (b/(1000000+(abs $ centreAsBall b)))

ballsPositive :: [MPBall]
ballsPositive = filter (!>! 0) balls

balls :: [MPBall]
balls = listFromGen arbitrary
