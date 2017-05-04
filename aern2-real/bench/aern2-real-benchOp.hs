{-|
    Module      :  Main (file aern2-real-benchOp)
    Description :  execute a CR operation for benchmarking
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import Numeric.MixedTypes
-- import Prelude

import Text.Printf

import System.Environment

import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import Test.QuickCheck

import AERN2.Utils.Bench

import AERN2.MP
import AERN2.Real
import AERN2.Real.Tests () -- instance Arbitrary CauchyReal

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
processArgs [op, countS, accuracyS] =
    return (computationDescription, results)
    where
    computationDescription =
        printf "computing %s (%d times) using accuracy %d" op count ac
    ac :: Integer
    ac = read accuracyS
    count :: Integer
    count = read countS

    results =
      case op of
        "exp" ->
          map ((? (bitsSG ac)) . exp) $
            unsafePerformIO $ pickValues valuesSmall count
        "log" ->
          map ((? (bitsSG ac)) . log) $
            unsafePerformIO $ pickValues valuesPositive count
        "sqrt" ->
          map ((? (bitsSG ac)) . sqrt) $
            unsafePerformIO $ pickValues valuesPositive count
        "cos" ->
          map ((? (bitsSG ac)) . cos) $
            unsafePerformIO $ pickValues values count
        "add" ->
          map ((? (bitsSG ac)) . (uncurry (+))) $
            unsafePerformIO $ pickValues2 values values count
        "mul" ->
          map ((? (bitsSG ac)) . (uncurry (*))) $
            unsafePerformIO $ pickValues2 values values count
        "div" ->
          map ((? (bitsSG ac)) . (uncurry (/))) $
            unsafePerformIO $ pickValues2 values valuesPositive count
        "logistic" ->
          map ((? (bitsSG ac)) . (logistic 3.82 count)) $
            [real 0.125]
        _ -> error $ "unknown op " ++ op
processArgs _ =
    error "expecting arguments: <operation> <count> <precision>"

logistic :: Rational -> Integer -> CauchyReal -> CauchyReal
logistic c n x
  | n == 0 = x
  | otherwise = logistic c (n-1) $ c * x * (1-x)

pickValues2 :: [CauchyReal] -> [CauchyReal] -> Integer -> IO [(CauchyReal, CauchyReal)]
pickValues2 vals1 vals2 count =
  do
  p1 <- pickValues vals1 count
  p2 <- pickValues vals2 count
  return $ zip p1 p2

pickValues :: [CauchyReal] -> Integer -> IO [CauchyReal]
pickValues vals count =
  sequence $
  [
    do
    i1 <- randomRIO (1,maxIndex)
    let x = vals !! i1
    return x
  | _j <- [1..count]
  ]

maxIndex :: Integer
maxIndex = 1000

valuesSmall :: [CauchyReal]
valuesSmall = map makeSmall values
  where
  makeSmall :: CauchyReal -> CauchyReal
  makeSmall x
    | abs (getBall x) !<! 1000000 = x
    | otherwise = 1000000 * (x/(1000000+(abs x)))
    where
    getBall :: CauchyReal -> MPBall
    getBall xx = xx ? (bitsSG 53)

valuesPositive :: [CauchyReal]
valuesPositive = filter ((!>! 0) . getBall) values
    where
    getBall :: CauchyReal -> MPBall
    getBall x = x ? (bitsSG 53)

values :: [CauchyReal]
values = listFromGen (real <$> (arbitrary :: Gen Rational))
