{-|
    Module      :  Main (file aern2-fun-chPoly-benchOp)
    Description :  execute a ChPoly operation for benchmarking
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
-- import AERN2.Real

import AERN2.Poly.Cheb
import AERN2.Poly.Cheb.Tests () -- instance Arbitrary ChPolyMB

type ChPolyMB = ChPoly MPBall

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, results) <- processArgs args
    putStrLn $ computationDescription
    putStrLn $ "accuracies = " ++ show (map getAccuracy results)

processArgs ::
    [String] ->
    IO (String, [ChPolyMB])
processArgs [op, countS] =
    return (computationDescription, results)
    where
    computationDescription =
        printf "computing %s (%d times) using parameters: " op count
    -- ac :: Integer
    -- ac = read accuracyS
    count :: Integer
    count = read countS

    results =
      case op of
        "mul" ->
          map (uncurry (*)) $
            unsafePerformIO $ pickValues2 values values count
        _ -> error $ "unknown op " ++ op
processArgs _ =
    error "expecting arguments: <operation> <count> <precision>"

pickValues2 :: [ChPolyMB] -> [ChPolyMB] -> Integer -> IO [(ChPolyMB, ChPolyMB)]
pickValues2 vals1 vals2 count =
  do
  p1 <- pickValues vals1 count
  p2 <- pickValues vals2 count
  return $ zip p1 p2

pickValues :: [ChPolyMB] -> Integer -> IO [ChPolyMB]
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

-- valuesSmall :: [ChPolyMB]
-- valuesSmall = map makeSmall values
--   where
--   makeSmall :: ChPolyMB -> ChPolyMB
--   makeSmall x
--     | abs (getBall x) !<! 1000000 = x
--     | otherwise = 1000000 * (x/(1000000+(abs x)))
--     where
--     getBall :: ChPolyMB -> MPBall
--     getBall xx = qaMakeQuery xx (bits 53)
--
-- valuesPositive :: [ChPolyMB]
-- valuesPositive = filter ((!>! 0) . getBall) values
--     where
--     getBall :: ChPolyMB -> MPBall
--     getBall x = qaMakeQuery x (bits 53)

valuesWithDeg :: Integer -> [ChPolyMB]
valuesWithDeg deg =
  filter ((>= deg) . degree) values

values :: [ChPolyMB]
values = listFromGen arbitrary -- (real <$> (arbitrary :: Gen Rational))
