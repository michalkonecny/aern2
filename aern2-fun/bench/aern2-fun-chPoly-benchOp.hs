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
import qualified Prelude as P

import Text.Printf

import System.Environment

import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import System.Clock

import Test.QuickCheck

import AERN2.Utils.Bench

import AERN2.MP
-- import AERN2.Real

import AERN2.Interval

import AERN2.Poly.Cheb
import AERN2.Poly.Cheb.Tests

type ChPolyMB = ChPoly MPBall

main :: IO ()
main =
    do
    args <- getArgs
    let (op, deg, p, count) = processArgs args
    putStrLn $ computationDescription op deg p count
    runBenchmark op deg p count
    where
    computationDescription op deg p count =
        printf "computing %s on ChPoly(s) (deg = %d, p = %s, count = %d samples)" op deg (show p) count
    runBenchmark op deg p count =
      do
      reportProgress "computing arguments"
      paramPairsPre <- pick (valuePairsWithDeg deg) count
      let paramPairs = map (mapBoth (setPrecision p)) paramPairsPre
      print $ map (mapBoth getAccuracy) paramPairs
      reportProgress $ "computing operation " ++ op
      let results = computeResults paramPairs
      -- reportProgress $ "evaluating results"
      putStrLn $ "result accuracies = " ++ show (map getAccuracy results)
      reportProgress $ "done"
      where
      computeResults paramPairs =
        case op of
          "mul" -> map (uncurry (*)) paramPairs
          _ -> error $ "unknown op " ++ op
      reportProgress msg =
        do
        now <- getTime ProcessCPUTime
        printf "[%06d.%03d] ChPoly benchmark: %s\n" (sec now) ((nsec now) `div` (P.fromInteger 1000000)) msg

mapBoth :: (t1 -> t2) -> (t1,t1) -> (t2,t2)
mapBoth f (a,b) = (f a, f b)

processArgs :: [String] -> (String, Integer, Precision, Integer)
processArgs [op, countS, degS, precS] =
  (op, read degS, prec (read precS :: Integer), read countS)
processArgs _ =
  error "expecting arguments: <operation> <count> <degree> <precision>"

pick :: [t] -> Integer -> IO [t]
pick ts count =
  sequence $
  [
    do
    i1 <- randomRIO (1,maxIndex)
    let t = ts !! i1
    return t
  | _j <- [1..count]
  ]

maxIndex :: Integer
maxIndex = 200

valuesWithDeg :: Integer -> [ChPolyMB]
valuesWithDeg deg =
  map (reduceDegree deg) $
    filter degreeLargeEnough $
      map fst $ valuePairsWithMinOps (3*deg)
  where
  degreeLargeEnough p1 = degree p1 >= deg

valuePairsWithDeg :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithDeg deg =
  map reduceDegrees $
    filter degreesLargeEnough $
      valuePairsWithMinOps (3*deg)
  where
  degreesLargeEnough (p1,p2) = degree p1 >= deg && degree p2 >= deg
  reduceDegrees = mapBoth (centreAsBall . reduceDegree deg)

valuePairsWithMinOps :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithMinOps minOps =
  listFromGen $
    do
    dom <- arbitraryNonEmptySmallInterval
    p1C <- arbitraryWithMinOpsDom minOps dom
    p2C <- arbitraryWithMinOpsDom minOps dom
    return (chPolyFromOps p1C, chPolyFromOps p2C)
