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

-- import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import System.Clock

-- import Test.QuickCheck

import AERN2.Utils.Bench

import AERN2.MP
-- import AERN2.Real

import AERN2.Interval

import AERN2.Poly.Cheb hiding (minimum)
import AERN2.Poly.Cheb.Tests

main :: IO ()
main =
    do
    args <- getArgs
    let (mode, op, deg, p, count) = processArgs args
    runBenchmark mode op deg p count
    where
    runBenchmark mode op deg p count =
      do
      tStart <- getTime ProcessCPUTime
      reportProgress tStart computationDescription
      reportProgress tStart "computing arguments"
      paramPairsPre <- pick (valuePairsWithDeg deg) count
      let paramPairs = map (mapBoth (setPrecision p)) paramPairsPre
      let paramAccuracies = concat $ map (\(a,b) -> [getAccuracy a, getAccuracy b]) paramPairs
      case minimum paramAccuracies of
        Exact -> pure ()
        ac -> putStrLn $ printf "A parameter is not exact! (ac = %s)" (show ac)
      tGotParams <- getTime ProcessCPUTime
      reportProgress tGotParams $ "computing operation " ++ op
      let results = computeResults paramPairs
      tsResults <- mapM reportResult $ zip [1..] results
      tDone <- getTime ProcessCPUTime
      reportProgress tDone $ "done"
      csvLine tStart tGotParams tsResults tDone
      where
      computationDescription =
          printf "computing %s on ChPoly(s) (deg = %d, p = %s, count = %d samples)" op deg (show p) count
      computeResults paramPairs =
        case op of
          "mul" -> map (uncurry (*)) paramPairs
          _ -> error $ "unknown op " ++ op
      reportResult (i,result) =
        do
        tiRes <- getTime ProcessCPUTime
        let ac = getAccuracy result
        case ac of
          Exact -> reportProgress tiRes $ printf "result %d accuracy = %s" i (show ac)
          _ -> putStrLn $ printf "Result %d is not exact! (ac = %s)" i (show ac)
        return tiRes
      reportProgress now msg =
        case mode of
          Verbose ->
            printf "[%06d.%06d] ChPoly benchmark: %s\n" (sec now) (msec now) msg
          _ -> pure ()
        where
        msec time = nsec time `div` (P.fromInteger 1000)
      csvLine tStart tGotParams tsResults tDone =
        case mode of
          CSV ->
            putStrLn $ printf "%s,%3d,%4d,%4d,%16.9f,%13.9f,%13.9f,%13.9f"
                        op deg (integer p) count
                        (toSec dPrepParams)
                        (toSec dGetResMean)
                        (toSec dGetResWorst)
                        (toSec dGetResStDev)
            where
            toSec ns = (double ns) / (10^9)
            a .-. b = toNanoSecs $ diffTimeSpec a b
            dPrepParams = tGotParams .-. tStart
            dsResults = zipWith (.-.) ((tail tsResults) ++ [tDone]) tsResults
            n = length tsResults
            dGetResMean = round $ (sum dsResults) / n
            dGetResWorst = foldl1 max dsResults
            dGetResStDev =
              round $ sqrt $ double $
                (sum $ map (^2) $ (map (\x -> x-dGetResMean) dsResults))
                  / (n - 1)
          _ -> pure ()


mapBoth :: (t1 -> t2) -> (t1,t1) -> (t2,t2)
mapBoth f (a,b) = (f a, f b)

data Mode = CSV | Verbose
  deriving (Show, Read)

processArgs :: [String] -> (Mode, String, Integer, Precision, Integer)
processArgs [modeS, op, degS, precS, countS] =
  (read modeS, op, read degS, prec (read precS :: Integer), read countS)
processArgs _ =
  error "expecting arguments: <mode> <operation> <degree> <precision> <count>"

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
    map fst $ valuePairsWithMinDeg deg

valuePairsWithDeg :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithDeg deg =
  map reduceDegrees $
    valuePairsWithMinDeg deg
  where
  reduceDegrees = mapBoth (centreAsBall . reduceDegree deg)

valuePairsWithMinDeg :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithMinDeg deg =
  listFromGen $
    do
    (p1,_) <- arbitraryWithDegDom deg dom
    (p2,_) <- arbitraryWithDegDom deg dom
    return (p1, p2)
  where
  dom = dyadicInterval (0.0,1.0)
