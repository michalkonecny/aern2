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

import AERN2.RealFun.Tests (FnAndDescr(..))

import qualified AERN2.Poly.Cheb as ChPoly
import AERN2.Poly.Cheb (ChPolyMB)
import AERN2.Poly.Cheb.Tests

main :: IO ()
main =
  do
  args <- getArgs
  let (mode, op, deg, p, ac, count) = processArgs args
  runBenchmark mode op deg p ac count

data Mode = SummaryCSV | CSV | Verbose
  deriving (Show, Read)

processArgs :: [String] -> (Mode, String, Integer, Precision, Accuracy, Integer)
processArgs [modeS, op, degS, precS, acS, countS] =
  (read modeS, op, read degS, readPrec precS, readAc acS, read countS)
  where
  readPrec = prec . read
  readAc "exact" = Exact
  readAc "any" = NoInformation
  readAc s = bits (read s :: Integer)
processArgs _ =
  error "expecting arguments: <mode> <operation> <degree> <precision> <count>"


runBenchmark :: Mode -> String -> Integer -> Precision -> Accuracy -> Integer -> IO ()
runBenchmark mode op deg p acGuide count =
  do
  tStart <- getTime ProcessCPUTime
  reportProgress tStart computationDescription

  reportProgress tStart "computing arguments"
  paramPairsPre <- pick (valuePairsWithDeg deg) count

  let paramPairs =
        map (mapBoth (setPrecision p)) $
        map makeFn2PositiveSmallRange $ paramPairsPre
  let paramAccuracies = concat $ map (\(a,b) -> [getAccuracy a, getAccuracy b]) paramPairs
  case minimum paramAccuracies of
    Exact -> pure ()
    ac -> putStrLn $ printf "An argument is not exact! (ac = %s)" (show ac)
  tGotParams <- getTime ProcessCPUTime

  reportProgress tGotParams $ "computing operation " ++ op
  let results = computeResults paramPairs
  tasResults <- mapM getResultCompDurationAndAccuracy $ zip [1..] results

  tDone <- getTime ProcessCPUTime
  reportProgress tDone $ "done"
  csvSummaryLine mode tStart tGotParams tasResults tDone

  where
  computationDescription =
      printf "computing %s on ChPoly(s) (deg = %d, p = %s, count = %d samples)" op deg (show p) count
  computeResults paramPairs =
    case op of
      "add" -> map (uncurry (+)) paramPairs
      "mul" -> map (uncurry (*)) paramPairs
      "div" -> map (uncurry (ChPoly.chebDivideDCT acGuide)) paramPairs
      _ -> error $ "unknown op " ++ op
  getResultCompDurationAndAccuracy (i,result) =
    do
    tiResStart <- getTime ProcessCPUTime
    let ac = getAccuracy result
    reportProgress tiResStart $ printf "result %d accuracy = %s" i (show ac)
    tiResEnd <- seq ac $ getTime ProcessCPUTime
    csvLine mode i tiResStart tiResEnd ac
    return (tiResStart, ac)
  csvLine CSV i tResStart tResEnd ac =
    putStrLn $ printf "%s,%3d,%4d,%3d,%13.9f,%s"
                op deg (integer p) i
                (toSec dRes)
                (showAC ac)
    where
    dRes = tResEnd .-. tResStart
  csvLine _ _ _ _ _ = pure ()
  csvSummaryLine SummaryCSV tStart tGotParams tasResults tDone =
    putStrLn $ printf "%s,%3d,%4d,%4d,%16.9f,%13.9f,%13.9f,%13.9f,%s,%s"
                op deg (integer p) count
                (toSec dPrepParams)
                (toSec dGetResMean)
                (toSec dGetResWorst)
                (toSec dGetResStDev)
                (showAC acWorst)
                (showAC acBest)
    where
    (tsResults, acResults) = unzip tasResults
    acWorst = minimum acResults
    acBest = maximum acResults
    dPrepParams = tGotParams .-. tStart
    dsResults = zipWith (.-.) ((tail tsResults) ++ [tDone]) tsResults
    n = length tsResults
    dGetResMean = round $ (sum dsResults) / n
    dGetResWorst = foldl1 max dsResults
    dGetResStDev =
      round $ sqrt $ double $
        (sum $ map (^2) $ (map (\x -> x-dGetResMean) dsResults))
          / (n - 1)
  csvSummaryLine _ _ _ _ _ = pure ()

  a .-. b = toNanoSecs $ diffTimeSpec a b
  toSec ns = (double ns) / (10^9)
  showAC Exact = "exact"
  showAC NoInformation = "noinformation"
  showAC ac = show $ fromAccuracy ac
  reportProgress now msg =
    case mode of
      Verbose ->
        printf "[%06d.%06d] ChPoly benchmark: %s\n" (sec now) (msec now) msg
      _ -> pure ()
    where
    msec time = nsec time `div` (P.fromInteger 1000)


mapBoth :: (t1 -> t2) -> (t1,t1) -> (t2,t2)
mapBoth f (a,b) = (f a, f b)

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
  map (ChPoly.reduceDegree deg) $
    map fst $ valuePairsWithMinDeg deg

valuePairsWithDeg :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithDeg deg =
  map reduceDegrees $
    valuePairsWithMinDeg deg
  where
  reduceDegrees = mapBoth (centreAsBall . ChPoly.reduceDegree deg)

valuePairsWithMinDeg :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithMinDeg deg =
  listFromGen $
    do
    (p1,_) <- arbitraryWithDegDom deg dom
    (p2,_) <- arbitraryWithDegDom deg dom
    return (p1, p2)
  where
  dom = dyadicInterval (0.0,1.0)

makeFn2Positive :: (ChPolyMB, ChPolyMB) -> (ChPolyMB, ChPolyMB)
makeFn2Positive = mapSecondFD makeFnPositive

makeFn2PositiveSmallRange :: (ChPolyMB, ChPolyMB) -> (ChPolyMB, ChPolyMB)
makeFn2PositiveSmallRange = mapSecondFD (makeFnPositiveSmallRange 10)

mapSecondFD f (a,b) = (a, fb)
  where
  FnAndDescr fb _ = f (FnAndDescr b "")
