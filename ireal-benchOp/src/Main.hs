{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Data.Number.IReal
import Data.Number.IReal.IReal
import Data.Number.IReal.IntegerInterval
import Data.Number.IReal.Auxiliary

import Text.Printf

import System.Environment

import System.IO.Unsafe (unsafePerformIO)

import System.Random (randomRIO)

import Test.QuickCheck

import AERN2.Utils.Bench
import AERN2.MP.Accuracy

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, results) <- processArgs args
    putStrLn $ computationDescription
    putStrLn $ "accuracies = " ++ show (map getAccuracy results)

type MPBall = (IntegerInterval, Precision)

instance HasAccuracy MPBall where
  getAccuracy (I (l,u), b) =
     bits (b - lg2 (u-l) + 1)

realWithAccuracy :: IReal -> Accuracy -> MPBall
realWithAccuracy x ac =
  (appr x b, b)
  where
  b = fromInteger $ fromAccuracy ac

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
          map (flip realWithAccuracy (bits ac) . exp) $
            unsafePerformIO $ pickValues valuesSmall count
        "log" ->
          map (flip realWithAccuracy (bits ac) . log) $
            unsafePerformIO $ pickValues valuesPositive count
        "sqrt" ->
          map (flip realWithAccuracy (bits ac) . sqrt) $
            unsafePerformIO $ pickValues valuesPositive count
        "cos" ->
          map (flip realWithAccuracy (bits ac) . cos) $
            unsafePerformIO $ pickValues values count
        "add" ->
          map (flip realWithAccuracy (bits ac) . (uncurry (+))) $
            unsafePerformIO $ pickValues2 values values count
        "mul" ->
          map (flip realWithAccuracy (bits ac) . (uncurry (*))) $
            unsafePerformIO $ pickValues2 values values count
        "div" ->
          map (flip realWithAccuracy (bits ac) . (uncurry (/))) $
            unsafePerformIO $ pickValues2 values valuesPositive count
        "logistic" ->
          map (flip realWithAccuracy (bits ac) . (logistic 3.82 count)) $
            [0.125]
        _ -> error $ "unknown op " ++ op
processArgs _ =
    error "expecting arguments: <operation> <count> <precision>"

logistic :: Rational -> Integer -> IReal -> IReal
logistic c n x
  | n == 0 = x
  | otherwise = logistic c (n-1) $ (fromRational c) * x * (1-x)

pickValues2 :: [IReal] -> [IReal] -> Integer -> IO [(IReal, IReal)]
pickValues2 vals1 vals2 count =
  do
  p1 <- pickValues vals1 count
  p2 <- pickValues vals2 count
  return $ zip p1 p2

pickValues :: [IReal] -> Integer -> IO [IReal]
pickValues vals count =
  sequence $
  [
    do
    i1 <- randomRIO (1,maxIndex)
    let x = vals !! (fromInteger i1)
    return x
  | _j <- [1..count]
  ]

maxIndex :: Integer
maxIndex = 1000

valuesSmall :: [IReal]
valuesSmall = map makeSmall values
  where
  makeSmall :: IReal -> IReal
  makeSmall x
    | abs x < 1000000 = x
    | otherwise = 1000000 * (x/(1000000+(abs x)))

valuesPositive :: [IReal]
valuesPositive = filter (> 0) values

values :: [IReal]
values = listFromGen (fromRational <$> (arbitrary :: Gen Rational))
