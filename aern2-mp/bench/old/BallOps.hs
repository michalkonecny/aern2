{-|
    Module      :  Main (file BallOps.hs)
    Description :  criterion benchmarks for MPBall operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
import MixedTypesNumPrelude
-- import Prelude

import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import Criterion.Main

import AERN2.MP.Float
import AERN2.MP.Ball
import AERN2.MP.Ball.Tests () -- instance Arbitrary MPBall

import Values

main :: IO ()
main = defaultMain
    [ bgroupOp "exp" exp ballsExact 100
    , bgroupOp "log" log ballsExactPositive 100
    , bgroupOp "sqrt" sqrt ballsExactPositive 1000
    , bgroupOp2 "add" (+) balls balls 1000
    , bgroupOp2 "mul" (*) balls balls 1000
    , bgroupOp2 "div" (/) balls ballsPositive 1000
    ]

bgroupOp :: String -> (MPBall -> MPBall) -> [MPBall] -> Integer -> Benchmark
bgroupOp name op vals step = bgroup name
  [
    bench (show i) $
      nf
        (benchmarkOp op
          (vals !! (unsafePerformIO $ randomRIO (1,maxIndex))))
        (prec i)
    | i <- map (step *) [1..10]
  ]

bgroupOp2 :: String -> (MPBall -> MPBall -> MPBall) -> [MPBall] -> [MPBall] -> Integer -> Benchmark
bgroupOp2 name op vals1 vals2 step = bgroup name
  [
    bench (show i) $
      nf
        (benchmarkOp2 op
          (vals1 !! (unsafePerformIO $ randomRIO (1,maxIndex)))
          (vals2 !! (unsafePerformIO $ randomRIO (1,maxIndex)))
        )
        (prec i)
    | i <- map (step *) [1..10]
  ]

maxIndex :: Integer
maxIndex = 1000

benchmarkOp :: (MPBall -> MPBall) -> MPBall -> Precision -> MPBall
benchmarkOp op x p = op $ setPrecision p x

benchmarkOp2 :: (MPBall -> MPBall -> MPBall) -> MPBall -> MPBall -> Precision -> MPBall
benchmarkOp2 op x y p = op (setPrecision p x) (setPrecision p y)
