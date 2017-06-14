{-|
    Module      :  Main (file BallOps.hs)
    Description :  weight benchmarks for MPBall operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
import MixedTypesNumPrelude
-- import Prelude

import Text.Printf

import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import Weigh

import AERN2.MP.Float
import AERN2.MP.Ball
import AERN2.MP.Ball.Tests () -- instance Arbitrary MPBall

import Values

main :: IO ()
main = mainWith $ sequence
    [ bgroupOp "exp" exp ballsSmallExact 1000
    , bgroupOp "log" log ballsExactPositive 1000
    , bgroupOp "sqrt" sqrt ballsExactPositive 10000
    , bgroupOp2 "add" (+) balls balls 10000
    , bgroupOp2 "mul" (*) balls balls 10000
    , bgroupOp2 "div" (/) balls ballsPositive 10000
    ]

bgroupOp :: String -> (MPBall -> MPBall) -> [MPBall] -> Integer -> Weigh ()
bgroupOp name op vals step = sequence_
  [
    func (name ++ show p) (benchmarkOp op) $
      unsafePerformIO $ sequence
      [
        do
        i1 <- randomRIO (1,maxIndex)
        let x = setPrecision (prec p) $ vals !! i1
        -- printf "bgroupOp: %s: p = %d, j = %d, i1 = %d, x = %s\n" name p _j i1 (show x)
        return x
      | _j <- [1..1000]
      ]
  | p <- map (step *) [1..10]
  ]

bgroupOp2 :: String -> (MPBall -> MPBall -> MPBall) -> [MPBall] -> [MPBall] -> Integer -> Weigh ()
bgroupOp2 name op vals1 vals2 step = sequence_
  [
    func (name ++ show p) (benchmarkOp2 op) $
      unsafePerformIO $ sequence
      [
        do
        i1 <- randomRIO (1,maxIndex)
        i2 <- randomRIO (1,maxIndex)
        return (setPrecision (prec p) $ vals1 !! i1, setPrecision (prec p) $ vals2 !! i2)
      | _ <- [1..100]
      ]
  | p <- map (step *) [1..10]
  ]

maxIndex :: Integer
maxIndex = 1000

benchmarkOp :: (MPBall -> MPBall) -> [MPBall] -> [MPBall]
benchmarkOp op xs = map op xs

benchmarkOp2 :: (MPBall -> MPBall -> MPBall) -> [(MPBall, MPBall)] -> [MPBall]
benchmarkOp2 op pairs = map (uncurry op) pairs
