{-|
    Module      :  Main (file aern2-real-benchOp)
    Description :  execute a simple CR expression
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

import AERN2.MP
import AERN2.Real
import AERN2.Real.Tests () -- instance Arbitrary CauchyReal

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, result) <- processArgs args
    putStrLn $ computationDescription
    putStrLn $ "result = " ++ show result

processArgs ::
    [String] ->
    IO (String, MPBall)
processArgs [op, accuracyS] =
    return (computationDescription, result)
    where
    computationDescription =
        printf "computing %s using accuracy %d" op ac
    ac :: Integer
    ac = read accuracyS

    result =
      case op of
        -- "exp" ->
        --   map ((? (bitsS ac)) . exp) $
        --     unsafePerformIO $ pickValues valuesSmall count
        -- "log" ->
        --   map ((~!) . (? (bitsS ac)) . log) $
        --     unsafePerformIO $ pickValues valuesPositive count
        "sqrt2" ->
          ((~!) . (? (bitsS ac)) . sqrt) 2
        -- "cos" ->
        --   map ((? (bitsS ac)) . cos) $
        --     unsafePerformIO $ pickValues values count
        -- "add" ->
        --   map ((? (bitsS ac)) . (uncurry (+))) $
        --     unsafePerformIO $ pickValues2 values values count
        -- "mul" ->
        --   map ((? (bitsS ac)) . (uncurry (*))) $
        --     unsafePerformIO $ pickValues2 values values count
        -- "div" ->
        --   map ((~!) . (? (bitsS ac)) . (uncurry (/))) $
        --     unsafePerformIO $ pickValues2 values valuesPositive count
        -- "logistic" ->
        --   map ((? (bitsS ac)) . (logistic 3.82 count)) $
        --     [real 0.125]
        _ -> error $ "unknown op " ++ op
processArgs _ =
    error "expecting arguments: <operation> <precision>"

-- logistic :: Rational -> Integer -> CauchyReal -> CauchyReal
-- logistic c n x
--   | n == 0 = x
--   | otherwise = logistic c (n-1) $ c * x * (1-x)

