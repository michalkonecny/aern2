module Main where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Control.Applicative (liftA2)

import System.Environment

import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall

import AERN2.Poly.Cheb
import AERN2.Poly.Cheb.Maximum
import AERN2.Poly.Cheb.MaxNaive

import qualified AERN2.Local as Local
import qualified AERN2.Local.Poly as Local
import AERN2.Local.DPoly

main :: IO ()
main =
  do
  args <- getArgs
  (computationDescription, result) <- processArgs args
  putStrLn $ computationDescription
  putStrLn $ "result = " ++ show result
  putStrLn $ "accuracy: " ++ show (getAccuracy result)
  putStrLn $ "precision = " ++ show (getPrecision result)

processArgs :: [String] -> IO (String, MPBall)
processArgs
  [alg, fun] =
    let
    desc =
      "Computing maximum of "++(funName fun)++" over [-1,1] using "++(algName alg)++"."
    in
    case alg of
      "A" ->
        do
        let f = case fun of
                "0" ->
                  let x = setAccuracyGuide (bits 70) _chPolyX in
                    sin(10*x) + cos(20*x) + 7*x^!3
                "1" ->
                  let x = setAccuracyGuide (bits 80) _chPolyX in
                    10*sin(10*x)^!2 + 20*cos(20*x)^!2
                "2" ->
                  let x = setAccuracyGuide (bits 70) _chPolyX in
                    sin(10*sin(10*x) + 20*x^!2) + cos(20*x)
                _ -> error "unkown function code."
        return (desc, AERN2.Poly.Cheb.MaxNaive.maxNaive f (-1.0) (1.0) (bits 70))
      "B" ->
        do
        let f = case fun of
                "0" ->
                  let x = setAccuracyGuide (bits 70) _chPolyX in
                  sin(10*x) + cos(20*x) + 7*x^!3
                "1" ->
                  let x = setAccuracyGuide (bits 80) _chPolyX in
                  10*sin(10*x)^!2 + 20*cos(20*x)^!2
                "2" ->
                  let x = setAccuracyGuide (bits 70) _chPolyX in
                  sin(10*sin(10*x) + 20*x^!2) + cos(20*x)
                _ -> error "unkown function code."
        return (desc, AERN2.Poly.Cheb.Maximum.maximumOptimised f (mpBall $ -1) (mpBall 1) 5 5)
      "C" ->
        do
        let x = Local.variable
        let f = case fun of
                "0" -> sin(10*x) + cos(20*x) + 7*x^!3
                "1" -> 10*sin(10*x)^!2 + 20*cos(20*x)^!2
                "2" -> sin(10*sin(10*x) + 20*x^!2) + cos(20*x)
                _ -> error "unkown function code."
        return (desc, Local.maximum f (mpBall $ -1) (mpBall 1) (bits 53))
      "D" ->
        do
        let x = Local.variable
        let f = case fun of
                "0" ->
                  DPoly (sin(10*x) + cos(20*x) + 7*x^!3)
                        (\z -> sin(10*z) + cos(20*z) + 7*z^!3)
                        (\z -> 10*cos(10*z) - 20*sin(20*z) + 21*z^!2)
                "1" ->
                  DPoly (10*sin(10*x)^!2 + 20*cos(20*x)^!2)
                        (\z -> 10*sin(10*z)^!2 + 20*cos(20*z)^!2)
                        (\z -> 200*sin(10*z)*cos(10*z) - 800*cos(20*z)*sin(20*z))
                "2" ->
                  DPoly (sin(10*sin(10*x) + 20*x^!2) + cos(20*x))
                        (\z -> sin(10*sin(10*z) + 20*z^!2) + cos(20*z))
                        (\z -> cos(10*sin(10*z) + 20*z^!2)*(100*cos(10*z) + 40*z) - 20*sin(20*z))
                _ -> error "unkown function code."
        return (desc, Local.maximum f (mpBall $ -1) (mpBall 1) (bits 53))
      _   -> error "unkown algorithm code."
processArgs _ = error "usage: <algorithm code> <function code>"

algName :: String -> String
algName "A" = "naive maximisation"
algName "B" = "maximisation with global approximations"
algName "C" = "maximisation with local approximations"
algName "D" = "maximisation with local approximations and enrichments"
algName _ = undefined

funName :: String -> String
funName "0" = "sin(10*x) + cos(20*x) + 7*x^3"
funName "1" = "10*sin(10*x)^2 + 20*cos(20*x)^2"
funName "2" = "sin(10*sin(10*x) + 20*x^2) + cos(20*x)"
funName _ = undefined
