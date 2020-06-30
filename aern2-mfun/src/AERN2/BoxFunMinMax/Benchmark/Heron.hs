module Main where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import qualified Data.List as List

import AERN2.AD.Differential hiding (x)
-- import AERN2.Util.Util
import AERN2.BoxFun.Type
import AERN2.Linear.Vector.Type ((!), Vector)
import qualified AERN2.Linear.Vector.Type as V
import AERN2.BoxFunMinMax.Type
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Expressions.Eliminator
import AERN2.BoxFunMinMax.Expressions.TestFunctions

main :: IO ()
main = 
  print $ checkECNF (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi1)))) [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] (bits 200) (prec 200)