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
  print $ checkECNF 
      heronCNF 
      -- (heronDisjunction 1) 
      [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] (bits 100) (prec 100)
  where
  heronDisjunction j = [heronCNF !! (j-1)]
  heronCNF = (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi5))))


{-
  A run by Michal on 1st July 2020:

  $ /usr/bin/time -v aern2-mfun-heron-benchmark +RTS -N4 |& tee i5.log | grep ": { " > i5.boxes
  $ grep "wall clock" i5.log 
        Elapsed (wall clock) time (h:mm:ss or m:ss): 2:05:43
  $ wc -l i5.boxes
  7531826 i5.boxes
  
-}