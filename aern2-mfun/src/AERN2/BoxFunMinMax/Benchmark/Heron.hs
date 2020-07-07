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
import System.Environment
import System.IO.Unsafe

main :: IO ()
main = 
  do
    args <- getArgs
    case args of
      [vc] ->
        case (vc) of
          "sine" -> print checkSineVC
          "heronInit" -> print checkHeronInitExact
          _ ->
            print "Not supported"
      (vc: [i]) ->
        case (vc) of
          "heronPreservation" -> print $ checkHeronPreservationExact (read i :: Integer)
          "heronPreservationYGE" -> print $ checkHeronPreservationExactYGE (read i :: Integer)
          "heronPreservationYLE" -> print $ checkHeronPreservationExactYLE (read i :: Integer)
          _ -> print "Not supported"
      _ -> print "Not supported"
