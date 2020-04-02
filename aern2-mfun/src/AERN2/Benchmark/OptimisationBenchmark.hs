module Main where

import MixedTypesNumPrelude
import AERN2.BoxFun.TestFunctions
import AERN2.BoxFun.Optimisation
import AERN2.MP.Ball
-- import AERN2.Real
-- import AERN2.MP.Dyadic
-- import AERN2.MP.Accuracy
-- import AERN2.AD.Differential

-- import qualified Data.List as List

-- import AERN2.BoxFun.Type
-- import AERN2.Linear.Vector.Type as V

main :: IO ()
main = 
    let
        ac = bits 100
    in
    do
    putStrLn $ "ratz: "       ++ (show $ minFun ratz4        ac)
    putStrLn $ "griewank 2: " ++ (show $ minFun (griewank 2) ac)
    putStrLn $ "shekel 2: "   ++ (show $ minFun shekel       ac)
    putStrLn $ "himmelblau: " ++ (show $ minFun himmelblau   ac)
    putStrLn $ "rosenbrock: " ++ (show $ minFun rosenbrock   ac)
    putStrLn $ "trefethen: "  ++ (show $ minFun siam4        ac)
    putStrLn $ "griewank 5: " ++ (show $ minFun (griewank 5) ac)
    putStrLn $ "griewank 7: " ++ (show $ minFun (griewank 7) ac)
