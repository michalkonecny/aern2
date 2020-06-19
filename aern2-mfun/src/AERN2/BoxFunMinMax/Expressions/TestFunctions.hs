module AERN2.BoxFunMinMax.Expressions.TestFunctions where

import MixedTypesNumPrelude
import AERN2.BoxFunMinMax.Expressions.Eliminator
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Expressions.Translators.DReal
import AERN2.BoxFunMinMax.Expressions.Translators.Tree
import qualified AERN2.BoxFunMinMax.Type as T

simpleMax = EBinOp Max (Lit 1.0) (EUnOp Negate (Lit 1.0))
simpleMin = EBinOp Min (Lit 1.0) (EUnOp Negate (Lit 1.0))

simpleMixed = EBinOp Add (EBinOp Min (Lit 4.0) (Lit (-8.0))) (EBinOp Max (Lit 7.9) (Lit 4.0))

heronPreservationM =
    FConn
      Impl -- ->
      (FComp 
        Le -- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) -- |sqrt x - y|
        (EBinOp Add (EBinOp Pow (Lit 0.5) (EBinOp Pow (Lit 2.0) (EBinOp Sub (Var "i") (Lit 1.0)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (EBinOp Sub (Var "i") (Lit 1.0)))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le -- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) -- |sqrt x - (y+x/y)/2|
        (EBinOp Add (EBinOp Pow (Lit 0.5) (EBinOp Pow (Lit 2.0) (Var "i"))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (EBinOp Sub (Var "i") (Lit 1.0)))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi1 =
    FConn
      Impl -- ->
      (FComp 
        Le -- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) -- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^!0)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 0.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le -- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) -- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^!1)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 0.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi2 =
    FConn
      Impl -- ->
      (FComp 
        Le -- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) -- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^!1)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 1.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le -- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) -- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^!2)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 1.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi3 =
    FConn
      Impl -- ->
      (FComp 
        Le -- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) -- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^!2)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 2.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le -- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) -- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^!3)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 2.0))))  -- 0.5^(2^i) + 6 eps * (i-1)


heronPreservationMi4 =
    FConn
      Impl -- ->
      (FComp 
        Le -- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) -- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^!3)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 3.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le -- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) -- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^!4)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 3.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi5 =
    FConn
      Impl -- ->
      (FComp 
        Le -- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) -- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^!4)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 4.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le -- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) -- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^!5)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/!8388608))) (Lit 4.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

generateSeperateHeronDrealFiles :: IO ()
generateSeperateHeronDrealFiles =
  do
    writeFile 
      "heronPreservationMi1.smt2" 
      (expressionAndDomainsToDreal
        (simplifyE (qualifiedEsToCNF (minMaxAbsEliminator (fToE heronPreservationMi1))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi2.smt2" 
      (expressionAndDomainsToDreal
        (simplifyE (qualifiedEsToCNF (minMaxAbsEliminator (fToE heronPreservationMi2))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi3.smt2" 
      (expressionAndDomainsToDreal
        (simplifyE (qualifiedEsToCNF (minMaxAbsEliminator (fToE heronPreservationMi3))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi4.smt2" 
      (expressionAndDomainsToDreal
        (simplifyE (qualifiedEsToCNF (minMaxAbsEliminator (fToE heronPreservationMi4))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi5.smt2" 
      (expressionAndDomainsToDreal
        (simplifyE (qualifiedEsToCNF (minMaxAbsEliminator (fToE heronPreservationMi5))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))

generateUnifiedHeronDrealFiles :: IO ()
generateUnifiedHeronDrealFiles =
  writeFile 
    "heronPreservationM.smt2" 
    (expressionAndDomainsToDreal
      (simplifyE (qualifiedEsToCNF (minMaxAbsEliminator (fToE heronPreservationM))))
      [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [("i", (0.0, 5.0))] (0.5^!(-23)))

generateHeronTree :: T.MinMaxTree
generateHeronTree =
  T.Min
  [
    qualifiedEsToTree (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi1))) [("x", (0.5, 2.0)), ("y", (0.8, 1.8))],
    qualifiedEsToTree (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi2))) [("x", (0.5, 2.0)), ("y", (0.8, 1.8))],
    qualifiedEsToTree (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi3))) [("x", (0.5, 2.0)), ("y", (0.8, 1.8))],
    qualifiedEsToTree (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi4))) [("x", (0.5, 2.0)), ("y", (0.8, 1.8))],
    qualifiedEsToTree (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi5))) [("x", (0.5, 2.0)), ("y", (0.8, 1.8))]
  ]
