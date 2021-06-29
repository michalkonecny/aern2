{-# OPTIONS_GHC -Wno-missing-signatures #-}
module AERN2.BoxFunMinMax.Expressions.TestFunctions where

import MixedTypesNumPrelude
-- import AERN2.BoxFunMinMax.Expressions.Eliminator
import AERN2.BoxFunMinMax.Expressions.Type
-- import AERN2.BoxFunMinMax.Expressions.Translators.DReal
-- import AERN2.BoxFunMinMax.Expressions.Translators.MetiTarski
import qualified AERN2.BoxFunMinMax.Type as T
import AERN2.MP.Precision (prec)

simpleMax = EBinOp Max (Lit 1.0) (EUnOp Negate (Lit 1.0))
simpleMin = EBinOp Min (Lit 1.0) (EUnOp Negate (Lit 1.0))

simpleMixed = EBinOp Add (EBinOp Min (Lit 4.0) (Lit (-8.0))) (EBinOp Max (Lit 7.9) (Lit 4.0))

heronPreservationM =
    FConn
      Impl -- ->
      (FComp 
        Le ------ |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) ------ |sqrt x - y|
        (EBinOp Add (EBinOp Pow (Lit 0.5) (EBinOp Pow (Lit 2.0) (EBinOp Sub (Var "i") (Lit 1.0)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (EBinOp Sub (Var "i") (Lit 1.0)))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le ------ |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) ------ |sqrt x - (y+x/y)/2|
        (EBinOp Add (EBinOp Pow (Lit 0.5) (EBinOp Pow (Lit 2.0) (Var "i"))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (EBinOp Sub (Var "i") (Lit 1.0)))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi1 =
    FConn
      Impl -- ->
      (FComp 
        Le ---- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) ---- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^0)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 0.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le ---- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) ---- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^1)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 0.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi2 =
    FConn
      Impl -- ->
      (FComp 
        Le ---- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) ---- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^1)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 1.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le ---- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) ---- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^2)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 1.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi3 =
    FConn
      Impl -- ->
      (FComp 
        Le ---- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) ---- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^2)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 2.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le ---- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) ---- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^3)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 2.0))))  -- 0.5^(2^i) + 6 eps * (i-1)


heronPreservationMi4 =
    FConn
      Impl -- ->
      (FComp 
        Le ---- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) ---- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^3)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 3.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le ---- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) ---- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^4)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 3.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

heronPreservationMi5 =
    FConn
      Impl -- ->
      (FComp 
        Le ---- |sqrt x - y| <= 0.5^(2^(i-1)) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (Var "y"))) ---- |sqrt x - y|
        (EBinOp Add (PowI (Lit 0.5) (2^4)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 4.0))))  -- 0.5^(2^(i-1)) + 6 eps * (i-1)
      (FComp 
        Le ---- |sqrt x - (y+x/y)/2| <= 0.5^(2^i) + 6 eps * (i-1)
        (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "x")) (EBinOp Div (EBinOp Add (Var "y") (EBinOp Div (Var "x") (Var "y"))) (Lit 2.0)))) ---- |sqrt x - (y+x/y)/2|
        (EBinOp Add (PowI (Lit 0.5) (2^5)) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (1/8388608))) (Lit 4.0))))  -- 0.5^(2^i) + 6 eps * (i-1)

-- heronInitExact2 =
--   vc
--   where
--     vc =
--       FConn
--       Impl
--       (FConn 
--         And
--         (FComp Ge (EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 8.940697e-08)) (Lit 0.0))
--         (FComp Ge (EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 8.940697e-08)) (Lit 0.0)))
--       (FComp Ge 
--         (EBinOp Sub
--         (EBinOp Add
--           (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0)))))
--           (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^1))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit 1.0) (Lit (1/8388608))))))
--         (Lit 8.940697e-08)) (Lit 0.0))

maxFloat :: Rational
maxFloat = (2.0 - 2.0^(-23)) * 2.0^127

testX = [(Var "X")]
testMx = [EBinOp Sub (Lit 0.0) (Var "X")]
testX2 = [EBinOp Mul (Var "X") (Lit 2.0)]

testXm2 = [EBinOp Mul (Var "X") (Lit (-2.0))]
testXs2 = [EBinOp Sub (Var "X") (Lit (2.0))]
testXp2s4 = [EBinOp Sub (EBinOp Mul (Var "X") (Var "X")) (Lit (4.0))]

testXY = [EBinOp Mul (Var "X") (Var "Y")]
testXp2 = [EBinOp Add (EBinOp Mul (Var "X") (Var "X")) (Lit testEps)]
testXp3 = [EBinOp Mul (Var "X") (EBinOp Mul (Var "X") (Var "X"))]
testMXp3 :: [E]
testMXp3 = [EBinOp Mul (EUnOp Negate (Var "X")) (EBinOp Mul (EUnOp Negate (Var "X")) (EUnOp Negate (Var "X")))]

-- testXp3Float = [Float (EBinOp Mul (Var "X") (EBinOp Mul (Var "X") (Var "X"))) 2]

testEps :: Rational
testEps = (1) % 1000000

-- [("X", (0.5, 2.0))]
-- heronInit1PlusXDiv1 = Float (EBinOp Add (Lit 1.0) (Float (EBinOp Div (Var "X") (Lit 1.0)) 24)) 24

-- List.map (List.map (\e -> apply (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testMoreDep
-- List.map (List.map (\e -> gradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testMoreDep
-- List.map (List.map (\e -> gradientUsingGradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testMoreDep
testMoreDep =
  [
    [
      -- X^2 + X - eps
      EBinOp Sub (EBinOp Add (PowI (Var "X") 2) (Var "X")) (Lit testEps),
      -- X^2 - X - eps
      EBinOp Sub (EBinOp Sub (PowI (Var "X") 2) (Var "X")) (Lit testEps)
    ]
  ]

-- List.map (List.map (\e -> apply (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testLessDep
-- List.map (List.map (\e -> gradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testLessDep
-- List.map (List.map (\e -> gradientUsingGradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testLessDep
testLessDep =
  [
    [
      -- (X + 1/2)^2 - 1/4 - eps
      EBinOp Sub (EBinOp Sub (PowI (EBinOp Add (Var "X") (Lit 0.5)) 2) (Lit 0.25)) (Lit testEps),
      -- (X - 1/2)^2 + 1/4 - eps
      EBinOp Sub (EBinOp Sub (PowI (EBinOp Sub (Var "X") (Lit 0.5)) 2) (Lit 0.25)) (Lit testEps)
    ]
  ]

-- List.map (List.map (\e -> gradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testXp3OrMXp3Pow
-- List.map (List.map (\e -> gradientUsingGradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testXp3OrMXp3Pow
testXp3OrMXp3Pow = 
  [
    [
      -- X^3 - testEps >= 0
      EBinOp Sub (PowI (Var "X") 3) (Lit testEps), 
      -- (-X)^3 - testEps >=0
      EBinOp Sub (PowI (EUnOp Negate (Var "X")) 3) (Lit testEps)
    ]
  ]

-- List.map (List.map (\e -> gradient (expressionToBoxFun e [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)))) testXp3Yp3OrMXp3MYp3Pow
-- List.map (List.map (\e -> gradientUsingGradient (expressionToBoxFun e [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)))) testXp3Yp3OrMXp3MYp3Pow
testXp3Yp3OrMXp3MYp3Pow = 
  [
    [
      -- X^3 + Y^3 - testEps >= 0
      EBinOp Sub (EBinOp Add (PowI (Var "X") 3) (PowI (Var "Y") 3)) (Lit testEps), 
      -- (-X)^3 + (-Y)^3 - testEps >=0
      EBinOp Sub (EBinOp Add (PowI (EUnOp Negate (Var "X")) 3) (PowI (EUnOp Negate (Var "Y")) 3)) (Lit testEps)
    ]
  ]

-- List.map (List.map (\e -> gradient (expressionToBoxFun e [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)))) testXp3Yp3OrMXp3MYp3PowFloat
-- List.map (List.map (\e -> gradientUsingGradient (expressionToBoxFun e [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0)), ("Y", (-1.0, 1.0))] (prec 100)))) testXp3Yp3OrMXp3MYp3PowFloat
-- testXp3Yp3OrMXp3MYp3PowFloat = 
--   [
--     [
--       -- X^3 + Y^3 - testEps >= 0
--       Float (EBinOp Sub (EBinOp Add (PowI (Var "X") 3) (PowI (Var "Y") 3)) (Lit testEps)) 24, 
--       -- (-X)^3 + (-Y)^3 - testEps >=0
--       Float (EBinOp Sub (EBinOp Add (PowI (EUnOp Negate (Var "X")) 3) (PowI (EUnOp Negate (Var "Y")) 3)) (Lit testEps)) 24
--     ]
--   ]


-- List.map (List.map (\e -> gradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testXp3OrMXp3
-- List.map (List.map (\e -> gradientUsingGradient (expressionToBoxFun e [("X", (-1.0, 1.0))] (prec 100)) (fromVarMap [("X", (-1.0, 1.0))] (prec 100)))) testXp3OrMXp3
testXp3OrMXp3 = 
  [
    [
      -- X^3 - testEps >= 0
      EBinOp Sub (EBinOp Mul (Var "X") (EBinOp Mul (Var "X") (Var "X"))) (Lit testEps), 
      -- (-X)^3 - testEps >=0
      EBinOp Sub (EBinOp Mul (EUnOp Negate (Var "X")) (EBinOp Mul (EUnOp Negate (Var "X")) (EUnOp Negate (Var "X")))) (Lit testEps)
    ]
  ]

-- checkECNFSimplex (minMaxAbsEliminatorECNF (heronPreservationExact 1)) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)

badHeron = EUnOp Negate (EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (2 % 1))) (Lit (11744051 % 16777216))) (Lit (1192093 % 10000000000000)))
-- badF = EUnOp Negate (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))))
-- gradient (expressionToBoxFun badHeron [("X", (0.5, 2.0)), ("Y1", (0.7, 1.8))] (prec 100)) (fromVarMap [("X", (0.5, 2.0)), ("Y1", (0.7, 1.8))] (prec 100))

-- getValueIfNoErrorCE badCn (const 1.0) (const 0.0)  returns 0.0
-- getValueIfNoErrorCE goodCn (const 1.0) (const 0.0) returns 1.0

-- getValueIfNoErrorCE goodCn (id) (const (mpBall 0)) returns the value of cn

-- badCn = gradient (expressionToBoxFun badHeron [("X", (0.5, 2.0)), ("Y1", (0.7, 1.8))] (prec 100)) (fromVarMap [("X", (0.5, 2.0)), ("Y1", (0.7, 1.8))] (prec 100))

-- branch =
--   if badCn
--     then error "branch 1"
--     else error "branch 2"

-- Can also case using getMaybeValueCE

-- Try two expressions (X^3 and -X^3) are above just under zero
-- Try two expressions (X^3 and -X^3) are just above zero/at zero (should zoom in)


heronInitExact =
  -- ps -> q
  -- !ps \/ q
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context=
      [
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 8.940697e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 8.940697e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07)
      ]  
    goal = 
      (EBinOp Sub
      (EBinOp Add
        (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0)))))
        (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^1))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit 1.0) (Lit (1/8388608))))))
      (Lit 8.940697e-08))

heronPreservationExact :: Integer -> [[E]]
heronPreservationExact i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Lit maxFloat)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (Lit (-maxFloat))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Lit maxFloat)) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (-maxFloat))) (Lit 2.384186e-07)
      ]
    -- goal = -- Heron Pres Sub
    --   EBinOp Sub
    --     (EBinOp Add
    --       (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
    --       (EBinOp Sub (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
    --     (Lit 1.192093e-07)
    -- goal = -- Heron Pres Swap
    --   EBinOp Sub
    --     (EBinOp Add
    --       (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "Y1")) (EBinOp Div (EBinOp Add (Var "X") (EBinOp Div (Var "Y1") (Var "X"))) (Lit 2.0)))))
    --       (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
    --     (Lit 1.192093e-07)    
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) 
            (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X"))
                       (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Add 
            (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) 
            (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
        (Lit 1.192093e-07)
        -- (Lit 0.1)

heronPreservationExactIVar :: [[E]]
heronPreservationExactIVar =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (EBinOp Pow (Lit 2.0) (EBinOp Pow (Lit 2.0) (Var "i"))))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Var "i")) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Lit maxFloat)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (Lit (-maxFloat))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Lit maxFloat)) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (-maxFloat))) (Lit 2.384186e-07)
      ]
      
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) 
            (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X"))
                       (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Add 
            (EBinOp Div (Lit 1.0) (EBinOp Pow (Lit 2.0) (EBinOp Pow (Lit 2.0) (EBinOp Add (Var "i") (Lit 1.0))))) 
            (EBinOp Mul (Lit 6.0) (EBinOp Mul (EBinOp Add (Var "i") (Lit 1.0)) (Lit (1/8388608))))))
        (Lit 1.192093e-07)
 
heronPreservationExactSwap :: Integer -> [[E]]
heronPreservationExactSwap i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Lit maxFloat)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (Lit (-maxFloat))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Lit maxFloat)) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (-maxFloat))) (Lit 2.384186e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "Y1")) (EBinOp Div (EBinOp Add (Var "X") (EBinOp Div (Var "Y1") (Var "X"))) (Lit 2.0)))))
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
        (Lit 1.192093e-07)

heronPreservationExactSub :: Integer -> [[E]]
heronPreservationExactSub i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Lit maxFloat)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (Lit (-maxFloat))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Lit maxFloat)) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (-maxFloat))) (Lit 2.384186e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Sub (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
        (Lit 1.192093e-07)


heronPreservationExactYGE i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Lit maxFloat)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (Lit (-maxFloat))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Lit maxFloat)) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (-maxFloat))) (Lit 2.384186e-07)
      ]
    goal =
      EBinOp Sub (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07)

heronPreservationExactYLE i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Lit maxFloat)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (Lit (-maxFloat))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Lit maxFloat)) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (-maxFloat))) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07)
      ]
    goal =
        EBinOp Sub (EBinOp Add (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)

sineVC =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Mul (Var "X") (Var "X")) (Lit maxFloat)) (Lit 2.980232e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Mul (Var "X") (Var "X"))) (Lit maxFloat)) (Lit 2.980232e-08),
        EBinOp Add (EBinOp Add (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0)))) (Lit maxFloat)) (Lit 1.198778e-09),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0))))) (Lit maxFloat)) (Lit 1.198778e-09),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0))))) (Lit maxFloat)) (Lit 8.649358e-09),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0)))))) (Lit maxFloat)) (Lit 8.649358e-09),
        EBinOp Add (EBinOp Add (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0)))))) (Lit maxFloat)) (Lit 2.081273e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0))))))) (Lit maxFloat)) (Lit 2.081273e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 1.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0))))))) (Lit maxFloat)) (Lit 5.960586e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Sub (Lit 1.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0)))))))) (Lit maxFloat)) (Lit 5.960586e-08),
        EBinOp Add (EBinOp Add (EBinOp Mul (Var "X") (EBinOp Sub (Lit 1.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0)))))))) (Lit maxFloat)) (Lit 8.034919e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Mul (Var "X") (EBinOp Sub (Lit 1.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0))))))))) (Lit maxFloat)) (Lit 8.034919e-08)
      ]
    goal =
      EBinOp Sub (EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sin (Var "X")) (EBinOp Mul (Var "X") (EBinOp Sub (Lit 1.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0)))))))))))
      (Lit 0.000003000000106112565845251083374023437500)) (EBinOp Mul (Lit 12.0) (Lit 0.0000001192092895507812500000000000000000000000))) (Lit 8.060364e-08)

heronInitExactNoMaxFloat =
  -- ps -> q
  -- !ps \/ q
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context=
      [
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 8.940697e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 8.940697e-08)
      ]  
    goal = 
      (EBinOp Sub
      (EBinOp Add
        (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit 2.0)))))
        (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^1))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit 1.0) (Lit (1/8388608))))))
      (Lit 8.940697e-08))

heronPreservationExactNoMaxFloat :: Integer -> [[E]]
heronPreservationExactNoMaxFloat i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
        (Lit 1.192093e-07)

heronPreservationExactNoMaxFloatSwap :: Integer -> [[E]]
heronPreservationExactNoMaxFloatSwap i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "Y1")) (EBinOp Div (EBinOp Add (Var "X") (EBinOp Div (Var "Y1") (Var "X"))) (Lit 2.0)))))
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
        (Lit 1.192093e-07)

heronPreservationExactNoMaxFloatSub :: Integer -> [[E]]
heronPreservationExactNoMaxFloatSub i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Sub (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/8388608))))))
        (Lit 1.192093e-07)


heronPreservationExactYGENoMaxFloat i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608)))
      ]
    goal =
      EBinOp Sub (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07)

heronPreservationExactYLENoMaxFloat i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07)
      ]
    goal =
        EBinOp Sub (EBinOp Add (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)

sineVCNoMaxFloat =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
      ]
    goal =
      EBinOp Sub (EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sin (Var "X")) (EBinOp Mul (Var "X") (EBinOp Sub (Lit 1.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0)))))))))))
      (Lit 0.000003000000106112565845251083374023437500)) (EBinOp Mul (Lit 12.0) (Lit 0.0000001192092895507812500000000000000000000000))) (Lit 8.060364e-08)

bisectionRootFinder = 
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Sub (Lit 0.0) (Var "A")) (Var "B"),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Sqrt (EBinOp Sub (Var "A") (Lit 1.0)))) (EUnOp Sin (Var "A"))) (Lit 2.033996e-04),
        EBinOp Add (EBinOp Sub (EUnOp Sqrt (EBinOp Sub (Var "B") (Lit 1.0))) (EUnOp Sin (Var "B"))) (Lit 2.033996e-04),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Sub (Var "B") (Var "B"))) (Lit maxFloat)) (Lit 4.656613e-10),
        EBinOp Add (EBinOp Add (EBinOp Sub (Var "B") (Var "A")) (Lit maxFloat)) (Lit 4.656613e-10),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Sub (Var "B") (Var "A"))) (Lit 0.001000000047497451305389404296875)) (Lit 4.656613e-10)
      ]
    goal =
      EBinOp Sub (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (EBinOp Sub (Var "B") (Lit 1.0))) (EUnOp Sin (Var "B"))))) (Lit 0.00200000009499490261077880859375)) (Lit 2.033996e-04)

yannickSineVC =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = []
    goal =
      (EBinOp Add (EBinOp Sub (EBinOp Sub (Lit 0.0) (EBinOp Mul (Var "X") (EBinOp Sub (Lit 1.0) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.16666667163372039794921875) (EBinOp Mul (EBinOp Mul (Var "X") (Var "X")) (EBinOp Sub (Lit 0.008333333767950534820556640625) (EBinOp Div (EBinOp Mul (Var "X") (Var "X")) (Lit 5040.0))))))))) (Lit 6.321845e-07)) (EBinOp Mul (Lit 12.0) (Lit 0.0000001192092895507812500000000000000000000000)))

testDisjunction = [EBinOp Sub (PowI (Var "X") 3) (Lit 1.0), EBinOp Sub (PowI (Var "X") 2) (Lit 2.0)]
testDisjunction2 = [EBinOp Sub (PowI (EBinOp Sub (Var "X") (Lit 2.0)) 2) (Lit 1.0),
                    EBinOp Add (EBinOp Mul (PowI (Var "X") 3) (Lit (-1.0))) (Lit 2.0)]


checkBisectionRootFinder = T.checkECNF bisectionRootFinder [("A", (1.0001, 5.0)), ("B", (1.0001, 5.0))] (prec 100)

-- generateHeronInitMetiTarski =
--   writeFile
--     "heronInitExact.tptp"
--     (cnfExpressionAndDomainsToMetiTarski heronInitExact
--         [("X", (0.5, 2.0))] (0.5^(-23)))

-- generateHeronPreservationExactMetiTarski =
--   do
--     writeFile 
--       "heronPreservationExacti1.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 1)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExacti2.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 2)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExacti3.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 3)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExacti4.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 4)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))

-- generateHeronPreservationExactMetiTarskiYGE =
--   do
--     writeFile 
--       "heronPreservationExactYGEi1.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 1)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYGEi2.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 2)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYGEi3.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 3)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYGEi4.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 4)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))

-- generateHeronPreservationExactMetiTarskiYLE =
--   do
--     writeFile 
--       "heronPreservationExactYLEi1.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 1)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYLEi2.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 2)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYLEi3.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 3)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYLEi4.tptp" 
--       (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 4)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^(-23)))

-- generateSineExactMetiTarski =
--   writeFile
--     "sineExact.tptp"
--     (cnfExpressionAndDomainsToMetiTarski sineVC
--       [("X", (-(1.0), 1.0))] (0.5^(-23)))

-- generateSeperateHeronPreservationExactDrealFiles =
--   do
--     writeFile 
--       "heronPreservationExacti1.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 1)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExacti2.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 2)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExacti3.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 3)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExacti4.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 4)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))

-- generateSeperateHeronPreservationExactYGEDrealFiles =
--   do
--     writeFile 
--       "heronPreservationExactYGEi1.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 1)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYGEi2.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 2)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYGEi3.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 3)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYGEi4.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 4)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))

-- generateSeperateHeronPreservationExactYLEDrealFiles =
--   do
--     writeFile 
--       "heronPreservationExactYLEi1.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 1)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYLEi2.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 2)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYLEi3.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 3)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))
--     writeFile 
--       "heronPreservationExactYLEi4.smt2" 
--       (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 4)
--         [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^(-23)))

-- generateHeronInitExactDreal =
--   writeFile
--     "heronInitExact.smt2"
--     (cnfExpressionAndDomainsToDreal heronInitExactNoMaxFloat
--       [("X", (0.5, 2.0))] [] (0.5^(-23)))

-- generateSineExactDreal =
--   writeFile
--     "sineExact.smt2"
--     (cnfExpressionAndDomainsToDreal sineVCNoMaxFloat
--       [("X", (-1.0, 1.0))] [] (0.5^(-23)))

{-  
Circle packing

Unit square = [("X", (-1, 1)), ("Y", (-1, 1))] ?

checkECNFSimplex will check if a conjunction of disjunctions of expressions is true

We could use the Simplex library to caclulate the centre points/radius of the circles?

Or just verify known packings

We shrink the square by the radius of the circles on all sides
Distance is calculated as maxWidth of shrunk square

Assuming we know the centre and radius of each circle, the following should be verifiable

circleN are variables

Bound circles within square
circle1 >= (-1, 1)
circle1 <= (-1, 1)
circle2 >= (-1, 1)
circle2 <= (-1, 1)

circleNCentre is a point in X,Y
circleRadius is a rational number

circle1 <= circle1Centre + circleRadius
circle1 >= circle1Centre - circleRadius

circle2 <= circle2Centre + circleRadius
circle2 >= circle2Centre - circleRadius

Actually...

circleN are variables

Bound circles within square
pointSquareX <= X - circleRadius
pointSquareX >= X + circleRadius
pointSquareY <= Y - circleRadius
pointSquareY >= Y + circleRadius

circle1PointX <= pointSquareX - circleRadius
circle1PointX >= pointSquareX + circleRadius
circle1PointY <= pointSquareY - circleRadius
circle1PointY >= pointSquareY + circleRadius

circle2PointX <= pointSquareX - circleRadius
circle2PointX >= pointSquareX + circleRadius
circle2PointY <= pointSquareY - circleRadius
circle2PointY >= pointSquareY + circleRadius

Goals:

|circle1PointX - circle2PointX| <= maxDistance
|circle1PointY - circle2PointY| <= maxDistance

------------------------------




circle1X <= X - circleRadius
circle1X >= X + circleRadius
circle1Y <= Y - circleRadius
circle1Y >= Y + circleRadius

circle2X <= X - circleRadius
circle2X >= X + circleRadius
circle2Y <= Y - circleRadius
circle2Y >= Y + circleRadius

circleNCentre is a point in X,Y
circleNRadius is a rational number

circle1 <= circle1Centre + circle1Radius
circle1 >= circle1Centre - circle1Radius

circle2 <= circle2Centre + circle2Radius
circle2 >= circle2Centre - circle2Radius

Case for n=2

Context:
X1 in [0, 1]
Y1 in [0, 1]
X2 in [0, 1]
Y2 in [0, 1]

Goal:
(X1 - X2)^2 + (Y1 - Y2)^2 <= 2 + eps

Case for n=3

Context:
X1 in [0, 1]
Y1 in [0, 1]
X2 in [0, 1]
Y2 in [0, 1]
X3 in [0, 1]
Y3 in [0, 1]

Goal:
(X1 - X2)^2 + (Y1 - Y2)^2 <= (sqrt(6) - sqrt(2))^2 + eps
\/
(X1 - X3)^2 + (Y1 - Y3)^2 <= (sqrt(6) - sqrt(2))^2 + eps
\/
(X2 - X3)^2 + (Y2 - Y3)^2 <= (sqrt(6) - sqrt(2))^2 + eps

-}

-- gradient (expressionToBoxFun sqrtTest [("X", (1.0, 2.0))] (prec 100)) (fromVarMap [("X", (1.0, 2.0))] (prec 100))
-- gradientUsingGradient (expressionToBoxFun sqrtTest [("X", (1.0, 2.0))] (prec 100)) (fromVarMap [("X", (1.0, 2.0))] (prec 100))
sqrtTest = EUnOp Sqrt (PowI (Var "X") 3)
-- gradient (expressionToBoxFun bugTest [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0))] (prec 100)) (fromVarMap [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0))] (prec 100))
-- gradientUsingGradient (expressionToBoxFun sqrtTest [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0))] (prec 100)) (fromVarMap [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0))] (prec 100))
bugTest = (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2)

epsC = -1.0

-- List.map (\f -> apply f (domain f)) (List.map (\e -> expressionToBoxFun e [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0))] (prec 100)) (head square2p))
-- List.map (\f -> gradient f (domain f)) (List.map (\e -> expressionToBoxFun e [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0))] (prec 100)) (head square2p))
-- decideDisjunctionWithSimplex (head square2p) [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0))] 0.00000001 1.2 (prec 100)
-- checkECNFSimplex square2p [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0))] 0.00000001 1.2 1000 (prec 100)
square2p =
  [negatedContext ++ goal]
  where
    negatedContext = map (EUnOp Negate) context
    context = []
    -- sqrt((X1 - Y1)^2 + (X2 - Y2)^2) <= sqrt(2) + eps
    goal = [fToE goalF testEps]
    goalF = 
      -- FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "Y1")) 2) (PowI (EBinOp Sub (Var "X2") (Var "Y2")) 2)))) (EBinOp Add (EUnOp Sqrt (Lit 2.0)) (Lit epsC))
      -- (X1 - X2)^2 + (Y1 - Y2)^2 <= 2 + eps

      -- (X1 - X2) ^ 2 + (Y1 - Y2) ^ 2 <= 0.1
      FComp Le ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2))) (EBinOp Add ((Lit 2.0)) (Lit epsC))

-- -2X1 + 2*X2
-- -X1^2 + 2*X1*X2
-- (-1 * (X1^2 - 2*X1*X2))
-- (-1 * ((X1 - X2) ^ 2 + (Y1 - Y2) ^ 2) + (2 - 1)
-- EBinOp Add (EUnOp Negate (EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2))) (EBinOp Add (Lit (2 % 1)) (Lit ((-1) % 1)))

-- createPontPackingGoal :: Integer -> [E]
-- createPointPackingGoal numPoints =
--   undefined
--   where
--     epsilon = 0.0000000000000001 

--     knownPackings :: [(Integer, E)]
--     knownPackings = 
--       [
--         (2, EBinOp Add (EUnOp Sqrt (Lit 2.0)) (Lit epsilon)),
--         (3, EBinOp Add (EBinOp Sub (EUnOp Sqrt (Lit 6.0)) (EUnOp Sqrt (Lit 2.0))) (Lit epsilon)),
--         (4, EBinOp Add (Lit 1.0) (Lit epsilon)),
--         (5, EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsilon)),
--         (6, EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsilon))
--       ]

{-
for epsC = 0.001
proved false with apply "X1": [0.0, 8.081025067701121e-9] 
"X2": [0.11662721297468709, 0.11662722105569868] 
"Y1": [0.0, 3.811795632212609e-9] 
"Y2": [0.24717805338943316, 0.24717805720122257] 
"Z1": [0.0, 9.42454136737588e-10] 
"Z2": [0.9995846297270826, 0.9995846306695354] 

(Just False,Just [("X1",(0 % 1,9628098550069506954516869511104103031955687356279045355263268359948734079134134606027470658536031718308707680549843714274807925915133083431306083180549530298656887980212778752227243121086169743191606001180983449746793542881863 % 1191445202731996321957967493221168940227148573062013962661116182880376552179433741123626655902071826533426593117353509168374392620204271619841316133978988379356804722859249253311280022714748916000205017291075471561749696331667996672000)),("X2",(319722502748371388903517821476584499105240114594583234381104910674264464951519161937885164544990095177908447064286955275397450071186308521044707312112783447080781389443851202722056267686371014769119822789794841924127221715760807 % 2741405668484630080496526592652139785553290904860637194862926189526132284277801843571807103017138534569049373963245792874818350424324702964883406683663502163939652388185510195832009454448038159639877054845906240650370699482890240,72664210204932354407616047225914086355896075448999744405782100310519902024439743876040036092718341479314141858009890707170934913238797468095802245816288436553485766223759254917581453988210726064233004138768909927742849963332863 % 623046742837415927385574225602759042171202478377417544287028679437757337335864055357228887049349666947511221355283134744276897823710159764746228791741705037259011906405797771780002148738190490827244785192251418329629704427929600)),("Y1",(0 % 1,9628098550069506954516869511104103031955687356279045355263268359948734079134134606027470658536031718308707680549843714274807925915133083431306083180549530298656887980212778752227243121086169743191606001180983449746793542881863 % 2525869558353196651376315445743575087098041905664003880154252964433080428779489546530673090509179980492737086083045242520653481881033862016901419993068357463388333057519366203290504385376069584650365942492497712466512821471030542336000)),("Y2",(11971230644813230600722867176140021851421884280500806565087156854568634163800999240610314155983918003876395746853604954359918253334297960401401822280067703315222160667769872568220468746455310396634759417073771829088039012951330879 % 48431608230008818557678394070285507131621093260859903396466653272765516324562893260199183607272081420955002373376358486138218601240027380289133911769430071788579417178664883834031519692881068910917226205870460846278147765908275200,37831282309763893729513672943323176253070426814089894801279004280515510994737368697083199640588876241989244989808024153730892940162949973360532847790697217385873188029229264989361599 % 153052753703643781640487342677522891468492519788054898714139024804137153680556409827748270227397237013162713888152071125009486741276099285864582804517338539563390032161548537115443200)),("Z1",(0 % 1,9628098550069506954516869511104103031955687356279045355263268359948734079134134606027470658536031718308707680549843714274807925915133083431306083180549530298656887980212778752227243121086169743191606001180983449746793542881863 % 10215986300827608685207813144049762133005650137501655989686707213619499215273264131266399845694039454162223931736860562475942230269971749336593692981279900522673716002315912694766399726683785935287117057565688751333926319443251560448000)),("Z2",(117481383384747044965435388917933959362357694149638248647540693999739716867660981795559689260170756270562334276681764648151273225566213742431516653935220928720200935886684319805384772619074654442069971568598833315605880845627810649 % 117530201936801562919371202106925570061340517199106980324401958415855912776099313867979840685933896334362434966363006323768194954215556910038913408612902839072800649632204046319722651994090943425806963280127377108771068353991147520,2532428020494619112667007566946970430007870228189837062666766891030368619716664031485889155 % 2533480350531564728026353853279034676745065367550492266329849359332196278482502516092174336))])
-}

-- whole expressions
-- List.map (\f -> apply f (domain f)) (List.map (\e -> expressionToBoxFun e [("X1",(0 % 1,7905428304193763147344722058718882042087942999468583184570219366057484495240075289542401481871026624221 % 34886534858083592458438108985189172331832961488267752582992728857402954519777907739991605337513582374420480)),("X2",(144173611575899455984131794759632339019976210590231542509381925887784554922223588548169931387138570119459 % 1216632319040745753051812134546809768496513228717601005551617162030736315339709310701698662952076773949440,154312214234572337259563562547522420424113820829544884222312068904707357550054499470684086911739209902371 % 1297741140310128803255266276849930419729614110632107739255058306166118736362356598081811907148881892212736)),("Y1",(0 % 1,7905428304193763147344722058718882042087942999468583184570219366057484495240075289542401481871026624221 % 27233603147288186058564644894358024090590840208347048593665520287653113522522361933319063180587844061102080)),("Y2",(929040978017919951708752896168254361941134578059289554997557998184202105011880170864074767288470718834979 % 3747785625288454859424390472907892975932890084235032552728512870201038358028480984614468675081366981836800,7440233252447553376817367891404753777571164567473785023165034204839674324590281442202140539789636777304053 % 29982285002307638875395123783263143807463120673880260421828102961608306864227847876915749400650935854694400)),("Z1",(0 % 1,3508560102841960851568957642463473750909417650935436429759984465073323977785604424049452563830796333941 % 15959940570439369231991184892179414550957703686951300035117286911439870201643826269648468922385144888688640)),("Z2",(20703937620634699628164764458430925305649852189395656369571371536217293265246596228851304902042305399810339 % 20711843048938893391312109180489644187691940132395124952755941755583350749741836304140847303524176426434560,1 % 1))] (prec 100)) (head square3p))
-- just left side
-- List.map (\f -> apply f (domain f)) (List.map (\e -> expressionToBoxFun e [("X1",(0 % 1,7905428304193763147344722058718882042087942999468583184570219366057484495240075289542401481871026624221 % 34886534858083592458438108985189172331832961488267752582992728857402954519777907739991605337513582374420480)),("X2",(144173611575899455984131794759632339019976210590231542509381925887784554922223588548169931387138570119459 % 1216632319040745753051812134546809768496513228717601005551617162030736315339709310701698662952076773949440,154312214234572337259563562547522420424113820829544884222312068904707357550054499470684086911739209902371 % 1297741140310128803255266276849930419729614110632107739255058306166118736362356598081811907148881892212736)),("Y1",(0 % 1,7905428304193763147344722058718882042087942999468583184570219366057484495240075289542401481871026624221 % 27233603147288186058564644894358024090590840208347048593665520287653113522522361933319063180587844061102080)),("Y2",(929040978017919951708752896168254361941134578059289554997557998184202105011880170864074767288470718834979 % 3747785625288454859424390472907892975932890084235032552728512870201038358028480984614468675081366981836800,7440233252447553376817367891404753777571164567473785023165034204839674324590281442202140539789636777304053 % 29982285002307638875395123783263143807463120673880260421828102961608306864227847876915749400650935854694400)),("Z1",(0 % 1,3508560102841960851568957642463473750909417650935436429759984465073323977785604424049452563830796333941 % 15959940570439369231991184892179414550957703686951300035117286911439870201643826269648468922385144888688640)),("Z2",(20703937620634699628164764458430925305649852189395656369571371536217293265246596228851304902042305399810339 % 20711843048938893391312109180489644187691940132395124952755941755583350749741836304140847303524176426434560,1 % 1))] (prec 100)) [(EUnOp Sqrt (EBinOp Add (EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2)) (PowI (EBinOp Sub (Var "Z1") (Var "Z2")) 2)))])
-- = [1.036776562210151 ± 3.9254e-4 <2^(-11)] for above counterexample using lowerbounds
-- wolfram agrees? https://www.wolframalpha.com/input/?i=sqrt%28%280+-+0.11662721297468709%29%5E2+%2B+%280+-+0.24717805338943316%29%5E2+%2B+%280+-+0.9995846297270826%29%5E2%29
-- checkECNFSimplex  square3p [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("X3", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0)), ("Y3", (0.0, 1.0))] 0.00000001 1.2 1000 (prec 100)
-- sqrt(6) - sqrt(2) == 1.0352761804100830493955953504961933133962756052797220552560128292
square3p =
  [negatedContext ++ goal]
  where
    negatedContext = map (EUnOp Negate) context
    context = []
    -- 2 + eps - (X1 - X2)^2 - (Y1 - Y2) >= 0
    goal = map (`fToE` testEps) goalF
    goalF = --dist(x,y) <= dn \/ dist(x,z) <= dn \/ dist(y,z) <= dn
      [
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2)))) (EBinOp Add (EBinOp Sub (EUnOp Sqrt (Lit 6.0)) (EUnOp Sqrt (Lit 2.0))) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y3")) 2)))) (EBinOp Add (EBinOp Sub (EUnOp Sqrt (Lit 6.0)) (EUnOp Sqrt (Lit 2.0))) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y3")) 2)))) (EBinOp Add (EBinOp Sub (EUnOp Sqrt (Lit 6.0)) (EUnOp Sqrt (Lit 2.0))) (Lit epsC))
      ]
      -- FComp Le (EUnOp Sqrt (EBinOp Add (EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2)) (PowI (EBinOp Sub (Var "Z1") (Var "Z2")) 2))) (EBinOp Add (EBinOp Sub (EUnOp Sqrt (Lit 6.0)) (EUnOp Sqrt (Lit 2.0))) (Lit epsC))

-- checkECNFSimplex  square4p [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("X3", (0.0, 1.0)), ("X4", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0)), ("Y3", (0.0, 1.0)), ("Y4", (0.0, 1.0))] 0.00000001 1.2 1000 (prec 100)
square4p =
  [negatedContext ++ goal]
  where
    negatedContext = map (EUnOp Negate) context
    context = []
    -- 2 + eps - (X1 - X2)^2 - (Y1 - Y2) >= 0
    goal = map (`fToE` testEps) goalF
    goalF = 
      [
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2)))) (EBinOp Add (Lit 1.0) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y3")) 2)))) (EBinOp Add (Lit 1.0) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y4")) 2)))) (EBinOp Add (Lit 1.0) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y3")) 2)))) (EBinOp Add (Lit 1.0) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y4")) 2)))) (EBinOp Add (Lit 1.0) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X3") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y3") (Var "Y4")) 2)))) (EBinOp Add (Lit 1.0) (Lit epsC))

      ]

-- checkECNFSimplex square5p [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("X3", (0.0, 1.0)), ("X4", (0.0, 1.0)), ("X5", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0)), ("Y3", (0.0, 1.0)), ("Y4", (0.0, 1.0)), ("Y5", (0.0, 1.0))] 0.00000001 1.2 1000 (prec 100)
square5p =
  [negatedContext ++ goal]
  where
    negatedContext = map (EUnOp Negate) context
    context = []
    -- 2 + eps - (X1 - X2)^2 - (Y1 - Y2) >= 0
    goal = map (`fToE` testEps) goalF
    goalF = 
      [
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y3")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y4")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y3")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y4")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X3") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y3") (Var "Y4")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X3") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y3") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X4") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y4") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 2.0)) (Lit 2.0)) (Lit epsC))
      ]

-- checkECNFSimplex square6p [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("X3", (0.0, 1.0)), ("X4", (0.0, 1.0)), ("X5", (0.0, 1.0)), ("X6", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0)), ("Y3", (0.0, 1.0)), ("Y4", (0.0, 1.0)), ("Y5", (0.0, 1.0)), ("Y6", (0.0, 1.0))] 0.00000001 1.2 1000 (prec 100)
square6p =
  [negatedContext ++ goal]
  where
    negatedContext = map (EUnOp Negate) context
    context = []
    -- 2 + eps - (X1 - X2)^2 - (Y1 - Y2) >= 0
    goal = map (`fToE` testEps) goalF
    goalF = 
      [
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X2")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y2")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y3")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y4")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X1") (Var "X6")) 2) (PowI (EBinOp Sub (Var "Y1") (Var "Y6")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X3")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y3")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y4")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X2") (Var "X6")) 2) (PowI (EBinOp Sub (Var "Y2") (Var "Y6")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X3") (Var "X4")) 2) (PowI (EBinOp Sub (Var "Y3") (Var "Y4")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X3") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y3") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X3") (Var "X6")) 2) (PowI (EBinOp Sub (Var "Y3") (Var "Y6")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X4") (Var "X5")) 2) (PowI (EBinOp Sub (Var "Y4") (Var "Y5")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X4") (Var "X6")) 2) (PowI (EBinOp Sub (Var "Y4") (Var "Y6")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC)),
        FComp Le (EUnOp Sqrt ((EBinOp Add (PowI (EBinOp Sub (Var "X5") (Var "X6")) 2) (PowI (EBinOp Sub (Var "Y5") (Var "Y6")) 2)))) (EBinOp Add (EBinOp Div (EUnOp Sqrt (Lit 13.0)) (Lit 6.0)) (Lit epsC))
      ]
      
testInf =
  [negatedContext ++ goal]
  where
    negatedContext = map (EUnOp Negate) context
    context = []
    -- 2 + eps - (X1 - X2)^2 - (Y1 - Y2) >= 0
    goal = [EBinOp Add (EBinOp Add (EBinOp Add (PowI (Var "X") 3) (PowI (Var "Y") 3)) (PowI (Var "Z") 3)) (PowI (Var "A") 3)]

testBug = EBinOp Add (EBinOp Mul (EUnOp Sqrt (PowI (Var "x") 4)) (Var "y")) (Lit 1.0)
-- testBug = EBinOp Add (EBinOp Mul (EUnOp Sqrt (Var "x")) (EUnOp Negate (Var "y"))) (Lit 2.0)
-- Create dReal files
-- writeFile "points3.smt2" $ cnfExpressionAndDomainsToDreal square3p [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0)), ("Z1", (0.0, 1.0)), ("Z2", (0.0, 1.0))] [] 0.0
-- writeFile "points4.smt2" $ cnfExpressionAndDomainsToDreal square3p [("X1", (0.0, 1.0)), ("X2", (0.0, 1.0)), ("Y1", (0.0, 1.0)), ("Y2", (0.0, 1.0)), ("Z1", (0.0, 1.0)), ("Z2", (0.0, 1.0)), ("A1", (0.0, 1.0)), ("A2", (0.0, 1.0))] [] 0.0

checkHeronInitExact = T.checkECNF heronInitExact [("X", (0.5, 2.0))] (prec 100)
checkHeronPreservationExact i = T.checkECNF (heronPreservationExact i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)
checkHeronPreservationExactGE i = T.checkECNF (heronPreservationExactYGE i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)
checkHeronPreservationExactLE i = T.checkECNF (heronPreservationExactYLE i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)
checkSineVC = T.checkECNF sineVC [("X", (-1.0, 1.0))] (prec 100)
checkSineVC2 = T.checkECNF yannickSineVC [("X", (-3.1, -3.0))] (prec 100)

checkHeronInitExactCE = T.checkECNF heronInitExact [("X", (0.5, 2.0))] (prec 100)
checkHeronPreservationExactCE i = T.checkECNFCE (heronPreservationExact i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] 30 500 1.2 (prec 100)
checkHeronPreservationExactGECE i = T.checkECNFCE (heronPreservationExactYGE i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] 30 500 1.2 (prec 100)
checkHeronPreservationExactLECE i = T.checkECNFCE (heronPreservationExactYLE i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] 30 500 1.2 (prec 100)
checkSineVCCE = T.checkECNFCE sineVC [("X", (-1.0, 1.0))] 30 500 1.2 (prec 100)
checkSineVC2CE = T.checkECNFCE yannickSineVC [("X", (-3.1, -3.0))] 30 500 1.2 (prec 100)

checkHeronPreservationExactIVarCE = T.checkECNFCE (heronPreservationExactIVar) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750)), ("i", (1.0, 3.0))] 30 500 1.2 (prec 100)
