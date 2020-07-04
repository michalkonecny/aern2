module AERN2.BoxFunMinMax.Expressions.TestFunctions where

import MixedTypesNumPrelude
import AERN2.BoxFunMinMax.Expressions.Eliminator
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Expressions.Translators.DReal
import AERN2.BoxFunMinMax.Expressions.Translators.MetiTarski
import qualified AERN2.BoxFunMinMax.Type as T
import AERN2.MP.Precision (prec)

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
--           (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!1))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit 1.0) (Lit (1/!8388608))))))
--         (Lit 8.940697e-08)) (Lit 0.0))

maxFloat :: Rational
maxFloat = (2.0 - 2.0^!(-23)) * 2.0^!127

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
        (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!1))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit 1.0) (Lit (1/!8388608))))))
      (Lit 8.940697e-08))

heronPreservationExact :: Integer -> [[E]]
heronPreservationExact i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
        EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875),
        EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750),
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
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
        (Lit 1.192093e-07)

heronPreservationExactYGE i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
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
      EBinOp Sub (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 2.384186e-07)

heronPreservationExactYLE i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit maxFloat)) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (Lit (-maxFloat))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Lit maxFloat)) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (Lit (-maxFloat))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Lit maxFloat)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (Lit (-maxFloat))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Lit maxFloat)) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit (-maxFloat))) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 2.384186e-07)
      ]
    goal =
        EBinOp Sub (EBinOp Add (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 1.79999995231628417968750)) (Lit 2.384186e-07)
-- - (Y1 + (X / Y1)) / (2.0) + (1.79999995231628417968750) - 2.384186e-07 > 0

heronPreservationExactVarMaxFloat :: Integer -> [[E]]
heronPreservationExactVarMaxFloat i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
        EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875),
        EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Lit 1.0))) (Var "MaxFloat")) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Lit 1.0)) (EBinOp Sub (Lit 0.0) (Var "MaxFloat"))) (Lit 5.960464e-08),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0)))) (Var "MaxFloat")) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Lit 1.0) (EBinOp Div (Var "X") (Lit 1.0))) (EBinOp Sub (Lit 0.0) (Var "MaxFloat"))) (Lit 1.788139e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (Var "X") (Var "Y1"))) (Var "MaxFloat")) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Sub (EBinOp Div (Var "X") (Var "Y1")) (EBinOp Sub (Lit 0.0) (Var "MaxFloat"))) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1")))) (Var "MaxFloat")) (Lit 2.384186e-07),
        EBinOp Add (EBinOp Sub (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (EBinOp Sub (Lit 0.0) (Var "MaxFloat"))) (Lit 2.384186e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
        (Lit 1.192093e-07)

checkHeronInitExact = T.checkECNF (simplifyECNF (minMaxAbsEliminatorECNF heronInitExact)) [("X", (0.5, 2.0))] (prec 100)

checkHeronPreservationExact i = T.checkECNF (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExact i))) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)

checkHeronPreservationExactYGE i = T.checkECNF (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYGE i))) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)

checkHeronPreservationExactYLE i = T.checkECNF (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYLE i))) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)

generateHeronInitMetiTarski :: IO ()
generateHeronInitMetiTarski =
  writeFile
    "heronInitExact.tptp"
    (cnfExpressionAndDomainsToMetiTarski
    (simplifyECNF (minMaxAbsEliminatorECNF heronInitExact))
        [("X", (0.5, 2.0))] (0.5^!(-23)))

generateHeronPreservationExactMetiTarski :: IO () =
  do
    writeFile 
      "heronPreservationExacti1.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExact 1)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti2.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExact 2)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti3.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExact 3)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti4.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExact 4)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))

generateHeronPreservationExactYGE :: IO () =
  do
    writeFile 
      "heronPreservationExactYGEi1.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYGE 1)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi2.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYGE 2)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi3.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYGE 3)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi4.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYGE 4)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))

generateHeronPreservationExactYLE :: IO () =
  do
    writeFile 
      "heronPreservationExactYLEi1.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYLE 1)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi2.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYLE 2)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi3.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYLE 3)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi4.tptp" 
      (cnfExpressionAndDomainsToMetiTarski
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactYLE 4)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))

generateSeperateHeronDrealFiles :: IO ()
generateSeperateHeronDrealFiles =
  do
    writeFile 
      "heronPreservationExacti1.smt2" 
      (cnfExpressionAndDomainsToDreal
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactVarMaxFloat 1)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750)), ("MaxFloat", (maxFloat, maxFloat))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti2.smt2" 
      (cnfExpressionAndDomainsToDreal
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactVarMaxFloat 2)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750)), ("MaxFloat", (maxFloat, maxFloat))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti3.smt2" 
      (cnfExpressionAndDomainsToDreal
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactVarMaxFloat 3)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750)), ("MaxFloat", (maxFloat, maxFloat))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti4.smt2" 
      (cnfExpressionAndDomainsToDreal
        (simplifyECNF (minMaxAbsEliminatorECNF (heronPreservationExactVarMaxFloat 4)))
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750)), ("MaxFloat", (maxFloat, maxFloat))] [] (0.5^!(-23)))

generateUnifiedHeronDrealFiles :: IO ()
generateUnifiedHeronDrealFiles =
  writeFile 
    "heronPreservationM.smt2" 
    (expressionAndDomainsToDreal
      (simplifyE (qualifiedEsToCNF (minMaxAbsEliminator (fToE heronPreservationM))))
      [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [("i", (0.0, 5.0))] (0.5^!(-23)))


generateSeperateHeronDrealFilesCNF :: IO ()
generateSeperateHeronDrealFilesCNF =
  do
    writeFile 
      "heronPreservationMi1.smt2" 
      (cnfExpressionAndDomainsToDreal
        (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi1))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi2.smt2" 
      (cnfExpressionAndDomainsToDreal
        (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi2))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi3.smt2" 
      (cnfExpressionAndDomainsToDreal
        (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi3))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi4.smt2" 
      (cnfExpressionAndDomainsToDreal
        (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi4))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationMi5.smt2" 
      (cnfExpressionAndDomainsToDreal
        (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi5))))
        [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [] (0.5^!(-23)))

generateUnifiedHeronDrealFilesCNF :: IO ()
generateUnifiedHeronDrealFilesCNF =
  writeFile 
    "heronPreservationM.smt2" 
    (cnfExpressionAndDomainsToDreal
      (qualifiedEsToCNF2 (minMaxAbsEliminator (simplifyE (fToE heronPreservationM))))
      [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] [("i", (0.0, 5.0))] (0.5^!(-23)))


-- generateHeronTree :: T.MinMaxTree
-- generateHeronTree =
--     qualifiedEsToTree (minMaxAbsEliminator (simplifyE (fToE heronPreservationMi2))) [("x", (0.5, 2.0)), ("y", (0.8, 1.8))]
