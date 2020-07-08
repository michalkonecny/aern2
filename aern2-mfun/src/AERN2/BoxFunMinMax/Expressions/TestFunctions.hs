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
    --       (EBinOp Sub (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
    --     (Lit 1.192093e-07)
    -- goal = -- Heron Pres Swap
    --   EBinOp Sub
    --     (EBinOp Add
    --       (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "Y1")) (EBinOp Div (EBinOp Add (Var "X") (EBinOp Div (Var "Y1") (Var "X"))) (Lit 2.0)))))
    --       (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
    --     (Lit 1.192093e-07)    
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) 
            (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X"))
                       (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Add 
            (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) 
            (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
        (Lit 1.192093e-07)
        -- (Lit 0.1)

heronPreservationExactSwap :: Integer -> [[E]]
heronPreservationExactSwap i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
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
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
        (Lit 1.192093e-07)

heronPreservationExactSub :: Integer -> [[E]]
heronPreservationExactSub i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
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
          (EBinOp Sub (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
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
      EBinOp Sub (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07)

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
        (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!1))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit 1.0) (Lit (1/!8388608))))))
      (Lit 8.940697e-08))

heronPreservationExactNoMaxFloat :: Integer -> [[E]]
heronPreservationExactNoMaxFloat i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
        (Lit 1.192093e-07)

heronPreservationExactNoMaxFloatSwap :: Integer -> [[E]]
heronPreservationExactNoMaxFloatSwap i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "Y1")) (EBinOp Div (EBinOp Add (Var "X") (EBinOp Div (Var "Y1") (Var "X"))) (Lit 2.0)))))
          (EBinOp Add (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
        (Lit 1.192093e-07)

heronPreservationExactNoMaxFloatSub :: Integer -> [[E]]
heronPreservationExactNoMaxFloatSub i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context   
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
        EBinOp Add (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07),
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0))) (Lit 1.79999995231628417968750)) (Lit 1.192093e-07)
      ]
    goal =
      EBinOp Sub
        (EBinOp Add
          (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)))))
          (EBinOp Sub (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!(i1+1)))) (EBinOp Mul (Lit 6.0) (EBinOp Mul (Lit (i1+1.0)) (Lit (1/!8388608))))))
        (Lit 1.192093e-07)


heronPreservationExactYGENoMaxFloat i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608)))
      ]
    goal =
      EBinOp Sub (EBinOp Sub (EBinOp Div (EBinOp Add (Var "Y1") (EBinOp Div (Var "X") (Var "Y1"))) (Lit 2.0)) (Lit 0.699999988079071044921875)) (Lit 1.192093e-07)

heronPreservationExactYLENoMaxFloat i1 =
  [negatedContext ++ [goal]]
  where
    negatedContext = map (EUnOp Negate) context
    context = 
      [
        EBinOp Add (EBinOp Add (EBinOp Sub (Lit 0.0) (EUnOp Abs (EBinOp Sub (EUnOp Sqrt (Var "X")) (Var "Y1")))) (EBinOp Div (Lit 1.0) (PowI (Lit 2.0) (2^!i1)))) (EBinOp Mul (EBinOp Mul (Lit 6.0) (Lit (rational i1))) (Lit (1/!8388608))),
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

checkHeronInitExact = T.checkECNF heronInitExact [("X", (0.5, 2.0))] (prec 100)

checkHeronPreservationExact i = T.checkECNF (heronPreservationExact i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)

checkHeronPreservationExactYGE i = T.checkECNF (heronPreservationExactYGE i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)

checkHeronPreservationExactYLE i = T.checkECNF (heronPreservationExactYLE i) [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (prec 100)

checkSineVC = T.checkECNF sineVC [("X", (-1.0, 1.0))] (prec 100)

checkBisectionRootFinder = T.checkECNF bisectionRootFinder [("A", (1.0001, 5.0)), ("B", (1.0001, 5.0))] (prec 100)

generateHeronInitMetiTarski =
  writeFile
    "heronInitExact.tptp"
    (cnfExpressionAndDomainsToMetiTarski heronInitExact
        [("X", (0.5, 2.0))] (0.5^!(-23)))

generateHeronPreservationExactMetiTarski =
  do
    writeFile 
      "heronPreservationExacti1.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 1)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti2.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 2)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti3.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 3)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti4.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExact 4)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))

generateHeronPreservationExactMetiTarskiYGE =
  do
    writeFile 
      "heronPreservationExactYGEi1.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 1)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi2.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 2)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi3.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 3)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi4.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYGE 4)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))

generateHeronPreservationExactMetiTarskiYLE =
  do
    writeFile 
      "heronPreservationExactYLEi1.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 1)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi2.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 2)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi3.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 3)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi4.tptp" 
      (cnfExpressionAndDomainsToMetiTarski (heronPreservationExactYLE 4)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] (0.5^!(-23)))

generateSineExactMetiTarski =
  writeFile
    "sineExact.tptp"
    (cnfExpressionAndDomainsToMetiTarski sineVC
      [("X", (-(1.0), 1.0))] (0.5^!(-23)))

generateSeperateHeronPreservationExactDrealFiles =
  do
    writeFile 
      "heronPreservationExacti1.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 1)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti2.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 2)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti3.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 3)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExacti4.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactNoMaxFloat 4)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))

generateSeperateHeronPreservationExactYGEDrealFiles =
  do
    writeFile 
      "heronPreservationExactYGEi1.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 1)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi2.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 2)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi3.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 3)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYGEi4.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYGENoMaxFloat 4)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))

generateSeperateHeronPreservationExactYLEDrealFiles =
  do
    writeFile 
      "heronPreservationExactYLEi1.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 1)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi2.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 2)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi3.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 3)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))
    writeFile 
      "heronPreservationExactYLEi4.smt2" 
      (cnfExpressionAndDomainsToDreal (heronPreservationExactYLENoMaxFloat 4)
        [("X", (0.5, 2.0)), ("Y1", (0.699999988079071044921875, 1.79999995231628417968750))] [] (0.5^!(-23)))

generateHeronInitExactDreal =
  writeFile
    "heronInitExact.smt2"
    (cnfExpressionAndDomainsToDreal heronInitExactNoMaxFloat
      [("X", (0.5, 2.0))] [] (0.5^!(-23)))

generateSineExactDreal =
  writeFile
    "sineExact.smt2"
    (cnfExpressionAndDomainsToDreal sineVCNoMaxFloat
      [("X", (-1.0, 1.0))] [] (0.5^!(-23)))
