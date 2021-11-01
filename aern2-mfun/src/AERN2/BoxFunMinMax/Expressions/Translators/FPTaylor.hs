module AERN2.BoxFunMinMax.Expressions.Translators.FPTaylor where

import MixedTypesNumPrelude

import AERN2.BoxFunMinMax.Expressions.Type

import Data.List
import Data.Ratio
import System.IO.Unsafe (unsafePerformIO)
import AERN2.BoxFunMinMax.VarMap

import System.IO
import Debug.Trace
import Data.Scientific
import AERN2.MP.Precision
import AERN2.MP.Ball (piBallP)
import Data.Bifunctor

-- | All variables must appear in the VarMap
expressionToFPTaylor :: E -> String
expressionToFPTaylor (EBinOp op e1 e2) =
  case op of
    Add -> "(" ++ expressionToFPTaylor e1 ++ " + " ++ expressionToFPTaylor e2 ++ ")"
    Sub -> "(" ++ expressionToFPTaylor e1 ++ " - " ++ expressionToFPTaylor e2 ++ ")"
    Mul -> "(" ++ expressionToFPTaylor e1 ++ " * " ++ expressionToFPTaylor e2 ++ ")"
    Div -> "(" ++ expressionToFPTaylor e1 ++ " / " ++ expressionToFPTaylor e2 ++ ")"
    Mod -> "(" ++ expressionToFPTaylor e1 ++ " / " ++ expressionToFPTaylor e2 ++ ")" -- FIXME: not safe
    Min -> undefined
    Max -> undefined
    Pow -> undefined
expressionToFPTaylor (EUnOp op e) =
    case op of
    Sqrt -> "sqrt(" ++ expressionToFPTaylor e ++ ")"
    Negate -> "(-1 * " ++ expressionToFPTaylor e ++ ")"
    Abs -> "|" ++ expressionToFPTaylor e ++ "|"
    Sin -> "sin(" ++ expressionToFPTaylor e ++ ")"
    Cos -> "cos(" ++ expressionToFPTaylor e ++ ")"
expressionToFPTaylor Pi         = "(4 * atan(1))"
expressionToFPTaylor (PowI e i) = expressionToFPTaylor e ++ " ^ " ++ show i
expressionToFPTaylor (Lit r) = showFrac r
expressionToFPTaylor (Var v) = v
expressionToFPTaylor (Float32 mode e) = 
  case mode of
    RNE -> "rnd32(" ++ expressionToFPTaylor e ++ ")"
    RTP -> "rnd32_up(" ++ expressionToFPTaylor e ++ ")"
    RTN -> "rnd32_down(" ++ expressionToFPTaylor e ++ ")"
    RTZ -> "rnd32_0(" ++ expressionToFPTaylor e ++ ")"
    RNA -> error "Round nearest away not supported in FPTaylor"
expressionToFPTaylor (Float64 mode e) = 
  case mode of
    RNE -> "rnd64(" ++ expressionToFPTaylor e ++ ")"
    RTP -> "rnd64_up(" ++ expressionToFPTaylor e ++ ")"
    RTN -> "rnd64_down(" ++ expressionToFPTaylor e ++ ")"
    RTZ -> "rnd64_0(" ++ expressionToFPTaylor e ++ ")"
    RNA -> error "Round nearest away not supported in FPTaylor"
expressionToFPTaylor e@(Float _ _) = error "Float type with no precision found when translating to FPTaylor: " ++ show e
expressionToFPTaylor (RoundToInteger mode e) = expressionToFPTaylor e -- FIXME: is this ok because we are calculating abs error?
                                                                      -- alternative solution: manually add rounding logic for each case. possible without Ifs?
                                                                      -- This is ok for now
-- expressionToFPTaylor (Float e s) = "rnd32(" ++ expressionToFPTaylor e ++ ")" --TODO: FPTaylor only supports 16,32,64,128 floats. Use these numbers in PP2?

variableBoundsToFPTaylor :: VarMap -> String
variableBoundsToFPTaylor [] = ""
variableBoundsToFPTaylor ((v, (l, r)) : vs) = "real " ++ show v ++ " in [" ++ showFrac l ++ ", " ++ showFrac r ++ "];\n" ++ variableBoundsToFPTaylor vs
  where

showFrac :: Rational -> [Char]
showFrac rat = if den == 1.0 then show num else show num ++ " / " ++ show den
  where
    num = numerator rat
    den = denominator rat

expressionWithVarMapToFPTaylor :: E -> VarMap -> String
expressionWithVarMapToFPTaylor e vm =
  "Variables\n" ++ variableBoundsToFPTaylor vm ++ "\nExpressions\n" ++ expressionToFPTaylor e ++ ";"

-- parseFPTaylorOutput :: String -> Rational
-- parseFPTaylorOutput output =
--   case elemIndex "(exact):" wordsOutput of
--     Just i -> wordsOutput !! (i + 1)
--   where
--     wordsOutput = words output  

-- "1.788139e-07"
-- "1788140 % 10^-13"

-- To parse the left side:
--  Store length of string
--  Find position of dot
--  remove dot
--  parse integer
--  Add one to compensate for missing digits
--  divide integer using position of dot and length of string to get the rational we want

parseFPTaylorRational :: String -> Maybe Rational
parseFPTaylorRational output = mr
  where
    mr = toRational . (read :: String -> Scientific) <$> findErrorBound outputList

    outputList = words output

    findErrorBound :: [String] -> Maybe String
    findErrorBound [] = Nothing
    findErrorBound ("Absolute" : "error" : "(exact):" : errorBound : _) = Just errorBound
    findErrorBound (_ : xs) = findErrorBound xs
    -- ePos = Data.List.elemIndex 'e' output
    -- (decimal, exponentWithE) = Data.List.splitAt (ePos) output
    -- exponent = tail exponentWithE
    -- exponentInt = read exponent --Could throw exception

testOutput :: String 
testOutput = 
  "Loading configuration file: /home/junaid/Research/git/FPTaylor/default.cfg \
  \FPTaylor, version 0.9.2+dev \

  \Loading: heronInit1PlusXDiv1 copy.txt \
  \Processing: Expression 1 \

  \************************************* \
  \Taylor form for: rnd32((1 + rnd32((X / 1)))) \

  \Conservative bound: [1.500000, 3.000000] \

  \Simplified rounding: rnd[32,ne,1.00,-24,0]((1 + rnd32((X / 1)))) \
  \Building Taylor forms... \
  \Simplifying Taylor forms... \
  \success \
  \v0 = (1 + (X * (1 / 1))) \
  \-1 (9): exp = -24: (1/5070602400912917605986812821504) \
  \1 (3): exp = -24: floor_power2(((X * (1 / 1)) + 0)) \
  \2 (5): exp = -24: floor_power2(((1 + (X * (1 / 1))) + interval(-5.96046447753906382349e-08, 5.96046447753906382349e-08))) \

  \Corresponding original subexpressions: \
  \1: rnd32((X / 1)) \
  \2: rnd[32,ne,1.00,-24,0]((1 + rnd32((X / 1)))) \

  \bounds: [1.500000e+00, 3.000000e+00] \

  \Computing absolute errors \
  \-1: exp = -24: 1.972152e-31 (low = 1.972152e-31, subopt = 0.0%) \

  \Solving the exact optimization problem \
  \exact bound (exp = -24): 3.000000e+00 (low = 3.000000e+00, subopt = 0.0%) \
  \total2: 1.175494e-38 (low = 1.175494e-38, subopt = 0.0%) \
  \exact total: 1.788139e-07 (low = 1.788139e-07, subopt = 0.0%) \

  \Elapsed time: 0.36412 \
  \************************************* \

  \------------------------------------------------------------------------------- \
  \Problem: Expression 1 \

  \Optimization lower bounds for error models: \
  \The absolute error model (exact): 1.788139e-07 (suboptimality = 0.0%) \
 
  \Bounds (without rounding): [1.500000e+00, 3.000000e+00] \
  \Bounds (floating-point): [1.49999982118606545178e+00, 3.00000017881393477026e+00] \
 
  \Absolute error (exact): 1.788139e-07 \
 
  \Elapsed time: 0.36 \
 
 
 
  \"