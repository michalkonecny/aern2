module AERN2.BoxFunMinMax.Expressions.Translators.BoxFun where

import MixedTypesNumPrelude

import AERN2.AD.Differential
import AERN2.BoxFun.Type
import AERN2.BoxFun.TestFunctions (fromListDomain)
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.VarMap
import AERN2.MP.Ball

import qualified AERN2.Linear.Vector.Type as V

import Data.List

expressionToBoxFun :: E -> VarMap -> Precision -> BoxFun
expressionToBoxFun e domain p =
  BoxFun
    (fromIntegral (Data.List.length domain))
    (expressionToDifferential e)
    vectorDomain
  where

    expressionToDifferential2 :: E -> V.Vector (Differential (CN MPBall)) -> Differential (CN MPBall)
    expressionToDifferential2 e v = expressionToDifferential e v
      where
        ev  = expressionToDifferential e v
        evc = expressionToDifferential e vc
        vc  = V.map (fmap (fmap centreAsBall)) v
        

    -- TODO: Change to bfEval
    expressionToDifferential :: E -> V.Vector (Differential (CN MPBall)) -> Differential (CN MPBall)
    expressionToDifferential (Float e significand) v = undefined
    expressionToDifferential (EBinOp op e1 e2) v = 
      case op of
        Min -> undefined
        Max -> undefined
        Pow -> undefined
        Add -> expressionToDifferential e1 v + expressionToDifferential e2 v
        Sub -> expressionToDifferential e1 v - expressionToDifferential e2 v
        Mul -> expressionToDifferential e1 v * expressionToDifferential e2 v
        Div -> expressionToDifferential e1 v / expressionToDifferential e2 v
    expressionToDifferential (EUnOp op e) v = 
      case op of
        Abs -> abs (expressionToDifferential e v)
        Sqrt -> sqrt (expressionToDifferential e v)
        Negate -> negate (expressionToDifferential e v)
        Sin -> sin (expressionToDifferential e v)
        Cos -> cos (expressionToDifferential e v)
    expressionToDifferential (Lit e) _ = differential 2 $ cn (mpBallP p e)
    expressionToDifferential (Var e) v = 
      case elemIndex e variableOrder of
        Nothing -> error $ "Variable: " ++ show e ++ " not found in varMap: " ++ show domain ++ " when translating expression: " ++ show e 
        Just i -> v V.! (fromIntegral i)
    expressionToDifferential (PowI e i) v = expressionToDifferential e v ^! i
    expressionToDifferential (Float32 _ e) v = expressionToDifferential e v
    expressionToDifferential (Float64 _ e) v = expressionToDifferential e v


    variableOrder = map fst domain
    vectorDomain  = fromListDomain (map snd domain)
