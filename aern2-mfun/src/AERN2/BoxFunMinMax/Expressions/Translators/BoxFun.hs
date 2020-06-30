module AERN2.BoxFunMinMax.Expressions.Translators.BoxFun where

import MixedTypesNumPrelude

import AERN2.AD.Differential
import AERN2.BoxFun.Type
import AERN2.BoxFun.TestFunctions (fromListDomain)
import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.MP.Ball

import qualified AERN2.Linear.Vector.Type as V

import Data.List

expressionToBoxFun :: E -> [(String, (Rational, Rational))] -> BoxFun
expressionToBoxFun e domain =
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
        Abs -> undefined
        Sqrt -> sqrt (expressionToDifferential e v)
        Negate -> negate (expressionToDifferential e v)
    expressionToDifferential (Lit e) _ = differential 2 $ cn (mpBallP (prec 100) e) -- TODO: paramaterise precision
    expressionToDifferential (Var e) v = 
      case elemIndex e variableOrder of
        Nothing -> undefined
        Just i -> v V.! (fromIntegral i)
    expressionToDifferential (PowI e i) v = expressionToDifferential e v ^! i


    variableOrder = map fst domain
    vectorDomain  = fromListDomain (map snd domain)
