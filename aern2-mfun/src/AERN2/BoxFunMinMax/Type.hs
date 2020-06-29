module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Optimisation (SearchBox)
import AERN2.BoxFunMinMax.Optimisation
import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.Translators.BoxFun
import qualified AERN2.BoxFunMinMax.Expressions.Type as E


import Debug.Trace (trace)

import Data.List (filter)

checkECNF :: [[E.E]] -> VarMap -> Accuracy -> Precision -> Rational -> (Maybe Bool, Maybe SearchBox)
checkECNF [] _ _ _ _              = (Just True, Nothing) 
checkECNF (es : cnf) vMap ac p n  = 
  case filteredEs of
    []  -> (Just False, Nothing)
    es' ->
      if or roughCheck
        then checkECNF cnf vMap ac p n
        else 
          case checkEs es' of
            (Just True, _)  -> checkECNF cnf vMap ac p n
            _               -> 
              case checkInversion invertedEs of
                (Just True, _) -> checkECNF cnf vMap ac p n
                _ ->
                  if width vMap !>! 1 / 10000000 then -- make this threshold quite small (maybe 10^-7)
                    trace ("Bisected boxes: " ++ show newBoxes) $
                    case checkBoxes newBoxes filteredEs of
                      (Just True, _) -> checkECNF cnf vMap ac p n
                      r -> r
                  else
                    trace "Stopping bisections (Box too small)" (Nothing, Nothing)
  where
    filteredEs = filter (\e -> not (applyE e !<! n))  es
    roughCheck = map    (\e -> applyE e !>=! n)       filteredEs

    applyE e = apply f (setPrecision p (domain f))
      where
        f = expressionToBoxFun e vMap

    newBoxes = fullBisect vMap

    checkBoxes []          _  = (Just True, Nothing)
    checkBoxes (v : vs) es'   = case checkECNF [es'] v ac p n of
                                (Just True, _) -> checkBoxes vs es'
                                o              -> trace ("found false at " ++ show v) o

    checkEs [] = (Just False, Nothing)
    checkEs [e] =
      let 
        f = expressionToBoxFun e vMap
        f' = BoxFun (dimension f) (bf_eval f) (setPrecision p (domain f))
      in
        globalMinimumAboveN f' ac p (cn (mpBallP p (width vMap /! 4))) (cn (mpBallP p 0.0))
    checkEs (e : es') =
      case checkEs [e] of
        r@(Just True, _) -> r
        _ -> checkEs es'
    
    invertedEs = map (E.EUnOp E.Negate) es
    
    --    negation of max e1 e2 e3 >= 0 ...
    -- == min -e1 -e2 -e3 < 0
    -- == min (-e1 < 0) (-e2 < 0) (-e3 < 0)
    -- == min (not (-e1 >= 0)) (not (-e2 >= 0)) (not (-e3 >= 0))
    checkInversion [] = (Just True, Nothing)
    checkInversion (e : es') = 
      case globalMinimumAboveN f' ac p (cn (mpBallP p (width vMap /! 4))) (cn (mpBallP p 0.0)) of
        (Just True, c)  -> (Just False, c)
        (Just False, _) -> checkInversion es'
        r@(Nothing, _)  -> r
      where 
        f = expressionToBoxFun e vMap
        f' = BoxFun (dimension f) (bf_eval f) (setPrecision p (domain f))
