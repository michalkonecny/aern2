module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Optimisation (SearchBox)
import AERN2.BoxFunMinMax.Optimisation
import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.Translators.BoxFun
import qualified AERN2.BoxFunMinMax.Expressions.Type as E

import Control.Parallel.Strategies

import Debug.Trace (trace)

import Data.List (filter)

-- | Check a CNF (Conjunctive Normal Form) of Expressions in the form of a list of lists
-- over the given VarMap.
-- This function: 
-- Takes the first list from the CNF (which is a list of lists).
-- Then, filters out all the expressions in this list where we can ensure that an
-- expression is under n using AERN2.BoxFun.Type.apply.
-- If the filter removes all expressions in this result, return a False result
-- Then, try to cheaply check if any of the expressions are above 0 using AERN2.BoxFun.Type.apply.
-- If we can cheaply verify one of the expressions is above 0, we check the next list in the CNF.
-- If we can't cheaply verify an expression is above n, we call the 
-- AERN2.BoxFunMinMax.Optimisation.globalMinimumAboveN algorithm.
-- We first use this algorithm to filter our all false results in the current list
-- by proving the negation of an expression is above zero.
-- Again, if the list is empty, we return a False result.
-- If the list is a singleton, we run the algorithm on the singleton with a very
-- small cutoff, else we map the algorithm across all elements in the list with
-- a large cutoff. 
-- If this algorithm returns a True result, we check the next element in the list
-- If the algorith returns a False or Nothing (indeterminate) result, we bisect each dimension
-- in the domain and recurse on each new domain.
-- If the recursive call returns True, we check the next element in the CNF list, else we return
-- a False or indeterminate result with a possible counterexample.
-- Once we've exhausted the CNF (list of lists), we return a True result.

checkECNF :: [[E.E]] -> VarMap -> Accuracy -> Precision -> (Maybe Bool, Maybe SearchBox)
checkECNF cnf vMapInit ac p = 
  checkConjunctionResults parCheckConjunction
  where
    parCheckConjunction = parMap rseq (\esInit -> checkDisjunction esInit vMapInit) cnf

    checkConjunctionResults []        = (Just True, Nothing)
    checkConjunctionResults (x : xs)  =
      case x of
        (Just True, _)  -> checkConjunctionResults xs
        o               -> o

    checkDisjunction es vMap =
      case filterOutFalseEsUsingApply of
        []  -> (Just False, Nothing) -- TODO: return current vMap as SearchBox counterexample
        es' ->
          if or checkIfEsTrueUsingApply
            then (Just True, Nothing)
            else 
              case filterOutFalseUsingGlobalMinimum es' of
                [] -> (Just False, Nothing) -- TODO: return current vMap as SearchBox counterexample
                [e] -> globalMinimumAboveN f' ac p (cn (mpBallP p (1 /! 10000000))) (cn (mpBallP p 0.0))
                  where
                    f = expressionToBoxFun e vMap
                    f' = BoxFun (dimension f) (bf_eval f) (setPrecision p (domain f))
                es'' ->
                  case checkIfTrueUsingGlobalMinimum es'' of
                    r@(Just True, _)  -> r
                    _               -> 
                      if width vMap !>! 1 / 10000000 then -- make this threshold quite small (maybe 10^-7)
                        trace ("Bisected boxes: " ++ show newBoxes) $
                        case checkBoxes es'' newBoxes of
                          r@(Just True, _) -> r
                          r -> r
                      else
                        trace "Stopping bisections (Box too small)" (Nothing, Nothing)
        where
          esWithRanges = zip es (map applyE es)

          filterOutFalseTerms     = (filter (\(_, range) -> not (range !<! 0))  esWithRanges)
          checkIfEsTrueUsingApply = map (\(_, range) -> range !>=! 0)  filterOutFalseTerms

          filterOutFalseEsUsingApply = map fst filterOutFalseTerms

          applyE e = apply f (setPrecision p (domain f))
            where
              f = expressionToBoxFun e vMap
          
          newBoxes = fullBisect vMap

          filterOutFalseUsingGlobalMinimum =
            filter (not . isFalseOnBox)
            where
              isFalseOnBox e = 
                case globalMinimumAboveN fNeg' ac p (cn (mpBallP p (width vMap /! 2))) (cn (mpBallP p 0.0)) of
                  (Just True, _) -> True
                  _ -> False
                where
                  eNeg = E.EUnOp E.Negate e
                  fNeg = expressionToBoxFun eNeg vMap
                  fNeg' = BoxFun (dimension fNeg) (bf_eval fNeg) (setPrecision p (domain fNeg))
           -- Try to prove the disjunction of the given list of expressions is True
          checkIfTrueUsingGlobalMinimum [] = (Just False, Nothing) -- will only reach here if checkEs is called with an empty list
          checkIfTrueUsingGlobalMinimum [e] =
            let 
              f = expressionToBoxFun e vMap
              f' = BoxFun (dimension f) (bf_eval f) (setPrecision p (domain f))
            in
              globalMinimumAboveN f' ac p (cn (mpBallP p (width vMap /! 2))) (cn (mpBallP p 0.0))
          checkIfTrueUsingGlobalMinimum (e : es') =
            case checkIfTrueUsingGlobalMinimum [e] of
              r@(Just True, _) -> r
              _ -> checkIfTrueUsingGlobalMinimum es'

    checkBoxes _ []           = (Just True, Nothing)
    checkBoxes es' (v : vs)   = 
      case checkDisjunction es' v of
        (Just True, _) -> checkBoxes es' vs
        o              -> trace ("found false at " ++ show v) o

    
    --    negation of max e1 e2 e3 >= 0 ...
    -- == min -e1 -e2 -e3 < 0
    -- == min (-e1 < 0) (-e2 < 0) (-e3 < 0)
    -- == min (not (-e1 >= 0)) (not (-e2 >= 0)) (not (-e3 >= 0))

    -- we take e1 (x,y) >= 0 and we want to prove this to be false for all x,y in box
    -- if (-e1 (x,y) >= 0) is true, we have disproven e1 (x,y) >= 0

    -- Try to prove the conjuction of the given list of expressions is False.
    -- The conjunction is expected to be of the following form:
    -- (not (e1 >= 0)) /\ (not (e2 >= 0)) /\ (not (e3 >= 0)) ...
            

    -- checkInversion should check if a negation is false
    -- it is like a filter