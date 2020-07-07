module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Optimisation
import AERN2.BoxFunMinMax.Optimisation
import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.Translators.BoxFun
import qualified AERN2.BoxFunMinMax.Expressions.Type as E

import Control.Parallel.Strategies

import Debug.Trace (trace)

import Data.List (filter, find)
import qualified Data.Sequence as Seq

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

checkECNF :: [[E.E]] -> VarMap -> Precision -> (Maybe Bool, Maybe SearchBox)
checkECNF cnf vMapInit p = 
  checkConjunctionResults parCheckConjunction
  where
    parCheckConjunction = parMap rseq (\(j, esInit) -> checkDisjunction j PaveDFS (Seq.singleton (zip [1..] esInit, vMapInit))) (zip [1..] cnf)

    checkConjunctionResults []        = (Just True, Nothing)
    checkConjunctionResults (x : xs)  =
      case x of
        (Just True, _)  -> checkConjunctionResults xs
        o               -> o

    checkDisjunction :: Integer -> PavingMode -> Seq.Seq ([(Integer, E.E)], VarMap) -> (Maybe Bool, Maybe SearchBox)
    checkDisjunction j mode0 jobQueue0 = checkDisjunctionJ mode0 jobQueue0
      where
      checkDisjunctionJ mode jobQueue =
        case (mode, jobQueue) of
          (_, Seq.Empty) -> (Just True, Nothing)
          (PaveDFS, (es, vMap) Seq.:<| restJobs) -> checkBox PaveDFS es vMap restJobs
          (PaveBFS, restJobs Seq.:|> (es, vMap)) | Seq.length jobQueue < 2000 -> checkBox PaveBFS es vMap restJobs
          (_,restJobs Seq.:|> (es, vMap)) -> 
                        trace "Stopping bisections (too many boxes)" (Nothing,  Just (toSearchBox vMap (cn $ mpBall 0)))
      checkBox mode es vMap restJobs =
          case filterOutFalseEsUsingApply of
            []  -> (Just False, Just (toSearchBox vMap (maximum (map snd esWithRanges))))
            -- [(i,e)] -> 
            --   case globalMinimumAboveN f' ac p (cn (mpBallP p (1 /! 10000000))) (cn (mpBallP p 0.0)) of
            --     r@(Just True, _) -> trace (vMapToJSON (i+4) vMap j) r
            --     o -> o
            --     where
            --       f = expressionToBoxFun e vMap p
            --       f' = BoxFun (dimension f) (bf_eval f) (setPrecision p (domain f))
            es' ->
              case find snd checkIfEsTrueUsingApply of
                Just (i,_) -> 
                  -- trace (vMapToJSON i vMap j)
                  checkDisjunctionJ mode restJobs
                _ ->
                  if width vMap !>! 1 / 10000000 then -- make this threshold quite small (maybe 10^-7)
                    -- trace ("Bisected boxes: " ++ show newBoxes) $
                    checkDisjunctionJ mode (Seq.fromList (map (\box -> (es',box)) newBoxes) <> restJobs)
                  else
                    case mode of
                      PaveBFS ->
                        trace "Stopping bisections (Box too small)" (Nothing,  Just (toSearchBox vMap (maximum (map snd esWithRanges))))
                      PaveDFS -> 
                        checkDisjunctionJ PaveBFS jobQueue0 -- try to find a counterexample using BFS
            where
              esWithRanges = zip es (parMap rseq applyE es)

              filterOutFalseTerms     = (filter (\(_, range) -> not (range !<! 0))  esWithRanges)
              checkIfEsTrueUsingApply = map (\((i,_), range) -> (i, range !>=! 0))  filterOutFalseTerms

              filterOutFalseEsUsingApply = map fst filterOutFalseTerms

              -- applyE e = apply f (setPrecision p (domain f))
              applyE e = applyLipschitz f (setPrecision p (domain f))
                where
                  f = expressionToBoxFun (snd e) vMap p
              
              newBoxes = fullBisect vMap

    -- checkBoxes _ []         j  = (Just True, Nothing)
    -- checkBoxes es' (v : vs) j  = 
    --   case checkDisjunction j es' v of
    --     (Just True, _) -> checkBoxes es' vs j
    --     o              -> o --trace ("found false at " ++ show v) o 

    vMapToJSON colour vm j = show j ++ ": { \"colour\": " ++ show colour ++ ", \"xL\":" ++ show (fst (snd (vm' !! 0))) ++ ", \"xU\": " ++ show (snd (snd (vm' !! 0))) ++ ", \"yL\": " ++ show (fst (snd (vm' !! 1))) ++ ", \"yU\": " ++ show (snd (snd (vm' !! 1))) ++ " }"
      where vm' = map (\(v, (l,u)) -> (v, (double l, double u))) vm
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


data PavingMode = PaveDFS | PaveBFS
