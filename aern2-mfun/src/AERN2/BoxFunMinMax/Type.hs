module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Optimisation
import AERN2.BoxFunMinMax.Optimisation
import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.Translators.BoxFun
import qualified AERN2.BoxFunMinMax.Expressions.Type as E
import AERN2.BoxFun.Box (createEnclosingBox, Box, fromVarMap, intersectionCertainlyEmpty, nonEmptyIntersection)
import qualified AERN2.Linear.Vector.Type as V
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

    checkDisjunctionOld :: Integer -> PavingMode -> Seq.Seq ([(Integer, E.E)], VarMap) -> (Maybe Bool, Maybe SearchBox)
    checkDisjunctionOld j mode0 jobQueue0 = checkDisjunctionJ mode0 jobQueue0
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
                  -- trace (vMapToJSON i mode vMap j) -- proved the disjunction on this box
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
                        -- trace "Stopping bisections (Box too small)" (Nothing,  Just (toSearchBox vMap (maximum (map snd esWithRanges))))
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
              case checkIfEsTrueUsingGlobalMinimum filteredEsGlobalMinimum Nothing of
                (Just True, _)-> 
                  -- trace (vMapToJSON i mode vMap j) -- proved the disjunction on this box
                  checkDisjunctionJ mode restJobs
                (_, box) ->
                  if width vMap !>! 1 / 10000000 then -- make this threshold quite small (maybe 10^-7)
                    -- trace ("Bisected boxes: " ++ show newBoxes) $
                    case box of
                      Just box' ->
                        checkDisjunctionJ mode (Seq.fromList (map (\b -> (es',b)) (fullBisect (fromBox box' (map fst vMap)))) <> restJobs)
                  else
                    case mode of
                      PaveBFS ->
                        trace "Stopping bisections (Box too small)" (Nothing,  Just (toSearchBox vMap (maximum (map snd esWithRanges))))
                      PaveDFS -> 
                        -- trace "Stopping bisections (Box too small)" (Nothing,  Just (toSearchBox vMap (maximum (map snd esWithRanges))))
                        checkDisjunctionJ PaveBFS jobQueue0 -- try to find a counterexample using BFS
            where
              esWithRanges = zip es (parMap rseq applyE es)


              filterOutFalseTerms     = (filter (\(_, range) -> not (range !<! 0))  esWithRanges)
              checkIfEsTrueUsingApply = map (\((i,_), range) -> (i, range !>=! 0))  filterOutFalseTerms
              filteredEsGlobalMinimum = (parMap rseq globalMinimumE (map fst filterOutFalseTerms))

              checkIfEsTrueUsingGlobalMinimum :: [(Maybe Bool, Maybe SearchBox)] -> Maybe Box -> (Maybe Bool, Maybe Box)
              checkIfEsTrueUsingGlobalMinimum [] box = (Nothing, box)
              checkIfEsTrueUsingGlobalMinimum (e:es) box = 
                case e of
                  (Just True, _) -> (Just True, Nothing)
                  (_, sbox) -> 
                    case box of
                      Nothing -> checkIfEsTrueUsingGlobalMinimum es (fmap extents sbox)
                      Just box' -> 
                        case sbox of
                          Nothing    -> checkIfEsTrueUsingGlobalMinimum es $ Just box'
                          Just sbox' -> checkIfEsTrueUsingGlobalMinimum es $ Just (createEnclosingBox box' (extents sbox'))
                  _ -> undefined --TODO: Remove False Es? will have to check inversion

              filterOutFalseEsUsingApply = map fst filterOutFalseTerms

              globalMinimumE e = globalMinimumAboveN2 f (bits 100) p (cn (mpBallP p 0.0))
                where
                  f = expressionToBoxFun (snd e) vMap p

              -- applyE e = apply f (setPrecision p (domain f))
              applyE e = applyLipschitz f (setPrecision p (domain f))
                where
                  f = expressionToBoxFun (snd e) vMap p
              
              newBoxes = fullBisect vMap

-- Plan: Modify case on l76 to use globalMinimum2. If we have any true results, check the rest of the disjunctions.
-- If only false or indeterminate results, bisect the box which contains all the false/indeterminate regions* and recurse.

-- *One way to get a box which contains all the false/indeterminate regions is to get a list of all lefts/rights of all Vars, and make boxes of the minimums/maximums.


    -- checkBoxes _ []         j  = (Just True, Nothing)
    -- checkBoxes es' (v : vs) j  = 
    --   case checkDisjunction j es' v of
    --     (Just True, _) -> checkBoxes es' vs j
    --     o              -> o --trace ("found false at " ++ show v) o 

    vMapToJSON colour mode vm j = 
      show j ++ " " ++ show mode ++ ": { " 
      ++ "\"colour\": " ++ show colour 
      ++ ", \"xL\":" ++ show (fst (snd (vm' !! 0))) 
      ++ ", \"xU\": " ++ show (snd (snd (vm' !! 0))) 
      ++ ", \"yL\": " ++ show (fst (snd (vm' !! 1))) 
      ++ ", \"yU\": " ++ show (snd (snd (vm' !! 1))) ++ " }"
      where 
      vm' = map (\(v, (l,u)) -> (v, (double l, double u))) vm
    --    negation of max e1 e2 e3 >= 0 ...
    -- == min -e1 -e2 -e3 < 0
    -- == min (-e1 < 0) (-e2 < 0) (-e3 < 0)
    -- == min (not (-e1 >= 0)) (not (-e2 >= 0)) (not (-e3 >= 0))

    -- we take e1 ("x",y) >= 0 and we want to prove this to be false for all x,y in box
    -- if (-e1 ("x",y) >= 0) is true, we have disproven e1 ("x",y) >= 0

    -- Try to prove the conjuction of the given list of expressions is False.
    -- The conjunction is expected to be of the following form:
    -- (not (e1 >= 0)) /\ (not (e2 >= 0)) /\ (not (e3 >= 0)) ...
            

    -- checkInversion should check if a negation is false
    -- it is like a filter


data PavingMode = PaveDFS | PaveBFS
  deriving Show

applyExpression :: E.E -> VarMap -> Precision -> CN MPBall
applyExpression expression varMap p =
  apply f (fromVarMap varMap p)
  where
    f = expressionToBoxFun expression varMap p

applyExpressionLipschitz :: E.E -> VarMap -> Precision -> CN MPBall
applyExpressionLipschitz expression varMap p =
  applyLipschitz f (fromVarMap varMap p)
  where
    f = expressionToBoxFun expression varMap p

filterOutFalseTermsInDisjunction :: [E.E] -> VarMap -> Precision -> [E.E]
filterOutFalseTermsInDisjunction expressions varMap p = filter (\e -> not (applyExpressionLipschitz e varMap p !>=! cnMPBallP p (cn 0))) expressions

decideDisjunctionWithBisectionUntilCutoff :: [E.E] -> VarMap -> Precision -> Rational -> (Maybe Bool, Maybe Box)
decideDisjunctionWithBisectionUntilCutoff expressions varMap p widthCutoff =
  -- if null (filterOutFalseTermsInDisjunction expressions varMap p) then
    -- (Just False, Just (fromVarMap varMap p))
  -- else
    case decideDisjunction expressions' varMap p Nothing of 
      r@(Just True, _) -> r
      r@(_, mAreaContainingMinimum) ->
        if width varMap > widthCutoff then
          let
            checkBisectionResults [] = (Just True, Nothing)
            checkBisectionResults (r:rs) =
              case r of
                (Just True, Nothing) -> checkBisectionResults rs
                o -> o
            bisectionResults = map (\area -> decideDisjunctionWithBisectionUntilCutoff expressions' area p widthCutoff)
          in
            case mAreaContainingMinimum of
                Just areaContainingMinimum -> trace "bisecting newton box" checkBisectionResults $ bisectionResults (fullBisect (fromBox areaContainingMinimum (map fst varMap)))
                _ -> checkBisectionResults $ bisectionResults (fullBisect varMap)
        else 
          case mAreaContainingMinimum of 
            Just areaContainingMinimum -> 
              if null (filterOutFalseTermsInDisjunction expressions (fromBox areaContainingMinimum (map fst varMap)) p) then
                (Just False, Just areaContainingMinimum)
              else
                (Nothing, Just areaContainingMinimum)  
            Nothing ->
              if null (filterOutFalseTermsInDisjunction expressions varMap p) then
                (Just False, Just (fromVarMap varMap p))
              else
                (Nothing, Just (fromVarMap varMap p))
  where
    -- expressions' = filterOutFalseTermsInDisjunction expressions varMap p
    expressions' = expressions

    -- TODO: Getting false results
    -- ideas
    -- 1. call filterOutFalseTermsInDisjunctions at the beggining of this function. 
    -- If resulting list is empty, return a False result over the given varMap. This seems wasteful?
    -- This does slow down the algorithm. It also doesnt help with heronPreservationSwap:r
    -- 2. After getting indeterminate result when reaching cutoff, try looking for false results using the filterOutFalseTermsInDisjunction.
    -- Could even use newton step to zoom in on a minimal false counterexample?
    -- Currently, filterOutFalseTermsInDisjunction is not helping here either

decideDisjunction :: [E.E] -> VarMap -> Precision -> Maybe Box -> (Maybe Bool, Maybe Box)
decideDisjunction [] _ _ mOldBox = (Nothing, mOldBox)
decideDisjunction (expression : expressions) varMap p mOldBox =
  if all (\corner -> applyExpression expression corner p !>! 0) corners then
    trace "all corners above zero" $
    if V.null newBox then
      trace "empty newBox"
      (Just True, Nothing)
    else
      case mOldBox of
        Nothing     -> decideDisjunction expressions (fromBox newBox (map fst varMap)) p (Just newBox)
        Just oldBox -> 
          if intersectionCertainlyEmpty oldBox newBox then
            trace "empty newBox after intersection with oldBox"
            (Just True, Nothing)
          else
            decideDisjunction expressions (fromBox newBox (map fst varMap)) p (Just (nonEmptyIntersection newBox oldBox))
  else
    trace "can't say all corners are above zero"
    decideDisjunction expressions varMap p mOldBox
  where
    corners = getCorners varMap
    newBox = locateNegationOfInequality expression varMap p

locateNegationOfInequality :: E.E -> VarMap -> Precision -> Box
locateNegationOfInequality expression varMap p =
  if roughRange !>! 0 then
    trace "roughRange above zero"
    V.empty
  else
    if roughRange !<=! 0 then
      trace "roughRange under zero"
      originalBox
    else
      if all (\edge -> applyExpression expression (lowerbound edge) p !>! 0 && applyExpression expression (upperbound edge) p !>! 0) edges then 
        -- unsafe, still need to check whole edge
        -- To get better results, check edges using locateNegationOfInequality recursively
        case newtonStepResult of
          Just True -> 
            trace "newton step returned empty box"
            V.empty
          _         ->
            case mFinalBox of
              Just finalBox -> 
                trace "returning finalBox from newton step"
                extents finalBox
              _             -> 
                trace "shouldn't be here"
                fromVarMap varMap p -- current implementation means we should not get here

      else
        trace "at least one edge possibly under zero"
        originalBox
  where
    roughRange = applyExpressionLipschitz expression varMap p
    edges = getEdges varMap
    
    originalBox = fromVarMap varMap p

    (newtonStepResult, mFinalBox) = minimumAboveN2 f originalBox (bits 100) p (cn (mpBallP p 0.0))
      where
        f = expressionToBoxFun expression varMap p
