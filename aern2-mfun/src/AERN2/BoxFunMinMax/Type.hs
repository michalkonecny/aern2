module AERN2.BoxFunMinMax.Type where

import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN
import AERN2.MP.Ball
import AERN2.BoxFun.Type
import AERN2.BoxFun.Optimisation
import AERN2.BoxFunMinMax.Optimisation
import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.Translators.BoxFun
import qualified AERN2.BoxFunMinMax.Expressions.Type as E
import AERN2.BoxFun.Box (createEnclosingBox, Box, fromVarMap, intersectionCertainlyEmpty, nonEmptyIntersection, lowerBounds, upperBounds)
import qualified AERN2.Linear.Vector.Type as V
import AERN2.Kleenean
import Control.Parallel.Strategies
import Data.Bifunctor
import qualified Simplex as S

import Data.List (find, intercalate)
import qualified Data.Sequence as Seq

import AERN2.MP.Dyadic (Dyadic, dyadic)

import qualified Debug.Trace as T

import qualified Data.PQueue.Prio.Min as Q

import Data.Maybe
import Control.CollectErrors
trace a x = x

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
            --   case globalMinimumAboveN f' ac p (cn (mpBallP p (1 / 10000000))) (cn (mpBallP p 0.0)) of
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
                  if maxWidth vMap !>! 1 / 10000000 then -- make this threshold quite small (maybe 10^-7)
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
            --   case globalMinimumAboveN f' ac p (cn (mpBallP p (1 / 10000000))) (cn (mpBallP p 0.0)) of
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
                  if maxWidth vMap !>! 1 / 10000000 then -- make this threshold quite small (maybe 10^-7)
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

applyDisjunction :: [E.E] -> VarMap -> Precision -> [CN MPBall]
applyDisjunction expressions varMap p =
  map 
  (\e -> apply (expressionToBoxFun e varMap p) box)
  expressions
  where
    box = fromVarMap varMap p

applyECNF :: [[E.E]] -> VarMap -> Precision -> [[CN MPBall]]
applyECNF cnf varMap p = map (\d -> applyDisjunction d varMap p) cnf

rangeAboveZero :: CN MPBall -> Bool
rangeAboveZero r = r !>! 0 

disjunctionRangesAboveZero :: [CN MPBall] -> Bool
disjunctionRangesAboveZero = any rangeAboveZero

conjunctionRangesAboveZero :: [[CN MPBall]] -> Bool
conjunctionRangesAboveZero = all disjunctionRangesAboveZero

rangeBelowZero :: CN MPBall -> Bool
rangeBelowZero r = r !<! 0 

disjunctionRangesBelowZero :: [CN MPBall] -> Bool
disjunctionRangesBelowZero = all rangeBelowZero

conjunctionRangesBelowZero :: [[CN MPBall]] -> Bool
conjunctionRangesBelowZero = all disjunctionRangesBelowZero

checkFWithApply :: E.F -> VarMap -> Precision -> CN Kleenean
checkFWithApply (E.FComp op e1 e2) varMap p = 
  case op of
    E.Ge -> e1Val >= e2Val
    E.Gt -> e1Val >  e2Val
    E.Le -> e1Val <= e2Val
    E.Lt -> e1Val <  e2Val
    E.Eq -> e1Val == e2Val
  where
    e1Val = applyExpression e1 varMap p
    e2Val = applyExpression e2 varMap p
checkFWithApply (E.FConn op f1 f2) varMap p =
  case op of
    E.And   -> f1Val && f2Val
    E.Or    -> f1Val || f2Val
    E.Equiv -> 
      case f1Val of
        (CollectErrors (Just CertainTrue) error1) ->
          case f2Val of
            (CollectErrors (Just CertainTrue) error2)  -> prependErrors error2 $ CollectErrors (Just CertainTrue) error1
            (CollectErrors (Just CertainFalse) error2) -> prependErrors error2 $ CollectErrors (Just CertainFalse) error1
            (CollectErrors (Just TrueOrFalse) error2)  -> prependErrors error2 $ CollectErrors (Just TrueOrFalse) error1
            (CollectErrors Nothing error2)             -> prependErrors error2 $ CollectErrors Nothing error1
        (CollectErrors (Just CertainFalse) error1) ->
          case f2Val of
            (CollectErrors (Just CertainTrue) error2)  -> prependErrors error2 $ CollectErrors (Just CertainFalse) error1
            (CollectErrors (Just CertainFalse) error2) -> prependErrors error2 $ CollectErrors (Just CertainTrue) error1
            (CollectErrors (Just TrueOrFalse) error2)  -> prependErrors error2 $ CollectErrors (Just TrueOrFalse) error1
            (CollectErrors Nothing error2)             -> prependErrors error2 $ CollectErrors Nothing error1
        (CollectErrors (Just TrueOrFalse) error1) ->
          case f2Val of
            (CollectErrors Nothing error2)             -> prependErrors error2 $ CollectErrors Nothing error1
            (CollectErrors _ error2)                   -> prependErrors error2 $ CollectErrors (Just TrueOrFalse) error1
        (CollectErrors Nothing error1)                 -> CollectErrors Nothing error1
    E.Impl  -> not f1Val || f2Val
  where
    f1Val = checkFWithApply f1 varMap p
    f2Val = checkFWithApply f2 varMap p
checkFWithApply (E.FNot f) varMap p = not $ checkFWithApply f varMap p
checkFWithApply E.FTrue _ _         = cn CertainTrue
checkFWithApply E.FFalse _ _        = cn CertainFalse

filterOutFalseTermsInDisjunction :: [E.E] -> VarMap -> Precision -> [E.E]
filterOutFalseTermsInDisjunction expressions varMap p = filter (\e -> not (applyExpressionLipschitz e varMap p !>=! cnMPBallP p (cn 0))) expressions

decideDisjunctionWithBisectionUntilCutoff :: [E.E] -> VarMap -> Precision -> Rational -> (Maybe Bool, Maybe Box)
decideDisjunctionWithBisectionUntilCutoff expressions varMap p widthCutoff =
  -- if null (filterOutFalseTermsInDisjunction expressions varMap p) then
    -- (Just False, Just (fromVarMap varMap p))
  -- else
    case decideDisjunction expressions' varMap p Nothing of 
      r@(Just True, _) -> r
      (_, mAreaContainingMinimum) ->
        if maxWidth varMap > widthCutoff then
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
                extents finalBox -- This is wrong
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

checkECNFSimplex :: [[E.E]] -> VarMap -> Rational -> Rational -> Integer -> Precision -> (Maybe Bool, Maybe VarMap)
checkECNFSimplex [] _ _ _ _ _ = (Just True, Nothing)
checkECNFSimplex (disjunction : disjunctions) varMap maxWidthCutoff relativeImprovementCutoff maxBoxesCutoff p =
  case decideDisjunctionWithSimplex (map (\e -> (e, expressionToBoxFun e varMap p)) disjunction) varMap maxWidthCutoff relativeImprovementCutoff p of
    (Just True, _) -> 
      checkECNFSimplex disjunctions varMap maxWidthCutoff relativeImprovementCutoff maxBoxesCutoff p
    r@(Just False, _)              -> r
    (Nothing, Just indeterminateArea) ->
      let
        areaToSearch = intersectVarMap varMap $ increaseRadius indeterminateArea ((maxWidth indeterminateArea) / 10.0) --TODO: Parameterise
                                                                                                                        --TODO: Could be better to increase the radius of each variable seperately
        
        checkIndeterminateArea areaToCheck = decideDisjunctionWithSimplex (map (\e -> (e, expressionToBoxFun e areaToCheck p)) disjunction) areaToCheck (maxWidth areaToCheck) relativeImprovementCutoff p

        checkAreas :: [VarMap] -> [(Maybe Bool, Maybe VarMap)]
        checkAreas = parMap rseq checkIndeterminateArea

        checkBFSResults :: [(Maybe Bool, Maybe VarMap)] -> [VarMap] -> Integer -> (Maybe Bool, Maybe VarMap)
        checkBFSResults [] [] _ = (Just True, Nothing)
        checkBFSResults [] indeterminateAreas boxesChecked =
          let
            newAreas = 
              concatMap 
              (\vm -> 
                let
                  (leftBisection, rightBisection) = bisectVar vm (fst (widestInterval (tail vm) (head vm))) 
                in
                  [leftBisection, rightBisection]
              )
              indeterminateAreas
          in
            if boxesChecked < maxBoxesCutoff
              then checkBFSResults (checkAreas newAreas) [] boxesChecked
              else (Nothing, Just (head indeterminateAreas))
        checkBFSResults (result : results) indeterminateAreas boxesChecked =
          if boxesChecked < maxBoxesCutoff then
            case result of
              (Just True, _) -> checkBFSResults results indeterminateAreas boxesChecked
              (Just False, _) -> result
              (Nothing, Just indeterminateArea) -> checkBFSResults results (indeterminateArea : indeterminateAreas) (boxesChecked + 1)
              (Nothing, Nothing) -> error "Given indeterminate result without indeterminate area"
          else (Nothing, Just indeterminateArea)
        -- We have a cutoff for the boxes we have examined in total
        checkBFS :: [VarMap] -> [VarMap] -> Integer -> (Maybe Bool, Maybe VarMap)
        checkBFS [] [] _ = (Just True, Nothing)
        checkBFS [] indeterminateAreas boxesProcessed =
          let 
            -- (leftVarMap, rightVarMap) = bisectVar varMapToBisect (fst (widestInterval (tail varMapToBisect) (head varMapToBisect)))
            -- newAreas = concatMap fullBisect indeterminateAreas
            newAreas = 
              concatMap 
              (\vm -> 
                let
                  (leftBisection, rightBisection) = bisectVar vm (fst (widestInterval (tail vm) (head vm))) 
                in
                  [leftBisection, rightBisection]
              )
              indeterminateAreas
          in
            if boxesProcessed < maxBoxesCutoff
              then checkBFS newAreas [] boxesProcessed
              else (Nothing, Just (head indeterminateAreas)) -- (Smallest area?)
        checkBFS (area : areas) indeterminateAreas boxesProcessed =
          if boxesProcessed < maxBoxesCutoff
            then
              case checkIndeterminateArea area of
                (Just True, _) -> checkBFS areas indeterminateAreas (boxesProcessed + 1)
                r@(Just False, _) -> r
                (Nothing, Just indeterminateArea) -> checkBFS areas (indeterminateArea : indeterminateAreas) (boxesProcessed + 1)
                (Nothing, Nothing) -> error "Given indeterminate result without indeterminate area"
            else (Nothing, Just indeterminateArea)

        -- checkBFS :: [VarMap] -> [VarMap] -> (Maybe Bool, Maybe VarMap)
        -- checkBFS [] [] = (Just True, Nothing)
        -- checkBFS [] indeterminateAreas =
        --   let 
        --     newAreas = concatMap fullBisect indeterminateAreas
        --   in
        --     if length newAreas < maxBoxesCutoff
        --       then checkBFS newAreas []
        --       else (Nothing, Just (head indeterminateAreas)) -- (Smallest area?)
        -- checkBFS (area : areas) indeterminateAreas =
        --   case checkIndeterminateArea area of
        --     r@(Just True, _) -> checkBFS areas indeterminateAreas
        --     r@(Just False, _) -> r
        --     (Nothing, Just indeterminateArea) -> checkBFS areas (indeterminateArea : indeterminateAreas)
        --     (Nothing, Nothing) -> error "Given indeterminate result without indeterminate area"
      in
        -- undefined
        case checkBFSResults (checkAreas [areaToSearch]) [] 0 of
          (Just True, _) ->
            case checkBFSResults (checkAreas [varMap]) [] 0 of
              (Just True, _) -> checkECNFSimplex disjunctions varMap maxWidthCutoff relativeImprovementCutoff maxBoxesCutoff p
              r@(_, _) -> r
          r@(Just False, _) -> r
          (Nothing, _) -> checkBFSResults (checkAreas [varMap]) [] 0
    (Nothing, Nothing) -> error "Given indeterminate result without indeterminate area"
    -- TODO: If this is indeterminate, increase the radius of this box to capture a neighbourhood surrounding the box
    -- Make sure the new box is completely within the original box
    -- Use simplex to cut off any true area
    -- Look into using queues and modes for breadth-first/depth-first search

    -- (Nothing, Just indeterminateArea) -> checkUsingGlobalMinimum disjunction Nothing
    --   where
    --     checkUsingGlobalMinimum []       mFalseArea = 
    --       case mFalseArea of
    --         Nothing         -> (Nothing, Just indeterminateArea)
    --         Just falseArea  -> (Just False, Just (fromBox (extents falseArea) (map fst varMap)))

    --     checkUsingGlobalMinimum (e : es) mFalseArea = 
    --       case globalMinimumAboveN2 (expressionToBoxFun e indeterminateArea p) (bits 100) p (cn (mpBallP p 0)) of
    --         (Just True, _)                -> checkECNFSimplex disjunctions varMap p
    --         (Just False, Just falseArea)  -> checkUsingGlobalMinimum es (Just falseArea)
    --         (Nothing, _)                  -> checkUsingGlobalMinimum es mFalseArea
    --         (Just False, _)               -> error "False result from globalMinimumAboveN2 did not return a SearchBox"
    -- (Nothing, _) -> error "Indeterminate result from decideDisjunctionWithSimplex did not return indeterminate area"

-- TODO: Can look for true results also
-- Use simplex to cutoff true areas
decideDisjunctionWithBreadthFirst :: [E.E] -> [VarMap] -> [VarMap] -> Rational -> Precision -> Maybe VarMap
decideDisjunctionWithBreadthFirst expressions [] checkedVarMaps maxWidthCutoff p =
  if (MixedTypesNumPrelude.minimum (map maxWidth checkedVarMaps)) !>=! maxWidthCutoff  --FIXME what if checkedVarMaps is empty?
    then
      trace "all expressions indeterminate, bisecting all varMaps"
      decideDisjunctionWithBreadthFirst expressions (concatMap fullBisect checkedVarMaps) [] maxWidthCutoff p
    else 
      trace "reached breadth-first cutoff and expressions still indeterminate"
      Nothing
decideDisjunctionWithBreadthFirst expressions (varMap : varMaps) checkedVarMaps maxWidthCutoff p =
  if null filteredExpressions
    then
      trace ("proved false with apply (breadth first) " ++ show varMap)
      (Just varMap)
    else
      if (and areExpressionsTrue) 
        then
          trace (show (length expressions))
          trace ("expressions true over varMap" ++ show varMap)
          decideDisjunctionWithBreadthFirst filteredExpressions varMaps checkedVarMaps maxWidthCutoff p
        else
          trace (show (length expressions))
          trace ("checking next expression (breadth first) " ++ show varMap)
          decideDisjunctionWithBreadthFirst filteredExpressions varMaps (varMap : checkedVarMaps) maxWidthCutoff p
  where
    applyE vm e = applyLipschitz f (setPrecision p (domain f))
      where
        f = expressionToBoxFun e vm p
    esWithRanges            = zip expressions (parMap rseq (applyE varMap) expressions)
    filterOutFalseTerms     = filter (\(_, range) -> not (range !<! 0))  esWithRanges -- Could make this cleaner with list comprehension
    areExpressionsTrue      = map (\(_, range) -> range !>! 0) esWithRanges
    filteredExpressions     = map fst filterOutFalseTerms

type BoxAddress = [(String, Int)]

checkECNFCE :: [[E.E]] -> VarMap -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe VarMap)
-- checkECNFCE [] _ _ _ _ mIndeterminateArea _ = 
--   case mIndeterminateArea of
--     Nothing -> (Just True, Nothing)
--     area@(Just _) -> (Nothing, area)
-- checkECNFCE (disjunction : disjunctions) varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff mIndeterminateArea p =
--   case decideDisjunctionWithSimplexCE (map (\e -> (e, expressionToBoxFun e varMap p)) disjunction) varMap 0 depthCutoff bfsBoxesCutoff relativeImprovementCutoff p of
--     (Just True, _) -> checkECNFCE disjunctions varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff mIndeterminateArea p
--     (Nothing, Just indeterminateArea) -> checkECNFCE disjunctions varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff (Just indeterminateArea) p
--     r@(Just False, _) -> r
--     (Nothing, _) -> undefined 
checkECNFCE disjunctions varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff p =
  checkResults disjunctionResults Nothing
  where
    disjunctionResults = parMap rseq (\disjunction ->  decideDisjunctionWithSimplexCE (map (\e -> (e, expressionToBoxFun e varMap p)) disjunction) varMap 0 depthCutoff bfsBoxesCutoff relativeImprovementCutoff p) disjunctions

    checkResults :: [(Maybe Bool, Maybe VarMap)] -> Maybe VarMap -> (Maybe Bool, Maybe VarMap)
    checkResults [] Nothing = (Just True, Nothing)
    checkResults [] indeterminateArea@(Just _) = (Nothing, indeterminateArea)
    checkResults (result : results) mIndeterminateArea =
      case result of
        (Just True, _) -> checkResults results mIndeterminateArea
        r@(Just False, _) -> r
        (Nothing, indeterminateArea@(Just _)) -> checkResults results indeterminateArea
        (Nothing, Nothing) -> undefined
  -- case decideDisjunctionWithSimplexCE (map (\e -> (e, expressionToBoxFun e varMap p)) disjunction) varMap 0 depthCutoff bfsBoxesCutoff relativeImprovementCutoff p of
  --   (Just True, _) -> checkECNFCE disjunctions varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff mIndeterminateArea p
  --   (Nothing, Just indeterminateArea) -> checkECNFCE disjunctions varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff (Just indeterminateArea) p
  --   r@(Just False, _) -> r
  --   (Nothing, _) -> undefined 

-- TODO: Implement own record type with hasorder instance
-- Use this https://hackage.haskell.org/package/pqueue-1.4.1.3/docs/Data-PQueue-Min.html
-- Define P.Ord
instance HasOrderAsymmetric (Dyadic, VarMap) (Dyadic, VarMap)  where
  lessThan (v1, _) (v2, _) = lessThan v1 v2
  leq (v1, _) (v2, _) = leq v1 v2

decideDisjunctionBFS :: [VarMap] -> [E.E] -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe VarMap)
decideDisjunctionBFS [] _ _ _ _ _ = (Just True, Nothing)
decideDisjunctionBFS (varMap : varMaps) expressions numberOfBoxesExamined numberOfBoxesCutoff relativeImprovementCutoff p =
  if numberOfBoxesExamined !<! numberOfBoxesCutoff then
    T.trace ("b: " ++ show numberOfBoxesExamined) $
    case decideDisjunctionWithSimplexCE (map (\e -> (e, expressionToBoxFun e varMap p)) expressions) varMap 0 0 0 relativeImprovementCutoff p of
      (Just True, _) -> decideDisjunctionBFS varMaps expressions (numberOfBoxesExamined + 1) numberOfBoxesCutoff relativeImprovementCutoff p 
      r@(Just False, _) -> r
      (Nothing, Just indeterminateVarMap) -> 
        let (leftVarMap, rightVarMap) = bisectVar indeterminateVarMap (fst (widestInterval (tail varMap) (head varMap)))
        in decideDisjunctionBFS (varMaps ++ [leftVarMap, rightVarMap]) expressions (numberOfBoxesExamined + 1) numberOfBoxesCutoff relativeImprovementCutoff p
      (Nothing, Nothing) -> undefined
  else (Nothing, Just varMap) -- TODO: 'best' indeterminate area?

mean :: [CN Dyadic] -> CN Rational
mean xs = sum xs / length xs

decideDisjunctionBestFirst :: Q.MinPQueue (CN Dyadic) VarMap -> [(E.E, BoxFun)] -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe VarMap)
decideDisjunctionBestFirst queue expressionsWithFunctions numberOfBoxesExamined numberOfBoxesCutoff relativeImprovementCutoff p =
  case Q.minView queue of
    Just (varMap, queueWithoutVarMap) ->
      if numberOfBoxesExamined !<! numberOfBoxesCutoff then
        T.trace (show numberOfBoxesExamined) $
        case decideDisjunctionWithSimplexCE expressionsWithFunctions varMap 0 0 0 relativeImprovementCutoff p of
          (Just True, _) -> decideDisjunctionBestFirst queueWithoutVarMap expressionsWithFunctions (numberOfBoxesExamined + 1) numberOfBoxesCutoff relativeImprovementCutoff p 
          r@(Just False, _) -> r
          (Nothing, Just indeterminateVarMap) -> T.trace "h" $
            let 
              -- esWithRanges = parMap rseq (\ (e, f) -> ((e, f), apply f (fromVarMap indeterminateVarMap p))) expressionsWithFunctions
              -- filteredExpressionsWithFunctions = map fst $ filter (\ (_, range) -> not (range !<! 0)) esWithRanges
              filteredExpressionsWithFunctions = -- Can't recurse with this. Maybe add this to the queue? Each box would have expressions to check. Inefficient?
                [
                  notFalseTerms | 
                  notFalseTerms@(_, f) <- expressionsWithFunctions,
                  not (apply f (fromVarMap indeterminateVarMap p) !<! 0)
                ]

              (leftVarMap, rightVarMap) = T.trace "bisecting" bisectVar indeterminateVarMap (fst (widestInterval (tail varMap) (head varMap)))
              -- [leftVarMapWithMean, rightVarMapWithMean] = 
              --   [
              --     [(apply f (fromVarMap leftVarMap p), leftVarMap), (apply f (fromVarMap rightVarMap p), rightVarMap)]
              --     |
              --     (e, f) <- filteredExpressionsWithFunctions,
              --     let tmp = apply f (fromVarMap leftVarMap p)
              --   ]
              leftVarMapWithMean  = T.trace (show (map fst expressionsWithFunctions)) $ T.trace "left"
                (
                  maximum $ map (\(_, f) -> AERN2.MP.Ball.centre (apply f (fromVarMap leftVarMap p))) expressionsWithFunctions, 
                  leftVarMap
                )
              rightVarMapWithMean = T.trace "right"
                (
                  maximum $ map (\(_, f) -> AERN2.MP.Ball.centre (apply f (fromVarMap rightVarMap p))) expressionsWithFunctions, 
                  rightVarMap
                )
            in 
              decideDisjunctionBestFirst 
              (uncurry Q.insert rightVarMapWithMean (uncurry Q.insert leftVarMapWithMean queueWithoutVarMap))
              expressionsWithFunctions (numberOfBoxesExamined + 1) numberOfBoxesCutoff relativeImprovementCutoff p
          (Nothing, Nothing) -> error "Got nothing with indeterminate varMap in decideDisjunctionBestFirst"
      else (Nothing, Just varMap)   -- Reached number of boxes cutoff
    Nothing -> (Just True, Nothing) -- All areas in queue verified

decideDisjunctionWithSimplexCE :: [(E.E, BoxFun)] -> VarMap -> Integer -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe VarMap)
decideDisjunctionWithSimplexCE expressionsWithFunctions varMap currentDepth depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
  | null filterOutFalseTerms = 
    trace ("proved false with apply " ++ show varMap)
    (Just False, Just varMap)
  | checkIfEsTrueUsingApply = 
    trace "proved true with apply" 
    (Just True, Nothing)
  -- | greatestCentre !<! 0.0 =
  --   trace "checking BFS" $
  --   case decideDisjunctionBFS [varMap] filteredExpressions 0 bfsBoxesCutoff relativeImprovementCutoff p of
  --     r@(Just False, _) -> r
  --     (_, _) -> checkSimplex
  | otherwise = checkSimplex
  where
      box  = fromVarMap varMap p
      boxL = lowerBounds box
      boxU = upperBounds box

      esWithRanges            = parMap rseq (\ (e, f) -> ((e, f), apply f box)) expressionsWithFunctions
      filterOutFalseTerms     = filter (\ (_, range) -> not (range !<! 0)) esWithRanges
      -- greatestCentre          = maximum $ map (AERN2.MP.Ball.centre . snd) esWithRanges
      checkIfEsTrueUsingApply = any (\ (_, range) -> range !>=! 0) filterOutFalseTerms

      filteredExpressionsWithFunctions = map fst filterOutFalseTerms
      filteredExpressions              = map fst filteredExpressionsWithFunctions

      -- (rangeAtLeftCornerOfBox, rangeAtRightCornerOfBox, firstDerivativesOverBox) for each filtered expression
      cornerRangesWithDerivatives = 
        parMap rseq
        (\ ((_, f), _) -> (apply f boxL, apply f boxU, gradient f box))
        filterOutFalseTerms

      -- Keep the functions where we can calculate all derivatives
      filteredCornerRangesWithDerivatives =
        filter
        (\(_, _, c) -> not (V.any hasError c)) -- Filter out functions where any partial derivative contains an error TODO: check corners as well?
        cornerRangesWithDerivatives

      breadthFirstCheck varMapToCheck =
        decideDisjunctionBFS 
        [varMapToCheck]
        filteredExpressions
        0
        bfsBoxesCutoff
        relativeImprovementCutoff
        p
    

      bestFirstCheck varMapToCheck =
        decideDisjunctionBestFirst 
          (Q.singleton (maximum (map (\(_, f) -> AERN2.MP.Ball.centre (apply f (fromVarMap varMapToCheck p))) filteredExpressionsWithFunctions)) varMapToCheck)
          filteredExpressionsWithFunctions
          0
          bfsBoxesCutoff
          relativeImprovementCutoff
          p

      bisectWidestDimensionAndRecurse varMapToBisect = 
        let
          (widestVar, (_, _)) = widestInterval (tail varMapToBisect) (head varMapToBisect)
          (leftVarMap, rightVarMap) = bisectVar varMapToBisect widestVar
          (leftR, rightR) =
            withStrategy
            (parTuple2 rseq rseq)
            (
              decideDisjunctionWithSimplexCE filteredExpressionsWithFunctions leftVarMap (currentDepth + 1) depthCutoff bfsBoxesCutoff relativeImprovementCutoff p, 
              decideDisjunctionWithSimplexCE filteredExpressionsWithFunctions rightVarMap (currentDepth + 1) depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
            )
        in
          case leftR of
            (Just True, _)
              -> case rightR of
                (Just True, _) -> (Just True, Nothing)
                r -> r
            r -> r

      bestFirstCheckThenBisect varMapToCheck =
        if currentDepth !<! depthCutoff -- Best first
          then
            if rangeMean !<! 0.0 then -- Trigger best-first search only if this succeeds
              -- T.trace "best first" $
              case bestFirstCheck varMapToCheck of
                (Just True, _) -> (Just True, Nothing)
                r@(Just False, _) -> r
                (Nothing, _) -> bisectWidestDimensionAndRecurse varMapToCheck -- Wasted effort here
            else
              bisectWidestDimensionAndRecurse varMapToCheck
          else
            bestFirstCheck varMapToCheck --Last ditch bestFirst check
        where
          rangeMean = maximum $ map (AERN2.MP.Ball.centre . snd) filterOutFalseTerms

        -- if currentDepth !<! depthCutoff -- Original
        --   then
        --     -- if rangeMean !<! 0.1 then -- Trigger best-first search only if this succeeds
        --     --   T.trace "best first" $
        --     --   case bestFirstCheck varMapToCheck of
        --     --     (Just True, _) -> (Just True, Nothing)
        --     --     r@(Just False, _) -> r
        --     --     (Nothing, _) -> bisectWidestDimensionAndRecurse varMapToCheck -- Wasted effort here
        --     -- else
        --     bisectWidestDimensionAndRecurse varMapToCheck
        --   else
        --     breadthFirstCheck varMapToCheck --Last ditch bestFirst check
        -- where
        --   rangeMean = maximum $ map (AERN2.MP.Ball.centre . snd) filterOutFalseTerms



      checkSimplex
        -- If we can calculate all derivatives
        -- | and (concatMap (\ (_, _, c) -> V.toList (V.map (not . CN.hasError) c)) cornerRangesWithDerivatives) = T.trace "decideWithSimplex start" $
        | (not . null) filteredCornerRangesWithDerivatives = T.trace "decideWithSimplex start" $
          case decideWithSimplex filteredCornerRangesWithDerivatives varMap of
            r@(Just True, _) -> T.trace "decideWithSimplex true" r
            (Nothing, Just newVarMap) -> T.trace "decideWithSimplex indet" $
              let 
                newBox  = fromVarMap newVarMap p
                newBoxL = lowerBounds box
                newBoxU = upperBounds box
                newCornerRangesWithDerivatives =
                  parMap rseq
                  (\ ((_, f), _) -> (apply f newBoxL, apply f newBoxU, gradient f newBox))
                  filterOutFalseTerms
                varNames = map fst newVarMap
                roundedVarMap = fromBox newBox varNames --FIXME: this is unsafe
              in T.trace "findFalsePointWithSimplex start" $
                case findFalsePointWithSimplex newCornerRangesWithDerivatives newVarMap of
                  Nothing             -> T.trace "findFalsePointWithSimplex indet" recurseOnVarMap newVarMap
                  Just counterExample -> T.trace "findFalsePointWithSimplex false" (Just False, Just counterExample)
            _ -> undefined
        | otherwise = bestFirstCheckThenBisect varMap

      recurseOnVarMap newVarMap
        | taxicabWidth varMap / taxicabWidth newVarMap !>=! cn relativeImprovementCutoff = 
          trace ("recursing with simplex with varMap: " ++ show newVarMap) $ 
          decideDisjunctionWithSimplexCE filteredExpressionsWithFunctions newVarMap currentDepth depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
        | otherwise = bestFirstCheckThenBisect newVarMap

setupSystem :: [E.E] -> VarMap -> ([(CN MPBall, CN MPBall, Box)])
setupSystem expressions varMap = 
  map  
  (\e ->
    let
      f = expressionToBoxFun e varMap (prec 100)
    in
      (
        -- left corner range
        apply f boxL,
        -- right corner range
        apply f boxU,
        -- derivatives
        gradient f box
      )
  )
  expressions
  where
    box  = fromVarMap varMap (prec 100)
    boxL = lowerBounds box
    boxU = upperBounds box

decideDisjunctionWithSimplex :: [(E.E, BoxFun)] -> VarMap -> Rational -> Rational -> Precision -> (Maybe Bool, Maybe VarMap)
decideDisjunctionWithSimplex expressionsWithFunctions varMap maxWidthCutoff relativeImprovementCutoff p = 
  -- trace (showVarMapWithDecimals varMap) $
  -- unsafePerformIO $ do
  --   appendFile "/home/junaid/Research/git/aern2-base/aern2/boxes/3s2oneCorner.txt" (show varMap ++ "\n")
  --   return $ 
  if null filterOutFalseTerms
    then 
      trace ("proved false with apply " ++ showVarMapWithDecimals varMap)
      (Just False, Just varMap)
    else 
      if checkIfEsTrueUsingApply 
        then 
          trace "proved true with apply" 
          (Just True, Nothing)
        else 
          -- Only call decideWithSimplex if all derivatives can be calculated
          if and (concatMap (\(_, _, c) -> V.toList (V.map (not . CN.hasError) c)) cornerRangesWithDerivatives)
            then 
              case decideWithSimplex cornerRangesWithDerivatives varMap of
                r@(Just True, _) -> 
                  trace "proved true with simplex" 
                  r
                r@(Nothing, Just newVarMap) ->
                  -- let
                  --   checkUsingGlobalMinimum :: [E.E] -> (Maybe Bool, Maybe VarMap)
                    
                  --   checkUsingGlobalMinimum [] = (Nothing, Just newVarMap)

                  --   checkUsingGlobalMinimum (e : es) = 
                  --     case globalMinimumAboveN2 (expressionToBoxFun e newVarMap p) (bits 100) p (cn (mpBallP p 0)) of
                  --       (Just True, _)                    -> (Just True, Nothing)
                  --       (_, _)                            -> checkUsingGlobalMinimum es
                  -- in
                  --   checkUsingGlobalMinimum expressions 
                  let 
                    -- lastBox = fromVarMap varMap p
                    -- newBox  = fromVarMap newVarMap p
                    -- boxChangeWidth = abs(Box.width (lastBox - newBox))
                    -- boxChangeWidth = (taxicabWidth varMap - taxicabWidth newVarMap)
                  in
                  if taxicabWidth varMap / taxicabWidth newVarMap !>=! cn relativeImprovementCutoff
                    then
                      -- trace "--------------------"
                      -- trace (show varMap)
                      -- trace (show newVarMap)
                      -- trace "--------------------"
                      trace ("recursing with simplex with varMap: " ++ show newVarMap) $
                      decideDisjunctionWithSimplex filteredExpressionsWithFunctions newVarMap maxWidthCutoff relativeImprovementCutoff p
                    else
                      if maxWidth newVarMap !>=! maxWidthCutoff
                        then 
                          trace ("bisecting with varMap from Simplex: " ++ show newVarMap) $
                          bisectWidestDimensionAndRecurse newVarMap
                        else 
                          trace ("varMap too small to bisect after simplex" ++ show newVarMap) $ 
                          r
                _ -> undefined
              else
                if maxWidth varMap !>=! maxWidthCutoff 
                  then trace ("bisecting without simplex" ++ show varMap) $ bisectWidestDimensionAndRecurse varMap
                  else trace ("varMap too small to bisect" ++ show varMap) (Nothing, Just varMap)  
  where
    showVarMapWithDecimals :: VarMap -> String
    showVarMapWithDecimals = concatMap (\(v, (l, u)) -> show v ++ ": [" ++ ((show . double)  l) ++ ", " ++ ((show . double) u) ++ "] \n")

    box  = fromVarMap varMap p
    boxL = lowerBounds box
    boxU = upperBounds box

    esWithRanges            = map (\(e, f) -> ((e, f), apply f box)) expressionsWithFunctions
    filterOutFalseTerms     = filter (\(_, range) -> not (range !<! 0))  esWithRanges -- Could make this cleaner with list comprehension
    
    filteredExpressionsWithFunctions = map fst filterOutFalseTerms
    
    checkIfEsTrueUsingApply = any (\(_, range) -> range !>=! 0)  filterOutFalseTerms

    cornerRangesWithDerivatives = 
      map
      (\((_,f),_) ->
        (
          -- left corner range
          apply f boxL,
          -- right corner range
          apply f boxU,
          -- derivatives
          gradient f box
        )
      )
      filterOutFalseTerms
    

    bisectWidestDimensionAndRecurse varMapToBisect =
      let
        (leftVarMap, rightVarMap) = bisectVar varMapToBisect (fst (widestInterval (tail varMapToBisect) (head varMapToBisect)))
        (leftR, rightR) = 
          withStrategy 
          (parTuple2 rseq rseq) 
          (
            decideDisjunctionWithSimplex filteredExpressionsWithFunctions leftVarMap maxWidthCutoff relativeImprovementCutoff p, 
            decideDisjunctionWithSimplex filteredExpressionsWithFunctions rightVarMap maxWidthCutoff relativeImprovementCutoff p
          )
      in
        case leftR of
            (Just True, _) ->
              case rightR of
                (Just True, _) -> (Just True, Nothing)
                r -> r
            r -> r

    bisectAllDimensionsAndRecurse varMapToBisect =
      let
        bisectedVarMaps = fullBisect varMapToBisect
        
        checkBisection :: [VarMap] -> Bool -> Maybe VarMap -> (Maybe Bool, Maybe VarMap)
        checkBisection []         foundIndeterminate indeterminateVarMap = if foundIndeterminate 
          -- Recurse with 'breadth-first' mode
          then (Nothing, indeterminateVarMap) 
          else (Just True, Nothing)
        checkBisection (vm : vms) foundIndeterminate indeterminateVarMap = 
          case decideDisjunctionWithSimplex filteredExpressionsWithFunctions vm maxWidthCutoff relativeImprovementCutoff p of
            (Just True, _)                -> checkBisection vms foundIndeterminate indeterminateVarMap
            r@(Just False, _)             -> r 
            r@(Nothing, indeterminateArea)  -> -- Try depth first search until we reach cutoff.
              -- case decideDisjunctionWithBreadthFirst filteredExpressions vms [] maxWidthCutoff p of
              --   c@(Just _) -> (Just False, c)
              --   Nothing -> r

              -- r
              checkBisection vms True indeterminateArea
      in
        checkBisection bisectedVarMaps False Nothing

decideWithSimplex :: ([(CN MPBall, CN MPBall, Box)]) -> VarMap -> (Maybe Bool, Maybe VarMap)
decideWithSimplex cornerValuesWithDerivatives varMap =
  case encloseAreaUnderZeroWithSimplex cornerValuesWithDerivatives varMap of
    Just newVarMap -> (Nothing, Just newVarMap)
      -- if newVarMap == varMap 
        -- then trace (show newVarMap) (Nothing, Just newVarMap)
        -- else trace "recurse" $ decideWithSimplex expressions newVarMap p
    Nothing -> (Just True, Nothing)

-- |Create constraints for the given domains
--
-- Examples:
--   createDomainConstraints [("X", (1.0, 3.0))] 1
--     returns:
--       (
--         ([S.GEQ [(1, 1.0)] 1.0, S.LEQ [(1, 1.0)] 3.0]), []),
--         2
--       )
--   createDomainConstraints [("X", (1.0, 3.0)), ("Y", (0.0, 2.0))] 1
--     returns:
--       (
--         ([S.GEQ [(1, 1.0)] 1.0, S.LEQ [(1, 1.0)] 3.0], [S.GEQ [(2, 1.0)] 0.0, S.LEQ [(2, 1.0)] 2.0]), []),
--         3
--       )
--   createDomainConstraints [("X", (-1.0, 3.0))] 1
--     returns:
--       (
--         ([S.GEQ [(1, 1.0)] 0.0, S.LEQ [(1, 1.0)] 4.0], [(1, -1)]),
--         2
--       )
createDomainConstraints 
  :: VarMap -- ^ The domains to create constraints for
  -> Integer -- ^ The next available variable
  -> (([S.PolyConstraint], [(Integer, Rational)]), Integer) {- ^  The first item in the pair is the system of constraints created from the VarMap.
                                                                  The first item in the second item part of the pair stores the amount a variable
                                                                  has been shifted from the LHS.
                                                                  The second item in the second pair denotes the next integer variable available. -} 
createDomainConstraints [] nextAvailableVar = (([], []), nextAvailableVar)
createDomainConstraints ((_, (l, r)) : xs) currentIndex =
  let
    ((resultsL, resultsR), nextAvailableVar) = createDomainConstraints xs (currentIndex + 1)
  in
    if l < 0 then -- If the current domain is under zero, transform the domain by subtracting the left bound
                  -- This will make the left bound equal to zero and will make the right bound above zero
                  -- Store this transformation in resultsR
      (([S.GEQ [(currentIndex, rational 1)] (l - l), S.LEQ [(currentIndex, rational 1)] (r - l)] ++ resultsL, (currentIndex, l) : resultsR), nextAvailableVar)
    else
      (([S.GEQ [(currentIndex, rational 1)] l, S.LEQ [(currentIndex, rational 1)] r] ++ resultsL, resultsR), nextAvailableVar)

findFunctionCounterExample :: [(CN MPBall, CN MPBall, Box)] -> [Rational] -> [Rational] -> [(Integer, Rational)] -> [S.PolyConstraint]
findFunctionCounterExample [] _ _ _ = []
findFunctionCounterExample ((leftCornerValue, rightCornerValue, derivatives) : values) leftCorner rightCorner substVars =
  S.LEQ (zip [1..] upperDerivatives) (foldl add (-leftU - upperSubst - eps) upperDerivativesTimesLeftCorner)
    -- S.LEQ (zip [1..] negatedLowerDerivatives) (foldl add (rightU + lowerSubst) negatedLowerDerivativesTimesRightCorner)
    -- S.LEQ (zip [1..] lowerDerivatives) (foldl add (-rightU - lowerSubst - eps) lowerDerivativesTimesRightCorner)
  : findFunctionCounterExample values leftCorner rightCorner substVars
  where
    -- eps = 1/100000000000000000000000000000

    mpBallToRational :: CN MPBall -> (Rational, Rational)
    mpBallToRational = bimap rational rational . endpoints . reducePrecionIfInaccurate . unCN
    eps = 1/1000000000000

    -- mpBallToRational :: CN MPBall -> (Rational, Rational)
    -- mpBallToRational v = 
      -- let (l, r) = (endpointsAsIntervals . reducePrecionIfInaccurate . (~!)) v
      -- in ((rational . snd .  endpoints) l, (rational . fst . endpoints) r)
    -- mpBallToRational = bimap rational rational . endpoints . unCN
      -- bimap (endpoints . reducePrecionIfInaccurate)

    -- Get the lower and upper bounds of the function applied at the bottom left corner of the box
    -- FIXME: check that we are using correct bounds here
    {-
    We convert MPBall to rational
    conversion gives us accurate endpoints
    In the system, we are using the upperbound of the left corner this looks correct
    We use upperbounds for derivatives, this also looks correct
    -}

    (leftL, leftU) = mpBallToRational leftCornerValue
    -- leftU = (rational . snd . endpoints . snd . endpointsAsIntervals . (~!)) leftCornerValue
    (_, rightU) = mpBallToRational rightCornerValue
    
    -- Get the first derivatives as rationals
    firstDerivatives = V.map mpBallToRational derivatives 
    
    lowerDerivatives = V.toList $ V.map fst firstDerivatives
    upperDerivatives = V.toList $ V.map snd firstDerivatives
    -- upperDerivatives = V.toList $ V.map (rational . snd . endpoints . snd . endpointsAsIntervals . (~!)) derivatives

    negatedLowerDerivatives  = map negate lowerDerivatives
    negatedUpperDerivatives  = map negate upperDerivatives

    -- Get the values of multiplying the lower/upper bounds of the derivatives with the values 
    -- of the points at the bottom left corner of the box
    lowerDerivativesTimesLeftCorner = zipWith mul leftCorner lowerDerivatives
    upperDerivativesTimesLeftCorner = zipWith mul leftCorner upperDerivatives

    lowerDerivativesTimesRightCorner = zipWith mul rightCorner lowerDerivatives
    upperDerivativesTimesRightCorner = zipWith mul rightCorner upperDerivatives

    negatedLowerDerivativesTimesRightCorner = zipWith mul rightCorner negatedLowerDerivatives
    negatedUpperDerivativesTimesRightCorner = zipWith mul rightCorner negatedUpperDerivatives
    -- Map over the substVars, lookup the value of the first derivative for the current var
    -- being mapped over, multiply this value with the value the variable maps to in substVars
    -- (i.e. the value the variable was transformed from the left side in createDomainConstraints).
    -- Do this for both the lower and upper derivatives

    substValuesLower = 
      map (\(v, c) -> (lowerDerivatives !! (v - 1)) * c) substVars

    substValuesUpper =
      map (\(v, c) -> (upperDerivatives !! (v - 1)) * c) substVars

    -- Fold the above lists using addition
    lowerSubst = foldl add 0.0 substValuesLower
    upperSubst = foldl add 0.0 substValuesUpper
-- |Create constraints for the given parameters functions given as the first variable.
-- The second variable is the box for which we are creating constraints.
-- The third variable stores the integer variable that is available to be assigned.
-- The fourth variable stores a map of variables with the amount they have been transformed
-- from the left hand side
createFunctionConstraints 
  :: ([(CN MPBall, CN MPBall, Box)]) -- ^ Each item is the value of each function at the given corner along with the first derivatives for the function
  -> [Rational] -- ^ The corner for which we examine each function which leads to the values in the first parameter
                          -- Left or Right indicates whether or not this corner is the extreme left or extreme right corner of
                          -- the original box
  -> [Rational]
  -> Integer -- ^ The next integer variable available to be assigned
  -> [(Integer, Rational)] -- ^ The amount each variable needs to be shifted from the LHS
  -> [S.PolyConstraint] -- ^ Each item is a constraint for each function
createFunctionConstraints [] _ _ _ _ = []
createFunctionConstraints ((leftCornerValue, rightCornerValue, derivatives) : values) leftCorner rightCorner currentIndex substVars = 
  -- Here, we set the coefficient for the variable representing the current function to be 1
  -- We then append a list of the lower and upper bounds of the first derivatives for each
  -- respective constraint
  -- On the right hand side of each constraint, we add:
  --   the negation of the lower/upper bound of the function applied at the bottom left corner of the box.
  --   the values of the lower/upper derivatives multiplied by the values of the bottom left corner of the box.
  --   any transformations that need to take place as a result of shifting the constraints for the domain
  --     the above transformation only occurs when at least one domain is partly negative.
  [
    S.LEQ ((currentIndex, 1.0) : zip [1..] lowerDerivatives) (foldl add (-leftL - lowerSubst) lowerDerivativesTimesLeftCorner),
    -- S.GEQ ((currentIndex, 1.0) : zip [1..] upperDerivatives) (foldl add (-leftU - upperSubst) upperDerivativesTimesLeftCorner),
    -- FIXME: Swap order of subtraction for the right corner case, and then use the original order of constraints
    -- -y + (dx_1L * x_1) >= -yl + (dx_1L * x_1r)
    -- S.GEQ ((currentIndex, 1.0) : zip [1..] lowerDerivatives) (foldl add (-rightL - lowerSubst) lowerDerivativesTimesRightCorner),
    -- S.LEQ ((currentIndex, 1.0) : zip [1..] upperDerivatives) (foldl add (-rightU - upperSubst) upperDerivativesTimesRightCorner)
    -- S.LEQ ((currentIndex, 1.0) : zip [1..] (map (\v -> v * (-1)) lowerDerivatives)) (foldl add (-rightL + lowerSubst) lowerDerivativesTimesRightCorner),
    -- S.GEQ ((currentIndex, 1.0) : zip [1..] (map (\v -> v * (-1)) upperDerivatives)) (foldl add (-rightU + upperSubst) upperDerivativesTimesRightCorner)
    -- y + (dx_1L * x_1) >= yl + (dx_1L * x_1r)
    -- S.GEQ ((currentIndex, -1.0) : zip [1..] lowerDerivatives) (foldl add (rightL - lowerSubst) lowerDerivativesTimesRightCorner),
    -- S.LEQ ((currentIndex, -1.0) : zip [1..] upperDerivatives) (foldl add (rightU - upperSubst) upperDerivativesTimesRightCorner)
    -- y + (x_1 * (-dx_1R)) >= yl + (x_1r * (-dx_1R))
    S.GEQ ((currentIndex, -1.0) : zip [1..] negatedUpperDerivatives) (foldl add (rightL + upperSubst) negatedUpperDerivativesTimesRightCorner)
    -- S.LEQ ((currentIndex, -1.0) : zip [1..] negatedLowerDerivatives) (foldl add (rightU + lowerSubst) negatedLowerDerivativesTimesRightCorner)
    
  ]
  ++
  createFunctionConstraints values leftCorner rightCorner (currentIndex + 1) substVars
  
  where
    mpBallToRational :: CN MPBall -> (Rational, Rational)
    mpBallToRational = bimap rational rational . endpoints . reducePrecionIfInaccurate . unCN
      -- bimap (endpoints . reducePrecionIfInaccurate)

    -- Get the lower and upper bounds of the function applied at the bottom left corner of the box
    (leftL, leftU) = mpBallToRational leftCornerValue
    (rightL, rightU) = mpBallToRational rightCornerValue
    
    -- Get the first derivatives as rationals
    firstDerivatives = V.map mpBallToRational derivatives 
    
    lowerDerivatives = V.toList $ V.map fst firstDerivatives
    upperDerivatives = V.toList $ V.map snd firstDerivatives

    negatedLowerDerivatives  = map negate lowerDerivatives
    negatedUpperDerivatives  = map negate upperDerivatives

    -- Get the values of multiplying the lower/upper bounds of the derivatives with the values 
    -- of the points at the bottom left corner of the box
    lowerDerivativesTimesLeftCorner = zipWith mul leftCorner lowerDerivatives
    upperDerivativesTimesLeftCorner = zipWith mul leftCorner upperDerivatives

    lowerDerivativesTimesRightCorner = zipWith mul rightCorner lowerDerivatives
    upperDerivativesTimesRightCorner = zipWith mul rightCorner upperDerivatives

    negatedLowerDerivativesTimesRightCorner = zipWith mul rightCorner negatedLowerDerivatives
    negatedUpperDerivativesTimesRightCorner = zipWith mul rightCorner negatedUpperDerivatives

    -- Map over the substVars, lookup the value of the first derivative for the current var
    -- being mapped over, multiply this value with the value the variable maps to in substVars
    -- (i.e. the value the variable was transformed from the left side in createDomainConstraints).
    -- Do this for both the lower and upper derivatives
    substValuesLower = 
      map (\(v, c) -> (lowerDerivatives !! (v - 1)) * c) substVars

    substValuesUpper =
      map (\(v, c) -> (upperDerivatives !! (v - 1)) * c) substVars

    -- Fold the above lists using addition
    lowerSubst = foldl add 0.0 substValuesLower
    upperSubst = foldl add 0.0 substValuesUpper

-- | Enclose the area under zero where the given values of functions along with each functions
-- first derivatives enclose the area under zero which intersects with the given varMap.
encloseAreaUnderZeroWithSimplex 
  :: ([(CN MPBall, CN MPBall, Box)]) -- ^ [(valueOfFunctionAtLeftCorner, valueOfFunctionAtRightCorner, derivativesOfFunction)]
  -> VarMap -- ^ The domains for each function which leads to the first derivatives
  -> Maybe VarMap {- ^ This function returns:
                    Nothing when there is no area under zero to enclose (so the system is above zero). FIXME: Is it better to return an empty list here?
                    `Just [("variable1", (newLowerBound, newUpperBound)), ...]` when the created system
                      is feasible. The new VarMap may be the same as the old VarMap, which means
                      that the simplex was not able to shrink the original VarMap. -}
encloseAreaUnderZeroWithSimplex cornerValuesWithDerivatives varMap = 
  -- T.trace (show completeSystem) $ 
  --trace (show substVars) $
  -- If the first result from the list returned by the simplex method is empty,
  -- the system is infeasible, so we return nothing
  case head newPoints of
    (_, (Nothing, _)) ->
      Nothing
    _ -> 
      let
        -- Should never get an undefined result if the first resuls in newPoints is feasible
        extractResult :: Maybe (Integer, [(Integer, Rational)]) -> Rational
        extractResult mr =
          case mr of
            Just (v, rs) -> -- v refers to the objective variable. We extract the value of the objective
                            -- variable from rs (the result determined by the simplex method)
              case lookup v rs of
                Just r -> r
                Nothing -> undefined 
            Nothing -> undefined 
      in
        Just $
        map
        (\(v, r) ->
          --trace (show r)
          (v, 
          case lookup v indexedVariables of
            Just iv -> -- Get the integer variable for the current string variable 
              case lookup iv substVars of -- Check if any transformation needs to be done to get the final result
                Just c -> (bimap ((add c) . extractResult) ((add c) . extractResult) r) -- Add any needed transformation to lower/upper bounds
                Nothing -> bimap extractResult extractResult r
            Nothing -> undefined -- Should never get here
          )  
        )
        newPoints
  where
    -- Get the bottom left corner of the varMap

    -- boxFuns = map (\e -> expressionToBoxFun e corner p) expressions

    -- Create constraints for the domain
    -- substVars stores any variable transformations (for the LHS)
    ((domainConstraints, substVars), nextAvailableVar) = createDomainConstraints varMap 1

    numberOfFunctions = length cornerValuesWithDerivatives

    variables = [1 .. (nextAvailableVar - 1)]

    -- Set the variables that will be used to refer to each function
    functions = [nextAvailableVar .. nextAvailableVar + numberOfFunctions - 1]

    leftCorner = map (fst . snd) varMap
    rightCorner = map (snd . snd) varMap

    functionConstraints = createFunctionConstraints cornerValuesWithDerivatives leftCorner rightCorner nextAvailableVar substVars

    -- Map integer variables to their respective varMap
    indexedVarMap = zip variables varMap

    -- Map string variables to their respective integer variable
    indexedVariables = zip (map fst varMap) variables

    -- Create constraints to bound the area under zero for each function
    -- LEQ [(functionVariable, 1.0)] 0.0 would not work because the simplex method assumes >= 0
    -- for each variable, so we use GEQ [(functionVariable, 1.0)] 0.0 instead, and make the
    -- appropriate changes in createFunctionConstraints to effectively contain the area
    -- under zero for each function.
    underZeroConstraints =
      map
      (\v -> S.GEQ [(v, 1.0)] 0.0)
      functions

    completeSystem = domainConstraints ++ functionConstraints ++ underZeroConstraints

    -- Call the simplex method twice for each variable (setting the objective function to Min/Max of each
    -- variable). Map each (String) variable to a pair. The pair is the results determined by the simplex
    -- method when Min/Maxing the key variable. 
    newPoints :: [(String, (Maybe (Integer, [(Integer, Rational)]), Maybe (Integer, [(Integer, Rational)])))]
    newPoints =
      map
      (\v -> 
        case lookup v indexedVarMap of
          Just (sv, _) -> (sv, ((S.twoPhaseSimplex (S.Min [(v, 1.0)]) completeSystem), (S.twoPhaseSimplex (S.Max [(v, 1.0)]) completeSystem)))
          Nothing -> undefined
      )
      variables

findFalsePointWithSimplex  
  :: [(CN MPBall, CN MPBall, Box)] -- ^ [(valueOfFunctionAtLeftCorner, valueOfFunctionAtRightCorner, derivativesOfFunction)]
  -> VarMap -- ^ The domains for each function which leads to the first derivatives
  -> Maybe VarMap
findFalsePointWithSimplex cornerValuesWithDerivatives varMap =
  -- T.trace (show completeSystem) $
  case mNewPoints of
    Just newPoints ->
      -- T.trace "========================"
      -- T.trace (Data.List.intercalate "\n "(map S.prettyShowPolyConstraint completeSystem))
      -- T.trace "========================"
      Just $
      map 
      (\(s, v) ->
        case lookup v newPoints of
          Just r -> let c = fromMaybe 0.0 (lookup v substVars) in (s, (r + c, r + c))
          Nothing -> let c = fromMaybe 0.0 (lookup v substVars) in (s, (c, c)) 
      )
      indexedVariables
    Nothing -> Nothing
  where
    -- Get the bottom left corner of the varMap

    -- boxFuns = map (\e -> expressionToBoxFun e corner p) expressions

    -- Create constraints for the domain
    -- substVars stores any variable transformations (for the LHS)
    ((domainConstraints, substVars), nextAvailableVar) = createDomainConstraints varMap 1

    variables = [1 .. (nextAvailableVar - 1)]

    leftCorner = map (fst . snd) varMap
    rightCorner = map (snd . snd) varMap

    functionConstraints = findFunctionCounterExample cornerValuesWithDerivatives leftCorner rightCorner substVars

    -- Map integer variables to their respective varMap
    indexedVarMap = zip variables varMap

    -- Map string variables to their respective integer variable
    indexedVariables = zip (map fst varMap) variables

    completeSystem = domainConstraints ++ functionConstraints

    -- Get a feasible solution for this system
    mNewPoints :: Maybe [(Integer, Rational)]
    mNewPoints = S.findFeasibleSolution completeSystem

checkECNFVerify :: [[E.E]] -> VarMap -> Integer -> Integer -> Rational -> Precision -> Bool
checkECNFVerify cnf varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff p =
  case resultBox of
    Just box ->
      -- T.trace (show box) $
      -- T.trace (show resultBool) $
      -- T.trace (show (applyECNF cnf box p)) $
      conjunctionRangesBelowZero $ applyECNF cnf box p
    Nothing -> undefined
  where
    (resultBool, resultBox) = checkECNFCE cnf varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff p