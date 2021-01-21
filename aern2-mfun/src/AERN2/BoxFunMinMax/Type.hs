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
import Data.Bifunctor
import qualified Simplex as S

import Debug.Trace (trace)

import Data.List (filter, find)
import qualified Data.Sequence as Seq

import Control.CollectErrors (getValueIfNoErrorCE)
-- trace a x = x

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
      (_, mAreaContainingMinimum) ->
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

checkECNFSimplex :: [[E.E]] -> VarMap -> Precision -> (Maybe Bool, Maybe VarMap)
checkECNFSimplex [] _ _ = (Just True, Nothing)
checkECNFSimplex (disjunction : disjunctions) varMap p =
  case decideDisjunctionWithSimplex disjunction varMap p of
    (Just True, _) -> checkECNFSimplex disjunctions varMap p
    r@(_, _)              -> r
    -- (Nothing, Just indeterminateArea) -> checkUsingGlobalMinimum disjunction Nothing
    --   where
    --     checkUsingGlobalMinimum []       mFalseArea = 
    --       case mFalseArea of
    --         Nothing         -> (Nothing, Just indeterminateArea)
    --         Just falseArea  -> (Just False, Just (fromBox (extents falseArea) (map fst varMap)))

    --     checkUsingGlobalMinimum (e : es) mFalseArea = 
    --       case globalMinimumAboveN2 (expressionToBoxFun e indeterminateArea p) (bits 100) p (cn (mpBallP p 0)) of
    --         (Just True, _)                -> checkECNFSimplex disjunctions varMap p
    --         (Just False, Just falseArea)  -> checkUsingGlobalMinimum es (Just falseArea) -- TODO: Improve this by comparing current and new false areas. Keep the area with the smallest width overall
    --         (Nothing, _)                  -> checkUsingGlobalMinimum es mFalseArea
    --         (Just False, _)               -> error "False result from globalMinimumAboveN2 did not return a SearchBox"
    -- (Nothing, _) -> error "Indeterminate result from decideDisjunctionWithSimplex did not return indeterminate area"

decideDisjunctionWithSimplex :: [E.E] -> VarMap -> Precision -> (Maybe Bool, Maybe VarMap)
decideDisjunctionWithSimplex expressions varMap p = 
  -- trace (show varMap) $
  -- unsafePerformIO $ do
  --   appendFile "/home/junaid/Research/git/aern2-base/aern2/boxes/3s.txt" (show varMap ++ "\n")
  --   return $ 
      if null filteredExpressions
        then (Just False, Just varMap)
        else 
          if checkIfEsTrueUsingApply 
            then (Just True, Nothing)
            else 
              -- Only call decideWithSimplex if all derivatives can be calculated
              if and (concatMap V.toList (map (V.map (\value -> getValueIfNoErrorCE value (const True) (const False))) esDerivativesOverVarMap))
                then 
                  -- trace "simplex" $
                  case decideWithSimplex esRangesAtCorner esDerivativesOverVarMap varMap corner of
                    r@(Just True, _) -> r
                    (Nothing, Just newVarMap) ->
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
                        boxChangeWidth = abs(width varMap - width newVarMap) -- FIXME: Add support to VarMap to do this within VarMap? Will reduce unnecessary conversion
                      in
                      if (boxChangeWidth !>=! cn 0.01) -- FIXME: MPBall/Rational parameter
                        then
                          -- trace "--------------------"
                          -- trace (show varMap)
                          -- trace (show newVarMap)
                          -- trace "--------------------"
                          decideDisjunctionWithSimplex filteredExpressions newVarMap p
                        else
                          bisectAllDimensionsAndRecurse newVarMap
                    _ -> undefined
                  else
                    bisectAllDimensionsAndRecurse varMap
      
  where
    applyE vm e = applyLipschitz f (setPrecision p (domain f))
      where
        f = expressionToBoxFun e vm p
    esWithRanges            = zip expressions (parMap rseq (applyE varMap) expressions)
    filterOutFalseTerms     = filter (\(_, range) -> not (range !<! 0))  esWithRanges
    filteredExpressions     = map fst filterOutFalseTerms
    checkIfEsTrueUsingApply = any (\(_, range) -> range !>=! 0)  filterOutFalseTerms
    
    corner                      = map (\(v, (l, _)) -> (v, (l, l))) varMap
    esRangesAtCorner            = parMap rseq (applyE corner) filteredExpressions
    esDerivativesOverVarMap     = parMap rseq (\e -> gradientUsingGradient (expressionToBoxFun e varMap p) (fromVarMap varMap p)) filteredExpressions
    -- filteredEsAboveZeroAtCorner = map fst $ filter (\(_, range) -> range !>! 0) filteredEsWithRangesAtCorner 

    bisectAllDimensionsAndRecurse varMapToBisect =
      let
        bisectedVarMaps = fullBisect varMapToBisect
        
        checkBisection :: [VarMap] -> (Maybe Bool, Maybe VarMap)
        checkBisection []         = (Just True, Nothing)
        checkBisection (vm : vms) = 
          case decideDisjunctionWithSimplex filteredExpressions vm p of
            (Just True, _) -> checkBisection vms
            (_, indeterminateArea) -> (Nothing, indeterminateArea)
      in
        if width varMapToBisect !>! 1 / 10000000
          then 
            --trace "bisecting" 
            checkBisection bisectedVarMaps
          else 
            --trace "stopping bisections" 
            (Nothing, Just varMap)

decideWithSimplex :: [CN MPBall] -> [Box] -> VarMap -> VarMap -> (Maybe Bool, Maybe VarMap)
decideWithSimplex valuesAtCorner derivativesOverVarMap varMap corner =
  case encloseAreaUnderZeroWithSimplex valuesAtCorner derivativesOverVarMap varMap corner of
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

-- |Create constraints for the given parameters functions given as the first variable.
-- The second variable is the box for which we are creating constraints.
-- The third variable stores the integer variable that is available to be assigned.
-- The fourth variable stores a map of variables with the amount they have been transformed
-- from the left hand side
createFunctionConstraints 
  :: [CN MPBall] -- ^ Each item is the value of a function examined at the given corner
  -> [Box] -- ^ The first derivatives of each function over the entire varMap
  -> VarMap -- ^ The corner for which we examine each function which leads to the values in the first parameter
  -> Integer -- ^ The next integer variable available to be assigned
  -> [(Integer, Rational)] -- ^ The amount each variable needs to be shifted from the LHS
  -> [S.PolyConstraint] -- ^ Each item is a constraint for each function
createFunctionConstraints [] derivatives _ _ _ = if null derivatives then [] else error "derivatives not null"
createFunctionConstraints cornerValues [] _ _ _ = if null cornerValues then [] else error "cornerValues not null"
createFunctionConstraints (cornerValue : cornerValues) (currentDerivatives : derivatives) corner currentIndex substVars = 
  (
    [
      -- Here, we set the coefficient for the variable representing the current function to be 1
      -- We then append a list of the lower and upper bounds of the first derivatives for each
      -- respective constraint
      -- On the right hand side of each constraint, we add:
      --   the negation of the lower/upper bound of the function applied at the bottom left corner of the box.
      --   the values of the lower/upper derivatives multiplied by the values of the bottom left corner of the box.
      --   any transformations that need to take place as a result of shifting the constraints for the domain
      --     the above transformation only occurs when at least one domain is partly negative.
      S.LEQ ((currentIndex, 1.0) : zip [1..] lowerDerivatives) (foldl add (-fl - lowerSubst) lowerDerivativesAtCorner),
      S.GEQ ((currentIndex, 1.0) : zip [1..] upperDerivatives) (foldl add (-fr - upperSubst) upperDerivativesAtCorner)
    ]
    ++
    createFunctionConstraints cornerValues derivatives corner (currentIndex + 1) substVars
  )
  where
    -- Get the lower and upper bounds of the function applied at the bottom left corner of the box
    (fl, fr) = bimap (rational . (~!)) (rational . (~!)) $ endpoints cornerValue
    
    -- Get the first derivatives as rationals
    firstDerivatives = V.map (bimap (rational . (~!)) (rational . (~!)) . endpoints) currentDerivatives
    
    lowerDerivatives = V.toList $ V.map fst firstDerivatives
    upperDerivatives = V.toList $ V.map snd firstDerivatives

    -- Get the rational values of the corner
    rationalCornerValue = map (\(_, (v, _)) -> (rational . (~!)) v) corner

    -- Get the values of multiplying the lower/upper bounds of the derivatives with the values 
    -- of the points at the bottom left corner of the box
    lowerDerivativesAtCorner = zipWith mul rationalCornerValue lowerDerivatives
    upperDerivativesAtCorner = zipWith mul rationalCornerValue upperDerivatives

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
  :: [CN MPBall] -- ^ The value of each function at the given corner
  -> [Box] -- ^ The first derivatives for each function over the given varMap
  -> VarMap -- ^ The domains for each function which leads to the first derivatives
  -> VarMap -- ^ The corner at which we get the value for each function for the first parameter
  -> Maybe VarMap {- ^ This function returns:
                    Nothing when there is no area under zero to enclose (so the system is above zero). FIXME: Is it better to return an empty list here?
                    Just [("variable1", (newLowerBound, newUpperBound)), ...] when the created system
                      is feasible. The new VarMap may be the same as the old VarMap, which means
                      that the simplex was not able to shrink the original VarMap.
                  -}
encloseAreaUnderZeroWithSimplex cornerValues derivativesOverVarMap varMap corner = 
  --trace (show completeSystem) $
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

    numberOfFunctions = length cornerValues

    variables = [1 .. (nextAvailableVar - 1)]

    -- Set the variables that will be used to refer to each function
    functions = [nextAvailableVar .. nextAvailableVar + numberOfFunctions - 1]

    functionConstraints = createFunctionConstraints cornerValues derivativesOverVarMap corner nextAvailableVar substVars

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
