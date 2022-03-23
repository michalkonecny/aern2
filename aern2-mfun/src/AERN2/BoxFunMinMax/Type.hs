{-# LANGUAGE LambdaCase #-}

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
import AERN2.BoxFun.Box (createEnclosingBox, Box, intersectionCertainlyEmpty, nonEmptyIntersection, lowerBounds, upperBounds)
import qualified AERN2.Linear.Vector.Type as V
import AERN2.Kleenean
import Control.Parallel.Strategies
import Data.Bifunctor
import qualified Simplex as S

import Data.List (find, intercalate, nub)
import qualified Data.Sequence as Seq

import AERN2.MP.Dyadic (Dyadic, dyadic)

-- import qualified Debug.Trace as T

import qualified Data.PQueue.Prio.Min as Q

import Data.Maybe
import Control.CollectErrors
import TcHoleErrors (TypedHole)
import GHC.Arr (Ix(range))
import System.Log.FastLogger
import System.Log.FastLogger.Date (simpleTimeFormat)
import qualified Data.Map as M
import qualified Prelude as P
trace a x = x


log :: TimedFastLogger -> LogStr -> IO ()
log logger msg = logger (\time -> toLogStr (show time) <> toLogStr " " <> msg <> toLogStr "\n")

log1 msg =
  do
    clock <- newTimeCache simpleTimeFormat -- Do YYYY-MM-DD HH:MM:SS: time
    (logger, cleanup) <- newTimedFastLogger clock (LogStdout defaultBufSize)
    logger (\time -> toLogStr time <> toLogStr " " <> msg <> toLogStr "\n")
    cleanup

applyExpression :: E.E -> VarMap -> Precision -> CN MPBall
applyExpression expression varMap p =
  apply f (varMapToBox varMap p)
  where
    f = expressionToBoxFun expression varMap p

applyExpressionLipschitz :: E.E -> VarMap -> Precision -> CN MPBall
applyExpressionLipschitz expression varMap p =
  applyLipschitz f (varMapToBox varMap p)
  where
    f = expressionToBoxFun expression varMap p

applyDisjunction :: [E.E] -> VarMap -> Precision -> [CN MPBall]
applyDisjunction expressions varMap p =
  map
  (\e -> apply (expressionToBoxFun e varMap p) box)
  expressions
  where
    box = varMapToBox varMap p

applyECNF :: [[E.E]] -> VarMap -> Precision -> [[CN MPBall]]
applyECNF cnf varMap p = map (\d -> applyDisjunction d varMap p) cnf

applyESafeCNF :: [[E.ESafe]] -> VarMap -> Precision -> [[CN MPBall]]
applyESafeCNF cnf varMap p = applyECNF (map (map (E.extractSafeE)) cnf) varMap p


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
    E.Impl  -> not f1Val || f2Val
  where
    f1Val = checkFWithApply f1 varMap p
    f2Val = checkFWithApply f2 varMap p
checkFWithApply (E.FNot f) varMap p = not $ checkFWithApply f varMap p
checkFWithApply E.FTrue _ _         = cn CertainTrue
checkFWithApply E.FFalse _ _        = cn CertainFalse

filterOutFalseExpressions :: [((E.ESafe, BoxFun), CN MPBall)] -> [((E.ESafe, BoxFun), CN MPBall)]
filterOutFalseExpressions =
  filter
  (\((safeE, _), range) ->
    case safeE of
      E.EStrict _ ->    hasError range || not (range !<=! 0) -- We cannot decide on ranges with errors, so do not filter them out
      E.ENonStrict _ -> hasError range || not (range !<! 0)
  )

decideRangesGEZero :: [((E.E, BoxFun), CN MPBall)] -> Bool
decideRangesGEZero = any (\(_, range) -> not (hasError range) && range !>=! 0)

decideDisjunctionRangesTrue :: [((E.ESafe, BoxFun), CN MPBall)] -> Bool
decideDisjunctionRangesTrue =
  any
  (\((safeE, _), range) ->
    case safeE of
      E.EStrict _     -> not (hasError range) && range !>! 0
      E.ENonStrict _  -> not (hasError range) && range !>=! 0
  )

decideDisjunctionRangesFalse :: [((E.ESafe, BoxFun), CN MPBall)] -> Bool
decideDisjunctionRangesFalse =
  all
  (\((safeE, _), range) ->
    case safeE of
      E.EStrict _     -> not (hasError range) && not (hasError range) && range !<=! 0
      E.ENonStrict _  -> not (hasError range) && range !<! 0
  )

decideDisjunctionFalse :: [(E.ESafe, BoxFun)] -> TypedVarMap -> Precision -> Bool
decideDisjunctionFalse expressionsWithFunctions varMap p =
  all
  (\(safeE, f) ->
    let
      range = apply f (typedVarMapToBox varMap p)
    in
      case safeE of
        E.EStrict _    -> not (hasError range) && range !<=! 0
        E.ENonStrict _ -> not (hasError range) && range !<! 0
  )
  expressionsWithFunctions

decideConjunctionFalse :: [[(E.ESafe, BoxFun)]] -> TypedVarMap -> Precision -> Bool
decideConjunctionFalse c v p = any (\d -> decideDisjunctionFalse d v p) c

decideESafeCNFFalse :: [[E.ESafe]] -> TypedVarMap -> Precision -> Bool
decideESafeCNFFalse c v p = decideConjunctionFalse (map (map (\t -> (t, expressionToBoxFun (E.extractSafeE t) (typedVarMapToVarMap v) p))) c) v p

bisectWidestInterval :: VarMap -> (VarMap, VarMap)
bisectWidestInterval [] = error "Given empty box to bisect"
bisectWidestInterval vm = bisectVar vm widestVar
  where
    (widestVar, _) = widestInterval (tail vm) (head vm)

bisectWidestTypedInterval :: TypedVarMap -> (TypedVarMap, TypedVarMap)
bisectWidestTypedInterval [] = error "Given empty box to bisect"
bisectWidestTypedInterval vm = bisectTypedVar vm widestVar
  where
    (widestVar, _) = widestTypedInterval (tail vm) $ typedVarIntervalToVarInterval (head vm)

-- |Ensures that the first varMap is within the second varMap
-- If it is, returns the first varMap.
-- If it isn't modifies the varMap so that the returned varMap is within the second varMap
-- Both varmaps must have the same number of vars in the same order (order of vars not checked)
ensureVarMapWithinVarMap :: VarMap -> VarMap -> VarMap
ensureVarMapWithinVarMap [] [] = []
ensureVarMapWithinVarMap ((v, (roundedL, roundedR)) : rvm) ((_, (originalL, originalR)) : ovm) =
  (v, (if roundedL < originalL then originalL else roundedL, if roundedR > originalR then originalR else roundedR))
  : ensureVarMapWithinVarMap rvm ovm
ensureVarMapWithinVarMap _ _ = error "Different sized varMaps"

setupBestFirstCheck :: [(E.ESafe, BoxFun)] -> TypedVarMap -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
setupBestFirstCheck expressionsWithFunctions typedVarMap bfsBoxesCutoff relativeImprovementCutoff p =
  decideDisjunctionBestFirst
    -- (Q.singleton (maximum (map (\(_, f) -> (snd . endpointsAsIntervals) (apply f (typedVarMapToBox typedVarMap p))) expressionsWithFunctions)) typedVarMap)
    (Q.singleton (maximum (map (\(_, f) -> (fst . endpointsAsIntervals) (apply f (typedVarMapToBox typedVarMap p))) expressionsWithFunctions)) typedVarMap)
    -- (Q.singleton (maximum (map (\(_, f) -> AERN2.MP.Ball.centre (apply f (typedVarMapToBox typedVarMap p))) expressionsWithFunctions)) typedVarMap)
    expressionsWithFunctions
    0
    bfsBoxesCutoff
    relativeImprovementCutoff
    p

computeCornerValuesAndDerivatives :: [((E.ESafe, BoxFun), CN MPBall)] -> Box -> [(CN MPBall, CN MPBall, V.Vector (CN MPBall))]
computeCornerValuesAndDerivatives esWithRanges box = filteredCornerRangesWithDerivatives
  where
    boxL = lowerBounds box
    boxU = upperBounds box

    -- filteredCornerRangesWithDerivatives = 
    --   [
    --     value |
    --     value <- parMap rseq (\((_, f), _) -> (apply f boxL, apply f boxU, gradient f box)) esWithRanges,
    --     not (hasError value)
    --   ]

    cornerRangesWithDerivatives =
      parMap rseq
      (\ ((_, f), _) -> (apply f boxL, apply f boxU, gradient f box))
      esWithRanges

    -- Keep the functions where we can calculate all derivatives
    filteredCornerRangesWithDerivatives =
      filter
      (\(l, r, c) -> not (hasError l || hasError r || V.any hasError c)) -- Filter out functions where any partial derivative or corners contain an error
      cornerRangesWithDerivatives

checkConjunctionResults :: [(Maybe Bool, Maybe a)] -> Maybe a -> (Maybe Bool, Maybe a)
checkConjunctionResults [] Nothing = (Just True, Nothing)
checkConjunctionResults [] indeterminateArea@(Just _) = (Nothing, indeterminateArea)
checkConjunctionResults (result : results) mIndeterminateArea =
  case result of
    (Just True, _) -> checkConjunctionResults results mIndeterminateArea
    r@(Just False, _) -> r
    (Nothing, indeterminateArea@(Just _)) -> checkConjunctionResults results indeterminateArea
    (Nothing, Nothing) -> undefined

checkConjunctionResults2 :: [(Maybe Bool, Maybe a, Maybe b)] -> Maybe a -> Maybe b-> (Maybe Bool, Maybe a, Maybe b)
checkConjunctionResults2 [] Nothing _ = (Just True, Nothing, Nothing)
checkConjunctionResults2 [] indeterminateArea@(Just _) indeterminateDisjunction = (Nothing, indeterminateArea, indeterminateDisjunction)
checkConjunctionResults2 (result : results) mIndeterminateArea mIndeterminateDisjunction =
  case result of
    (Just True, _, _) -> checkConjunctionResults2 results mIndeterminateArea mIndeterminateDisjunction
    r@(Just False, _, _) -> r
    (Nothing, indeterminateArea@(Just _), indeterminateDisjunction) -> checkConjunctionResults2 results indeterminateArea indeterminateDisjunction
    (Nothing, Nothing, _) -> undefined


-- checkConjunctionResults :: [(Maybe Bool, Maybe VarMap)] -> (Maybe Bool, Maybe VarMap)
-- checkConjunctionResults [] = (Just True, Nothing)
-- checkConjunctionResults (result : results) =
--   case result of
--     (Just True, _) -> T.trace "proved" checkConjunctionResults results 
--     r@(Just False, _) -> r
--     r@(Nothing, _) -> r

checkECNFDepthFirstWithApply :: [[E.ESafe]] -> TypedVarMap -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
checkECNFDepthFirstWithApply disjunctions typedVarMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff p =
  checkConjunctionResults disjunctionResults Nothing
  where
    varMap = typedVarMapToVarMap typedVarMap
    disjunctionResults = parMap rseq (\disjunction ->  decideDisjunctionDepthFirstWithApply (map (\e -> (e, expressionToBoxFun (E.extractSafeE e) varMap p)) disjunction) typedVarMap 0 depthCutoff bfsBoxesCutoff relativeImprovementCutoff p) disjunctions

checkECNFDepthFirstWithSimplex :: [[E.ESafe]] -> TypedVarMap -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
checkECNFDepthFirstWithSimplex disjunctions typedVarMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff p =
  checkConjunctionResults disjunctionResults Nothing
  where
    varMap = typedVarMapToVarMap typedVarMap
    disjunctionResults = parMap rseq (\disjunction ->  decideDisjunctionDepthFirstWithSimplex (map (\e -> (e, expressionToBoxFun (E.extractSafeE e) varMap p)) disjunction) typedVarMap 0 depthCutoff bfsBoxesCutoff relativeImprovementCutoff p) disjunctions

checkECNFBestFirstWithSimplexCE :: [[E.ESafe]] -> TypedVarMap -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
checkECNFBestFirstWithSimplexCE disjunctions typedVarMap bfsBoxesCutoff relativeImprovementCutoff p =
  checkConjunctionResults disjunctionResults Nothing
  where --FIXME, best first...
    varMap = typedVarMapToVarMap typedVarMap
    disjunctionResults = parMap rseq (\disjunction -> setupBestFirstCheck (map (\e -> (e, expressionToBoxFun (E.extractSafeE e) varMap p)) disjunction) typedVarMap bfsBoxesCutoff relativeImprovementCutoff p) disjunctions

mean :: [CN Dyadic] -> CN Rational
mean xs = sum xs / length xs

-- |Avoids exceptions which occur when using the MixedTypesNumPrelude.maximum by ignoring
-- any numbers with errors
safeMaximum :: (HasOrderAsymmetric a a, CanTestCertainly (OrderCompareType a a), CanTestErrorsPresent a) =>
  a -> [a] -> a
safeMaximum currentMax [] = currentMax
safeMaximum currentMax (x : xs) =
  if hasError x
    then safeMaximum currentMax xs
    else safeMaximum (if x !>! currentMax then x else currentMax) xs

safeCentre :: (CanTestErrorsPresent t, IsBall t, CentreType t ~ CollectErrors CN.NumErrors Dyadic)
  => t -> CollectErrors CN.NumErrors Dyadic
safeCentre r = if hasError r then cn (dyadic (-1048576)) else AERN2.MP.Ball.centre r

safeMaximumCentre :: [BoxFun] -> Box -> Maybe (CN Dyadic) -> Maybe (CN Dyadic)
safeMaximumCentre []       _   mCurrentCentre = mCurrentCentre
safeMaximumCentre (f : fs) box mCurrentCentre =
  if hasError range
    then safeMaximumCentre fs box mCurrentCentre
    else
      case mCurrentCentre of
        Just currentMax ->
          if currentMax !>=! rangeCentre
            then safeMaximumCentre fs box mCurrentCentre
            else safeMaximumCentre fs box (Just rangeCentre)
        Nothing -> safeMaximumCentre fs box (Just rangeCentre)
  where
    range = apply f box
    rangeCentre = AERN2.MP.Ball.centre range

safeMaximumMinimum :: [BoxFun] -> Box -> Maybe (CN MPBall) -> Maybe (CN MPBall)
safeMaximumMinimum []       _   mCurrentMin = mCurrentMin
safeMaximumMinimum (f : fs) box mCurrentMin =
  if hasError range
    then safeMaximumMinimum fs box mCurrentMin
    else
      case mCurrentMin of
        Just currentMin ->
          if currentMin !>=! rangeMin
            then safeMaximumMinimum fs box mCurrentMin
            else safeMaximumMinimum fs box (Just rangeMin)
        Nothing -> safeMaximumMinimum fs box (Just rangeMin)
  where
    range = apply f box
    rangeMin = fst $ endpointsAsIntervals range

safeMaximumMaximum :: [BoxFun] -> Box -> Maybe (CN MPBall) -> Maybe (CN MPBall)
safeMaximumMaximum []       _   mCurrentMax = mCurrentMax
safeMaximumMaximum (f : fs) box mCurrentMax =
  if hasError range
    then safeMaximumMaximum fs box mCurrentMax
    else
      case mCurrentMax of
        Just currentMax ->
          if currentMax !>=! rangeMax
            then safeMaximumMinimum fs box mCurrentMax
            else safeMaximumMinimum fs box (Just rangeMax)
        Nothing -> safeMaximumMinimum fs box (Just rangeMax)
  where
    range = apply f box
    rangeMax = snd $ endpointsAsIntervals range

-- TODO: Make this more efficient
-- Return filteredFunctions from decideDisjunctionWithSimplexCE
-- Add these filtered functions to the queue
decideDisjunctionBestFirst :: Q.MinPQueue (CN MPBall) TypedVarMap -> [(E.ESafe, BoxFun)] -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
decideDisjunctionBestFirst queue expressionsWithFunctions numberOfBoxesExamined numberOfBoxesCutoff relativeImprovementCutoff p =
  case Q.minView queue of
    Just (typedVarMap, queueWithoutVarMap) ->
      if numberOfBoxesExamined !<! numberOfBoxesCutoff then
        trace (show numberOfBoxesExamined) $
        case decideDisjunctionWithSimplexCE expressionsWithFunctions typedVarMap relativeImprovementCutoff p of
          (Just True, _) -> decideDisjunctionBestFirst queueWithoutVarMap expressionsWithFunctions (numberOfBoxesExamined + 1) numberOfBoxesCutoff relativeImprovementCutoff p
          r@(Just False, _) -> r
          (Nothing, Just indeterminateVarMap) -> trace "h" $
            let
              -- esWithRanges = parMap rseq (\ (e, f) -> ((e, f), apply f (varMapToBox indeterminateVarMap p))) expressionsWithFunctions
              -- filteredExpressionsWithFunctions = map fst $ filter (\ (_, range) -> not (range !<! 0)) esWithRanges
              -- filteredExpressionsWithFunctions = -- Can't recurse with this. Maybe add this to the queue? Each box would have expressions to check. Inefficient?
              --   [
              --     notFalseTerms | 
              --     notFalseTerms@(_, f) <- expressionsWithFunctions,
              --     apply f (varMapToBox indeterminateVarMap p) !<! 0
              --   ]
              functions = map snd expressionsWithFunctions

              (leftVarMap, rightVarMap) = trace "bisecting" bisectTypedVar indeterminateVarMap (fst (widestTypedInterval (tail typedVarMap) ((\(TypedVar iv _) -> iv) (head typedVarMap))))
              -- [leftVarMapWithMean, rightVarMapWithMean] = 
              --   [
              --     [(apply f (varMapToBox leftVarMap p), leftVarMap), (apply f (varMapToBox rightVarMap p), rightVarMap)]
              --     |
              --     (e, f) <- filteredExpressionsWithFunctions,
              --     let tmp = apply f (varMapToBox leftVarMap p)
              --   ]
              leftVarMapWithMean  = trace (show (map fst expressionsWithFunctions)) $ trace "left"
                (
                  fromMaybe (cn (mpBallP p 1000000000000)) (safeMaximumMinimum functions (typedVarMapToBox leftVarMap p) Nothing),
                  -- fromMaybe (cn (mpBallP p 100000000000)) (safeMaximumMaximum functions (typedVarMapToBox leftVarMap p) Nothing),
                  -- fromMaybe (cn (dyadic 1048576)) (safeMaximumCentre functions (typedVarMapToBox leftVarMap p) Nothing),
                  leftVarMap
                )
              rightVarMapWithMean = trace "right"
                (
                  fromMaybe (cn (mpBallP p 1000000000000)) (safeMaximumMinimum functions (typedVarMapToBox rightVarMap p) Nothing),
                  -- fromMaybe (cn (mpBallP p 100000000000)) (safeMaximumMaximum functions (typedVarMapToBox rightVarMap p) Nothing),
                  -- fromMaybe (cn (dyadic 1048576)) (safeMaximumCentre functions (typedVarMapToBox rightVarMap p) Nothing),
                  rightVarMap
                )
            in
              decideDisjunctionBestFirst
              (uncurry Q.insert rightVarMapWithMean (uncurry Q.insert leftVarMapWithMean queueWithoutVarMap))
              expressionsWithFunctions (numberOfBoxesExamined + 1) numberOfBoxesCutoff relativeImprovementCutoff p
          (Nothing, Nothing) -> error "Got nothing with indeterminate typedVarMap in decideDisjunctionBestFirst"
      else (Nothing, Just typedVarMap)   -- Reached number of boxes cutoff
    Nothing -> (Just True, Nothing) -- All areas in queue verified

decideDisjunctionDepthFirstWithApply :: [(E.ESafe, BoxFun)] -> TypedVarMap -> Integer -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
decideDisjunctionDepthFirstWithApply expressionsWithFunctions typedVarMap currentDepth depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
  | null filterOutFalseTerms =
    trace ("proved false with apply " ++ show roundedVarMap)
    (Just False, Just roundedVarMap)
  | checkIfEsTrueUsingApply =
    trace "proved true with apply"
    (Just True, Nothing)
  | otherwise = bisectUntilCutoff roundedVarMap
  where
      box  = typedVarMapToBox typedVarMap p
      varNamesWithTypes = getVarNamesWithTypes typedVarMap
      roundedVarMap =
        case safeBoxToTypedVarMap box varNamesWithTypes of
          Just rvm -> rvm
          Nothing -> error $ "Rounded the following varMap makes it inverted: " ++ show typedVarMap

      esWithRanges            = parMap rseq (\ (e, f) -> ((e, f), apply f box)) expressionsWithFunctions
      filterOutFalseTerms     = filterOutFalseExpressions esWithRanges
      checkIfEsTrueUsingApply = decideDisjunctionRangesTrue filterOutFalseTerms

      filteredExpressionsWithFunctions = map fst filterOutFalseTerms

      bestFirstCheck varMapToCheck = setupBestFirstCheck filteredExpressionsWithFunctions varMapToCheck bfsBoxesCutoff relativeImprovementCutoff p

      bisectUntilCutoff varMapToCheck =
        if currentDepth !<! depthCutoff -- Best first
          then
              bisectWidestDimensionAndRecurse varMapToCheck
          else
            bestFirstCheck varMapToCheck --Last ditch bestFirst check

      bisectWidestDimensionAndRecurse varMapToBisect =
        let
          (leftVarMap, rightVarMap) = bisectWidestTypedInterval varMapToBisect
          (leftR, rightR) =
            withStrategy
            (parTuple2 rseq rseq)
            (
              decideDisjunctionDepthFirstWithApply filteredExpressionsWithFunctions leftVarMap (currentDepth + 1) depthCutoff bfsBoxesCutoff relativeImprovementCutoff p,
              decideDisjunctionDepthFirstWithApply filteredExpressionsWithFunctions rightVarMap (currentDepth + 1) depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
            )
        in
          case leftR of
            (Just True, _)
              -> case rightR of
                (Just True, _) -> (Just True, Nothing)
                r -> r
            r -> r

decideDisjunctionDepthFirstWithSimplex :: [(E.ESafe, BoxFun)] -> TypedVarMap -> Integer -> Integer -> Integer -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
decideDisjunctionDepthFirstWithSimplex expressionsWithFunctions typedVarMap currentDepth depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
  | null filterOutFalseTerms =
    trace ("proved false with apply " ++ show roundedVarMap)
    (Just False, Just roundedVarMap)
  | checkIfEsTrueUsingApply =
    trace "proved true with apply"
    (Just True, Nothing)
  | otherwise = checkSimplex
  where
      box  = typedVarMapToBox typedVarMap p
      varNamesWithTypes = getVarNamesWithTypes typedVarMap
      roundedVarMap =
        case safeBoxToTypedVarMap box varNamesWithTypes of
          Just rvm -> rvm
          Nothing -> error $ "Rounded the following varMap makes it inverted: " ++ show typedVarMap
      untypedRoundedVarMap = typedVarMapToVarMap roundedVarMap

      esWithRanges            = parMap rseq (\ (e, f) -> ((e, f), apply f box)) expressionsWithFunctions
      filterOutFalseTerms     = filterOutFalseExpressions esWithRanges --FIXME: Filtering out true cases
      checkIfEsTrueUsingApply = decideDisjunctionRangesTrue filterOutFalseTerms

      indeterminateExpressions = map (fst . fst) filterOutFalseTerms

      filteredExpressionsWithFunctions = map fst filterOutFalseTerms

      filteredCornerRangesWithDerivatives = computeCornerValuesAndDerivatives filterOutFalseTerms box

      bestFirstCheck varMapToCheck = setupBestFirstCheck filteredExpressionsWithFunctions varMapToCheck bfsBoxesCutoff relativeImprovementCutoff p

      bisectWidestDimensionAndRecurse varMapToBisect =
        let
          (leftVarMap, rightVarMap) = bimap (`unsafeIntersectVarMap` varMapToBisect) (`unsafeIntersectVarMap` varMapToBisect) $ bisectWidestTypedInterval varMapToBisect

          (leftR, rightR) =
            withStrategy
            (parTuple2 rseq rseq)
            (
              decideDisjunctionDepthFirstWithSimplex filteredExpressionsWithFunctions leftVarMap (currentDepth + 1) depthCutoff bfsBoxesCutoff relativeImprovementCutoff p,
              decideDisjunctionDepthFirstWithSimplex filteredExpressionsWithFunctions rightVarMap (currentDepth + 1) depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
            )
        in
          case leftR of
            (Just True, _)
              -> case rightR of
                (Just True, _) -> (Just True, Nothing)
                r -> r
            r -> r

      bisectUntilCutoff varMapToCheck =
        if currentDepth !<! depthCutoff -- Best first
          then
              bisectWidestDimensionAndRecurse varMapToCheck
          else
            (Nothing, Just varMapToCheck)
            -- bestFirstCheck varMapToCheck --Last ditch bestFirst check

      checkSimplex
        -- If we can calculate any derivatives
        | (not . null) filteredCornerRangesWithDerivatives = trace "decideWithSimplex start" $
          case decideWithSimplex filteredCornerRangesWithDerivatives untypedRoundedVarMap of
            (Just True, _) -> trace ("decideWithSimplex true: " ++ show roundedVarMap) (Just True, Nothing)
            (Nothing, Just newVarMap) -> trace "decideWithSimplex indet" $
              case safeVarMapToTypedVarMap newVarMap varNamesWithTypes of
                Just nvm -> recurseOnVarMap $ unsafeIntersectVarMap nvm roundedVarMap
                Nothing -> (Just True, Nothing) -- This will only happen when all integers in the varMap have been verified
            _ -> undefined
        | otherwise = bisectUntilCutoff roundedVarMap

      recurseOnVarMap recurseVarMap
        | typedMaxWidth recurseVarMap == 0 = (Nothing, Just recurseVarMap)
        | typedMaxWidth roundedVarMap / typedMaxWidth recurseVarMap >= relativeImprovementCutoff =
          trace ("recursing with simplex with roundedVarMap: " ++ show recurseVarMap) $
          decideDisjunctionDepthFirstWithSimplex filteredExpressionsWithFunctions recurseVarMap currentDepth depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
        | otherwise = bisectUntilCutoff recurseVarMap

decideDisjunctionWithSimplexCE :: [(E.ESafe, BoxFun)] -> TypedVarMap -> Rational -> Precision -> (Maybe Bool, Maybe TypedVarMap)
decideDisjunctionWithSimplexCE expressionsWithFunctions typedVarMap relativeImprovementCutoff p
  | null filterOutFalseTerms =
    trace ("proved false with apply " ++ show roundedVarMap)
    (Just False, Just roundedVarMap)
  | checkIfEsTrueUsingApply =
    trace "proved true with apply"
    (Just True, Nothing)
  | otherwise = checkSimplex
  where
      box  = typedVarMapToBox typedVarMap p
      varNamesWithTypes = getVarNamesWithTypes typedVarMap
      roundedVarMap =
        case safeBoxToTypedVarMap box varNamesWithTypes of
          Just rvm -> rvm
          Nothing -> error $ "Rounded the following varMap makes it inverted: " ++ show typedVarMap
      untypedRoundedVarMap = typedVarMapToVarMap roundedVarMap

      esWithRanges            = parMap rseq (\ (e, f) -> ((e, f), apply f box)) expressionsWithFunctions
      filterOutFalseTerms     = filterOutFalseExpressions esWithRanges
      checkIfEsTrueUsingApply = decideDisjunctionRangesTrue filterOutFalseTerms

      filteredExpressionsWithFunctions = map fst filterOutFalseTerms

      filteredCornerRangesWithDerivatives = computeCornerValuesAndDerivatives filterOutFalseTerms box

      checkSimplex
        -- If we can calculate any derivatives
        | (not . null) filteredCornerRangesWithDerivatives = trace "decideWithSimplex start" $
          case decideWithSimplex filteredCornerRangesWithDerivatives untypedRoundedVarMap of
            (Just True, _) -> trace "decideWithSimplex true" (Just True, Nothing)
            (Nothing, Just newVarMap) -> trace "decideWithSimplex indet" $
              let
                newBox  = varMapToBox newVarMap p
                newCornerRangesWithDerivatives = computeCornerValuesAndDerivatives filterOutFalseTerms newBox
              in trace "findFalsePointWithSimplex start" $
                case safeBoxToTypedVarMap newBox varNamesWithTypes of
                  Just roundedNewVarMap ->
                    case findFalsePointWithSimplex newCornerRangesWithDerivatives (typedVarMapToVarMap roundedNewVarMap) of
                      Nothing             -> trace "findFalsePointWithSimplex indet" recurseOnVarMap roundedNewVarMap
                      Just counterExample -> trace "findFalsePointWithSimplex false" $
                        let
                          -- This is safe because all intervals in the counterexample are singletons
                          roundCounterExample =
                            map
                            (\(v, (l, _)) ->
                              case lookup v varNamesWithTypes of
                                Just Integer -> TypedVar (v, (round l % 1, round l % 1)) Integer
                                _            -> TypedVar (v, (l, l)) Real
                            )
                            counterExample
                        in
                          if decideDisjunctionFalse (map fst filterOutFalseTerms) roundCounterExample (prec 1000) -- maybe use higher p than the one passed in? i.e. * (3/2)
                            then (Just False, Just roundCounterExample)
                            else trace "counterexample incorrect" recurseOnVarMap roundedNewVarMap
                  Nothing -> (Just True, Nothing)
            _ -> undefined
        | otherwise = (Nothing, Just roundedVarMap)

      recurseOnVarMap recurseVarMap
        | typedMaxWidth recurseVarMap == 0 = (Nothing, Just recurseVarMap)
        | typedMaxWidth roundedVarMap / typedMaxWidth recurseVarMap !>=! cn relativeImprovementCutoff =
          trace ("recursing with simplex with box: " ++ show recurseVarMap) $
          decideDisjunctionWithSimplexCE filteredExpressionsWithFunctions recurseVarMap relativeImprovementCutoff p
        | otherwise = (Nothing, Just recurseVarMap)

decideWithSimplex :: [(CN MPBall, CN MPBall, Box)] -> VarMap -> (Maybe Bool, Maybe VarMap)
decideWithSimplex cornerValuesWithDerivatives varMap =
  case mOptimizedVars of
    Just optimizedVars -> (Nothing, Just optimizedVars)
    Nothing            -> (Just True, Nothing)
  where
    -- Create the system for the simplex method, store stringVar -> intVar map
    (simplexSystem, stringIntVarMap) = constraintsToSimplexConstraints $ createConstraintsToEncloseAreaUnderZero cornerValuesWithDerivatives varMap

    vars = map fst varMap

    mFeasibleSolution = S.findFeasibleSolution simplexSystem

    extractSimplexResult :: Maybe (Integer, [(Integer, Rational)]) -> Rational
    extractSimplexResult maybeResult =
      case maybeResult of
        Just (optimizedIntVar, result) -> -- optimizedIntVar refers to the objective variable. We extract the value of the objective
                                          -- variable from the result
          case lookup optimizedIntVar result of
            Just optimizedVarResult -> optimizedVarResult
            Nothing -> error "Extracting simplex result after finding feasible solution resulted in an infeasible result. This should not happen"
        Nothing -> undefined

    mOptimizedVars =
      case mFeasibleSolution of
        Just (feasibleSystem, slackVars, artificialVars, objectiveVar) ->
          Just $
          map -- Optimize (minimize and maximize) all variables in the varMap
          (\var ->
            case M.lookup var stringIntVarMap of
              Just intVar -> 
                case lookup var varMap of
                  Just (originalL, _) -> -- In the simplex system, the original lower bound of each var was shifted to 0. We undo this shift after optimization.
                    (
                      var, 
                      (
                        originalL + extractSimplexResult (S.optimizeFeasibleSystem (S.Min [(intVar, 1.0)]) feasibleSystem slackVars artificialVars objectiveVar), 
                        originalL + extractSimplexResult (S.optimizeFeasibleSystem (S.Max [(intVar, 1.0)]) feasibleSystem slackVars artificialVars objectiveVar)
                      )
                    )
                  Nothing -> error "Optimized var not found in original varMap. This should not happen."
              Nothing -> error "Integer version of var not found. This should not happen."
          )
          vars
        Nothing -> Nothing
  -- case encloseAreaUnderZeroWithSimplex cornerValuesWithDerivatives varMap of
  --   Just newVarMap -> (Nothing, Just newVarMap)
  --     -- if newVarMap == varMap 
  --       -- then trace (show newVarMap) (Nothing, Just newVarMap)
  --       -- else trace "recurse" $ decideWithSimplex expressions newVarMap p
  --   Nothing -> (Just True, Nothing)

data Constraint = GEQ [(String, Rational)] Rational | LEQ [(String, Rational)] Rational

constraintLeftSide :: Constraint -> [(String, Rational)]
constraintLeftSide (GEQ lhs _) = lhs
constraintLeftSide (LEQ lhs _) = lhs

constraintVars :: [Constraint] -> [String]
constraintVars cs = nub $ aux cs
  where
    aux :: [Constraint] -> [String]
    aux [] = []
    aux (x : xs) = map fst (constraintLeftSide x) ++ aux xs 

constraintsToSimplexConstraints :: [Constraint] -> ([S.PolyConstraint], M.Map String Integer)
constraintsToSimplexConstraints constraints =
  (
    map
    (\case
      GEQ varsWithCoeffs rhs -> S.GEQ (map (\(stringVar, coeff) -> (fromJust (M.lookup stringVar stringIntVarMap), coeff)) varsWithCoeffs) rhs
      LEQ varsWithCoeffs rhs -> S.LEQ (map (\(stringVar, coeff) -> (fromJust (M.lookup stringVar stringIntVarMap), coeff)) varsWithCoeffs) rhs
    )
    constraints,
    stringIntVarMap
  )
  where
    stringVars = constraintVars constraints
    stringIntVarMap = M.fromList $ zip stringVars [1..]

createConstraintsToEncloseAreaUnderZero :: [(CN MPBall, CN MPBall, Box)] -> VarMap -> [Constraint]
createConstraintsToEncloseAreaUnderZero cornerValuesWithDerivatives varMap =
  domainConstraints ++ functionConstraints
  where
    vars = map fst varMap
    varsNewUpperBounds = map (\(_, (l, r)) -> r - l) varMap
    -- intVarMap = zip [1..] $ map snd varMap

    domainConstraints =
      map
      (\(var, (varLower, varUpper)) -> 
        -- Lowerbound not needed, varLower - varLower = 0, and var >= 0 is assumed by the simplex method
        LEQ [(var, 1.0)] $ varUpper - varLower
      )
      varMap

    functionConstraints =
      concatMap
      (\(fnInt, (fLeftRange, fRightRange, fPartialDerivatives)) ->
        let
          fPartialDerivativesLowerBounds = map (fst . mpBallToRational) $ V.toList fPartialDerivatives
          fPartialDerivativesUpperBounds = map (snd . mpBallToRational) $ V.toList fPartialDerivatives
          fLeftLowerBound = fst $ mpBallToRational fLeftRange
          fRightLowerBound = fst $ mpBallToRational fRightRange

          fNegN = "fNeg" ++ show fnInt
        in
          [
            LEQ 
              ((fNegN, 1.0) : zip vars fPartialDerivativesLowerBounds)
              (-fLeftLowerBound), -- Partial derivatives multiplied with left corner is omitted because left corner is 0
            LEQ 
              ((fNegN, 1.0) : zip vars fPartialDerivativesUpperBounds)
              $ foldl add (-fRightLowerBound) $ zipWith mul varsNewUpperBounds fPartialDerivativesUpperBounds
            -- LEQ 
            --   ((fNegN, 1.0) : zip vars fPartialDerivativesLowerBounds)
            --   $ foldl add (-fRightLowerBound) $ zipWith mul varsNewUpperBounds fPartialDerivativesLowerBounds
            -- fNegN >= 0 will be assumed by the simplex method
          ]
      )
      $
      zip
      [1..]
      cornerValuesWithDerivatives

    mpBallToRational :: CN MPBall -> (Rational, Rational)
    mpBallToRational = bimap rational rational . endpoints . reducePrecionIfInaccurate . unCN

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
  -> ([S.PolyConstraint], [(Integer, Rational)], Integer) {- ^  The first element is the system of constraints created from the VarMap.
                                                                  The second element stores the amount a variable has been shifted from the LHS.
                                                                  The third element denotes the next integer variable available. -}
createDomainConstraints [] nextAvailableVar = trace "finished domain constraints" ([], [], nextAvailableVar)
createDomainConstraints ((_, (l, r)) : xs) currentIndex =
  let
    (resultsL, resultsR, nextAvailableVar) = createDomainConstraints xs (currentIndex + 1)
  in
    if l < 0 then -- If the current domain is under zero, transform the domain by subtracting the left bound
                  -- This will make the left bound equal to zero and will make the right bound above zero
                  -- Store this transformation in resultsR
      ([S.GEQ [(currentIndex, rational 1)] (l - l), S.LEQ [(currentIndex, rational 1)] (r - l)] ++ resultsL, (currentIndex, l) : resultsR, nextAvailableVar)
    else
      ([S.GEQ [(currentIndex, rational 1)] l, S.LEQ [(currentIndex, rational 1)] r] ++ resultsL, resultsR, nextAvailableVar)

findFunctionCounterExample :: [(CN MPBall, CN MPBall, Box)] -> [Rational] -> [Rational] -> [(Integer, Rational)] -> [S.PolyConstraint]
findFunctionCounterExample [] _ _ _ = trace "finished function constraints" []
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
    S.LEQ ((currentIndex, 1.0) : zip [1..] lowerDerivatives) (foldl add (-leftL - lowerSubst) lowerDerivativesTimesLeftCorner)
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
    -- S.GEQ ((currentIndex, 1.0) : zip [1..] negatedUpperDerivatives) (foldl add (rightL + upperSubst) negatedUpperDerivativesTimesRightCorner)
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

    negatedLowerDerivativesTimesLeftCorner = zipWith mul leftCorner negatedLowerDerivatives
    negatedUpperDerivativesTimesLeftCorner = zipWith mul leftCorner negatedUpperDerivatives

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
  -- trace (show completeSystem) $ 
  -- trace (show variables) $
  --trace (show substVars) $
  -- If the first result from the list returned by the simplex method is empty,
  -- the system is infeasible, so we return nothing
  case mNewPoints of
    Just newPoints ->
      case head newPoints of
        (_, (Nothing)) ->
          Nothing -- Should never get here
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
                    case lookup v varMap of
                      Just (_, upperBound) ->
                        case lookup iv substVars of -- Check if any transformation needs to be done to get the final result
                              Just c -> (c + extractResult r, upperBound) -- Add any needed transformation to lower/upper bounds
                              Nothing -> (extractResult r, upperBound)
                      Nothing -> undefined
                Nothing -> undefined -- Should never get here
              -- case lookup v indexedVariables of
              --   Just iv -> -- Get the integer variable for the current string variable 
              --     case lookup iv substVars of -- Check if any transformation needs to be done to get the final result
              --       Just c -> (bimap ((add c) . extractResult) ((add c) . extractResult) r) -- Add any needed transformation to lower/upper bounds
              --       Nothing -> bimap extractResult extractResult r
              --   Nothing -> undefined -- Should never get here
              )
            )
            newPoints
    Nothing -> Nothing
  -- case mNewPoints of
  --   (Nothing, Just _) -> error "Max feasible, but Min infeasible"
  --   (Just _, Nothing) -> error "Min feasible, but Max infeasible"
  --   (Nothing, Nothing) -> Nothing
  --   (Just (_, newPointsL), Just (_, newPointsR)) -> 
  --     Just
  --     $
  --     map
  --     (\(iv, (v, (originalL, originalR))) ->
  --       case lookup iv newPointsL of
  --         Just newL ->
  --           case lookup iv newPointsR of
  --             Just newR -> let c = fromMaybe 0.0 (lookup iv substVars) in (v, (newL + c, newR + c))
  --             Nothing   -> let c = fromMaybe 0.0 (lookup iv substVars) in (v, (newL + c, c))
  --         Nothing ->
  --           case lookup iv newPointsR of
  --             Just newR -> let c = fromMaybe 0.0 (lookup iv substVars) in (v, (c, newR + c))
  --             Nothing   -> let c = fromMaybe 0.0 (lookup iv substVars) in (v, (c, c))
  --     )
  --     indexedVarMap
  where
    -- Get the bottom left corner of the varMap

    -- boxFuns = map (\e -> expressionToBoxFun e corner p) expressions

    -- Create constraints for the domain
    -- substVars stores any variable transformations (for the LHS)
    (domainConstraints, substVars, nextAvailableVar) = trace "creating domain constraints" createDomainConstraints varMap 1

    numberOfFunctions = length cornerValuesWithDerivatives

    variables = [1 .. (nextAvailableVar - 1)]

    -- Set the variables that will be used to refer to each function
    functions = [nextAvailableVar .. nextAvailableVar + numberOfFunctions - 1]

    leftCorner = map (fst . snd) varMap
    rightCorner = map (snd . snd) varMap

    functionConstraints = trace "creating function constraints" createFunctionConstraints cornerValuesWithDerivatives leftCorner rightCorner nextAvailableVar substVars

    -- Map integer variables to their respective varMap
    indexedVarMap = zip variables varMap

    -- Map string variables to their respective integer variable
    indexedVariables = zip (map fst varMap) variables

    -- Create constraints to bound the area under zero for each function
    -- LEQ [(functionVariable, 1.0)] 0.0 would not work because the simplex method assumes >= 0
    -- for each variable, so we use GEQ [(functionVariable, 1.0)] 0.0 instead, and make the
    -- appropriate changes in createFunctionConstraints to effectively contain the area
    -- under zero for each function.
    underZeroConstraints = trace "creating underZero constraints"
      map
      (\v -> S.GEQ [(v, 1.0)] 0.0)
      functions

    completeSystem = trace "creating system" domainConstraints ++ functionConstraints ++ underZeroConstraints

    -- Call the simplex method twice for each variable (setting the objective function to Min/Max of each
    -- variable). Map each (String) variable to a pair. The pair is the results determined by the simplex
    -- method when Min/Maxing the key variable. 
    -- newPoints :: [(String, (Maybe (Integer, [(Integer, Rational)]), Maybe (Integer, [(Integer, Rational)])))]
    -- newPoints =
    --   map
    --   (\v -> 
    --     case lookup v indexedVarMap of
    --       Just (sv, _) -> (sv, (S.twoPhaseSimplex (S.Min [(v, 1.0)]) completeSystem, S.twoPhaseSimplex (S.Max [(v, 1.0)]) completeSystem))
    --       Nothing -> undefined
    --   )
    --   variables

    -- Call the simplex method twice for each variable (setting the objective function to Min/Max of each
    -- variable). Map each (String) variable to a pair. The pair is the results determined by the simplex
    -- method when Min/Maxing the key variable. 
    mNewPoints :: Maybe [(String, (Maybe (Integer, [(Integer, Rational)])))]
    mNewPoints =
      case S.findFeasibleSolution completeSystem of
        Just (feasibleSystem, slackVars, artificialVars, objectiveVar) ->
          Just $
          map
          (\v ->
            case lookup v indexedVarMap of
              -- Just (sv, _) -> (sv, (S.optimizeFeasibleSystem (S.Min [(v, 1.0)]) feasibleSystem slackVars artificialVars objectiveVar, S.optimizeFeasibleSystem (S.Max [(v, 1.0)]) feasibleSystem slackVars artificialVars objectiveVar))
              Just (sv, _) -> (sv, S.optimizeFeasibleSystem (S.Min [(v, 1.0)]) feasibleSystem slackVars artificialVars objectiveVar)
              Nothing -> undefined
          )
          variables
        Nothing -> Nothing


    -- mNewPoints :: (Maybe (Integer, [(Integer, Rational)]), Maybe (Integer, [(Integer, Rational)]))
    -- mNewPoints = (S.twoPhaseSimplex (S.Min (map (\v -> (v, 1.0)) variables)) completeSystem, S.twoPhaseSimplex (S.Max (map (\v -> (v, 1.0)) variables)) completeSystem)

findFalsePointWithSimplex
  :: [(CN MPBall, CN MPBall, Box)] -- ^ [(valueOfFunctionAtLeftCorner, valueOfFunctionAtRightCorner, derivativesOfFunction)]
  -> VarMap -- ^ The domains for each function which leads to the first derivatives
  -> Maybe VarMap
findFalsePointWithSimplex cornerValuesWithDerivatives varMap =
  -- trace (show completeSystem) $
  case mNewPoints of
    Just newPoints ->
      trace "counterexample found"
      -- trace "========================"
      -- trace (Data.List.intercalate "\n "(map S.prettyShowPolyConstraint completeSystem))
      -- trace "========================"
      Just $
      map
      (\(s, v) ->
        case lookup v newPoints of
          Just r -> let c = fromMaybe 0.0 (lookup v substVars) in (s, (r + c, r + c))
          Nothing -> let c = fromMaybe 0.0 (lookup v substVars) in (s, (c, c)) --FIXME: is this correct, shouldn't it be originalVal? Or does Nothing mean that this value is 0?
      )
      indexedVariables
    Nothing -> trace "no counterexample" Nothing
  where
    -- Get the bottom left corner of the varMap

    -- boxFuns = map (\e -> expressionToBoxFun e corner p) expressions

    -- Create constraints for the domain
    -- substVars stores any variable transformations (for the LHS)
    (domainConstraints, substVars, nextAvailableVar) = trace "creating domain constraints" createDomainConstraints varMap 1

    variables = [1 .. (nextAvailableVar - 1)]

    leftCorner = map (fst . snd) varMap
    rightCorner = map (snd . snd) varMap

    functionConstraints = trace "creating function constraints" findFunctionCounterExample cornerValuesWithDerivatives leftCorner rightCorner substVars

    -- Map integer variables to their respective varMap
    indexedVarMap = zip variables varMap

    -- Map string variables to their respective integer variable
    indexedVariables = zip (map fst varMap) variables

    completeSystem = trace "creating system" domainConstraints ++ functionConstraints

    -- Get a feasible solution for this system
    mNewPoints :: Maybe [(Integer, Rational)]
    mNewPoints = S.displayDictionaryResults . (\(feasibleSystem,_,_,_) -> feasibleSystem) <$> S.findFeasibleSolution completeSystem

checkECNFVerify :: [[E.ESafe]] -> TypedVarMap -> Integer -> Integer -> Rational -> Precision -> Bool
checkECNFVerify cnf varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff p = undefined
  -- case resultVarMap of
  --   Just varMap ->
  --     -- trace (show box) $
  --     -- trace (show resultBool) $
  --     -- trace (show (applyECNF cnf box p)) $
  --     decideConjunctionFalse (map (map (\e -> (e, expressionToBoxFun (E.extractSafeE e) varMap p))) cnf) varMap p
  --   Nothing -> undefined
  -- where
  --   (resultBool, resultVarMap) = checkECNFDepthFirstWithSimplex cnf varMap depthCutoff bfsBoxesCutoff relativeImprovementCutoff p
