{-# LANGUAGE LambdaCase #-}
module AERN2.BoxFunMinMax.Tester where

import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.Type
import Test.QuickCheck
import MixedTypesNumPrelude
import qualified Prelude as P
import AERN2.BoxFunMinMax.Expressions.DeriveBounds
import qualified Data.Map as Map
import AERN2.BoxFunMinMax.Expressions.Eliminator

prop_substituteConjunctionEqualities :: F -> Property
prop_substituteConjunctionEqualities f =
  forAllBlind variablePoints $ \points ->
    let
      -- disjunction = fDNFToEDNF . simplifyFDNF . fToFDNF $ f

      disjunction = fDNFToEDNF . simplifyFDNF . fToFDNF . simplifyF . removeVariableFreeComparisons $ f


      -- get the dnf used in LPPaver
      -- disjunction = fDNFToEDNF . simplifyFDNF . fToFDNF . simplifyF . minMaxAbsEliminatorF . simplifyF . removeVariableFreeComparisons $ f

      varMap = map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
      varBoundMap = Map.fromList (map (\(v, (l, r)) -> (v, (Just l, Just r))) varMap)
      
      decideDisjunction [] isIndeterminate =
        if isIndeterminate
          then Nothing
          else Just False
      decideDisjunction (c : d) isIndeterminate =
        case decideConjunction c False of
          Nothing -> decideDisjunction d True
          Just False -> decideDisjunction d isIndeterminate
          Just True -> Just True

      decideConjunction [] isIndeterminate = 
        if isIndeterminate
          then Nothing
          else Just True
      decideConjunction (e : c) isIndeterminate = 
        let 
          mResult =
            case e of
              EStrict e' -> checkFWithEval (FComp Ge e' (Lit 0.0)) varBoundMap
              ENonStrict e' -> checkFWithEval (FComp Gt e' (Lit 0.0)) varBoundMap
        in
          case mResult of
            Nothing -> decideConjunction c True
            Just True -> decideConjunction c isIndeterminate
            Just False -> Just False

      mDisjunctionResult = decideDisjunction disjunction False
      substitutedDisjunction = map substituteConjunctionEqualities disjunction
      mSubstitutedDisjunctionResult = decideDisjunction substitutedDisjunction False
      -- mFResult           = checkFWithEval f varBoundMap
      -- eliminatedF        = minMaxAbsEliminatorF f
      -- mEliminatedFResult = checkFWithEval eliminatedF varBoundMap
      -- varMap            = Map.fromList $ map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
      -- fResult           = evalF_Rational varMap f
      -- simplifiedFResult = evalF_Rational varMap (simplifyF f)

      quickCheckMessage = if disjunction P.== substitutedDisjunction then "Same : " else "Diff : "
    in
      -- undefined
      case (mDisjunctionResult, mSubstitutedDisjunctionResult) of
        (Just disjunctionResult, Just substitutedDisjunctionResult) ->
          if disjunctionResult
            then if substitutedDisjunctionResult
              then label (quickCheckMessage ++ "Subst and non-subst are both true") True
              else counterexample (quickCheckMessage ++ "Subst says true but non-subst says false: F : " ++ show f ++ " Domain : " ++ show varMap) False
            else if substitutedDisjunctionResult
              then counterexample (quickCheckMessage ++ "Subst says false but non-subst says true: F : " ++ show f ++ " Domain : " ++ show varMap) False
              else label (quickCheckMessage ++ "Subst and non-subst are both false") True
        (Nothing, Nothing)   -> label (quickCheckMessage ++ "Subst and non-subt f are both indeterminate") True
        (Nothing, Just bool) -> counterexample (quickCheckMessage ++ "non-subst says indeterminate, but subst says " ++ show bool) True
        (Just bool, Nothing) -> counterexample (quickCheckMessage ++ "subst says indeterminate, but non-subst says " ++ show bool) True

    -- if hasMinMaxAbsF eliminatedF
    --   then counterexample ("eliminated still has min/max/abs: Function: " ++ show f ++ " Domain: " ++ show varMap) False
    --   else case (mFResult, mEliminatedFResult) of
    --     (Just fResult, Just eliminatedFResult) ->
    --       if fResult
    --         then if eliminatedFResult
    --           then label "Eliminated and non-eliminated f are both true" True
    --           else counterexample ("non-eliminated says true, but eliminated says false: Function: " ++ show f ++ " Domain: " ++ show varMap) False
    --         else if eliminatedFResult
    --           then counterexample ("non-eliminated says false, but eliminated says true: Function: " ++ show f ++ " Domain: " ++ show varMap) False
    --           else label "Eliminated and non-eliminated f are both false" True
    --     (Nothing, Nothing)   -> label "Eliminated and non-eliminated f are both indeterminate" True
    --     (Nothing, Just bool) -> counterexample ("non-eliminated says indeterminate, but eliminated says " ++ show bool) True
    --     (Just bool, Nothing) -> counterexample ("eliminated says indeterminate, but non-eliminated says " ++ show bool) True
  where
    variables = extractVariablesF f

    variablePoints :: Gen [Integer]
    variablePoints = vectorOf (int (length variables)) arbitrary
