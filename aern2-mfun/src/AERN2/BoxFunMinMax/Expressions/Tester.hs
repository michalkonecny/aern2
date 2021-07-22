module AERN2.BoxFunMinMax.Expressions.Tester where

import AERN2.BoxFunMinMax.Expressions.Type 
import AERN2.BoxFunMinMax.Type
import Test.QuickCheck
import MixedTypesNumPrelude
import AERN2.MP.Ball
import AERN2.Kleenean
import Control.CollectErrors
import qualified Prelude as P
import AERN2.BoxFunMinMax.Expressions.DeriveBounds
import qualified Data.Map as Map
import Data.List (nub)
import AERN2.BoxFunMinMax.Expressions.Eliminator
-- type VarMap = [(String, (Rational, Rational))]
import qualified Debug.Trace as T

epsilon :: Rational
epsilon = 1/(2^112)

hasMinMaxE :: E -> Bool 
hasMinMaxE (EBinOp Min _ _) = True
hasMinMaxE (EBinOp Max _ _) = True
hasMinMaxE (EBinOp _ e1 e2) = hasMinMaxE e1 || hasMinMaxE e2
hasMinMaxE (EUnOp _ e) = hasMinMaxE e
hasMinMaxE (Float32 _ e) = hasMinMaxE e
hasMinMaxE (Float64 _ e) = hasMinMaxE e
hasMinMaxE (Float _ e) = hasMinMaxE e
hasMinMaxE (PowI e _) = hasMinMaxE e
hasMinMaxE (Lit _) = False
hasMinMaxE (Var _) = False

hasMinMaxF :: F -> Bool 
hasMinMaxF (FComp _ e1 e2) = hasMinMaxE e1 || hasMinMaxE e2
hasMinMaxF (FConn _ f1 f2) = hasMinMaxF f1 || hasMinMaxF f2
hasMinMaxF (FNot f)        = hasMinMaxF f
hasMinMaxF FTrue           = False
hasMinMaxF FFalse          = False

-- Errors caught

-- Exception:
--   log: argument must be > 0: [0 ± 0]
--   CallStack (from HasCallStack):
--     error, called at src/AERN2/MP/Ball/Elementary.hs:108:11 in aern2-mp-0.2.6.0-FRCXSj7FK9DITey8WaLH2n:AERN2.MP.Ball.Elementary
-- EBinOp Pow (Lit (0 % 1)) (Var "Name \"b\"")

-- Exception:
--   compare: comparisons are partial on Approx
--   CallStack (from HasCallStack):
--     error, called at src/Data/CDAR/Approx.hs:652:19 in cdar-mBound-0.1.0.2-K0yuCzkrB9VHbMIuFgdnHe:Data.CDAR.Approx
-- EBinOp Pow (Float64 RNE (Float32 RNE (EUnOp Sqrt (Lit (4 % 1))))) (PowI (EBinOp Add (Float64 RTN (EBinOp Div (Var "Name \"e\"") (Var "Name \"l\""))) (PowI (Lit ((-17) % 1)) 19)) 19)

-- |Using evalE makes this test hang less often
-- I think the tests hang because some of the generated expressions are very difficult to calclulate interval bounds for
-- I'm pretty sure that the tests are not hanging because of the size of expressions, because I ran a test
-- comparing equality between generated expressions. This test never hanged
prop_simplifyE :: E -> Property
prop_simplifyE e = 
  -- not (hasMinMaxE e) ==>
    forAllBlind variablePoints $ \points ->
      let
        -- varMap = map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
        -- eValAtVarMap           = applyExpression e varMap (prec 10)
        -- simplifiedEValAtVarMap = applyExpression (simplifyE e) varMap (prec 10)
        varMap                 = Map.fromList $ map (\(i, v) -> (v, (Just (rational (points !! i)), Just (rational (points !! i))))) (zip [0..] variables)
        eValAtVarMap           = evalE_Rational varMap e
        simplifiedEValAtVarMap = evalE_Rational varMap (simplifyE e)
      in
        counterexample ("Simplified and unsimplified expressions disagree: Expression: " ++ show e ++ " Domain: " ++ show varMap) $ eValAtVarMap == simplifiedEValAtVarMap
        -- e P.== e
        -- not (hasError eValAtVarMap) ==>
          -- case unCN $ eValAtVarMap == simplifiedEValAtVarMap of
          --   CertainTrue  -> label "Simplified and unsimplified expressions agree" True
          --   CertainFalse -> counterexample ("Simplified and unsimplified expressions agree: Expression: " ++ show e ++ " Domain: " ++ show varMap) False
          --   TrueOrFalse  -> label "Simplified and unsimplified undecideable" True
  where
    variables = extractVariablesE e

    variablePoints :: Gen [Integer]
    variablePoints = vectorOf (int (length variables)) arbitrary

prop_simplifyF :: F -> Property
prop_simplifyF f =
  -- not (hasMinMaxF f) ==>
    forAllBlind variablePoints $ \points ->
      let
        varMap = map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
        fResult           = checkFWithApply f varMap (prec 10)
        simplifiedFResult = checkFWithApply (simplifyF f) varMap (prec 10)
        -- varMap            = Map.fromList $ map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
        -- fResult           = evalF_Rational varMap f
        -- simplifiedFResult = evalF_Rational varMap (simplifyF f)
      in
        not (hasError fResult) ==>
          case unCN fResult of
            CertainTrue -> 
              case unCN simplifiedFResult of
                CertainTrue  -> label "Simplified and unsimplified f are both true" True
                _            -> counterexample ("Unsimplified says true, but simplified says false/undecideable: Function: " ++ show f ++ " Domain: " ++ show varMap) False
            CertainFalse ->
              case unCN simplifiedFResult of
                CertainFalse -> label "Simplified and unsimplified f are both false" True
                _            -> counterexample ("Unsimplified says false, but simplified says true/undecideable: Function: " ++ show f ++ " Domain: " ++ show varMap) False
            TrueOrFalse ->
              case unCN simplifiedFResult of
                TrueOrFalse -> label "Simplified and unsimplified f are both undecideable" True
                _           -> counterexample ("Unsimplified says undecideable, but simplified says true/false: Function: " ++ show f ++ " Domain: " ++ show varMap) False

        -- e P.== e
        -- not (hasError eValAtVarMap) ==>
          -- case unCN $ fResult == fResult of
          --   CertainTrue  -> label "Simplified and unsimplified expressions agree" True
          --   CertainFalse -> counterexample (show e) False
          --   TrueOrFalse  -> label "Simplified and unsimplified undecideable" True
  where
    variables = extractVariablesF f

    variablePoints :: Gen [Integer]
    variablePoints = vectorOf (int (length variables)) arbitrary

-- CE: FNot (FComp Ge (EUnOp Negate (Var "Name \"f\"")) (Var "Name \"e\"")) Domain: [("Name \"f\"",((-3) % 1,(-3) % 1)),("Name \"e\"",(3 % 1,3 % 1))]
-- because ECNF checks strictly... ignores non-strict constraint
-- We could make F strict, but then this gets the issue we have in the shwon CE
-- FNot Ge is technically non-strict, since !Ge = Lt
-- But makeFStrct turns this into FNot Gt, and !Gt = Le
-- Good thing is that we aren't turning false into true
-- So instead, we discard all touching cases, removing this counterexample
-- prop_fToECNF :: F -> Property 
-- prop_fToECNF f =
--   -- not (hasMinMaxF f) ==>
--     forAllBlind variablePoints $ \points ->
--       let
--         varMap = map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
--         -- strictF           = makeFStrict f
--         fResult           = checkFWithApply f varMap (prec 10)
--         eRanges           = map (map (\e -> applyExpression e varMap (prec 10))) (fToECNF f epsilon)
--         eResult           = map (map (>= 0)) eRanges
--         eResult2          = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResult
--       in
--         not (hasError fResult) ==>
--           case unCN fResult of
--             CertainTrue -> 
--               case unCN eResult2 of
--                 CertainTrue  -> label "original F and CNF are both true" True
--                 _            -> counterexample ("original F true, but CNF false/undecideable: Function: " ++ show f ++ " Domain: " ++ show varMap) False
--             CertainFalse ->
--               case unCN eResult2 of
--                 CertainFalse -> label "original F and CNF are both false" True
--                 _            -> counterexample ("original F false, but CNF true/undecideable: Function: " ++ show f ++ " Domain: " ++ show varMap) False
--             TrueOrFalse ->
--               case unCN eResult2 of
--                 TrueOrFalse  -> label "original F and CNF are both undecideable" True
--                 CertainFalse -> label "original F indeterminate and CNF false" True -- This will happen sometimes due to the way we deal with strict inequalities
--                 _            -> counterexample ("orignal F indeterminate, but CNF says true: Function: " ++ show f ++ " Domain: " ++ show varMap) False

--         -- e P.== e
--         -- not (hasError eValAtVarMap) ==>
--           -- case unCN $ fResult == fResult of
--           --   CertainTrue  -> label "Simplified and unsimplified expressions agree" True
--           --   CertainFalse -> counterexample (show e) False
--           --   TrueOrFalse  -> label "Simplified and unsimplified undecideable" True
--   where
--     variables = extractVariablesF f

--     variablePoints :: Gen [Integer]
--     variablePoints = vectorOf (int (length variables)) arbitrary

-- type VC = F

-- instance Arbitrary VC where

-- |Emulate the way we turn VCs to ECNF in the SMT parser
-- prop_fVCToECNF :: F -> F -> Property 
-- prop_fVCToECNF fContext fGoal = 
--   -- not (hasMinMaxF f) ==>
--     forAllBlind variablePoints $ \points ->
--       let
--         varMap = map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
--         -- strictF           = makeFStrict f
--         getContextAndGoal (FConn Impl context goal) = (context, goal)
--         getContextAndGoal _ = undefined

--         (contextF, goalF) = getContextAndGoal f

--         vcECNF = 
--           [
--             contextEs ++ goalEs 
--             | 
--             contextEs <- fToECNF (FNot contextF) epsilon,
--             goalEs    <- fToECNF goalF epsilon
--           ]
--   --         [
--   --   contextEs ++ goalEs 
--   --   | 
--   --   contextEs <- map (map (\e -> eliminateFloats e varMap True)) (fToECNF (FNot context)), 
--   --   goalEs    <- map (map (\e -> eliminateFloats e varMap False)) (fToECNF goal)
--   -- ]

--         fResult           = checkFWithApply f varMap (prec 10)
--         eRanges           = map (map (\e -> applyExpression e varMap (prec 10))) vcECNF
--         eResult           = map (map (>= 0)) eRanges
--         eResult2          = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResult
--       in
--         not (hasError fResult) ==>
--           case unCN fResult of
--             CertainTrue -> 
--               case unCN eResult2 of
--                 CertainTrue  -> label "original F and CNF are both true" True
--                 _            -> counterexample ("original F true, but CNF false/undecideable: Function: " ++ show f ++ " Domain: " ++ show varMap) False
--             CertainFalse ->
--               case unCN eResult2 of
--                 CertainFalse -> label "original F and CNF are both false" True
--                 _            -> counterexample ("original F false, but CNF true/undecideable: Function: " ++ show f ++ " Domain: " ++ show varMap) False
--             TrueOrFalse ->
--               case unCN eResult2 of
--                 TrueOrFalse  -> label "original F and CNF are both undecideable" True
--                 CertainFalse -> label "original F indeterminate and CNF false" True -- This will happen sometimes due to the way we deal with strict inequalities
--                 _            -> counterexample ("orignal F indeterminate, but CNF says true: Function: " ++ show f ++ " Domain: " ++ show varMap) False
--   where
--     variables = extractVariablesF f

--     variablePoints :: Gen [Integer]
--     variablePoints = vectorOf (int (length variables)) arbitrary

--     isImplication (FConn Impl _ _) = True
--     isImplication _                = False

--     f = FConn Impl fContext fGoal

-- prop_minMaxAbsEliminatorECNF :: [[E]] -> Property
-- prop_minMaxAbsEliminatorECNF cnf = 
--   forAllBlind variablePoints $ \points ->
--     let
--       varMap          = map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)
--       eRanges         = map (map (\e -> applyExpression e varMap (prec 10))) cnf
--       eResult         = map (map (>= 0)) eRanges
--       eResultKleenean = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResult

--       eRangesEliminator         = map (map (\e -> applyExpression e varMap (prec 10))) $ minMaxAbsEliminatorECNF epsilon cnf
--       eResultEliminator         = map (map (>= 0)) eRangesEliminator
--       eResultEliminatorKleenean = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResultEliminator

--       eRangesHasError = any (any hasError) eRanges 

--     in
--       not eRangesHasError ==>
--         case unCN eResultKleenean of
--           CertainTrue ->
--             case unCN eResultEliminatorKleenean of
--               CertainTrue -> label "Both eliminated and non-eliminated are true" True
--               _           -> counterexample ("non-eliminated true but eliminated false/undecideable: CNF " ++ show cnf ++ " Domain: " ++ show varMap) False
--           CertainFalse ->
--             case unCN eResultEliminatorKleenean of
--               CertainFalse -> label "Both eliminated and non-eliminated are false" True
--               _           -> counterexample ("non-eliminated false but eliminated true/undecideable: CNF " ++ show cnf ++ " Domain: " ++ show varMap) False
--           TrueOrFalse ->
--             case unCN eResultEliminatorKleenean of
--               TrueOrFalse -> label "Both eliminated and non-eliminated are indeterminate" True
--               _           -> counterexample ("non-eliminated indeterminate but eliminated false/true: CNF " ++ show cnf ++ " Domain: " ++ show varMap) False
--   where
--     variables = extractVariablesECNF cnf

--     variablePoints :: Gen [Integer]
--     variablePoints = vectorOf (int (length variables)) arbitrary

-- Bug: FComp Le (EBinOp Min (EUnOp Abs (Var "Name \"e\"")) (EBinOp Min (Var "Name \"e\"") (Var "Name \"e\""))) (Var "Name \"g\"")
--  VM: [("Name \"e\"",(1 % 1,1 % 1)),("Name \"g\"",((-2) % 1,(-2) % 1))]
-- Original is False
-- fToECNF is False
-- but minMaxAbsEliminatorECNF says true
-- Another one: FComp Eq (EUnOp Abs (Lit (0 % 1))) (EUnOp Sqrt (Lit (5034822885953 % 7657538426026)))
-- No varmap
-- These bugs have been fixed
-- prop_fToECNFAndMinMaxAbsEliminatorECNF :: F -> Property
-- prop_fToECNFAndMinMaxAbsEliminatorECNF f1 = 
--   forAllBlind variablePoints $ \points ->
--     let
--       cnf             = minMaxAbsEliminatorECNF epsilon $ fToECNF f epsilon

--       varMap          = map (\(i, v) -> (v, (rational (points !! i), rational (points !! i)))) (zip [0..] variables)

--       eRanges         = map (map (\e -> applyExpression e varMap (prec 10))) cnf
--       eResult         = map (map (>= 0)) eRanges
--       eResultKleenean = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResult

--       fResult         = checkFWithApply f varMap (prec 10)
--     in
--       not (hasError fResult) ==>
--         case unCN fResult of
--           CertainTrue ->
--             case unCN eResultKleenean of
--               CertainTrue -> label "Both F and cnf are true" True
--               _           -> counterexample ("F true but cnf false/undecideable: Function " ++ show f ++ " Domain: " ++ show varMap) False
--           CertainFalse ->
--             case unCN eResultKleenean of
--               CertainFalse -> label "Both F and cnf are false" True
--               _           -> counterexample ("F false but eliminated true/undecideable: Function " ++ show f ++ " Domain: " ++ show varMap) False
--           TrueOrFalse ->
--             case unCN eResultKleenean of
--               TrueOrFalse -> label "Both F and cnf are indeterminate" True
--               _           -> counterexample ("F indeterminate but eliminated false/true: Function " ++ show f ++ " Domain: " ++ show varMap) False
--   where
--     f = simplifyF f1
--     variables = extractVariablesF f


--     variablePoints :: Gen [Integer]
--     variablePoints = vectorOf (int (length variables)) arbitrary

-- prop_verifyCheckECNF :: [[E]] -> Property
-- prop_verifyCheckECNF cnf = 
--   forAllBlind variableDomains $ \domains ->
--     let
--       orderedDomains     = map (\(x, y) -> (min x y, max x y)) domains
--       varMap          = map (\(i, v) -> (v, (rational (fst (orderedDomains !! i)), rational (snd (orderedDomains !! i))))) (zip [0..] variables)
--       checkECNFResult = checkECNFDepthFirstWithSimplex cnf varMap 10 100 1.2 (prec 10)
--       eRanges         = map (map (\e -> applyExpression e varMap (prec 10))) cnf
--       eRangesHasError = any (any hasError) eRanges 
--     in
--       not eRangesHasError ==>
--         case checkECNFResult of
--           (Just False, Just counterExampleDomain) -> 
--             let
--               eRangesC         = map (map (\e -> applyExpression e varMap (prec 10))) cnf
--               eResultC         = map (map (>= 0)) eRangesC
--               eResultKleeneanC = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResultC
--             in
--               case unCN eResultKleeneanC of
--                 CertainFalse -> label "Counterexample verified false" True
--                 CertainTrue  -> 
--                   counterexample 
--                   ("Counterexample proven to be wrong: CNF " 
--                   ++ show cnf ++ " Domain: " 
--                   ++ show varMap
--                    ++ " Counterexample: " 
--                    ++ show counterExampleDomain)
--                    False
--                 TrueOrFalse  -> 
--                   label 
--                   "Counterexample verification returned indeterminate result"
--                   True
--           (Just True, _)  -> label "checkECNFCE returned a true result" True
--           (Nothing, _)    -> label "checkECNFCE returned an indeterminate result" True
--           (Just False, _) -> label "False returned without a counterexample, which should never happen" False
--   where
--     variables = extractVariablesECNF cnf

--     variableDomains :: Gen [(Rational, Rational)]
--     variableDomains = vectorOf (int (length variables)) arbitrary

-- |A version of prop_verifyCheckECNF which specifically checks VCs (FConn Impl _ _)
-- prop_verifyCheckECNFF :: F-> Property
-- prop_verifyCheckECNFF f = 
--   forAllBlind variableDomains $ \domains ->
--     let
--       vc              = f
--       cnf             = minMaxAbsEliminatorECNF epsilon $ fToECNF vc epsilon
--       orderedDomains  = map (\(x, y) -> (min x y, max x y)) domains
--       varMap          = map (\(i, v) -> (v, (rational (fst (orderedDomains !! i)), rational (snd (orderedDomains !! i))))) (zip [0..] variables)
--       checkECNFResult = checkECNFDepthFirstWithSimplex cnf varMap 10 100 1.2 (prec 10)
--       eRanges         = map (map (\e -> applyExpression e varMap (prec 10))) cnf

--       eRangesHasError = any (any hasError) eRanges 
--     in
--       -- T.trace "-----------------"
--       -- T.trace (show f)
--       -- T.trace "-----------------"
--       -- T.trace (show cnf)
--       -- T.trace "-----------------"
--       -- T.trace (show eRanges)
--       -- T.trace "-----------------"
--       -- not (hasError eRanges) ==> This doesn't work, hasError on a list only checks if a list is empty (I think?)
--       not eRangesHasError ==>
--         case checkECNFResult of
--           (Just False, Just counterExampleDomain) -> 
--             let
--               eRangesC         = map (map (\e -> applyExpression e varMap (prec 10))) cnf
--               eResultC         = map (map (>= 0)) eRangesC
--               eResultKleeneanC = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResultC
--             in
--               case unCN eResultKleeneanC of
--                 CertainFalse -> label "Counterexample verified false" True
--                 CertainTrue  -> 
--                   counterexample 
--                   ("Counterexample proven to be wrong: CNF " 
--                   ++ show cnf ++ " Domain: " 
--                   ++ show varMap
--                    ++ " Counterexample: " 
--                    ++ show counterExampleDomain)
--                    False
--                 TrueOrFalse  -> 
--                   label 
--                   "Counterexample verification returned indeterminate result"
--                    True
--           (Just True, _)  -> label "checkECNFCE returned a true result" True
--           (Nothing, _)    -> label "checkECNFCE returned an indeterminate result" True
--           (Just False, _) -> label "False returned without a counterexample, which should never happen" False
--   where
--     variables = extractVariablesF f

--     variableDomains :: Gen [(Rational, Rational)]
--     variableDomains = vectorOf (int (length variables)) arbitrary

-- |A version of prop_verifyCheckECNF which specifically checks VCs (FConn Impl _ _)
-- prop_verifyCheckECNFVC :: F -> F -> Property
-- prop_verifyCheckECNFVC context goal = 
--   forAllBlind variableDomains $ \domains ->
--     let
--       vc              = FConn Impl context goal
--       cnf             = minMaxAbsEliminatorECNF epsilon $ fToECNF vc epsilon
--       orderedDomains  = map (\(x, y) -> (min x y, max x y)) domains
--       varMap          = map (\(i, v) -> (v, (rational (fst (orderedDomains !! i)), rational (snd (orderedDomains !! i))))) (zip [0..] variables)
--       checkECNFResult = checkECNFDepthFirstWithSimplex cnf varMap 10 100 1.2 (prec 100)
--       eRanges         = map (map (\e -> applyExpression e varMap (prec 100))) cnf

--       eRangesHasError = any (any hasError) eRanges 
--     in
--       not eRangesHasError ==>
--         case checkECNFResult of
--           (Just False, Just counterExampleDomain) -> 
--             let
--               eRangesC         = map (map (\e -> applyExpression e counterExampleDomain (prec 100))) cnf
--               eResultC         = map (map (>= 0)) eRangesC
--               eResultKleeneanC = foldl and2 (cn CertainTrue) $ map (foldl or2 (cn CertainFalse)) eResultC
--             in
--               case unCN eResultKleeneanC of
--                 CertainFalse -> label "Counterexample verified false" True
--                 CertainTrue  -> 
--                   counterexample 
--                   ("Counterexample proven to be wrong: CNF " 
--                   ++ show cnf ++ " Domain: " 
--                   ++ show varMap
--                    ++ " Counterexample: " 
--                    ++ show counterExampleDomain)
--                    False
--                 TrueOrFalse  -> 
--                   label 
--                   "Counterexample verification returned indeterminate result"
--                   True
--           (Just True, _)  -> label "checkECNFCE returned a true result" True
--           (Nothing, _)    -> label "checkECNFCE returned an indeterminate result" True
--           (Just False, _) -> label "False returned without a counterexample, which should never happen" False
--   where
--     variables = nub $ extractVariablesF context ++ extractVariablesF goal

--     variableDomains :: Gen [(Rational, Rational)]
--     variableDomains = vectorOf (int (length variables)) arbitrary

-- |Turns all non-strict inequalities into strict inequalities
makeFStrict :: F -> F
makeFStrict (FComp op e1 e2) =
  case op of
    Lt -> FComp Lt e1 e2
    Le -> FComp Lt e1 e2
    Gt -> FComp Gt e1 e2
    Ge -> FComp Gt e1 e2
    Eq -> FComp Eq e1 e2
makeFStrict (FConn op f1 f2) = FConn op (makeFStrict f1) (makeFStrict f2)
makeFStrict (FNot f)         = FNot (makeFStrict f)
makeFStrict FTrue            = FTrue 
makeFStrict FFalse           = FFalse