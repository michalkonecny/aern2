{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-|
    Module      :  AERN2.BoxFunMinMax.Expressions.DeriveBounds
    Description :  Deriving ranges for variables from hypotheses inside a formula
    Copyright   :  (c) Michal Konecny 2013, 2021
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Deriving ranges for variables from hypotheses inside a formula
-}
module AERN2.BoxFunMinMax.Expressions.DeriveBounds 
(deriveBoundsAndSimplify, evalE_Rational, roundMPBall)
where

import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN

import qualified Prelude as P

import qualified Data.Map as Map
import qualified Data.List as List
-- import qualified Data.Bifunctor as Bifunctor
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.VarMap

import Debug.Trace (trace)
import Data.List

-- examples:

_f1 :: F -- eliminates "x"
_f1 = FConn Impl (FConn And (FComp Le (Var "x") (Lit 1.0)) (FComp Le (Lit 0.0) (Var "x"))) (FComp Eq (Lit 0.0) (Lit 0.0))

_f2 :: F
_f2 = FConn Impl (FConn And (FComp Le (Var "x") (Lit 1.0)) (FComp Le (Lit 0.0) (Var "x"))) (FComp Eq (Var "x") (Lit 0.0))

_f3 :: F -- underivable "x"
_f3 = FConn Impl (FConn And (FComp Le (Var "x") (Lit 1.0)) (FComp Le (Var "x") (Lit 0.0))) (FComp Eq (Var "x") (Lit 0.0))

_f4 :: F -- nested implication containing bound on "x" guarded by a condition on "n"
_f4 =
  FConn Impl 
    (FConn And 
      (FConn And 
        (FComp Le (Var "x") (Lit 1.0)) 
        (FComp Eq (Var "n") (Lit 1.0)))
      (FConn Impl (FComp Eq (Var "n") (Lit 1.0)) (FComp Le (Lit 0.0) (Var "x"))))
    (FComp Eq (Var "x") (Lit 0.0))

type VarName = String

deriveBoundsAndSimplify :: F -> Bool -> (F, VarMap, [VarName])
deriveBoundsAndSimplify form' isCNF =
  (finalSimplifiedF, derivedRangesWithoutPoints, map fst underivedRanges)
    where
    finalSimplifiedF = simplifyF simplifiedFWithSubstitutedPoints

    simplifiedFWithSubstitutedPoints = substitutePoints simplifiedF varsWithPoints

    (derivedRangesWithoutPoints, varsWithPoints) = seperatePoints derivedRanges

    derivedRanges = map removeJust mDerivedRanges

    (mDerivedRanges, underivedRanges) = List.partition isGood varRanges

    isPoint (l, r) = l == r


    substitutePoints :: F -> [(String, Rational)] -> F
    substitutePoints f [] = f
    substitutePoints f ((var, val) : varPoints) = substitutePoints (substituteVarF f var val) varPoints

    seperatePoints :: VarMap -> (VarMap, [(String, Rational)])
    seperatePoints [] = ([], [])
    seperatePoints ((var, bounds) : varMap) = 
      if isPoint bounds
        then (resultingVarMap, (var, fst bounds) : resultingPoints)
        else ((var, bounds) : resultingVarMap, resultingPoints)
      where
        (resultingVarMap, resultingPoints) = seperatePoints varMap

    form = if isCNF then FConn Impl form' FFalse else form'
    -- If given a CNF, make it imply False
    removeJust (v, (Just l, Just r)) = (v, (l, r))
    removeJust _ = error "deriveBounds: removeJust failed"
    varRanges = Map.toList box
    isGood (_v, (Just _,Just _)) = True
    isGood _ = False
    initBox = Map.fromList $ zip (extractVariablesF form) (repeat (Nothing, Nothing))
    (box, simplifiedF) = aux initBox $ form
      where
      aux b f 
        | b P.== b2 = (b, f2)
        | otherwise = aux b2 f2
        where
        f2 = -- TODO: if this is an implication, we could just do \(FConn Impl context goal) -> FConn Impl (simplifyF context) (simplifyF goal)
          if isCNF 
            then (\(FConn Impl context goal) -> FConn Impl (simplifyF context) goal) (evalF_comparisons b f) 
            else simplifyF $ evalF_comparisons b f
              -- simplify where possible with the knowledge we are restricted to box b
              -- If we are dealing with a CNF, only simplify the context (the goal will be FFalse)
        b' = Map.intersection b $ Map.fromList $ zip (extractVariablesF f2) (repeat ())
              -- remove variables that do not appear in f2
        b2 = scanHypotheses f2 b'
              -- attempt to improve the bounds on the variables

type VarBoundMap = Map.Map VarName (Maybe Rational, Maybe Rational)

scanHypotheses :: F -> VarBoundMap -> VarBoundMap
scanHypotheses (FConn Impl h c) =
    scanHypotheses c . scanHypothesis h False 
scanHypotheses _ = id

-- FIXME: We need FNot here
scanHypothesis :: F -> Bool -> VarBoundMap -> VarBoundMap
scanHypothesis (FNot h) isNegated intervals = scanHypothesis h (not isNegated) intervals 
scanHypothesis (FConn And (FConn Impl cond1 branch1) (FConn Impl (FNot cond2) branch2)) False intervals 
  | cond1 P.== cond2 = scanHypothesis (FConn Or branch1 branch2) False intervals
scanHypothesis (FConn And (FConn Impl cond1 branch1) (FConn Impl (FNot cond2) branch2)) False intervals 
  | sort (simplifyESafeCNF (fToECNF (simplifyF cond1))) P.== sort (simplifyESafeCNF (fToECNF (simplifyF cond2))) = scanHypothesis (FConn Or branch1 branch2) False intervals
scanHypothesis (FConn And h1 h2) isNegated intervals = 
  if isNegated
    then scanHypothesis (FConn Or (FNot h1) (FNot h2)) False intervals
    else (scanHypothesis h1 isNegated . scanHypothesis h2 isNegated) intervals
scanHypothesis (FConn Or h1 h2) isNegated intervals = 
  if isNegated
    then scanHypothesis (FConn And (FNot h1) (FNot h2)) False intervals
    else Map.unionWith mergeWorse box1 box2
      where
      box1 = iterateUntilNoChange (scanHypothesis h1 isNegated) intervals
      box2 = iterateUntilNoChange (scanHypothesis h2 isNegated) intervals
      mergeWorse (l1,r1) (l2,r2) = (min <$> l1 <*> l2, max <$> r1 <*> r2)

      iterateUntilNoChange refineBox b1
        | b1 P.== b2 = b1
        | otherwise = iterateUntilNoChange refineBox b2
        where
          b2 = refineBox b1
      -- iterateUntilNoChange b1 f
      --   | b1 P.== b2 = b1
      --   | otherwise = scanHypothesis f isNegated b1
scanHypothesis (FConn Impl h1 h2) isNegated intervals = scanHypothesis (FConn Or (FNot h1) h2) isNegated intervals
scanHypothesis (FConn Equiv h1 h2) isNegated intervals = scanHypothesis (FConn Or (FConn And h1 h2) (FConn And (FNot h1) (FNot h2))) isNegated intervals 
-- We need: data Comp = Gt | Ge | Lt | Le | Eq
scanHypothesis (FComp Eq _ _) True intervals = intervals
scanHypothesis (FComp Eq _e1@(Var v1) _e2@(Var v2)) False intervals = 
    Map.insert v1 val $
    Map.insert v2 val $
    intervals
    where
    Just val1 = Map.lookup v1 intervals
    Just val2 = Map.lookup v2 intervals
    val = updateUpper val1 $ updateLower val1 $ val2

scanHypothesis (FComp Eq (Var v) e) False intervals = 
    Map.insertWith updateUpper v val $
    Map.insertWith updateLower v val intervals
    where
    val = evalE_Rational intervals e

scanHypothesis (FComp Eq e (Var v)) False intervals = 
    Map.insertWith updateUpper v val $
    Map.insertWith updateLower v val intervals
    where
    val = evalE_Rational intervals e

-- Deal with negated inequalites
scanHypothesis (FComp Le e1 e2) True intervals =
  scanHypothesis (FComp Gt e1 e2) False intervals 
  
scanHypothesis (FComp Lt e1 e2) True intervals =
  scanHypothesis (FComp Ge e1 e2) False intervals 
  
scanHypothesis (FComp Gt e1 e2) True intervals =
  scanHypothesis (FComp Le e1 e2) False intervals 
  
scanHypothesis (FComp Ge e1 e2) True intervals =
  scanHypothesis (FComp Lt e1 e2) False intervals 

scanHypothesis (FComp Le _e1@(Var v1) _e2@(Var v2)) False intervals = 
    Map.insert v1 (updateUpper val2 val1) $
    Map.insert v2 (updateLower val1 val2) $
    intervals
    where
    Just val1 = Map.lookup v1 intervals
    Just val2 = Map.lookup v2 intervals

scanHypothesis (FComp Le (Var v) e) False intervals = 
    Map.insertWith updateUpper v (evalE_Rational intervals e) intervals
scanHypothesis (FComp Le e (Var v)) False intervals = 
    Map.insertWith updateLower v (evalE_Rational intervals e) intervals
-- Bounds for absolute values of Vars
scanHypothesis (FComp Le (EUnOp Abs (Var v)) e) False intervals =
  -- trace (show bounds)
    Map.insertWith updateLower v bounds $ Map.insertWith updateUpper v bounds intervals
    where
    (eValL, eValR) = evalE_Rational intervals e
    bounds         = (fmap negate eValL, eValR)
-- reduce Le, Geq, Ge on equivalent Leq (note that we treat strict and non-strict the same way):
-- Fixme: Some way to treat strict/non-strict with integer variables differently
scanHypothesis (FComp Lt e1 e2) False intervals = scanHypothesis (FComp Le e1 e2) False intervals 
scanHypothesis (FComp Ge e1 e2) False intervals = scanHypothesis (FComp Le e2 e1) False intervals
scanHypothesis (FComp Gt e1 e2) False intervals = scanHypothesis (FComp Le e2 e1) False intervals
scanHypothesis _ _False intervals = intervals

{-|
  Replace within a formula some comparisons with FTrue/FFalse, namely
  those comparisons that on the given box can be easily seen to be true/false.
  -}
evalF_comparisons :: VarBoundMap -> F -> F
evalF_comparisons intervals = eC
  where
  eC FTrue  = FTrue
  eC FFalse = FFalse
  eC (FNot f) = FNot (eC f)
  eC (FConn op f1 f2) = FConn op (eC f1) (eC f2)
  eC (FComp Gt e1 e2) = eC $ FComp Lt e2 e1
  eC (FComp Ge e1 e2) = eC $ FComp Le e2 e1
  eC (FComp Eq e1 e2) = eC $ FConn And (FComp Le e2 e1) (FComp Le e1 e2)
  eC f@(FComp Le e1 e2) =
    case (eE e1, eE e2) of
      ((_, Just e1R), (Just e2L, _)) | e1R <= e2L -> FTrue
      ((Just e1L, _), (_, Just e2R)) | e2R <  e1L -> FFalse 
      _ -> f
  eC f@(FComp Lt e1 e2) =
    case (eE e1, eE e2) of
      ((_, Just e1R), (Just e2L, _)) | e1R <  e2L -> FTrue
      ((Just e1L, _), (_, Just e2R)) | e2R <= e1L -> FFalse 
      _ -> f
  eE = evalE_Rational intervals

evalE_Rational :: 
  VarBoundMap -> E -> (Maybe Rational, Maybe Rational)
evalE_Rational intervals =
  rationalBounds . evalE (cn . mpBallP p) intervalsMPBall
  where
  intervalsMPBall = Map.map toMPBall intervals
  toMPBall :: (Maybe Rational, Maybe Rational) -> CN MPBall
  toMPBall (Just l, Just r) = cn $ (mpBallP p l) `hullMPBall` (mpBallP p r) 
  toMPBall _ = CN.noValueNumErrorCertain $ CN.NumError "no bounds"
  p = prec 100
  rationalBounds :: CN MPBall -> (Maybe Rational, Maybe Rational)
  rationalBounds cnBall =
    case CN.toEither cnBall of
      Right ball -> 
        let (l,r) = endpoints ball in
        (Just (rational l), Just (rational r)) 
      _ -> (Nothing, Nothing)

updateUpper :: 
    CanMinMaxSameType a =>
    (t, Maybe a) -> (t1, Maybe a) -> (t1, Maybe a)
updateUpper (_,Just u2) (l, Just u1) = (l, Just $ min u1 u2)
updateUpper (_,Just u2) (l, Nothing) = (l, Just $ u2)
updateUpper (_,Nothing) (l, Just u1) = (l, Just $ u1)
updateUpper (_,Nothing) (l, Nothing) = (l, Nothing)
--updateUpper _ _ = error "DeriveBounds: updateUpper failed"

updateLower :: 
    CanMinMaxSameType a =>
    (Maybe a, t) -> (Maybe a, t1) -> (Maybe a, t1)
updateLower (Just l2,_) (Just l1,u) = (Just $ max l1 l2, u)
updateLower (Just l2,_) (Nothing,u) = (Just $ l2, u)
updateLower (Nothing,_) (Just l1,u) = (Just $ l1, u)
updateLower (Nothing,_) (Nothing,u) = (Nothing, u)
--updateLower _ _ = error "DeriveBounds: updateLower failed"

-- | compute the value of E with Vars at specified points
-- | (a generalised version of computeE)
evalE :: 
  (Ring v, CanDivSameType v, CanPowBy v Integer,
   CanMinMaxSameType v, CanAbsSameType v, 
   CanPowBy v v, CanSqrtSameType v, CanSinCosSameType v,
   IsInterval v, CanAddThis v Integer, HasDyadics v, CanMinMaxSameType (IntervalEndpoint v), _
  ) 
  =>
  (Rational -> v) ->
  Map.Map VarName v -> E -> v
evalE fromR (varMap :: Map.Map VarName v) = evalVM
  where
  evalVM :: E -> v
  evalVM (EBinOp op e1 e2) = 
    case op of
      Min -> evalVM e1 `min` evalVM e2
      Max -> evalVM e1 `max` evalVM e2
      Add -> evalVM e1 + evalVM e2
      Sub -> evalVM e1 - evalVM e2
      Mul -> evalVM e1 * evalVM e2
      Div -> evalVM e1 / evalVM e2
      Pow -> evalVM e1 ^ evalVM e2 
  evalVM (EUnOp op e) =
    case op of
      Abs -> abs (evalVM e)
      Sqrt -> sqrt (evalVM e)
      Negate -> negate (evalVM e)
      Sin -> sin (evalVM e)
      Cos -> cos (evalVM e)
  evalVM (Var v) = 
    case Map.lookup v varMap of
      Nothing -> 
        error ("evalE: varMap does not contain variable " ++ show v)
      Just r -> r
  evalVM Pi      = cn (piBallP (prec 1000))
  evalVM (Lit i) = (fromR i)
  evalVM (PowI e i) = evalVM e  ^ i
  evalVM (Float32 _ e) = (onePlusMinusEpsilon * (evalVM e)) + zeroPlusMinusEpsilon
    where
      eps :: v
      eps = convertExactly $ dyadic $ 0.5^23
      onePlusMinusEpsilon :: v
      onePlusMinusEpsilon = fromEndpointsAsIntervals (1 + (-eps)) (1 + eps)
      epsD :: v
      epsD = convertExactly $ dyadic $ 0.5^149
      zeroPlusMinusEpsilon :: v
      zeroPlusMinusEpsilon = fromEndpointsAsIntervals (-epsD) epsD
  evalVM (Float64 _ e) = (onePlusMinusEpsilon * (evalVM e)) + zeroPlusMinusEpsilon
    where
      eps :: v
      eps = convertExactly $ dyadic $ 0.5^52
      onePlusMinusEpsilon :: v
      onePlusMinusEpsilon = fromEndpointsAsIntervals  (1 + (-eps)) (1 + eps)
      epsD :: v
      epsD = convertExactly $ dyadic $ 0.5^1074
      zeroPlusMinusEpsilon :: v
      zeroPlusMinusEpsilon = fromEndpointsAsIntervals (-epsD) epsD
  evalVM (RoundToInteger mode e) = fmap (roundMPBall mode) (evalVM e)
  evalVM e = error $ "evalE: undefined for: " ++ show e

roundMPBall :: (Real (IntervalEndpoint i), IsInterval i, IsInterval p,
 ConvertibleExactly Integer (IntervalEndpoint p)) =>
  RoundingMode -> i -> p
roundMPBall mode i =
        let 
          (l', r') = endpoints i
          l = toRational l'
          r = toRational r'
          lFloor = floor l
          lCeil = ceiling l
          rFloor = floor r
          rCeil = ceiling r
        in case mode of
          RNE ->
            fromEndpoints
              (if l - lFloor == 0.5
                then (if even lFloor then convertExactly lFloor else convertExactly lCeil) 
                else (if l - lFloor < 0.5 then convertExactly lFloor else convertExactly lCeil))
              (if r - rFloor == 0.5
                then (if even rFloor then convertExactly rFloor else convertExactly rCeil)
                else (if r - rFloor < 0.5 then convertExactly rFloor else convertExactly rCeil))
          RTP -> fromEndpoints (convertExactly lCeil)  (convertExactly rCeil)
          RTN -> fromEndpoints (convertExactly lFloor) (convertExactly rFloor)
          RTZ -> 
            fromEndpoints 
              (if isCertainlyPositive l then convertExactly lFloor else convertExactly lCeil) -- FIXME: check isCertainNegative?
              (if isCertainlyPositive r then convertExactly rFloor else convertExactly rCeil)
          RNA ->
            fromEndpoints
              (if l - lFloor == 0.5
                then (if isCertainlyPositive l then convertExactly lCeil else convertExactly lFloor) -- FIXME: check isCertainNegative?
                else (if l - lFloor < 0.5 then convertExactly lFloor else convertExactly lCeil))
              (if r  - rFloor == 0.5
                then (if isCertainlyPositive r then convertExactly rCeil else convertExactly rFloor)
                else (if r - rFloor < 0.5 then convertExactly rFloor else convertExactly rCeil))
