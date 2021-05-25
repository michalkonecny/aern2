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
module AERN2.BoxFunMinMax.Expressions.DeriveBounds where

import MixedTypesNumPrelude

import qualified Prelude as P

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.BoxFunMinMax.Expressions.Type
import AERN2.BoxFunMinMax.VarMap

import Debug.Trace

type VarName = String

deriveBounds :: F -> (VarMap, [VarName])
deriveBounds form =
  let (derivedRanges, underivedRanges) = List.partition isGood varRanges
  in (map removeJust derivedRanges, map fst underivedRanges)
    -- | allGood =
    --     Right (map removeJust varRanges)
    -- | otherwise = 
    --     trace (show varRanges) $ Left errorMessage
    where
    allGood = and $ map isGood varRanges
    removeJust (v, (Just l, Just r)) = (v, (l, r))
    removeJust _ = error "deriveBounds: removeJust failed"
    varRanges = Map.toList box
    isGood (_v, (Just _,Just _)) = True
    isGood _ = False
    errorMessage =
        unlines $ map reportBadVar $ filter (not . isGood) varRanges
        where
        reportBadVar (v, _) =
            "Failed to derive a bound for variable " ++ v ++ " in formula " ++ show form 
    varSet = getFreeVarsF form
    initBox
        = Map.fromAscList $ zip (Set.toAscList varSet) (repeat (Nothing, Nothing))
    box = 
        findRepeat initBox $ tail $ boxSeq
    boxSeq = 
        iterate (scanHypotheses form) initBox

findRepeat :: (P.Eq a, Show a) => a -> [a] -> a
findRepeat prev (next:rest)
    | prev P.== next = prev
    | otherwise = findRepeat next rest
findRepeat prev [] = prev

type VarBoundMap = Map.Map VarName (Maybe Rational, Maybe Rational)

scanHypotheses :: F -> VarBoundMap -> VarBoundMap
scanHypotheses (FConn Impl h c) =
    scanHypotheses c . scanHypothesis h 
scanHypotheses _ = id

scanHypothesis :: F -> VarBoundMap -> VarBoundMap
scanHypothesis (FConn And h1 h2) intervals = 
    (scanHypothesis h1 . scanHypothesis h2) intervals
scanHypothesis (FConn Or h1 h2) intervals = 
    Map.unionWith mergeWorse box1 box2
    where
    box1 = scanHypothesis h1 intervals 
    box2 = scanHypothesis h2 intervals
    mergeWorse (l1,r1) (l2,r2) = (min <$> l1 <*> l2, max <$> r1 <*> r2)
    
-- We need: data Comp = Gt | Ge | Lt | Le | Eq
scanHypothesis (FComp Eq _e1@(Var v1) _e2@(Var v2)) intervals = 
    Map.insert v1 val $
    Map.insert v2 val $
    intervals
    where
    Just val1 = Map.lookup v1 intervals
    Just val2 = Map.lookup v2 intervals
    val = updateUpper val1 $ updateLower val1 $ val2

scanHypothesis (FComp Eq (Var v) e) intervals = 
    Map.insertWith updateUpper v val $
    Map.insertWith updateLower v val intervals
    where
    val = evalE_Rational intervals e

scanHypothesis (FComp Eq e (Var v)) intervals = 
    Map.insertWith updateUpper v val $
    Map.insertWith updateLower v val intervals
    where
    val = evalE_Rational intervals e
    
scanHypothesis (FComp Le _e1@(Var v1) _e2@(Var v2)) intervals = 
    Map.insert v1 (updateUpper val2 val1) $
    Map.insert v2 (updateLower val1 val2) $
    intervals
    where
    Just val1 = Map.lookup v1 intervals
    Just val2 = Map.lookup v2 intervals

scanHypothesis (FComp Le (Var v) e) intervals = 
    Map.insertWith updateUpper v (evalE_Rational intervals e) intervals
scanHypothesis (FComp Le e (Var v)) intervals = 
    Map.insertWith updateLower v (evalE_Rational intervals e) intervals
-- Bounds for absolute values of Vars
scanHypothesis (FComp Le (EUnOp Abs (Var v)) e) intervals =
  -- trace (show bounds)
    Map.insertWith updateLower v bounds $ Map.insertWith updateUpper v bounds intervals
    where
    (eValL, eValR) = evalE_Rational intervals e
    bounds         = (-eValL, eValR)
-- reduce Le, Geq, Ge on equivalent Leq (note that we treat strict and non-strict the same way):
-- Fixme: Some way to treat strict/non-strict with integer variables differently
scanHypothesis (FComp Lt e1 e2) intervals = scanHypothesis (FComp Le e1 e2) intervals 
scanHypothesis (FComp Ge e1 e2) intervals = scanHypothesis (FComp Le e2 e1) intervals
scanHypothesis (FComp Gt e1 e2) intervals = scanHypothesis (FComp Le e2 e1) intervals
scanHypothesis _ intervals = intervals

evalE_Rational :: 
  VarBoundMap -> E -> (Maybe Rational, Maybe Rational)
evalE_Rational intervals =
  rationalBounds . evalE (cn . mpBallP p) intervalsMPBall
  where
  intervalsMPBall = Map.map toMPBall intervals
  toMPBall :: (Maybe Rational, Maybe Rational) -> CN MPBall
  toMPBall (Just l, Just r) = cn $ (mpBallP p l) `hullMPBall` (mpBallP p r) 
  toMPBall _ = noValueCN []
  p = prec 100
  rationalBounds :: CN MPBall -> (Maybe Rational, Maybe Rational)
  rationalBounds cnBall =
    case getMaybeValueCN cnBall of
      Just ball -> 
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


{-
  TODO: move the following to more appropriate modules.
  -}

-- | extract all variables from a formula
getFreeVarsF :: F -> Set.Set VarName
getFreeVarsF (FComp _ e1 e2) = 
  (getFreeVarsE e1) `Set.union` (getFreeVarsE e2)
getFreeVarsF (FConn _ f1 f2) = 
  (getFreeVarsF f1) `Set.union` (getFreeVarsF f2)
getFreeVarsF (FNot f) =
  getFreeVarsF f
getFreeVarsF _ = Set.empty
-- | extract all variables from an expression
getFreeVarsE :: E -> Set.Set VarName
getFreeVarsE (Var v) = 
  Set.singleton v
getFreeVarsE (EBinOp _ e1 e2) = 
  (getFreeVarsE e1) `Set.union` (getFreeVarsE e2)
getFreeVarsE (EUnOp _ e) =
  getFreeVarsE e
getFreeVarsE (PowI e _) =
  getFreeVarsE e
getFreeVarsE _ = Set.empty


-- | compute the value of E with Vars at specified points
-- | (a generalised version of computeE)
evalE :: 
  (Field v, EnsureCN v ~ v,
   CanMinMaxSameType v, CanAbsSameType v, 
   CanPowCNBy v v, CanSqrtSameType v, CanSinCosSameType v,
   IsInterval v, CanAddThis v Integer, HasDyadics v, CanMinMaxSameType (IntervalEndpoint v)
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
      Just r -> cn r
  evalVM (Lit i) = (fromR i)
  evalVM (PowI e i) = evalVM e  ^ i
  evalVM (Float32 _ e) = (onePlusMinusEpsilon * (evalVM e)) + zeroPlusMinusEpsilon
    where
      eps :: v
      eps = convertExactly $ dyadic $ 1/!(2^!23)
      onePlusMinusEpsilon :: v
      onePlusMinusEpsilon = fromEndpointsAsIntervals (1 + (-eps)) (1 + eps)
      epsD :: v
      epsD = convertExactly $ dyadic $ 1/!(2^!149)
      zeroPlusMinusEpsilon :: v
      zeroPlusMinusEpsilon = fromEndpointsAsIntervals (-epsD) epsD
  evalVM (Float64 _ e) = (onePlusMinusEpsilon * (evalVM e)) + zeroPlusMinusEpsilon
    where
      eps :: v
      eps = convertExactly $ dyadic $ 1/!(2^!52)
      onePlusMinusEpsilon :: v
      onePlusMinusEpsilon = fromEndpointsAsIntervals  (1 + (-eps)) (1 + eps)
      epsD :: v
      epsD = convertExactly $ dyadic $ 1/!(2^!1074)
      zeroPlusMinusEpsilon :: v
      zeroPlusMinusEpsilon = fromEndpointsAsIntervals (-epsD) epsD
  evalVM e = error $ "evalE: undefined for: " ++ show e
