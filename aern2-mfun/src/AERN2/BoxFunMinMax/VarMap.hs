{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module AERN2.BoxFunMinMax.VarMap where

import MixedTypesNumPrelude
import Data.List as L
import AERN2.BoxFun.Optimisation
import AERN2.MP.Ball (MPBall, endpoints, fromEndpointsAsIntervals, mpBallP)
import AERN2.BoxFun.TestFunctions (fromListDomain)
import AERN2.BoxFun.Box (Box)
import qualified AERN2.Linear.Vector.Type as V
import Data.Tuple.Extra

import Debug.Trace as T
import Prelude (Ord)
import qualified Prelude as P
import qualified Data.Functor.Contravariant as P
import qualified Data.Functor.Contravariant as P
import AERN2.MP.Precision
import Data.Ratio
-- data VarType = Integer | Real 
  -- deriving (Show, P.Eq, P.Ord) 

-- TODO: Add VarType to VarMap, or make new VarMap type
-- | An assosciation list mapping variable names to rational interval domains
data VarType = Real | Integer
  deriving (Show, P.Eq, P.Ord)

type VarInterval = (String, (Rational, Rational))

data TypedVarInterval = TypedVar VarInterval VarType
  deriving (Show, P.Eq, P.Ord)

type VarMap = [VarInterval]

type TypedVarMap = [TypedVarInterval]


-- instance P.Contravariant VarInterval where

-- | Get the width of the widest interval
-- Fixme: maxWidth
maxWidth :: VarMap -> Rational
maxWidth vMap = L.maximum (map (\(_, ds) -> snd ds - fst ds) vMap)

typedMaxWidth :: TypedVarMap -> Rational
typedMaxWidth vMap = L.maximum (map (\(TypedVar (_, ds) _) -> snd ds - fst ds) vMap)

-- | Get the sum of the width of each interval
taxicabWidth :: VarMap -> Rational
taxicabWidth vMap = L.sum (map (\(_, ds) -> snd ds - fst ds) vMap)

-- | Increase the diameter of all variables in a varMap by the given rational
increaseDiameter :: VarMap -> Rational -> VarMap
increaseDiameter [] _ = []
increaseDiameter ((v, (l, r)) : vs) d = ((v, (l - d, r + d)) : vs)

-- | Increase the radius of all variables in a varMap by the given rational
increaseRadius :: VarMap -> Rational -> VarMap
increaseRadius vm r = increaseDiameter vm (r/2)

-- | Bisect all elements in a given VarMap
fullBisect :: VarMap -> [VarMap]
fullBisect vMap = case L.length vMap of
        0 -> [vMap]
        l ->
            -- y is the dimension bisected in the current iteration
            -- x is a bisection of the previous dimension (tail recursion)
            concatMap (\x -> map (\y -> x ++ [y]) (bisectDimension (l-1))) (fullBisect (L.take (fromIntegral (l-1)) vMap))

            where
                bisectDimension n = [fst bn L.!! (int n), snd bn L.!! (int n)]
                    where bn = bisectN n vMap

-- | Bisect the domain of the given interval, resulting in a pair
-- Vars
bisectInterval :: (String, (Rational, Rational)) -> ((String, (Rational, Rational)), (String, (Rational, Rational)))
bisectInterval (var, (lower, upper)) = bisectedVar
  where
    varCentre = (lower + upper) / 2
    bisectedVar = ((var, (lower, varCentre)), (var, (varCentre, upper)))

bisectTypedInterval :: (String, (Rational, Rational)) -> VarType -> ((String, (Rational, Rational)), (String, (Rational, Rational)))
bisectTypedInterval (var, (lower, upper)) Real = bisectedVar
  where
    varCentre = (lower + upper) / 2
    bisectedVar = ((var, (lower, varCentre)), (var, (varCentre, upper)))
bisectTypedInterval (var, (lower, upper)) Integer = bisectedVar
  where
    varCentre = (lower + upper) / 2
    bisectedVar = ((var, (lower, floor varCentre % 1)), (var, (ceiling varCentre % 1, upper)))

-- | Bisect the given dimension of the given VarMap,
-- resulting in a pair of VarMaps
bisectN :: Integer ->  VarMap -> (VarMap, VarMap)
bisectN n vMap =
  (
    map (\v -> if fst v == fst fstBisect then fstBisect else v) vMap,
    map (\v -> if fst v == fst sndBisect then sndBisect else v) vMap
  )
  where
    (fstBisect, sndBisect) = bisectInterval (vMap L.!! (int n))

bisectVar :: VarMap -> String -> (VarMap, VarMap)
bisectVar [] _ = ([], [])
bisectVar (v@(currentVar, (_, _)) : vm) bisectionVar =
  if currentVar == bisectionVar
    then (leftBisection : vm, rightBisection : vm)
    else (v : leftList, v : rightList)
  where
    (leftBisection, rightBisection) = bisectInterval v
    (leftList, rightList) = bisectVar vm bisectionVar

bisectTypedVar :: TypedVarMap -> String -> (TypedVarMap, TypedVarMap)
bisectTypedVar [] _ = ([], [])
bisectTypedVar (v@((TypedVar i@(currentVar, (_, _)) Real)) : vm) bisectionVar =
  if currentVar == bisectionVar
    then (TypedVar leftBisection Real : vm, TypedVar rightBisection Real : vm)
    else (v : leftList, v : rightList)
  where
    (leftBisection, rightBisection) = bisectTypedInterval i Real
    (leftList, rightList) = bisectTypedVar vm bisectionVar
bisectTypedVar (v@((TypedVar i@(currentVar, (_, _)) Integer)) : vm) bisectionVar =
  if currentVar == bisectionVar
    then (TypedVar leftBisection Integer : vm, TypedVar rightBisection Integer : vm)
    else (v : leftList, v : rightList)
  where
    (leftBisection, rightBisection) = bisectTypedInterval i Integer
    (leftList, rightList) = bisectTypedVar vm bisectionVar


-- | Check whether or not v1 contain v2.
contains :: VarMap -> VarMap -> Bool
contains v1 v2 =
  L.all (\((v1v, (v1l, v1r)), (v2v, (v2l, v2r))) -> v1v == v2v && v1l !<=! v2l && v2r !<=! v1r) (zip v1' v2')
  where
    v1' = sort v1
    v2' = sort v2

-- | Convert VarMap to SearchBox with the provided minimum
toSearchBox :: VarMap -> CN MPBall -> SearchBox
toSearchBox vMap = SearchBox (fromListDomain (map snd vMap))

centre :: VarMap -> VarMap
centre = map (\(x,(dL,dR)) -> (x, ((dR+dL)/2,(dR+dL)/2)))

varMapToBox :: VarMap -> Precision -> Box
varMapToBox vs p = V.fromList $ map (\(_,(l,r)) -> fromEndpointsAsIntervals (cn (mpBallP p l)) (cn (mpBallP p r))) vs

typedVarMapToBox :: TypedVarMap -> Precision -> Box
typedVarMapToBox vs p = V.fromList $ map
  (\case
    TypedVar (_,(l,r)) _ -> fromEndpointsAsIntervals (cn (mpBallP p l)) (cn (mpBallP p r)))
  vs

-- Precondition, box and varNames have same length
boxToVarMap :: Box -> [String] -> VarMap
boxToVarMap box varNames = zip varNames $ V.toList $ V.map (both (rational . unCN) . endpoints) box

unsafeBoxToTypedVarMap :: Box -> [(String, VarType)] -> TypedVarMap
unsafeBoxToTypedVarMap box varNamesWithTypes =
  zipWith
  (\(varName, varType) varBounds ->
    case varType of
      Real -> TypedVar (varName, varBounds) Real
      Integer -> TypedVar (varName, (\(l,r) -> (ceiling l % 1, floor r % 1)) varBounds) Integer -- FIXME: may result in inverted interval
  )
  varNamesWithTypes $ V.toList $ V.map (both (rational . unCN) . endpoints) box

safeBoxToTypedVarMap :: Box -> [(String, VarType)] -> Maybe TypedVarMap
safeBoxToTypedVarMap box varNamesWithTypes =
  if any (\(TypedVar (_,(l, r)) _) -> l > r) unsafeTypedVarMap then Nothing else Just unsafeTypedVarMap
  where
    unsafeTypedVarMap = unsafeBoxToTypedVarMap box varNamesWithTypes

typedVarMapToVarMap :: TypedVarMap -> VarMap
typedVarMapToVarMap =
  map
  (\case TypedVar vm _ -> vm)

unsafeVarMapToTypedVarMap :: VarMap -> [(String, VarType)] -> TypedVarMap
unsafeVarMapToTypedVarMap [] _ = []
unsafeVarMapToTypedVarMap ((v, (l, r)) : vs) varTypes =
  case lookup v varTypes of
    Just Real    -> TypedVar (v, (l, r)) Real : unsafeVarMapToTypedVarMap vs varTypes
    Just Integer -> TypedVar (v, (ceiling l % 1, floor r % 1)) Integer : unsafeVarMapToTypedVarMap vs varTypes
    Nothing      -> TypedVar (v, (l, r)) Real : unsafeVarMapToTypedVarMap vs varTypes

safeVarMapToTypedVarMap :: VarMap -> [(String, VarType)] -> Maybe TypedVarMap
safeVarMapToTypedVarMap [] _ = Just []
safeVarMapToTypedVarMap ((v, (l, r)) : vs) varTypes =
  case lookup v varTypes of
    Just Real    ->
      case safeVarMapToTypedVarMap vs varTypes of
        Just rs -> Just $ TypedVar (v, (l, r)) Real : rs
        Nothing -> Nothing
    Just Integer ->
      if ceiling l > floor r
        then Nothing
        else
          case safeVarMapToTypedVarMap vs varTypes of
            Just rs -> Just $ TypedVar (v, (ceiling l % 1, floor r % 1)) Integer : rs
            Nothing -> Nothing
    Nothing      ->
      case safeVarMapToTypedVarMap vs varTypes of
        Just rs -> Just $ TypedVar (v, (l, r)) Real : rs
        Nothing -> Nothing

isVarMapInverted :: VarMap -> Bool
isVarMapInverted []                 = False
isVarMapInverted ((_, (l, r)) : vs) = l > r || isVarMapInverted vs

isTypedVarMapInverted :: TypedVarMap -> Bool
isTypedVarMapInverted []                              = False
isTypedVarMapInverted ((TypedVar (_, (l, r)) _) : vs) = l > r || isTypedVarMapInverted vs

getVarNamesWithTypes :: TypedVarMap -> [(String, VarType)]
getVarNamesWithTypes = map
  (\case
    TypedVar (v, (_,_)) t -> (v,t)
  )

getCorners :: VarMap -> [VarMap]
getCorners vm =
  nub . map sort $ map (\vm'@(v,_) -> vm' : filter (\(v',_) -> v /= v') rights)  lefts
                   ++ map (\vm'@(v,_) -> vm' : filter (\(v',_) -> v /= v') lefts)  lefts
                   ++ map (\vm'@(v,_) -> vm' : filter (\(v',_) -> v /= v') rights) rights
                   ++ map (\vm'@(v,_) -> vm' : filter (\(v',_) -> v /= v') lefts)  rights
  where
    lefts  = map (\(v,(l,_)) -> (v,(l,l))) vm
    rights = map (\(v,(_,r)) -> (v,(r,r))) vm

-- Order for two dimension VarMap, left bottom right top
getEdges :: VarMap -> [VarMap]
getEdges vm =
  nub . map sort $ map (\vm'@(v,_) -> vm' : filter (\(v',_) -> v /= v') vm)  lefts
                ++ map (\vm'@(v,_) -> vm' : filter (\(v',_) -> v /= v') vm) rights
  where
    lefts  = map (\(v,(l,_)) -> (v,(l,l))) vm
    rights = map (\(v,(_,r)) -> (v,(r,r))) vm

upperbound :: VarMap -> VarMap
upperbound = map (\(v,(_,r)) -> (v, (r, r)))

lowerbound :: VarMap -> VarMap
lowerbound = map (\(v,(l,_)) -> (v, (l, l)))


-- |Intersect two varMaps
-- This assumes that both VarMaps have the same variables in the same order
intersectVarMap :: VarMap -> VarMap -> VarMap
intersectVarMap =
  zipWith
    (\(v, (l1, r1)) (_, (l2, r2)) ->
      (v,
      (
        max l1 l2,
        min r1 r2
      )
      )
    )

-- | Returns the widest interval in the given VarMap
widestInterval :: VarMap -> (String, (Rational, Rational)) -> (String, (Rational, Rational))
widestInterval [] widest = widest
widestInterval (current@(_, (cL, cR)) : vm) widest@(_, (wL, wR)) =
  if widestDist >= currentDist then widestInterval vm widest else widestInterval vm current
  where
    widestDist = abs(wR - wL)
    currentDist = abs(cR - cL)

widestTypedInterval :: TypedVarMap -> (String, (Rational, Rational)) -> (String, (Rational, Rational))
widestTypedInterval [] widest = widest
widestTypedInterval (TypedVar current@(_, (cL,cR)) _ : vm) widest@(_, (wL, wR)) =
  if widestDist >= currentDist then widestTypedInterval vm widest else widestTypedInterval vm current
  where
    widestDist = abs(wR - wL)
    currentDist = abs(cR - cL)

typedVarIntervalToVarInterval :: TypedVarInterval -> VarInterval
typedVarIntervalToVarInterval (TypedVar vi _) = vi

-- | Get all the possible edges of a given VarMap as a list of VarMaps
-- Examples:
-- edges [("x", (0.5, 2.0))]                    = 
--   [[("x",(1 % 2,1 % 2))],[("x",(2 % 1,2 % 1))]]
-- edges [("x", (0.5, 2.0)), ("y", (0.8, 1.8))] = 
--   [[("x",(1 % 2,1 % 2)),("y",(4 % 5,4 % 5))],
--   [("x",(1 % 2,1 % 2)),("y",(9 % 5,9 % 5))],
--   [("x",(2 % 1,2 % 1)),("y",(4 % 5,4 % 5))],
--   [("x",(2 % 1,2 % 1)),("y",(9 % 5,9 % 5))]]

-- [("x", (0.5, 2.0)), ("y" (0.8, 0.8))]
-- [("x", (0.5, 2.0)), ("y" (1.8, 1.8))]
-- [("x", (0.5, 0.5)), ("y" (0.8, 1.8))]
-- [("x", (2.0, 2.0)), ("y" (0.8, 1.8))]
-- edges :: VarMap -> [VarMap]
-- edges vs =  (map (\(v, d) -> (filter (\(v', _) -> v /= v') vs)) vs)
-- where
--   points = []
--   points ([(v, (l, r))] : vs = [(v ((l, l), (r, r)))] ++ points vs

-- edges :: VarMap -> [VarMap]
-- edges vs = 
--   case L.length vs of
--     0 -> [[]]
--     1 -> concatTuple (endpoints (head vs)) []
--     _ -> 
--       -- concatMap ((\eps@((v, _), _) -> concatTuple eps (filter (\(v',_) -> v /= v') vs)) . endpoints) vs
--       -- trace (show (map endpoints vs)) $
--       -- map (\(l@(v,_), r) -> (filterOutVar v vsEdges)) vsEdges
--       -- map (\(l@, r)) vsEndpoints
--       -- joinEdges . sortAllEdges $ map endpoints vs
--       -- trace (show vsEndpoints) $
--       [l : leftEndpoints] ++ [r : leftEndpoints] ++ [l : rightEndpoints] ++ [r : rightEndpoints]

--   where
--     leftEndpoints = map fst (tail vsEndpoints)
--     rightEndpoints = map snd (tail vsEndpoints)
--     vsEndpoints = map endpoints vs
--     (l, r) = head vsEndpoints

--     -- fun [] = []
--     -- fun xs@(l',r') = case L.length xs of
--       -- 0 -> []
--       -- 1 -> [l, r]


--     -- vsEdges = (map (\v -> [endpoints v]) vs)
--     filterOutVar x xs = filter (\(x',_) -> x /= x') xs

--     -- joinVM vm (l, r) = (l : vm)

--     endpoints (v, (l, r)) = ((v, (l, l)), (v, (r, r)))

--     concatTuple (l, r) xs = [l : xs, r : xs]

--     joinEdges [] = []
--     joinEdges ((v, d) : es) = 
--       case filterOutSameVars of
--         [] -> []
--         es' ->
--           (map (\vd -> (v, d) : [vd])) es' ++ joinEdges es
--       where
--         filterOutSameVars = (filter (\(v',_) -> v /= v') es)

--     sortAllEdges es = sort . concat $ ls : [rs]
--       where
--         ls = map fst es
--         rs = map snd es

-- [0.5, 0.8, 3.0]
-- [2.0, 1.8]