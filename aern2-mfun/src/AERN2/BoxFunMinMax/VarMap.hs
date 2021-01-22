module AERN2.BoxFunMinMax.VarMap where

import MixedTypesNumPrelude
import Data.List as L
import AERN2.BoxFun.Optimisation
import AERN2.MP.Ball (MPBall, endpoints)
import AERN2.BoxFun.TestFunctions (fromListDomain)
import AERN2.BoxFun.Box (Box)
import qualified AERN2.Linear.Vector.Type as V


import Debug.Trace

-- | An assosciation list mapping variable names to rational interval domains
type VarMap = [(String, (Rational, Rational))]

-- | Get the width of the widest interval
-- Fixme: maxWidth
maxWidth :: VarMap -> Rational
maxWidth vMap = L.maximum (map (\(_, ds) -> snd ds - fst ds) vMap)

-- | Get the sum of the width of each interval
taxicabWidth :: VarMap -> Rational
taxicabWidth vMap = L.sum (map (\(_, ds) -> snd ds - fst ds) vMap)

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
                    where bn = bisect n vMap

-- | Bisect the domain of the given Var, resulting in a pair
-- Vars
bisectVar :: (String, (Rational, Rational)) -> ((String, (Rational, Rational)), (String, (Rational, Rational)))
bisectVar vMap = bisectedVar
  where
    varCentre = fst dom + (snd dom - fst dom) /! 2 where dom = snd vMap
    bisectedVar = ((var, (fst dom, varCentre)), (var, (varCentre, snd dom)))
      where 
        var = fst vMap
        dom = snd vMap

-- | Bisect the given dimension of the given VarMap,
-- resulting in a pair of VarMaps
bisect :: Integer ->  VarMap -> (VarMap, VarMap)
bisect n vMap = 
  (
    map (\v -> if fst v == fst fstBisect then fstBisect else v) vMap,
    map (\v -> if fst v == fst sndBisect then sndBisect else v) vMap
  )
  where
    (fstBisect, sndBisect) = bisectVar (vMap L.!! (int n))

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
centre = map (\(x,(dL,dR)) -> (x, ((dR+dL)/!2,(dR+dL)/!2)))

-- Precondition, box and varNames have same length
fromBox :: Box -> [String] -> VarMap
fromBox box varNames = zip varNames $ V.toList $ V.map (\i -> both (\x -> rational ((~!) x)) (endpoints i)) box
  where
    -- From https://hackage.haskell.org/package/extra-1.7.4/docs/src/Data.Tuple.Extra.html#both
    both :: (a -> b) -> (a, a) -> (b, b)
    both f (x,y) = (f x, f y)

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