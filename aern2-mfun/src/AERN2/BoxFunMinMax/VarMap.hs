module AERN2.BoxFunMinMax.VarMap where

import MixedTypesNumPrelude
import Data.List as L

-- | An assosciation list mapping variable names to rational interval domains
type VarMap = [(String, (Rational, Rational))]

-- | Get the width of the widest interval
width :: VarMap -> Rational
width vMap = 
  L.maximum (map (\(_, ds) -> snd ds - fst ds) vMap)

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