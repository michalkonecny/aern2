module AERN2.BoxFunMinMax.VarMap where

import MixedTypesNumPrelude
import Data.List as L

type VarMap = [(String, (Rational, Rational))]

width :: VarMap -> Rational
width vMap = 
  L.maximum (map (\(_, ds) -> snd ds - fst ds) vMap)

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

bisectVar :: (String, (Rational, Rational)) -> ((String, (Rational, Rational)), (String, (Rational, Rational)))
bisectVar vMap = bisectedVar
  where
    varCentre = fst dom + (snd dom - fst dom) /! 2 where dom = snd vMap
    bisectedVar = ((var, (fst dom, varCentre)), (var, (varCentre, snd dom)))
      where 
        var = fst vMap
        dom = snd vMap

bisect :: Integer ->  VarMap -> (VarMap, VarMap)
bisect n vMap = 
  (
    map (\v -> if fst v == fst fstBisect then fstBisect else v) vMap,
    map (\v -> if fst v == fst sndBisect then sndBisect else v) vMap
  )
  where
    (fstBisect, sndBisect) = bisectVar (vMap L.!! (int n))
