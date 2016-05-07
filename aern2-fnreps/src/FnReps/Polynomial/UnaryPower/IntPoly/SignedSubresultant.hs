module FnReps.Polynomial.UnaryPower.IntPoly.SignedSubresultant where

import AERN2.Num
import FnReps.Polynomial.UnaryPower.IntPoly.Basics
import qualified Math.Polynomial as MP
import qualified Data.Map as Map
import qualified Prelude as Prelude
import Data.List as List

separablePart :: IntPoly -> IntPoly
separablePart p = 
  if degree (derivative p) <= 0 then
    p
  else
    snd $ gcdAndgcdFreePart p (derivative p)
  {-
  fromFracPoly sepFrac
  where
  dpFrac = toFracPoly $ derivative p
  pFrac = toFracPoly p
  sepFrac = MP.quotPoly pFrac $ MP.gcdPoly pFrac dpFrac-}


{- For this algoritm, compare Basu, Pollack, Roy, Prop 10.14 -}
gcdAndgcdFreePart :: IntPoly -> IntPoly -> (IntPoly, IntPoly)
gcdAndgcdFreePart p q = 
  (fromFracPoly $ justLookup deggcd sResP, fromFracPoly $ justLookup (deggcd - 1) sResV) 
  where
  (sResP, _, sResV) = extendedSignedSubresultant p q
  deggcd = aux 0
  aux i = 
    if not $ MP.polyIsZero $ justLookup i sResP then
      i
    else
      aux (i + 1) 


-- signed subresultant sequence with discriminant and subdiscriminants.
-- cf. Algorithm 8.23 in Basu, Pollack, Roy
extendedSignedSubresultant :: 
  IntPoly -> IntPoly -> 
  (Map.Map Integer (MP.Poly Rational), Map.Map Integer (MP.Poly Rational), Map.Map Integer (MP.Poly Rational)) 
extendedSignedSubresultant p q = 
  aux1 $ aux0 $ initialiseESS
  where
  pFrac = toFracPoly p
  qFrac = toFracPoly q
  
  epsilon k = (-1.0)^(k*(k - 1) `Prelude.div` 2)
  a k = (MP.polyCoeffs MP.LE pFrac) !!! k
  b k = (MP.polyCoeffs MP.LE qFrac) !!! k
  lcof = head . MP.polyCoeffs MP.BE
  
  degp = integer $ degree p
  degq = integer $ degree q
  
  initialiseESS = 
    (sResP0, sResU0, sResV0, s0, t0, i0, j0)
    where
      sResP0 = 
        foldl'
        (\m k -> Map.insert k (MP.poly MP.LE [0.0]) m)
        ( Map.insert degq ((epsilon $ degp - degq) * ((b degq)^(degp - degq - 1)) * qFrac) $
          Map.fromList [(degp - 1, qFrac), (degp, pFrac)])
        [degq + 1 .. degp - 2]
      sResU0 =
        foldl'
        (\m k -> Map.insert k (MP.poly MP.LE [0.0]) m)
        (Map.insert degq MP.zero $
         Map.fromList [(degp - 1, (MP.poly MP.LE [0.0])), (degp, (MP.poly MP.LE [1.0]))])
        [degq + 1 .. degp - 2]
      sResV0 = 
         foldl'
         (\m k -> Map.insert k (MP.poly MP.LE [0.0]) m)
         (Map.insert degq (MP.poly MP.LE [(epsilon $ degp - degq) * (b degq)^(degp - degq)]) $
          Map.fromList [(degp - 1, MP.one), (degp, MP.zero)])
         [degq + 1 .. degp - 2]
      s0 = foldl' 
             (\m k -> Map.insert k 0.0 m) 
             (Map.insert degq ((epsilon $ degp - degq) * ((b degq)^(degp - degq))) $
              Map.fromList [(degp - 1, b degq) , (degp, 1.0)])
             [degq + 1 .. degp - 2]
      t0 = Map.fromList [(degp,1.0), (degp - 1, b degq)]
      i0 = degp + 1
      j0 = degp
  
  aux1 (sResP, sResU, sResV, s, t, i, j) = 
    (sResP', sResU, sResV)
    where
    sResP' = foldl'
             (\m k -> Map.insert k MP.zero m)
             sResP
             [0 .. j - 2]
  
  aux0 (sResP, sResU, sResV, s, t, i, j) = 
    {-trace 
    (
      "sResP: "++ show sResP ++"\n"++
      "sResU: "++ show sResU ++"\n"++
      "sResV: "++ show sResV ++"\n"++
      "s: "++ show s ++"\n"++
      "t: "++ show t ++"\n"++
      "j - 1: "++ show (j - 1) ++"\n"++
      "i - 1: "++ show (i - 1) ++"\n"
    )
    $-}
    if MP.polyIsZero $ justLookup (j - 1) sResP then
      (sResP, sResU, sResV, s, t, i, j)
    else
      let 
        k = integer $ MP.polyDegree $ justLookup (j - 1) sResP
      in
        if k == j - 1 then
          let
            s' = Map.insert (j - 1) (justLookup (j - 1) t) s
            s2 = (justLookup (j - 1) s')^2
            scale = 1/((justLookup j s') * (justLookup (i - 1) t))
            ck  = MP.quotPoly (s2 * (justLookup (i - 1) sResP)) (justLookup (j - 1) sResP)
            sResP' = Map.insert (k - 1) (scale * (-s2 * (justLookup (i - 1) sResP) + ck * (justLookup (j - 1) sResP))) sResP
            sResU' = Map.insert (k - 1) (scale * (-s2 * (justLookup (i - 1) sResU) + ck * (justLookup (j - 1) sResU))) sResU
            sResV' = Map.insert (k - 1) (scale * (-s2 * (justLookup (i - 1) sResV) + ck * (justLookup (j - 1) sResV))) sResV
            t' = Map.insert (k - 1) (lcof $ justLookup (k - 1) sResP') t
            i' = j
            j' = k
          in
            aux0 (sResP', sResU', sResV', s', t', i', j')
      else if k < j - 1 then
        let
          s' = Map.insert (j - 1) 0.0 s
          (sResP', sResU', sResV', t', s'') = aux01 sResP sResU sResV s' t j k
          (sResP'', sResU'', sResV'', s''') = aux02 sResP' sResU' sResV' s'' t' j k
          (sResP''', sResU''', sResV''') = aux03 sResP'' sResU'' sResV'' s''' t' i j k
          t'' = Map.insert (k - 1) (lcof $ justLookup (k - 1) sResP) t'
          i' = j
          j' = k
        in
          aux0 (sResP''', sResU''', sResV''', s''', t'', i', j')
      else
        error "signed subresultants: this should never happen"
  
  aux01 sResP sResU sResV s t j k = 
    (sResP', sResU', sResV', t', s')
    where
    t' = 
      foldl' 
      (\m d -> Map.insert (j - d - 1) ((-1.0)^d * justLookup (j - 1) m * justLookup (j - d) m/(justLookup j s)) m) 
      t 
      [1 .. j - k - 1]
    s' = Map.insert k (justLookup k t') s
    scale = (justLookup k s')/(justLookup (j - 1) t')
    sResP' = Map.insert k (scale * justLookup (j - 1) sResP) sResP
    sResU' = Map.insert k (scale * justLookup (j - 1) sResU) sResU
    sResV' = Map.insert k (scale * justLookup (j - 1) sResV) sResV
  aux02 sResP sResU sResV s t j k = 
    (sResP', sResU', sResV', s')
    where
    sResP' = 
      foldl'
      (\m l -> Map.insert l MP.zero m)
      sResP
      [j - 2 .. k + 1]
    sResU' = 
      foldl'
      (\m l -> Map.insert l MP.zero m)
      sResU
      [j - 2 .. k + 1]
    sResV' = 
      foldl'
      (\m l -> Map.insert l MP.zero m)
      sResV
      [j - 2 .. k + 1]
    s' = 
      foldl'
      (\m l -> Map.insert l 0.0 m)
      s
      [j - 2 .. k + 1]    
  aux03 sResP sResU sResV s t i j k = 
    (sResP', sResU', sResV') 
    where
    scale = (justLookup k s) * (justLookup (j - 1) t)
    quo   = 1/((justLookup j s) * (justLookup (i - 1) t))
    c = MP.quotPoly (scale * justLookup (i - 1) sResP) (justLookup (j - 1) sResP)
    sResP' = Map.insert (k - 1) (quo*(-scale * (justLookup (i - 1) sResP) + c * (justLookup (j - 1) sResP))) sResP
    sResU' = Map.insert (k - 1) (quo*(-scale * (justLookup (i - 1) sResU) + c * (justLookup (j - 1) sResU))) sResU
    sResV' = Map.insert (k - 1) (quo*(-scale * (justLookup (i - 1) sResV) + c * (justLookup (j - 1) sResV))) sResV

justLookup :: (Ord k, Show k, Show a) => k -> Map.Map k a -> a
justLookup x m = case Map.lookup x m of
                Just y  -> y
                Nothing -> error $ "no such element: "++ show x ++ " in "++ show m