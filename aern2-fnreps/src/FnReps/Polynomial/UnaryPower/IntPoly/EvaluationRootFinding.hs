module FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding
(
eval,
isolateRoots,
isolateRootsI
)
where

import AERN2.Num
import FnReps.Polynomial.UnaryPower.IntPoly.Basics

import qualified Data.Map as Map
import qualified Prelude as Prelude
import Data.Ratio

eval :: IntPoly -> Integer -> Integer
eval poly@(IntPoly ts) x = evalHornerAcc (degree poly) $ 0
                          where
                          evalHornerAcc 0 sm = x*sm + terms_lookupCoeff ts 0
                          evalHornerAcc k sm = evalHornerAcc (k - 1) $ x*sm + terms_lookupCoeff ts k

isolateRoots :: Rational -> Rational -> IntPoly -> [Interval Rational]
isolateRoots l r p = aux l r lambda0 bs0 []
                     where
                     separableP = remIntPoly p (derivative p)
                     (lambda0,bs0) = initialBernsteinCoefs l r separableP
                     aux l' r' c bs zs = -- "zs" are "boundary zeroes" that would lead to issues in the root approximation phase 
                                        let vrs = signVars bs 
                                            m    = 0.5*(r' + l')
                                        in
                                        if vrs == 0 then
                                            []
                                        else if vrs == 1 then
                                            if l' `elem` zs || r' `elem` zs then -- if there's a zero on the boundary
                                                let (c',bsL, bsR) = bernsteinCoefs l' r' m c bs in
                                                    aux l' m c' bsL zs ++ aux m r' c' bsR zs -- subdivide until the boundary is free of zeroes
                                            else
                                                [Interval l' r']
                                        else let (c',bsL, bsR) = bernsteinCoefs l' r' m c bs in
                                            if fromJust (Map.lookup 0 bsR) == 0 then -- we have bsR_0 = b^{(p)}_0 = P(m)
                                                [Interval m m] ++ aux l' m c' bsL (m : zs) ++ aux m r' c' bsR (m : zs)
                                            else
                                                aux l' m c' bsL zs ++ aux m r' c' bsR zs

isolateRootsI :: IntPoly -> [Interval Rational]
isolateRootsI p = aux (-1.0) 1.0 lambda0 bs0 []
                     where
                     separableP = remIntPoly p (derivative p)
                     (lambda0,bs0) = initialBernsteinCoefsI separableP
                     aux l' r' c bs zs = -- "zs" are "boundary zeroes" that would lead to issues in the root approximation phase 
                                        let vrs = signVars bs 
                                            m    = 0.5*(r' + l')
                                        in
                                        if vrs == 0 then
                                            []
                                        else if vrs == 1 then
                                            if l' `elem` zs || r' `elem` zs then -- if there's a zero on the boundary
                                                let (c',bsL, bsR) = bernsteinCoefs l' r' m c bs in
                                                    aux l' m c' bsL zs ++ aux m r' c' bsR zs -- subdivide until the boundary is free of zeroes
                                            else
                                                [Interval l' r']
                                        else let (c',bsL, bsR) = bernsteinCoefs l' r' m c bs in
                                            if fromJust (Map.lookup 0 bsR) == 0 then -- we have bsR_0 = b^{(p)}_0 = P(m)
                                                [Interval m m] ++ aux l' m c' bsL (m : zs) ++ aux m r' c' bsR (m : zs)
                                            else
                                                aux l' m c' bsL zs ++ aux m r' c' bsR zs

signVars :: Terms -> Integer
signVars ts = fst $ Map.foldl' (\(vrs,sg) c -> if c == 0 || sg == 0 || sgn c == sg then (vrs, if c /= 0 then sgn c else sg) else (vrs + 1, sgn c)) (0,0) ts
              where
              sgn x = if x == 0 then 0 else if x < 0 then -1 else 1

-- Input: l,m and Polynomial P.
-- Output: some positive integer constant c and coefficients of c*P in Bernstein basis on [l,r].
initialBernsteinCoefs :: Rational -> Rational -> IntPoly -> (Integer, Terms)
initialBernsteinCoefs l r p = (lambda, bs)
                         where
                         d = degree p
                         IntPoly csI = transform (-1) 1 p
                         binoms = Map.fromList [(k, binom d (d - k)) | k <- [0.. d]]
                         bsFrac = Map.mapWithKey (\k c -> (toRational c) / (toRational $ fromJust $ Map.lookup k binoms)) csI
                         lambdaI = Map.foldl' lcm 1 (Map.map denominator bsFrac)
                         bsI = Map.mapKeys (\k -> d - k) $ Map.mapWithKey (\k c -> lambdaI*c `Prelude.div` (fromJust $ Map.lookup k binoms)) csI
                         (lambdaL,_,bsL) = bernsteinCoefs (-1.0) 1.0 l lambdaI bsI
                         (lambda, bs, _) = bernsteinCoefs l 1.0 r lambdaL bsL

-- Input: l,m and Polynomial P.
-- Output: some positive integer constant c and coefficients of c*P in Bernstein basis on [-1,1].
initialBernsteinCoefsI :: IntPoly -> (Integer, Terms)
initialBernsteinCoefsI p = (lambdaI,bsI)
                         where
                         d = degree p
                         IntPoly csI = transform (-1) 1 p
                         binoms = Map.fromList [(k, binom d (d - k)) | k <- [0.. d]]
                         bsFrac = Map.mapWithKey (\k c -> (toRational c) / (toRational $ fromJust $ Map.lookup k binoms)) csI
                         lambdaI = Map.foldl' lcm 1 (Map.map denominator bsFrac)
                         bsI = Map.mapKeys (\k -> d - k) $ Map.mapWithKey (\k c -> lambdaI*c `Prelude.div` (fromJust $ Map.lookup k binoms)) csI

binom :: Integer -> Integer -> Integer                         
binom _ 0 = 1
binom n k =  binom n (k - 1) * (n - k + 1) `Prelude.div` k                   

-- Input: (l,r,m,c, List of coefficients of c*P in Bernstein basis on [l,r])
-- Output: c' and Lists of coefficients of c'*P in Bernstein basis on [l,m] and [m,r].
-- Note that m does not have to lie between l and r.
-- This is Algorithm 10.3 [Special Bernstein Coefficients] in 
-- Basu, Pollack, Roy: Algorithms in Real Algebraic Geometry
bernsteinCoefs :: Rational -> Rational -> Rational -> Integer -> Terms -> (Integer, Terms, Terms)
bernsteinCoefs l r m c bs = (diff^p*c, buildLeft p 1 Map.empty, buildRight p 1 Map.empty)
                           where
                           d  = toRational $ foldl1 lcm $ map denominator [l,r,m]
                           l' = numerator $ d*l
                           m' = numerator $ d*m
                           r' = numerator $ d*r
                           diff = r' - l'
                           p = terms_degree bs
                           mp = buildMap 0 0 Map.empty
                           buildRight j prd cs = if j < 0 then
                                                    cs
                                                 else 
                                                    buildRight (j - 1) (prd * diff) (Map.insert (p - j) (prd * (fromJust $ Map.lookup (j,p - j) mp)) cs)
                           buildLeft j prd cs = if j < 0 then
                                                    cs
                                                else 
                                                    buildLeft (j - 1) (prd*diff) (Map.insert j (prd * (fromJust $ Map.lookup (j,0) mp)) cs)
                           buildMap i j tri = if i > p then
                                               tri
                                              else if j > p - i then
                                               buildMap (i + 1) 0 tri
                                              else if i == 0 then
                                               buildMap i (j + 1) (Map.insert (i,j) (terms_lookupCoeff bs j) tri)
                                              else
                                                buildMap i (j + 1) (Map.insert (i,j) ((r' - m')*(fromJust $ Map.lookup (i - 1, j) tri) + (m' - l')*(fromJust $ Map.lookup (i - 1,j + 1) tri)) tri)

reflect :: IntPoly -> IntPoly
reflect poly@(IntPoly ts) = IntPoly ts'
                               where
                               ts' = Map.mapKeys (\p -> deg - p) ts
                               deg = degree poly 

translate :: Integer -> IntPoly -> IntPoly
translate t poly@(IntPoly ts) =
    translateAcc (degree poly - 1) $ (fromList [(0,terms_lookupCoeff ts (degree poly))])
    where
    translateAcc (-1) poly' = poly'
    translateAcc n poly' = let c = terms_lookupCoeff ts n in
                            translateAcc (n - 1) $ c + (shiftRight 1 poly') - (t*poly')

contract :: Integer -> IntPoly -> IntPoly
contract l (IntPoly ts) = IntPoly ts'
                    where
                    ts' = Map.mapWithKey (\p c -> c*l^p) ts
                                    
transform :: Integer -> Integer -> IntPoly -> IntPoly
transform l r = (translate (-1)) . (reflect) . (contract (r - l)) . (translate (-l))

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "."  