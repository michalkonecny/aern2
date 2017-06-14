module AERN2.Poly.Power.RootsIntVector
(
    initialBernsteinCoefs
  , bernsteinCoefs
  , signVars
  , reflect
  , contract
  , translate
  , transform
  , findRootsWithAccuracy
--  , findRoots
  --, reduce
  , Terms
)
where

import MixedTypesNumPrelude
import qualified Data.Map as Map
import qualified Prelude
import AERN2.Interval
import AERN2.MP.Ball hiding (iterateUntilAccurate)
import AERN2.MP.Dyadic
import Data.Maybe
import Data.Ratio

import AERN2.Poly.Power.Type
import AERN2.Poly.Basics hiding (Terms)
import AERN2.Poly.Power.Eval

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

--import AERN2.Poly.Power.SignedSubresultant

import Debug.Trace

shouldTrace :: Bool
shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

type Terms = (ErrorBound, Integer, Vector Integer)


{-instance (HasAccuracy c) => HasAccuracy (Terms c) where
  getAccuracy ts = Map.foldl' min (Exact) $ Map.map getAccuracy ts-}

ts_deg :: Terms -> Integer
ts_deg (_, _, ts) = fromIntegral $ V.length ts - 1

{-signVars :: Terms -> Maybe Integer
signVars ts@(e, c, cfs) =
  aux 0 0 0
  where
  ce = c * rational e
  eZero = ce == 0
  sgn0 x
   | x == 0 && eZero  = Just 0
   | x >  ce          = Just 1
   | x < -ce          = Just (-1)
   | otherwise        = Nothing
  sgn x
    | x == 0    = 0
    | x >  0    = 1
    | otherwise = -1
  dts = ts_deg ts
  aux vrs sg d =
    if d > dts then
      Just vrs
    else if d == 0 then
      case sgn0 (cfs ! int d) of
        Nothing   -> Nothing
        Just sgnx ->
          if sgnx == 0 || sg == 0 || sgnx == sg then
            aux vrs (if sgnx /= 0 then sgnx else sg) 1
          else
            aux (vrs + 1) sgnx 1
    else
      let
        sgnx = sgn (cfs ! int d)
      in
      if sgnx == 0 || sg == 0 || sgnx == sg then
        aux vrs (if sgnx /= 0 then sgnx else sg) (d + 1)
      else
        aux (vrs + 1) sgnx (d + 1)-}

signVars :: Terms -> Maybe Integer
signVars ts@(e, c, cfs) =
  aux 0 0 (ts_deg ts)
  where
  ce = c * rational e
  eZero = ce == 0
  sgn x
   | x == 0 && eZero  = Just 0
   | x >  ce          = Just 1
   | x < -ce          = Just (-1)
   | otherwise        = Nothing
  aux vrs _ (-1) = Just vrs
  aux vrs sg d =
    case sgn (cfs ! int d) of
      Nothing   -> Nothing
      Just sgnx ->
        if sgnx == 0 || sg == 0 || sgnx == sg then
          aux vrs (if sgnx /= 0 then sgnx else sg) (d - 1)
        else
          aux (vrs + 1) sgnx (d - 1)

-- Input: l,m and Polynomial P.
-- Output: some positive integer constant c and coefficients of c*P in Bernstein basis on [l,r].
initialBernsteinCoefs ::  PowPoly Integer -> ErrorBound -> Rational -> Rational -> Terms
initialBernsteinCoefs p e l r =
  (e, lambda, bs)
  where
  lI = if l == 1.0 then 2 else 1
  d = degree p
  PowPoly (Poly csI) = transform (-1) (integer lI) $ p
  binoms = V.generate (int $ d + 1) (\k -> binom d (d - k))
  csVect = V.generate (int $ d + 1) (\i -> terms_lookupCoeff csI (integer i))
  bsFrac =
    V.generate
    (int $ d + 1)
    (\k -> toRational (csVect ! k) / toRational (binoms ! int k))
  lambdaI = V.foldl' lcm 1 (V.map denominator bsFrac)
  bsI =
    V.generate
    (int $ d + 1)
    (\k -> numerator $ lambdaI * (bsFrac ! int (d - k)))
  (_, (_,lambdaL, bsL)) = bernsteinCoefs (-1.0) (rational lI) l (e, lambdaI, bsI)
  ( (_, lambda, bs), _) = bernsteinCoefs l (rational lI) r (e, lambdaL, bsL)

-- Input: (l,r,m,c, List of coefficients of c*P in Bernstein basis on [l,r])
-- Output: c' and Lists of coefficients of c'*P in Bernstein basis on [l,m] and [m,r].
-- Note that m does not have to lie between l and r.
-- This is Algorithm 10.3 [Special Bernstein Coefficients] in
-- Basu, Pollack, Roy: Algorithms in Real Algebraic Geometry
bernsteinCoefs :: Rational -> Rational -> Rational -> Terms -> (Terms, Terms)
bernsteinCoefs l r m ts@(e, c, bs) =
  ((e, c', bsL), (e, c', bsR))
  where
  c' = (diff^p)*c
  d  = toRational $ foldl1 lcm $ map denominator [l,r,m]
  l' = numerator $ d*l
  m' = numerator $ d*m
  r' = numerator $ d*r
  diff = r' - l'
  p  = ts_deg ts
  bi = biAcc 1 (Map.singleton 0 bs)
  biAcc i biM =
    if i > p then
      biM
    else
      biAcc
      (i + 1)
      (Map.insert i
        (V.generate
          (int $ p - i + 1)
          (\j ->
            let
              b = fromJust $ Map.lookup (i - 1) biM
            in
            (r' - m')*(b ! int j) + (m' - l')*(b ! int (j + 1))))
            biM)
  bsL =
    V.generate
      (int $ p + 1)
      (\j -> diff^(p - j) * (fromJust (Map.lookup (integer j) bi)) ! int 0)
  bsR =
    V.generate
      (int $ p + 1)
      (\j -> diff^j * (fromJust (Map.lookup (p - j) bi) ! j))

reflect :: PowPoly c -> PowPoly c
reflect poly@(PowPoly (Poly ts)) =
  PowPoly $ Poly ts'
  where
  ts' = Map.mapKeys (\p -> deg - p) ts
  deg = degree poly

translate :: Integer -> PowPoly Integer -> PowPoly Integer
translate t poly@(PowPoly (Poly ts)) =
    translateAcc ((degree poly) - 1) $
      (fromList [(0,terms_lookupCoeff ts (degree poly))])
    where
    translateAcc (-1) poly' = poly'
    translateAcc n poly' =
      let
        c = terms_lookupCoeff ts n
      in
        translateAcc (n - 1) $ c + (shiftRight 1 poly') - (t*poly')

contract :: (CanMulSameType c, CanPow c Integer, PowType c Integer ~ c)
  => c -> PowPoly c -> PowPoly c
contract l (PowPoly (Poly ts)) =
  PowPoly $ Poly $ Map.mapWithKey (\p c -> c*(l^p)) ts

transform :: Integer -> Integer -> PowPoly Integer -> PowPoly Integer
transform l r =
  (translate (-1)) .
    (reflect) .
    (contract (r - l)) .
    (translate (-l))

{- root finding algorithm: -}

data HasRoot =
  Yes | DontKnow
  deriving (Prelude.Eq)

instance HasEqAsymmetric HasRoot HasRoot where
  type EqCompareType HasRoot HasRoot = Bool

{-
  Upper bound on the roots of poly in the open interval (l,r),
  i.e. an ordered list of intervals whose union
  contains all roots. There is no guarantee that every interval in the
  list contains a root.
-}
findRootsWithAccuracy :: PowPoly Integer -> Accuracy -> Rational -> Rational -> [Interval Rational Rational]
findRootsWithAccuracy poly acc l r =
  splitUntilAccurate (Interval l r, bsI, DontKnow)
  where
  bsI = initialBernsteinCoefs poly (errorBound 0) l r -- TODO: allow non-zero error?
  splitUntilAccurate :: (Interval Rational Rational, Terms, HasRoot) -> [Interval Rational Rational]
  splitUntilAccurate (i@(Interval a b), bs, hasRoot) =
    if intervalAccurate i then
      [i]
    else
      case hasRoot of
        Yes      ->
          let
          m  = (a + b)/2
          fa = evalDirect poly a
          fm = evalDirect poly m
          in
          if fa*fm < 0 then
            splitUntilAccurate (Interval a m, bs, Yes)
          else
            splitUntilAccurate (Interval m b, bs, Yes)
        DontKnow ->
          let
            Just vars = signVars bs
            m    = (a + b)/2
            (bsL, bsR) = bernsteinCoefs a b m bs
            pm = (thd bsR) ! (int 0)
          in
            case vars of
              0 -> []
              1 -> splitUntilAccurate (i, (errorBound 0, 0, V.empty), Yes)
              _ ->
                if (pm :: Integer) == 0 then [Interval m m] else []
                ++ splitUntilAccurate (Interval a m, bsL, DontKnow)
                ++ splitUntilAccurate (Interval m b, bsR, DontKnow)
  intervalAccurate (Interval a b) =
    b - a < 0.5^(fromAccuracy $ acc)


{-
findRoots :: PowPoly Integer -> MPBall -> MPBall -> ((MPBall, MPBall) -> Bool) -> [(MPBall, MPBall)] -- TODO: if signVars == Nothing - recompute or give up? (currently giving up)
findRoots p l r intervalOK =      -- TODO: remove intervalOK and iterate until nix mehr geht
  splitUntilAccurate [(l, r, DontKnow, bsI)] []
  where
  lDown = rational $ dyadic (ball_value l) - dyadic (ball_error l)
  rUp   = rational $ dyadic (ball_value r) + dyadic (ball_error r)
  bsI = initialBernsteinCoefs p lDown rUp
  sgn x
   | x == 0 = 0
   | x >  0 = 1
   | x < 0  = -1
  splitUntilAccurate ::
    [(Rational, Rational, HasRoot, (Integer, Terms))]
    -> [(Rational, Rational)]
    -> [(Rational, Rational)]
  splitUntilAccurate [] res = res
  splitUntilAccurate ((a, b, hasRoot, (c, ts)) : is) res =
    let
      vars = signVars ts
    in
      maybeTrace (
      "interval " ++ (show (a, b)) ++ "\n" ++
       "coefs: " ++(show ts)++"\n"++
       "vars: "++(show vars)
      ) $
      if vars == 0 then
        splitUntilAccurate is res
      else if intervalOK (a, b) then
        splitUntilAccurate is ((a, b):res)
      else if hasRoot == Yes || vars == 1 then
        let
          m = 0.5*(a + b)
          sgnM     = sgn $ evalDirect p m  -- TODO: improve data structure to avoid redundant computations
          sgnLeft  = sgn $ evalDirect p a
          sgnRight = sgn $ evalDirect p b
        in
          maybeTrace (
           "m: "++ show m++"\n"++
           "sgnM: "++ show sgnM ++"\n"++
           "value: "++ show (evalDirect p m) ++"\n"++
           "comparison: "++ show (evalDirect p m == 0)
          ) $
          if (evalDirect p m == 0) == Just True then
            splitUntilAccurate is ((m,m):res)
          else if isJust sgnM && isJust sgnLeft && isJust sgnRight then
            if sgnLeft /= sgnM then
              splitUntilAccurate ((a, m, Yes, ts):is) res
            else
              splitUntilAccurate ((m, b, Yes, ts):is) res
          else
            splitUntilAccurate is ((a,b):res)
      else
        let
          findM cm =
            let
              sgnM = sgn $ evalDirect p cm
            in
              if isJust sgnM
                && fromJust sgnM /= 0
              then
                cm
              else
                findM $ 0.5*(cm + b) -- TODO: find a better perturbation function
          m = findM $ 0.5*(a + b)
          (bsL, bsR)  = bernsteinCoefs a b m ts
        in
          splitUntilAccurate
            ((a,m, DontKnow, bsL) : (m,b, DontKnow, bsR) : is) res-}

{- auxiliary functions -}

binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom n k =  binom n (k - 1) * (n - k + 1) `Prelude.div` k

thd :: (a,b,c) -> c
thd (_,_,x) = x
