module AERN2.Poly.Power.Roots
(
initialBernsteinCoefs,
initialBernsteinCoefsI,
bernsteinCoefs,
signVars,
reflect,
contract,
translate,
transform,
findRootsI,
Terms
)
where

import Numeric.MixedTypes
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Prelude
import AERN2.MP.Ball hiding (iterateUntilAccurate)
import Data.Maybe

import AERN2.Poly.Power.Type
import AERN2.Poly.Basics hiding (Terms)
import AERN2.Poly.Power.Eval
import AERN2.MP.Dyadic

import Debug.Trace

shouldTrace :: Bool
shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

type Terms c = Map Integer c

ts_deg :: Terms c -> Integer
ts_deg = fst . Map.findMax

signVars :: Terms MPBall -> Maybe Integer
signVars ts =
  aux 0 0 (ts_deg ts)
  where
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  aux vrs _ (-1) = Just vrs
  aux vrs sg d    =
    case sgn (fromJust $ Map.lookup d ts) of
      Nothing   -> Nothing
      Just sgnx ->
        if sgnx == 0 || sg == 0 || sgnx == sg then
          aux vrs (if sgnx /= 0 then sgnx else sg) (d - 1)
        else
          aux (vrs + 1) sgnx (d - 1)

-- Input: l,m and Polynomial P.
-- Output: the coefficients of P in Bernstein basis on [-1,1].
initialBernsteinCoefs :: MPBall -> MPBall -> PowPoly MPBall -> Terms MPBall
initialBernsteinCoefs l r p =
  bs
  where
  d = degree p
  PowPoly (Poly cs) = transform l r p
  binoms = Map.fromList [(k, binom d (d - k)) | k <- [0.. d]]
  bs =
    Map.mapKeys (\k -> d - k) $
    Map.mapWithKey (\k c -> c / (fromJust $ Map.lookup k binoms)) cs

-- Input: Polynomial P.
-- Output: the coefficients of P in Bernstein basis on [-1,1].
initialBernsteinCoefsI :: PowPoly MPBall -> Terms MPBall
initialBernsteinCoefsI p =
  bsI
  where
  d = degree p
  PowPoly (Poly csI) = transform (mpBall $ -1) (mpBall 1) p
  binoms = Map.fromList [(k, binom d (d - k)) | k <- [0.. d]]
  bsI =
    Map.mapKeys (\k -> d - k) $
    Map.mapWithKey (\k c -> c / (fromJust $ Map.lookup k binoms)) csI


-- Input: (l,r,m, List of coefficients of P in Bernstein basis on [l,r])
-- Output: Lists of coefficients of P in Bernstein basis on [l,m] and [m,r].
-- Note that m does not have to lie between l and r.
-- This is Algorithm 10.2 [Bernstein Coefficients] in
-- Basu, Pollack, Roy: Algorithms in Real Algebraic Geometry
bernsteinCoefs ::
  Dyadic -> Dyadic -> Dyadic -> Terms MPBall -> (Terms MPBall, Terms MPBall)
bernsteinCoefs l r m bs =
  (buildLeft p Map.empty, buildRight p Map.empty)
  where
  alpha = (r - m)/(r - l)
  beta  = (m - l)/(r - l)
  bijs = buildBijs 0 0 Map.empty
  p = ts_deg bs
  buildLeft j bs =
    if j < 0 then
      bs
    else
      buildLeft (j - 1) (Map.insert j (fromJust $ Map.lookup (j,0) bijs) bs)
  buildRight j bs =
    if j < 0 then
      bs
    else
      buildRight (j - 1) (Map.insert (p - j) (fromJust $ Map.lookup (j,p - j) bijs) bs)
  buildBijs i j tri =
    if i > p then
      tri
    else if j > p - i then
      buildBijs (i + 1) 0 tri
    else if i == 0 then
      buildBijs i (j + 1) (Map.insert (i,j) (fromJust $ Map.lookup j bs) tri)
    else
      let
        b0 = fromJust $ Map.lookup (i - 1, j) tri
        b1 = fromJust $ Map.lookup (i - 1, j + 1) tri
      in
        buildBijs i (j + 1) (Map.insert (i,j) (alpha * b0 + beta*b1) tri)

reflect :: PowPoly c -> PowPoly c
reflect poly@(PowPoly (Poly ts)) =
  PowPoly $ Poly ts'
  where
  ts' = Map.mapKeys (\p -> deg - p) ts
  deg = degree poly

translate :: MPBall -> PowPoly MPBall -> PowPoly MPBall
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

transform :: MPBall -> MPBall -> PowPoly MPBall -> PowPoly MPBall
transform l r =
  (translate (mpBall $ -1)) .
    (reflect) .
    (contract (r - l)) .
    (translate (-l))

{- root finding algorithm: -}

data HasRoot =
  Yes | DontKnow
  deriving (Prelude.Eq)

instance HasEqAsymmetric HasRoot HasRoot where
  type EqCompareType HasRoot HasRoot = Bool

findRootsI :: PowPoly MPBall -> ((Dyadic, Dyadic) -> Bool) -> [(Dyadic, Dyadic)] -- TODO: if signVars == Nothing - recompute or give up? (currently giving up)
findRootsI p intervalOK =
  splitUntilAccurate [(dyadic $ -1, dyadic 1, DontKnow, bsI)] []
  where
  bsI = initialBernsteinCoefsI p
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  splitUntilAccurate :: [(Dyadic, Dyadic, HasRoot, Terms MPBall)]
    -> [(Dyadic, Dyadic)]
    -> [(Dyadic, Dyadic)]
  splitUntilAccurate [] res = res
  splitUntilAccurate ((l, r, hasRoot, ts) : is) res =
    let
      vars = signVars ts
    in
      maybeTrace (
      "interval " ++ (show (mpBall l, mpBall r)) ++ "\n" ++
       "coefs: " ++(show ts)++"\n"++
       "vars: "++(show vars)
      ) $
      if vars == Just 0 then
        splitUntilAccurate is res
      else if intervalOK (l,r) then
        splitUntilAccurate is ((l,r):res)
      else if isNothing vars then
        error "findRootsI (line 196): Nothing." --[(l,r)]
      else if hasRoot == Yes || vars == Just 1 then
        let
          m = 0.5*(l + r)
          sgnM     = sgn $ evalDirect p (mpBall m)  -- TODO: improve data structure to avoid redundant computations
          sgnLeft  = sgn $ evalDirect p (mpBall l)
          sgnRight = sgn $ evalDirect p (mpBall r)
        in
          maybeTrace (
           "m: "++(show m)++"\n"++
           "sgnM: "++(show sgnM)++"\n"++
           "value: "++(show $ (evalDirect p (mpBall m)))++"\n"++
           "comparison: "++ (show $ (evalDirect p (mpBall m) == 0))
          ) $
          if (evalDirect p (mpBall m) == 0) == Just True then
            splitUntilAccurate is ((m,m):res)
          else if isJust sgnM && isJust sgnLeft && isJust sgnRight then
            if sgnLeft /= sgnM then
              splitUntilAccurate ((l, m, Yes, ts):is) res
            else
              splitUntilAccurate ((m, r, Yes, ts):is) res
          else
            splitUntilAccurate is ((l,r):res)
      else
        let
          findM cm =
            let
              sgnM = sgn $ evalDirect p (mpBall cm)
            in
              if isJust sgnM
                && fromJust sgnM /= 0
              then
                cm
              else
                findM $ 0.5*(cm + r) -- TODO: find a better perturbation function
          m = findM $ 0.5*(l + r)
          (bsL, bsR)  = bernsteinCoefs l r m ts
        in
          splitUntilAccurate
            ((l,m, DontKnow, bsL) : (m,r, DontKnow, bsR) : is) res

{- auxiliary functions -}

binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom n k =  binom n (k - 1) * (n - k + 1) `Prelude.div` k
