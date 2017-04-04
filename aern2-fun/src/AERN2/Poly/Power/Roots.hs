module AERN2.Poly.Power.Roots
(
initialBernsteinCoefs,
bernsteinCoefs,
signVars,
reflect,
contract,
translate,
transform,
findRoots,
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

import Debug.Trace

shouldTrace :: Bool
shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

type Terms c = Map Integer c

instance (HasAccuracy c) => HasAccuracy (Terms c) where
  getAccuracy ts = Map.foldl' min (Exact) $ Map.map getAccuracy ts

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
      Nothing   -> maybeTrace("sign of coefficient "++(show d)++" undefined.\ncoefficient is: "++(show $ fromJust $ Map.lookup d ts)) Nothing
      Just sgnx ->
        if sgnx == 0 || sg == 0 || sgnx == sg then
          aux vrs (if sgnx /= 0 then sgnx else sg) (d - 1)
        else
          aux (vrs + 1) sgnx (d - 1)

-- Input: Polynomial P and numbers l, r.
-- Output: the coefficients of P in Bernstein basis on [l,r].
initialBernsteinCoefs :: PowPoly MPBall -> MPBall -> MPBall -> Terms MPBall
initialBernsteinCoefs p l r =
  {-trace("computing initial bs coefs of "++(show p)) $
  trace("radius p "++(show $ radius p)) $
  trace("accuracy p "++(show $ getAccuracy p)) $
  trace("coefs: "++(show $ bs)) $-}
  bs
  where
  d = degree p
  PowPoly (Poly cs) = transform l r p
  binoms = Map.fromList [(k, binom d (d - k)) | k <- [0.. d]]
  bs =
    Map.mapKeys (\k -> d - k) $
    Map.mapWithKey (\k c -> c / (fromJust $ Map.lookup k binoms)) cs

-- Input: (l,r,m, List of coefficients of P in Bernstein basis on [l,r])
-- Output: Lists of coefficients of P in Bernstein basis on [l,m] and [m,r].
-- Note that m does not have to lie between l and r.
-- This is Algorithm 10.2 [Bernstein Coefficients] in
-- Basu, Pollack, Roy: Algorithms in Real Algebraic Geometry
bernsteinCoefs ::
  MPBall -> MPBall -> MPBall -> Terms MPBall -> (Terms MPBall, Terms MPBall)
bernsteinCoefs l r m bsI =
  (buildLeft p Map.empty, buildRight p Map.empty)
  where
  alpha = (r - m)/(r - l)
  beta  = (m - l)/(r - l)
  bijs = buildBijs 0 0 Map.empty
  p = ts_deg bsI
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
      buildBijs i (j + 1) (Map.insert (i,j) (fromJust $ Map.lookup j bsI) tri)
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

findRoots :: PowPoly MPBall -> MPBall -> MPBall -> ((MPBall, MPBall) -> Bool) -> [(MPBall, MPBall)] -- TODO: if signVars == Nothing - recompute or give up? (currently giving up)
findRoots p l r intervalOK =      -- TODO: remove intervalOK and iterate until nix mehr geht
  splitUntilAccurate [(l, r, DontKnow, bsI)] []
  where
  bsI = initialBernsteinCoefs p l r
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  splitUntilAccurate :: [(MPBall, MPBall, HasRoot, Terms MPBall)]
    -> [(MPBall, MPBall)]
    -> [(MPBall, MPBall)]
  splitUntilAccurate [] res = res
  splitUntilAccurate ((a, b, hasRoot, ts) : is) res =
    let
      vars = signVars ts
    in
      maybeTrace (
      "interval " ++ (show (a, b)) ++ "\n" ++
       "coefs: " ++(show ts)++"\n"++
       "vars: "++(show vars)
      ) $
      if vars == Just 0 then
        splitUntilAccurate is res
      else if intervalOK (a, b) then
        splitUntilAccurate is ((a, b):res)
      else if isNothing vars then
        error "findRootsI (line 196): Nothing." --[(l,r)]
      else if hasRoot == Yes || vars == Just 1 then
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
            ((a,m, DontKnow, bsL) : (m,b, DontKnow, bsR) : is) res

{- auxiliary functions -}

binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom n k =  binom n (k - 1) * (n - k + 1) `Prelude.div` k
