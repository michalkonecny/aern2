module AERN2.Poly.Power.Maximum
(
genericMaximumI,
maximumI,
maximumOptimisedI,
maximumNaiveI,
minimumI,
minimumOptimisedI,
minimumNaiveI,
)
where

import Numeric.MixedTypes
import AERN2.Poly.Power.Type
import AERN2.Poly.Power.Eval
import AERN2.Poly.Power.Roots
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import Data.List
import qualified Prelude
import Data.Maybe
import AERN2.Poly.Power.SizeReduction

import AERN2.PQueue (PQueue)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified AERN2.PQueue as Q

import Debug.Trace

shouldTrace :: Bool
shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

data MaximisationInterval =
    SearchInterval Dyadic Dyadic MPBall (Maybe MPBall) Integer (Terms MPBall) -- the first integer represents the degree of the derivative
  | CriticalInterval Dyadic Dyadic MPBall (Maybe MPBall) Integer Integer      -- the second integer represents the sign of the left endpoint
  | FinalInterval Dyadic Dyadic MPBall
  deriving (Prelude.Eq, Show)

mi_value :: MaximisationInterval -> MPBall
mi_value (SearchInterval   _ _ v _ _ _) = v
mi_value (CriticalInterval _ _ v _ _ _) = v
mi_value (FinalInterval _ _ v) = v

mi_oldValue :: MaximisationInterval -> Maybe MPBall
mi_oldValue (SearchInterval   _ _ _ ov _ _) = ov
mi_oldValue (CriticalInterval _ _ _ ov _ _) = ov
mi_oldValue FinalInterval{} = error "trying to get old value of final interval"

mi_derivative :: MaximisationInterval -> Integer
mi_derivative (SearchInterval   _ _ _ _ d _) = d
mi_derivative (CriticalInterval _ _ _ _ d _) = d
mi_derivative FinalInterval{} = error "trying to get derivative of final interval"

mi_isAccurate :: MaximisationInterval -> Integer -> Bool
mi_isAccurate FinalInterval{} _ = True
mi_isAccurate mi maxKey =
  getAccuracy (mi_value mi) == Exact
  || (mi_derivative mi == maxKey
  && not (isMoreAccurate (mi_value mi) (mi_oldValue mi)))

instance Prelude.Ord MaximisationInterval where
  (<=) mi0 mi1 =
    fromJust $ u0 >= u1
    where
    (_, u0 :: MPBall) = endpoints $ mi_value mi0
    (_, u1 :: MPBall) = endpoints $ mi_value mi1

genericMaximumI :: (MPBall -> MPBall) -> Map Integer (PowPoly MPBall) -> MPBall
genericMaximumI f dfs =
  let
    df0 = fromJust $ Map.lookup 0 dfs
    bsI = initialBernsteinCoefsI df0
    (d0 , Just sgnL) = tryFindSign (mpBall $ -1) 0 -- TODO: Just sgnL assumes exact poly
  in
    case signVars bsI of
      Just 1
        -> splitUntilAccurate $
            (let fx = evalfOnInterval (dyadic $ -1) (dyadic $ -1) in Q.insert (FinalInterval (dyadic $  -1) (dyadic $ -1) fx)) $
            (let fx = evalfOnInterval (dyadic 1) (dyadic 1) in Q.insert (FinalInterval (dyadic 1) (dyadic 1) fx)) $
            Q.singleton $
            let fx = evalfOnInterval (dyadic $ -1) (dyadic 1) in CriticalInterval (dyadic $ -1) (dyadic 1) fx Nothing d0 sgnL
      Just 0
        -> splitUntilAccurate $
            (let fx = evalfOnInterval (dyadic $ -1) (dyadic $ -1) in Q.insert (FinalInterval (dyadic $ -1) (dyadic $ -1) fx)) $
            Q.singleton $
            let fx = evalfOnInterval (dyadic 1) (dyadic 1) in FinalInterval (dyadic 1) (dyadic 1) fx
      _
        -> splitUntilAccurate $
            (let fx = evalfOnInterval (dyadic $ -1) (dyadic $ -1) in Q.insert (FinalInterval (dyadic $ -1) (dyadic $ -1) fx)) $
            (let fx = evalfOnInterval (dyadic 1) (dyadic 1) in Q.insert (FinalInterval (dyadic 1) (dyadic 1) fx)) $
            Q.singleton $
            let fx = evalfOnInterval (dyadic $ -1) (dyadic 1) in SearchInterval (dyadic $ -1) (dyadic 1) fx Nothing d0 bsI
  where
  maxKey = fst $ Map.findMax dfs
  evalfOnInterval l r =
    f (fromEndpoints (mpBall l) (mpBall r))
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  tryFindSign :: MPBall -> Integer -> (Integer, Maybe Integer)
  tryFindSign x n =
    let
      sg = sgn $ evalDirect (fromJust $ Map.lookup n dfs) (mpBall x)
    in
      if isJust sg || n == maxKey then
        (n, sg)
      else
        tryFindSign x (n + 1)
  splitUntilAccurate :: PQueue MaximisationInterval -> MPBall
  splitUntilAccurate q =
    let
      Just (mi, q') = Q.minView q
    in
      if mi_isAccurate mi maxKey then
        mi_value mi
      else
        case mi of
          SearchInterval l r v ov k ts ->
            if isNothing $ signVars ts then -- TODO avoid recomputation of sign variations
              if mi_derivative mi == maxKey then
                v
              else
                splitUntilAccurate $
                  Q.insert
                    (SearchInterval l r v ov (k + 1)
                    (initialBernsteinCoefs (mpBall l) (mpBall r)  -- TODO: alternatively keep track of Bernstein coefs on [-1,1] for all degrees
                      (fromJust $ Map.lookup (k + 1) dfs)))       --       and compute Bernstein coefs on [l,r] from them. Then we only compute
                    q' -- Recompute with higher degree derivative --       the initial coefs once per degree.
            else
              let
                findM cm =
                  let
                    (deg, sgM) = tryFindSign (mpBall cm) k
                  in
                    if isJust sgM
                      && fromJust sgM /= 0
                    then
                      (deg,cm)
                    else
                      findM $ 0.5*(cm + r) -- TODO: find a better perturbation function
                (dm, m) = findM $ 0.5*(l + r) -- TODO: findM assumes that the polynomial is exact.
                (dl, sgnL) = tryFindSign (mpBall l) k -- sgn $ evalDirect f' (mpBall l)
                (dm', sgnM) = tryFindSign (mpBall m) k -- sgn $ evalDirect f' (mpBall m)
                (bsL, bsR)  = bernsteinCoefs l r m ts
                varsL = signVars bsL
                varsR = signVars bsR
                miL = if varsL == Just 1 then
                        CriticalInterval l m (evalfOnInterval l m) (Just v) (max dl dm') (fromJust sgnL) -- TODO: fromJust assumes that poly is exact
                      else
                        SearchInterval l m (evalfOnInterval l m) (Just v) dm bsL
                miR = if varsR == Just 1 then
                        CriticalInterval m r (evalfOnInterval m r) (Just v) (max dl dm') (fromJust sgnM)
                      else
                        SearchInterval m r (evalfOnInterval m r) (Just v) dm bsR
              in
                case (varsL == Just 0, varsR == Just 0) of
                  (True, True)   -> splitUntilAccurate q'
                  (False, False) -> splitUntilAccurate
                                      $ Q.insert miL $ Q.insert miR q'
                  (False, True) -> splitUntilAccurate
                                      $ Q.insert miL q'
                  (True, False) -> splitUntilAccurate
                                      $ Q.insert miR q'
          CriticalInterval l r v _ k sgnL ->
            let
              m = 0.5*(l + r)
              fm = f (mpBall m)
              (dm, sgnM)  = tryFindSign (mpBall m) k
              (dl, nSgnL) = tryFindSign (mpBall l) dm
            in
              if sgnM == Just 0 then
                splitUntilAccurate $ Q.insert (CriticalInterval m m fm (Just v) dl sgnL) q'
              else if isJust sgnM then
                if sgnL /= fromJust sgnM then
                  splitUntilAccurate $ Q.insert (CriticalInterval l m (evalfOnInterval l m) (Just v) dl $ fromJust nSgnL) q'
                else
                  splitUntilAccurate $ Q.insert (CriticalInterval m r (evalfOnInterval m r) (Just v) dl $ fromJust sgnM) q'
              else
                v
          FinalInterval {} -> error "generic maximum: this point should never be reached"

maximumI :: PowPoly MPBall -> MPBall
maximumI f =
  genericMaximumI (evalDf f f') (Map.singleton 0 f')
  where
  f' = derivative f

maximumOptimisedI :: PowPoly MPBall -> Integer -> Integer -> MPBall
maximumOptimisedI f initialDegree steps =
  genericMaximumI (evalDf f f') dfs
  where
  f' = derivative f
  initialKey = initialDegree `Prelude.div` steps
  maxKey = degree f `Prelude.div` steps + 1 -- TODO: div with rounding up?
  dfs = Map.fromList [(k - 1, reduceDegreeI f' (steps*k)) | k <- [initialKey..maxKey]]

minimumOptimisedI :: PowPoly MPBall -> Integer -> Integer -> MPBall
minimumOptimisedI f initialDegree steps =
  -(maximumOptimisedI (-f) initialDegree steps)

minimumI :: PowPoly MPBall -> MPBall
minimumI f = -(maximumI (-f))

maximumNaiveI :: PowPoly MPBall -> Rational -> MPBall
maximumNaiveI f eps =
  maybeTrace (
   "roots: " ++ (show roots) ++ "\n" ++
   "critical values: "++(show values)
  ) $
  (foldl1' max values)
    + (fromEndpoints (-(mpBall $ dyadic err)) (mpBall $ dyadic err) :: MPBall) -- TODO: this conversion is somewhat terrible
  where
  fc  = powPoly_centre f
  err = powPoly_radius f
  fc' = derivative_exact fc
  values = critValues ++ boundaryValues
  boundaryValues = map (evalDirect fc) [(mpBall $ -1), (mpBall 1)]
  critValues = map (evalDf fc fc') $ map (\(l,r) -> fromEndpoints (mpBall l) (mpBall r)) roots
  roots  = findRootsI fc' (\(l,r) -> abs (r - l) < eps)

minimumNaiveI :: PowPoly MPBall -> Rational -> MPBall
minimumNaiveI f eps = -(maximumNaiveI (-f) eps)

{- auxiliary functions -}

isMoreAccurate :: MPBall -> Maybe MPBall -> Bool
isMoreAccurate _ Nothing  = True
isMoreAccurate x (Just y) = ball_error x < ball_error y
