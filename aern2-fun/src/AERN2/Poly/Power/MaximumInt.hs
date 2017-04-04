module AERN2.Poly.Power.MaximumInt
(
genericMaximum,
{-maximum,
maximumOptimised,
maximumNaive,
minimum,
minimumOptimised,
minimumNaive,-}
)
where

import Numeric.MixedTypes hiding (maximum, minimum)
import AERN2.Poly.Power.Type
import AERN2.Poly.Power.Eval
import AERN2.Poly.Power.RootsInt
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import Data.List hiding (maximum, minimum)
import qualified Prelude
import Data.Maybe
import AERN2.Poly.Power.SizeReduction

import AERN2.PQueue (PQueue)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified AERN2.PQueue as Q

import Debug.Trace

shouldTrace :: Bool
shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id

{-maximum :: PowPoly MPBall -> MPBall -> MPBall -> MPBall
maximum f =
  genericMaximum (evalDf f f') (Map.singleton 0 (evalDirect f', f')) (getAccuracy f)
  where
  f' = derivative f

minimum :: PowPoly MPBall -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r)

maximumOptimised :: PowPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f l r initialDegree steps =
  genericMaximum (evalDf f f') dfsWithEval (getAccuracy f) l r
  where
  f' = derivative f
  maxKey = ceiling $ (degree f - initialDegree) / steps
  dfsWithEval = Map.fromList [(k,(evalDirect df, df)) | (k,df) <- dfs]
  dfs = [(k, reduceDegree f' l r (initialDegree + steps*k)) | k <- [0..maxKey]]

minimumOptimised :: PowPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f l r initialDegree steps =
  -(maximumOptimised (-f) l r initialDegree steps)

maximumNaive :: PowPoly MPBall -> MPBall -> MPBall -> Rational -> MPBall
maximumNaive f l r eps =
  maybeTrace (
   "roots: " ++ show roots ++ "\n" ++
   "critical values: "++ show values
  ) $
  (foldl1' max values)
    + (fromEndpoints (-(mpBall $ dyadic err)) (mpBall $ dyadic err) :: MPBall) -- TODO: this conversion is somewhat terrible
  where
  fc  = powPoly_centre f
  err = powPoly_radius f
  fc' = derivative_exact fc
  values = critValues ++ boundaryValues
  boundaryValues = map (evalDirect fc) [l, r]
  critValues = map (evalDf fc fc') $ map (\(a,b) -> fromEndpoints a b) roots
  roots  = findRoots fc' l r (\(a,b) -> (abs (b - a) < eps) == Just True)

minimumNaive :: PowPoly MPBall -> MPBall -> MPBall -> Rational -> MPBall
minimumNaive f l r eps = -(maximumNaive (-f) l r eps)-}

genericMaximum
  :: (MPBall -> MPBall)
      -> Map Integer (MPBall -> MPBall, (ErrorBound, PowPoly Integer))
      -> Accuracy
      -> MPBall -> MPBall
      -> MPBall
genericMaximum f dfs bts lBall rBall =
  let
    l   = rational $ dyadic (ball_value lBall) - dyadic (ball_error lBall)
    r   = rational $ dyadic (ball_value rBall) + dyadic (ball_error rBall)
    (e0, df0) = snd $ fromJust $ Map.lookup 0 dfs
    bsI       = initialBernsteinCoefs df0 e0 l r
    (d0 , Just sgnL) = tryFindSign l (getPrecision lBall) 0 -- TODO: Just sgnL assumes exact poly
  in
    case signVars bsI of
       Just 1 -> splitUntilAccurate $
            (let fx = evalfOnBall lBall in Q.insert (FinalInterval l l fx)) $
            (let fx = evalfOnBall rBall in Q.insert (FinalInterval r r fx)) $
            Q.singleton $
            let fx = evalfOnInterval l r in
              CriticalInterval l r fx Nothing d0 sgnL
       Just 0 -> splitUntilAccurate $
            (let fx = evalfOnBall lBall in Q.insert (FinalInterval l l fx)) $
            Q.singleton $
            let fx = evalfOnBall rBall in FinalInterval r r fx
       _ -> splitUntilAccurate $
            (let fx = evalfOnBall lBall in Q.insert (FinalInterval l l fx)) $
            (let fx = evalfOnBall rBall in Q.insert (FinalInterval r r fx)) $
            Q.singleton $
            let fx = evalfOnInterval l r in SearchInterval l r fx Nothing 0 {-d0-} bsI
  where
  maxKey = fst $ Map.findMax dfs
  evalfOnBall :: MPBall -> MPBall
  evalfOnBall x =
    let
      aux p q ac =
        let
          try  = f (setPrecision p x)
        in
          if getAccuracy try >= bts
          || getAccuracy try <= ac then
            try
          else
            aux (p + q) p (getAccuracy try)
    in
      aux (getPrecision lBall)
          (getPrecision lBall) NoInformation
  evalfOnInterval :: Rational -> Rational -> MPBall
  evalfOnInterval a b =
    let
      aux p q ac =
        let
          x    = (fromEndpoints (mpBallP p a) (mpBallP p b))
          try  = f x
        in
          maybeTrace (
          "evaluating on interval "++(show a)++ ", "++(show b)++"\n"++
          "trying precision "++(show p)++"\n"++
          "new accuracy: "++(show $ getAccuracy try)++"\n"++
          "old accuracy: "++(show ac)
          ) $
          if getAccuracy try >= bts
          || getAccuracy try <= ac then
            try
          else
            aux (p + q) p (getAccuracy try)
    in
      aux (getPrecision lBall)
          (getPrecision lBall) NoInformation
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  tryFindSign :: Rational -> Precision -> Integer -> (Integer, Maybe Integer)
  tryFindSign x p n =
    let
      val = iterateUntilFixed (fst $ fromJust $ Map.lookup n dfs) (mpBallP p x) -- TODO: precision
      sg  = sgn val
    in
      if isJust sg then
        (n, sg)
      else if n == maxKey then
        tryFindSign x (2*p) n
      else
        tryFindSign x p (n + 1)
  splitUntilAccurate :: PQueue MaximisationInterval -> MPBall
  splitUntilAccurate q =
    let
      Just (mi, q') = {-trace("q size: "++(show $ Q.size q)) $-} Q.minView q
    in
      maybeTrace (
      "minimal interval " ++ (show mi)++
      "\nminimal interval value: "++(show $ mi_value mi)
      ) $
      if mi_isAccurate mi maxKey bts then
        maybeTrace (
         "mi accurate."
        ) $
        mi_value mi
      else
        maybeTrace (
         "mi not accurate."
        ) $
        case mi of
          SearchInterval a b v ov k ts ->
            maybeTrace("sign variations: "++(show $ signVars ts)) $
            if isNothing $ signVars ts then -- TODO avoid recomputation of sign variations
              maybeTrace (
               "signVars are undefined."
              ) $
              if mi_derivative mi == maxKey then
                trace ("sign variations are undefined ... giving up.") $
                v
              else
                let
                  (ek, dk) = (snd $ fromJust $ Map.lookup (k + 1) dfs)
                in
                splitUntilAccurate $
                  Q.insert
                    (SearchInterval a b v ov (k + 1)
                    (initialBernsteinCoefs dk ek -- TODO: alternatively keep track of Bernstein coefs on [-1,1] for all degrees
                    a b))                                                      --       and compute Bernstein coefs on [l,r] from them. Then we only compute
                    q' -- Recompute with higher degree derivative              --       the initial coefs once per degree.
            else
              let
                findM cm =
                  let
                    (deg, sgM) = tryFindSign cm (getPrecision lBall) k
                  in
                    if isJust sgM
                      && fromJust sgM /= 0
                    then
                      (deg,cm)
                    else
                      findM $ computeMidpoint cm b -- TODO: find a better perturbation function
                (dm, m) = findM $ computeMidpoint a b -- TODO: findM assumes that the polynomial is exact.
                (dl, sgnL) = tryFindSign a (getPrecision lBall) dm -- sgn $ evalDirect f' (mpBall l)
                (dm', sgnM) = tryFindSign m (getPrecision lBall) dl -- sgn $ evalDirect f' (mpBall m)
                (bsL, bsR)  = bernsteinCoefs a b m ts
                varsL = signVars bsL
                varsR = signVars bsR
                miL = if varsL == Just 1 then
                        CriticalInterval a m (evalfOnInterval a m) (Just v) (max dl dm') (fromJust sgnL) -- TODO: fromJust assumes that poly is exact
                      else
                        SearchInterval a m (evalfOnInterval a m) (Just v) k bsL
                miR = if varsR == Just 1 then
                        CriticalInterval m b (evalfOnInterval m b) (Just v) (max dl dm') (fromJust sgnM)
                      else
                        SearchInterval m b (evalfOnInterval m b) (Just v) k bsR
              in
                case (varsL == Just 0, varsR == Just 0) of
                  (True, True)   -> splitUntilAccurate q'
                  (False, False) -> splitUntilAccurate
                                      $ Q.insert miL $ Q.insert miR q'
                  (False, True) -> splitUntilAccurate
                                      $ Q.insert miL q'
                  (True, False) -> splitUntilAccurate
                                      $ Q.insert miR q'
          CriticalInterval a b v _ k sgnL ->
            let
              m = computeMidpoint a b --0.5*(a + b)
              mBall = (mpBallP (getPrecision lBall) m)
              fm = evalfOnBall mBall
              (dm, sgnM)  = tryFindSign m (getPrecision lBall) k
              (dl, nSgnL) = tryFindSign a (getPrecision lBall) dm
            in
              if sgnM == Just 0 then
                splitUntilAccurate $ Q.insert (CriticalInterval m m fm (Just v) dl sgnL) q'
              else if isJust sgnM then
                if sgnL /= fromJust sgnM then
                  splitUntilAccurate $ Q.insert (CriticalInterval a m (evalfOnInterval a m) (Just v) dl $ fromJust nSgnL) q'
                else
                  splitUntilAccurate $ Q.insert (CriticalInterval m b (evalfOnInterval m b) (Just v) dl $ fromJust sgnM) q'
              else
                trace (
                 "sgnM could not be determined ... giving up."
                ) $
                v
          FinalInterval {} -> error "generic maximum: this point should never be reached"

data MaximisationInterval =
    SearchInterval Rational Rational MPBall (Maybe MPBall) Integer Terms          -- the first integer represents the degree of the derivative
  | CriticalInterval Rational Rational MPBall (Maybe MPBall) Integer Integer      -- the second integer represents the sign of the left endpoint
  | FinalInterval Rational Rational MPBall
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

mi_isAccurate :: MaximisationInterval -> Integer -> Accuracy -> Bool
mi_isAccurate FinalInterval{} _ _ = True
mi_isAccurate mi maxKey bts =
  getAccuracy (mi_value mi) >= bts
  || (mi_derivative mi == maxKey
  && not (isMoreAccurate (mi_value mi) (mi_oldValue mi)))

instance Prelude.Ord MaximisationInterval where
  (<=) mi0 mi1 =
    fromJust $ u0 >= u1
    where
    (_, u0 :: MPBall) = endpoints $ mi_value mi0
    (_, u1 :: MPBall) = endpoints $ mi_value mi1

{- auxiliary functions -}

isMoreAccurate :: MPBall -> Maybe MPBall -> Bool
isMoreAccurate _ Nothing  = True
isMoreAccurate x (Just y) =
  radius x < radius y

computeMidpoint :: Rational -> Rational -> Rational
computeMidpoint l r = 0.5*(l + r)

{-evalDirectAccurately ::
 (Ring t, CanAddSubMulDivBy t Dyadic, CanDivBy t Integer,
  CanAddSubMulBy t c, Ring c, HasAccuracy t,
  HasPrecision t, CanSetPrecision t) =>
    ChPoly c -> t -> t
evalDirectAccurately f =
  iterateUntilFixed (evalDirect f)-}

iterateUntilFixed :: (HasAccuracy t, HasPrecision t, CanSetPrecision t)
  => (t -> t) -> t -> t
iterateUntilFixed f x =
  aux (f x) (getPrecision x + getPrecision x) (getPrecision x)
  where
  aux prevfx p q =
    let
      fx = (f . setPrecision p) x
    in
      if getAccuracy fx <= getAccuracy prevfx then
        prevfx
      else
        aux fx (p + q) p
