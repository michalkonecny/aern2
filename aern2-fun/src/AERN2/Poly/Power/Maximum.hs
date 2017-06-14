module AERN2.Poly.Power.Maximum
(
genericMaximum,
maximum,
maximumOptimised,
maximumNaive,
minimum,
minimumOptimised,
minimumNaive,
)
where

import MixedTypesNumPrelude hiding (maximum, minimum)
import AERN2.Poly.Power.Type
import AERN2.Poly.Power.Eval
import AERN2.Poly.Power.Roots
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

maximum :: PowPoly MPBall -> MPBall -> MPBall -> MPBall
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
  dfsWithEval = Map.fromList [(k,(evalDirect df :: MPBall -> MPBall, df)) | (k,df) <- dfs]
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
minimumNaive f l r eps = -(maximumNaive (-f) l r eps)

genericMaximum
  :: (MPBall -> MPBall)
      -> Map Integer (MPBall -> MPBall, PowPoly MPBall)
      -> Accuracy
      -> MPBall -> MPBall
      -> MPBall
genericMaximum f dfs bts l r =
  let
    df0 = fromJust $ Map.lookup 0 dfs
    bsI = initialBernsteinCoefs (snd df0) l r
    (d0 , Just sgnL) = tryFindSign l 0 -- TODO: Just sgnL assumes exact poly
  in
    case signVars bsI of
      Just 1
        ->  splitUntilAccurate $
            (let fx = evalfOnInterval l l in Q.insert (FinalInterval l l fx)) $
            (let fx = evalfOnInterval r r in Q.insert (FinalInterval r r fx)) $
            Q.singleton $
            let fx = evalfOnInterval l r in
              CriticalInterval l r fx Nothing d0 sgnL
      Just 0
        ->  splitUntilAccurate $
            (let fx = evalfOnInterval l l in Q.insert (FinalInterval l l fx)) $
            Q.singleton $
            let fx = evalfOnInterval r r in FinalInterval r r fx
      _
        ->  splitUntilAccurate $
            (let fx = evalfOnInterval l l in Q.insert (FinalInterval l l fx)) $
            (let fx = evalfOnInterval r r in Q.insert (FinalInterval r r fx)) $
            Q.singleton $
            let fx = evalfOnInterval l r in SearchInterval l r fx Nothing 0 {-d0-} bsI
  where
  maxKey = fst $ Map.findMax dfs
  evalfOnInterval a b =
    let
      aux p q ac =
        let
          try = f (fromEndpoints (setPrecision p a) (setPrecision p b))
        in
          maybeTrace (
          "evaluating on interval "++(show a)++ ", "++(show b)++"\n"++
          "trying precision "++(show p)++"\n"++
          "new accuracy: "++(show $ getAccuracy try)++"\n"++
          "old accuracy: "++(show ac)
          ) $
          if getAccuracy try <= ac then
            try
          else
            aux (p + q) p (getAccuracy try)
    in
      aux (max (getPrecision l) $ max (getPrecision a) (getPrecision b))
          (max (getPrecision l) $ max (getPrecision a) (getPrecision b))  NoInformation
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  tryFindSign :: MPBall -> Integer -> (Integer, Maybe Integer)
  tryFindSign x n =
    let
      val = iterateUntilFixed (fst $ fromJust $ Map.lookup n dfs) x
      sg  = sgn val
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
                --v
                let
                 m = computeMidpoint a b
                 (bsL, bsR)  = bernsteinCoefs a b m ts
                 --sgnM = tryFindSign m k
                 --bsL = initialBernsteinCoefs (snd $ fromJust $ Map.lookup k dfs) a m
                 --bsR = initialBernsteinCoefs (snd $ fromJust $ Map.lookup k dfs) m b
                in
                splitUntilAccurate $
                  Q.insert
                    (SearchInterval a m (evalfOnInterval a m) (Just v) k bsL) $
                  Q.insert
                    (SearchInterval m b (evalfOnInterval m b) (Just v) k bsR)
                    q'
              else
                splitUntilAccurate $
                  Q.insert
                    (SearchInterval a b v ov (k + 1)
                    (initialBernsteinCoefs (snd $ fromJust $ Map.lookup (k + 1) dfs) -- TODO: alternatively keep track of Bernstein coefs on [-1,1] for all degrees
                    a b))                                                      --       and compute Bernstein coefs on [l,r] from them. Then we only compute
                    q' -- Recompute with higher degree derivative              --       the initial coefs once per degree.
            else
              let
                findM cm =
                  let
                    (deg, sgM) = tryFindSign cm k
                  in
                    if isJust sgM
                      && fromJust sgM /= 0
                    then
                      (deg,cm)
                    else
                      findM $ computeMidpoint cm b -- TODO: find a better perturbation function
                (dm, m) = findM $ computeMidpoint a b -- TODO: findM assumes that the polynomial is exact.
                (dl, sgnL) = tryFindSign a k -- sgn $ evalDirect f' (mpBall l)
                (dm', sgnM) = tryFindSign m k -- sgn $ evalDirect f' (mpBall m)
                (bsL, bsR)  = bernsteinCoefs a b m ts
                varsL = signVars bsL
                varsR = signVars bsR
                miL = if varsL == Just 1 then
                        CriticalInterval a m (evalfOnInterval a m) (Just v) (max dl dm') (fromJust sgnL) -- TODO: fromJust assumes that poly is exact
                      else
                        SearchInterval a m (evalfOnInterval a m) (Just v) dm bsL
                miR = if varsR == Just 1 then
                        CriticalInterval m b (evalfOnInterval m b) (Just v) (max dl dm') (fromJust sgnM)
                      else
                        SearchInterval m b (evalfOnInterval m b) (Just v) dm bsR
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
              fm = f m
              (dm, sgnM)  = tryFindSign m k
              (dl, nSgnL) = tryFindSign a dm
            in
              if sgnM == Just 0 then
                splitUntilAccurate $ Q.insert (CriticalInterval m m fm (Just v) dl sgnL) q'
              else if isJust sgnM then
                if sgnL /= fromJust sgnM then
                  splitUntilAccurate $ Q.insert (CriticalInterval a m (evalfOnInterval a m) (Just v) dl $ fromJust nSgnL) q'
                else
                  splitUntilAccurate $ Q.insert (CriticalInterval m b (evalfOnInterval m b) (Just v) dl $ fromJust sgnM) q'
              else
                maybeTrace (
                 "sgnM could not be determined."
                ) $
                v
          FinalInterval {} -> error "generic maximum: this point should never be reached"

data MaximisationInterval =
    SearchInterval MPBall MPBall MPBall (Maybe MPBall) Integer (Terms MPBall) -- the first integer represents the degree of the derivative
  | CriticalInterval MPBall MPBall MPBall (Maybe MPBall) Integer Integer      -- the second integer represents the sign of the left endpoint
  | FinalInterval MPBall MPBall MPBall
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

computeMidpoint :: MPBall -> MPBall -> MPBall
computeMidpoint l r =
  let
    ip = (max (getPrecision l) (getPrecision r))
  in
  aux ip
      ip
      ((l + r)/2)
  where
  aux p q prevM =
    let
      tryM = ((setPrecision p l) + (setPrecision p r))/2
    in
      if getAccuracy tryM <= getAccuracy prevM then
        prevM
      else
        aux (p + q) p tryM

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
