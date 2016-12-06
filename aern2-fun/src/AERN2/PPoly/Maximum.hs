module AERN2.PPoly.Maximum where

import Numeric.MixedTypes hiding (maximum, minimum)
import qualified Prelude
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Poly.Power.Roots
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import AERN2.Poly.Power.Type
{-import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Derivative-}
import AERN2.Poly.Cheb as Cheb hiding (maximum, minimum, maximumOptimised)
import qualified AERN2.Poly.Cheb as Cheb (maximum, minimum, maximumOptimised)
import AERN2.Poly.Ball
import AERN2.PPoly.Type
import qualified AERN2.PPoly.Eval as PPE
import AERN2.PQueue (PQueue)
import qualified AERN2.PQueue as Q
import Data.List hiding (maximum, minimum, (!!))
import AERN2.Poly.Conversion
import AERN2.Interval

import Debug.Trace

-- instance CanMaximiseOverDom PPoly DyadicInterval where
--   type MaximumOverDomType PPoly DyadicInterval = MPBall
--   maximumOverDom f (Interval l r) = maximum f (mpBall l) (mpBall r)

minimum :: PPoly -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r)

minimumOptimised :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f l r initialDegree steps =
  -(maximumOptimised (-f) l r initialDegree steps)

maximumOptimisedI' :: PPoly -> Integer -> Integer -> MPBall
maximumOptimisedI' (PPoly ps _) initialDegree steps =
  foldl1 max
    [Cheb.maximumOptimised
      (updateRadius (+ (radius p)) $ centre p)
      (setPrecision (getPrecision p) $ mpBall a)
      (setPrecision (getPrecision p) $ mpBall b)
      initialDegree steps
      | (Interval a b,p) <- ps]

maximumOptimised :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised (PPoly ps dom) l r initialDegree steps =
  genericMaximum (PPE.evalDf f dfsCheb) dfsMap maxKeys nodes
  where
  lI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) l)
  rI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) r) -- TODO: properly work out required endpoint precision
  unit    = Interval (dyadic $ -1) (dyadic 1)
  f       = PPoly ps unit
  fs      = map snd ps
  dfsCheb = map (ballLift1R (Cheb.derivative . makeExactCentre)) fs
  dfcsCheb = map (ballLift1R (Cheb.derivative . centre . makeExactCentre)) fs
  dfcsChebReduced =
    map
    (\df ->
      let
        maxKey = ceiling $ (Cheb.degree df - initialDegree) / steps
      in
      [reduceDegree (initialDegree + k*steps) df
        | k <- [0 .. maxKey]])
    dfcsCheb
  --dfsPow  = map (cheb2Power . chPoly_poly . centre) dfsCheb
  dfsPow  = map (map (cheb2Power . chPoly_poly)) dfcsChebReduced
  dfsZipped =
    concatMap
        (\(k, (cdfs, pdfs)) ->
          zip [(k,i) | i <-  [0..]]
              $ zip (map Cheb.evalDirect cdfs) pdfs)
        (zip [0..] $ zip dfcsChebReduced dfsPow)
  dfsMap  = Map.fromList dfsZipped
  maxKeys =
    Map.fromList
      [(k, ceiling $ (Cheb.degree (dfsCheb !! k) - initialDegree) / steps) -- TODO: avoid slow list index lookup
        | k <- [0 .. integer $ length ps - 1]]
  nodes   =
    lI : [setPrecision (getPrecision f) $ mpBall n | n <- nodesI, (lI < n) == Just True, (n < rI) == Just True] ++ [rI]   -- note that the elements of nodesI are balls of radius 0,
  nodesI  =                                                                                                  -- so that if they overlap with lI or rI, then they are contained in it
    let
      ns = map fst ps
    in
      (endpointL $ head ns) : (endpointR $ head ns) : (map endpointR (tail ns))

maximum :: PPoly -> MPBall -> MPBall -> MPBall
maximum (PPoly ps dom) l r =
  genericMaximum (PPE.evalDf f dfsCheb) dfsMap maxKeys nodes
  where
  lI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) l)
  rI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) r) -- TODO: properly work out required endpoint precision
  unit    = Interval (dyadic $ -1) (dyadic 1)
  f       = PPoly ps unit
  fs      = map snd ps
  dfsCheb  = map (ballLift1R (Cheb.derivative . makeExactCentre)) fs
  dfcsCheb = map (ballLift1R (Cheb.derivative . centre . makeExactCentre)) fs
  dfsPow  = map (cheb2Power . chPoly_poly) dfcsCheb
  dfsMap  = Map.fromList $ zip (map (\k -> (k,0)) [0..]) $ zip (map Cheb.evalDirect dfcsCheb) dfsPow
  maxKeys = Map.fromList [(k,0) | k <- [0 .. integer $ length ps - 1]]
  nodes   =
    lI : [setPrecision (getPrecision f) $ mpBall n | n <- nodesI, (lI < n) == Just True, (n < rI) == Just True] ++ [rI]   -- note that the elements of nodesI are balls of radius 0,
  nodesI  =                                                                                                  -- so that if they overlap with lI or rI, then they are contained in it
    let
      ns = map fst ps
    in
      (endpointL $ head ns) : (endpointR $ head ns) : (map endpointR (tail ns))

{- We assume that the function is defined on the interval [-1,1] and that
   the nodes partition a subinterval of [-1,1]
 -}
genericMaximum
  :: (MPBall -> MPBall)
    -> Map (Integer, Integer) (MPBall -> MPBall, PowPoly MPBall)
    -> Map Integer Integer -> [MPBall]
    -> MPBall
genericMaximum f dfs maxKeys nodes =
  splitUntilAccurate initialQueue
  where
  initialQueue =
    let
      xs = initialSingletonIntervals ++ initialSearchIntervals
    in
      foldl' (flip Q.insert) (Q.singleton $ head xs) (tail xs)
  initialSingletonIntervals =
    map singletonInterval nodes
  initialSearchIntervals =
    map fromJust $ filter isJust $
     aux (tail nodes) (head nodes) 0 []
    where
    aux [] _ _ res = res
    aux (r : xs) l n res = aux xs r (n + 1) ((makeSearchInterval l r n):res)
  makeSearchInterval :: MPBall -> MPBall -> Integer -> Maybe MaximisationInterval
  makeSearchInterval l r k =
    let
      df0 = fromJust $ Map.lookup (k,0) dfs
      bsI = initialBernsteinCoefs (snd df0) l r
      (d0 , Just sgnL) = tryFindSign l k 0 -- TODO: Just sgnL assumes exact poly
    in
      case signVars bsI of
        Just 1
          -> let fx = evalfOnInterval l r in Just $ CriticalInterval l r fx Nothing (k,d0) sgnL
        Just 0
          -> Nothing
        _
          -> let fx = evalfOnInterval l r in Just $ SearchInterval l r fx Nothing (k,d0) bsI
  singletonInterval :: MPBall -> MaximisationInterval
  singletonInterval x =
    FinalInterval x x (f x)
  maxKey k = fromJust $ Map.lookup k maxKeys
  evalfOnInterval a b =
    f (fromEndpoints a b)
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  tryFindSign :: MPBall -> Integer -> Integer -> (Integer, Maybe Integer)
  tryFindSign x k n =
    let
      sg = sgn $ (fst $ fromJust $ Map.lookup (k,n) dfs) x
    in
      if isJust sg || n == maxKey k then
        (n, sg)
      else
        tryFindSign x k (n + 1)
  splitUntilAccurate :: PQueue MaximisationInterval -> MPBall
  splitUntilAccurate q =
    let
      Just (mi, q') = Q.minView q
    in
      {-trace (
      "minimum interval "++(show mi)
      ) $-}
      if mi_isAccurate mi (maxKey $ fst $ mi_derivative mi) then
        mi_value mi
      else
        case mi of
          SearchInterval a b v ov (k,n) ts ->
            if isNothing $ signVars ts then -- TODO avoid recomputation of sign variations
              if snd (mi_derivative mi) == maxKey (fst (mi_derivative mi)) then
                v
              else
                splitUntilAccurate $
                  Q.insert
                    (SearchInterval a b v ov (k, n + 1)
                    (initialBernsteinCoefs (snd $ fromJust $ Map.lookup (k, n + 1) dfs) -- TODO: alternatively keep track of Bernstein coefs on [-1,1] for all degrees
                    a b))                                                      --       and compute Bernstein coefs on [l,r] from them. Then we only compute
                    q' -- Recompute with higher degree derivative              --       the initial coefs once per degree.
            else
              let
                findM cm =
                  let
                    (deg, sgM) = tryFindSign cm k n
                  in
                    if isJust sgM
                      && fromJust sgM /= 0
                    then
                      (deg,cm)
                    else
                      findM $ 0.5*(cm + b) -- TODO: find a better perturbation function
                (dm, m) = findM $ 0.5*(a + b) -- TODO: findM assumes that the polynomial is exact.
                (dl, sgnL) = tryFindSign a k n -- sgn $ evalDirect f' (mpBall l)
                (dm', sgnM) = tryFindSign m k n -- sgn $ evalDirect f' (mpBall m)
                (bsL, bsR)  = bernsteinCoefs a b m ts
                varsL = signVars bsL
                varsR = signVars bsR
                miL = if varsL == Just 1 then
                        CriticalInterval a m (evalfOnInterval a m) (Just v) (k, max dl dm') (fromJust sgnL) -- TODO: fromJust assumes that poly is exact
                      else
                        SearchInterval a m (evalfOnInterval a m) (Just v) (k,dm) bsL
                miR = if varsR == Just 1 then
                        CriticalInterval m b (evalfOnInterval m b) (Just v) (k, max dl dm') (fromJust sgnM)
                      else
                        SearchInterval m b (evalfOnInterval m b) (Just v) (k, dm) bsR
              in
                case (varsL == Just 0, varsR == Just 0) of
                  (True, True)   -> splitUntilAccurate q'
                  (False, False) -> splitUntilAccurate
                                      $ Q.insert miL $ Q.insert miR q'
                  (False, True) -> splitUntilAccurate
                                      $ Q.insert miL q'
                  (True, False) -> splitUntilAccurate
                                      $ Q.insert miR q'
          CriticalInterval a b v _ (k,n) sgnL ->
            let
              m = 0.5*(a + b)
              fm = f m
              (dm, sgnM)  = tryFindSign m k n
              (dl, nSgnL) = tryFindSign a k dm
            in
              if sgnM == Just 0 then
                splitUntilAccurate $ Q.insert (CriticalInterval m m fm (Just v) (k, dl) sgnL) q'
              else if isJust sgnM then
                if sgnL /= fromJust sgnM then
                  splitUntilAccurate $ Q.insert (CriticalInterval a m (evalfOnInterval a m) (Just v) (k, dl) $ fromJust nSgnL) q'
                else
                  splitUntilAccurate $ Q.insert (CriticalInterval m b (evalfOnInterval m b) (Just v) (k, dl) $ fromJust sgnM) q'
              else
                v
          FinalInterval {} -> error "generic maximum: this point should never be reached"

data MaximisationInterval =
    SearchInterval MPBall MPBall MPBall (Maybe MPBall) (Integer, Integer) (Terms MPBall) -- the first integer represents the degree of the derivative
  | CriticalInterval MPBall MPBall MPBall (Maybe MPBall) (Integer, Integer) Integer      -- the second integer represents the sign of the left endpoint
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

mi_derivative :: MaximisationInterval -> (Integer, Integer)
mi_derivative (SearchInterval   _ _ _ _ d _) = d
mi_derivative (CriticalInterval _ _ _ _ d _) = d
mi_derivative FinalInterval{} = error "trying to get derivative of final interval"

mi_isAccurate :: MaximisationInterval -> Integer -> Bool
mi_isAccurate FinalInterval{} _ = True
mi_isAccurate mi@(CriticalInterval {}) _ =
  getAccuracy (mi_value mi) == Exact
  || (not (isMoreAccurate (mi_value mi) (mi_oldValue mi)))
mi_isAccurate mi maxKey =
  getAccuracy (mi_value mi) == Exact
  || (snd (mi_derivative mi) == maxKey
  && not (isMoreAccurate (mi_value mi) (mi_oldValue mi)))

instance Prelude.Ord MaximisationInterval where
  (<=) mi0 mi1 =
    fromJust $ u0 >= u1
    where
    (_, u0 :: MPBall) = endpoints $ mi_value mi0
    (_, u1 :: MPBall) = endpoints $ mi_value mi1

isMoreAccurate :: MPBall -> Maybe MPBall -> Bool
isMoreAccurate _ Nothing  = True
isMoreAccurate x (Just y) = ball_error x < ball_error y
