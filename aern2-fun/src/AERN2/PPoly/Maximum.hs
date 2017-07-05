module AERN2.PPoly.Maximum where

import MixedTypesNumPrelude hiding (maximum, minimum)
import qualified Prelude
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Poly.Power.RootsInt
import Data.Maybe
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import AERN2.Poly.Power.Type
{-import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Derivative-}
import AERN2.Poly.Cheb as Cheb hiding (maximum, minimum, maximumOptimised, maximumOptimisedWithAccuracy)
import qualified AERN2.Poly.Cheb as Cheb (maximumOptimised, maximumOptimisedWithAccuracy)
import qualified AERN2.Poly.Cheb.MaximumInt as Cheb (maximumOptimisedWithAccuracyAndBounds)
import AERN2.Poly.Ball hiding (ball_value)
import AERN2.PPoly.Type
import qualified AERN2.PPoly.Eval as PPE
import AERN2.PQueue (PQueue)
import qualified AERN2.PQueue as Q
import Data.List hiding (maximum, minimum, (!!))
import AERN2.Poly.Basics hiding (Terms)
import AERN2.Poly.Conversion
import AERN2.Interval
import Debug.Trace

--import Debug.Trace

-- instance CanMaximiseOverDom PPoly DyadicInterval where
--   type MaximumOverDomType PPoly DyadicInterval = MPBall
--   maximumOverDom f (Interval l r) = maximum f (mpBall l) (mpBall r)

minimum :: PPoly -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r)

minimumOptimised :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f l r initialDegree steps =
  -(maximumOptimised (-f) l r initialDegree steps)

minimumOptimisedWithAccuracy :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> Accuracy -> MPBall
minimumOptimisedWithAccuracy f l r initialDegree steps cutoff =
  -(maximumOptimisedWithAccuracy (-f) l r initialDegree steps cutoff)

maximumOptimisedWithAccuracySimple :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> Accuracy -> MPBall
maximumOptimisedWithAccuracySimple (PPoly ps dom@(Interval dL dR)) l r initialDegree steps cutoffAccuracy =
  foldl1 max
    [Cheb.maximumOptimisedWithAccuracy
      (min cutoffAccuracy (getFiniteAccuracy p))
      (updateRadius (+ err) $ ChPoly dom p bnds)
      (setPrecision (getPrecision p) $ max l (mpBall $ fromUnitIntervalToDom a))
      (setPrecision (getPrecision p) $ min r (mpBall $ fromUnitIntervalToDom b))
      initialDegree steps
      | (i@(Interval a b),(Ball (ChPoly _ p bnds) err)) <- ppoly_pieces f, intersectsLR i]
  where
  fromUnitIntervalToDom x = (dyadic 0.5)*((dR - dL)*x + (dR + dL))
  lI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) l)
  rI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) r) -- TODO: properly work out required endpoint precision
  unit    = Interval (dyadic $ -1) (dyadic 1)
  f       = PPoly ps unit
  lrInterval = Interval (mpBall lI) (mpBall rI)
  intersectsLR (Interval a b) =
    lrInterval `intersects` Interval (mpBall a) (mpBall b)
    && (b == lI) /= Just True
    && (a == rI) /= Just True

maximumOptimisedWithAccuracyAndDerivedBounds :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> Accuracy -> MPBall
maximumOptimisedWithAccuracyAndDerivedBounds f l r iDeg steps acc =
  maximumOptimisedWithAccuracyAndBounds f l r iDeg steps acc lower upper
  where
  rangeBall = PPE.evalDI f (fromEndpoints l r)
  (c,rad) = centreAsBallAndRadius rangeBall
  lower = c - mpBall rad
  upper = c + mpBall rad

maximumOptimisedWithAccuracyAndBounds :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> Accuracy -> MPBall -> MPBall -> MPBall
maximumOptimisedWithAccuracyAndBounds (PPoly ps dom) l r initialDegree steps cutoffAccuracy lower upper =
  ffst $
  foldl'
    (
    \(m, lw, up) ((Interval a b), p) ->
      let
        mu = (fromEndpoints m upper :: MPBall)
        m' = Cheb.maximumOptimisedWithAccuracyAndBounds
              (min cutoffAccuracy (getFiniteAccuracy p))
              (updateRadius (+ (radius p)) $ centre p)
              (setPrecision (getPrecision p) $ max lI (mpBall a))
              (setPrecision (getPrecision p) $ min rI (mpBall b))
              initialDegree steps
              lw up
      in
        if getAccuracy mu >= cutoffAccuracy then
          (mu, lw, up)
        else
          (max m m', max lw m', up)
    )
    (lower, lower, upper)
    [p | p@(i,_) <- ppoly_pieces f, intersectsLR i]
  where
  ffst (a,_,_) = a
  lI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) l)
  rI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) r) -- TODO: properly work out required endpoint precision
  unit    = Interval (dyadic $ -1) (dyadic 1)
  f       = PPoly ps unit
  lrInterval = Interval (mpBall lI) (mpBall rI)
  intersectsLR (Interval a b) =
    lrInterval `intersects` Interval (mpBall a) (mpBall b)
    && (b == lI) /= Just True
    && (a == rI) /= Just True

maximumOptimisedWithAccuracy :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> Accuracy -> MPBall
maximumOptimisedWithAccuracy = maximumOptimisedWithAccuracySimple

maximumOptimisedWithAccuracy' :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> Accuracy -> MPBall
maximumOptimisedWithAccuracy' (PPoly ps dom@(Interval dl dr)) l r initialDegree steps cutoffAccuracy =
  genericMaximum
    (PPE.evalLDf f
      --dfsCheb
      (map
        (\dfc -> (2/!(dr - dl)) * reduceToEvalDirectAccuracy dfc (bits $ 0))
        dfsCheb))
    dfsMap
    maxKeys
    nodes
    cutoffAccuracies
  where
  cutoffAccuracies =
    Map.fromList $ zip [0..]
                  [min cutoffAccuracy (getFiniteAccuracy p) | p <- fs]
  lI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) l)
  rI      = fromDomToUnitInterval dom (setPrecision (getPrecision f) r) -- TODO: properly work out required endpoint precision
  unit    = Interval (dyadic $ -1) (dyadic 1)
  f = PPoly ps unit
  lrInterval = Interval (mpBall lI) (mpBall rI)
  intersectsLR (Interval a b) =
    lrInterval `intersects` Interval (mpBall a) (mpBall b)
    && (b == lI) /= Just True
    && (a == rI) /= Just True
  {-reduceDegreeToAccuracy d g = -- TODO: cutoff accuracy for every interval
    let
      try = reduceDegree d g
    in
      if d >= Cheb.degree g
      || getAccuracy try >= cutoffAccuracy then
        try
      else
        reduceDegreeToAccuracy (d + 5) g-}
  {-fs = map ({-reduceDegreeToAccuracy 5 .-} ballLift1R makeExactCentre . snd)
        $ filter (intersectsLR . fst) ps-}
  fs  = [p | (_,p) <- fps]
  fps = [(i, ({-reduceDegreeToAccuracy 5 .-} ballLift1R makeExactCentre) p) | (i,p) <- ps]
  dfpsCheb =
    map (\(i,p) -> (i,(makeExactCentre . Cheb.derivativeExact . centre) p)) fps
  dfsCheb = map snd dfpsCheb
  dfcsCheb = map snd $ filter (intersectsLR . fst) dfpsCheb
  dfcsChebReduced =
    map
    (\df ->
      let
        maxKey = max 0 (ceiling $ (Cheb.degree df - initialDegree) /! steps)
      in
      {-trace(
      "maxKey: "++(show maxKey)
      ) $-}
      [reduceDegree (initialDegree + k*steps) df
        | k <- [0 .. maxKey + 1]])
    dfcsCheb
  --ch2Power :: (ErrorBound, Poly Integer) -> (ErrorBound, Pow.PowPoly Integer)
  ch2Power (e, p) = (e, cheb2Power p)
  dfsPow  = map (map (ch2Power . intify)) dfcsChebReduced
  dfsZipped =
    concatMap
        (\(k, (cdfs, pdfs)) ->
          zip [(k,i) | i <-  [0..]]
              $ zip (map (Cheb.evalDirect :: ChPolyMB -> MPBall -> MPBall) cdfs) pdfs)
        (zip [0..] $ zip dfcsChebReduced dfsPow)
  dfsMap  = Map.fromList dfsZipped
  maxKeys =
    Map.fromList
      [(k, max 0 (1 + ceiling ((Cheb.degree (dfcsCheb !! k) - initialDegree) /! steps))) -- TODO: avoid slow list index lookup
        | k <- [0 .. integer $ length ps - 1]]
  nodes   =
    lI : [setPrecision (getPrecision f) $ mpBall n | n <- nodesI, (lI < n) == Just True, (n < rI) == Just True] ++ [rI]   -- note that the elements of nodesI are balls of radius 0,
  nodesI  =                                                                                                  -- so that if they overlap with lI or rI, then they are contained in it
    let
      ns = map fst ps
    in
      (endpointL $ head ns) : (endpointR $ head ns) : (map endpointR (tail ns))

maximumOptimised :: PPoly -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised p l r initialDegree steps =
  maximumOptimisedWithAccuracy p l r initialDegree steps Exact

maximum :: PPoly -> MPBall -> MPBall -> MPBall
maximum (PPoly ps dom) l r =
  genericMaximum (PPE.evalDf f dfcsCheb) dfsMap maxKeys nodes cutoffAccuracies
  where
  cutoffAccuracies = Map.fromList $ zip [0..] [getAccuracy p | (_,p) <- ps]
  lI = fromDomToUnitInterval dom (setPrecision (getPrecision f) l)
  rI = fromDomToUnitInterval dom (setPrecision (getPrecision f) r) -- TODO: properly work out required endpoint precision
  unit    = Interval (dyadic $ -1) (dyadic 1)
  f       = makeExactCentre $ PPoly ps unit
  fs      = map snd ps
  --dfsCheb  = map (ballLift1R (Cheb.derivative . makeExactCentre)) fs
  dfcsCheb = map (ballLift1R (Cheb.derivativeExact . centre)) fs
  ch2Power (e, p) = (e, cheb2Power p)
  dfsPow  = map (ch2Power . intify) dfcsCheb
  dfsMap  = Map.fromList $ zip (map (\k -> (k,0)) [0..]) $
              zip (map (Cheb.evalDirect :: ChPolyMB -> MPBall -> MPBall) dfcsCheb) dfsPow
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
    -> Map (Integer, Integer) (MPBall -> MPBall, (ErrorBound, PowPoly Integer))
    -> Map Integer Integer -> [MPBall] -> Map Integer Accuracy
    -> MPBall
genericMaximum f dfs maxKeys nodes cutoffAccuracies =
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
  makeSearchInterval lBall rBall k =
    {-trace (
    "make search interval "++(show l)++" "++(show r)++" "++(show k)
    ) $-}
    let
      l   = rational $ dyadic (ball_value lBall) - dyadic (ball_error lBall)
      r   = rational $ dyadic (ball_value rBall) + dyadic (ball_error rBall)
      (e0, df0) = snd $ fromJust $ Map.lookup (k,0) dfs
      bsI = initialBernsteinCoefs df0 e0 l r
      (d0 , Just sgnL) = tryFindSign l k 0 -- TODO: Just sgnL assumes exact poly
    in
      case signVars bsI of
        Just 1
          -> let fx = evalfOnInterval l r in Just $ CriticalInterval l r fx Nothing (k,d0) sgnL
        Just 0
          -> Nothing
        _
          -> let fx = evalfOnInterval l r in Just $ SearchInterval l r fx Nothing (k,0) bsI
  singletonInterval :: MPBall -> MaximisationInterval
  singletonInterval x =
    let
      xRat = rational $ dyadic $ ball_value x
    in
    FinalInterval xRat xRat (evalfOnBall x)
  maxKey k = fromJust $ Map.lookup k maxKeys
  evalfOnBall x =
    let
      aux p q ac =
        let
          try  = f (setPrecision p x)
        in
          if getAccuracy try <= ac then
            try
          else
            aux (p + q) p (getAccuracy try)
    in
      aux (getPrecision $ head $ nodes)
          (getPrecision $ head $ nodes) NoInformation
  evalfOnInterval a b =
    --f (fromEndpoints a b)
    let
      aux p q ac =
        let
          try = f (fromEndpoints (mpBallP p a) (mpBallP p b))
        in
          {-trace (
          "evaluating on interval "++(show a)++ ", "++(show b)++"\n"++
          "trying precision "++(show p)++"\n"++
          "new accuracy: "++(show $ getAccuracy try)++"\n"++
          "old accuracy: "++(show ac)
          ) $-}
          if getAccuracy try <= ac then
            try
          else
            aux (p + q) p (getAccuracy try)
    in
      aux (getPrecision $ head $ nodes)
          (getPrecision $ head $ nodes)  NoInformation
  sgn x =
    case x > 0 of
      Just True  -> Just 1
      Just False ->
        if (x == 0) == Just True then Just 0 else Just (-1)
      Nothing -> Nothing
  tryFindSign :: Rational -> Integer -> Integer -> (Integer, Maybe Integer)
  tryFindSign x k n =
    let
      val = iterateUntilFixed (fst $ fromJust $ Map.lookup (k,n) dfs) (mpBallP (getPrecision (head nodes)) x) -- TODO: precision
      sg  = sgn val
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
      ) $
      trace (
      "minimum interval value "++(show $ mi_value mi)
      ) $-}
      if mi_isAccurate mi
        (fromJust $ Map.lookup (fst $ mi_derivative mi) cutoffAccuracies)
        (maxKey $ fst $ mi_derivative mi) then
          mi_value mi
      else
        case mi of
          SearchInterval a b v ov (k,n) ts ->
            {-trace (
            "sign variations: "++(show $ signVars ts)
            ) $-}
            if isNothing $ signVars ts then -- TODO avoid recomputation of sign variations
              if snd (mi_derivative mi) == maxKey (fst (mi_derivative mi)) then
                trace ("sign variations are undefined ... giving up.") $
                v
                {-let
                 m = computeMidpoint a b
                 (bsL, bsR)  = bernsteinCoefs a b m ts
                 --sgnM = tryFindSign m k
                 --bsL = initialBernsteinCoefs (snd $ fromJust $ Map.lookup k dfs) a m
                 --bsR = initialBernsteinCoefs (snd $ fromJust $ Map.lookup k dfs) m b
                in
                splitUntilAccurate $
                  Q.insert
                    (SearchInterval a m (evalfOnInterval a m) (Just v) (k,n) bsL) $
                  Q.insert
                    (SearchInterval m b (evalfOnInterval m b) (Just v) (k,n) bsR)
                    q'-}
              else
                let
                  (ek, dk) = (snd $ fromJust $ Map.lookup (k, n + 1) dfs)
                in
                splitUntilAccurate $
                  Q.insert
                    (SearchInterval a b v ov (k, n + 1)
                    (initialBernsteinCoefs dk ek                               -- TODO: alternatively keep track of Bernstein coefs on [-1,1] for all degrees
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
                      findM $ computeMidpoint cm b -- TODO: find a better perturbation function
                (dm, m) = findM $ computeMidpoint a  b -- TODO: findM assumes that the polynomial is exact.
                (dl, sgnL) = tryFindSign a k dm -- sgn $ evalDirect f' (mpBall l)
                (dm', sgnM) = tryFindSign m k dl -- sgn $ evalDirect f' (mpBall m)
                (bsL, bsR)  = bernsteinCoefs a b m ts
                varsL = signVars bsL
                varsR = signVars bsR
                miL = if varsL == Just 1 then
                        CriticalInterval a m (evalfOnInterval a m) (Just v) (k, dm') (fromJust sgnL) -- TODO: fromJust assumes that poly is exact
                      else
                        SearchInterval a m (evalfOnInterval a m) (Just v) (k, n) bsL
                miR = if varsR == Just 1 then
                        CriticalInterval m b (evalfOnInterval m b) (Just v) (k, dm') (fromJust sgnM)
                      else
                        SearchInterval m b (evalfOnInterval m b) (Just v) (k, n) bsR
              in
                case (varsL == Just 0, varsR == Just 0) of
                  (True, True)   -> splitUntilAccurate q'
                  (False, False) -> splitUntilAccurate
                                      $ Q.insert miL $ Q.insert miR q'
                  (False, True) -> splitUntilAccurate
                                      $ Q.insert miL q'
                  (True, False) -> splitUntilAccurate
                                      $ Q.insert miR q'
          CriticalInterval a b v ov (k,n) sgnL ->
            let
              m = computeMidpoint a b
              fm = evalfOnInterval m m
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
              --  v
                if n /= maxKey k then
                  splitUntilAccurate $ Q.insert (CriticalInterval a b v ov (k, max dl (n + 1)) sgnL) q'
                else
                  trace ("sgnM is undefined ... giving up.") $
                  v
          FinalInterval {} -> error "generic maximum: this point should never be reached"

data MaximisationInterval =
    SearchInterval Rational Rational MPBall (Maybe MPBall) (Integer, Integer) Terms -- the first integer represents the degree of the derivative
  | CriticalInterval Rational Rational MPBall (Maybe MPBall) (Integer, Integer) Integer      -- the second integer represents the sign of the left endpoint
  | FinalInterval Rational Rational MPBall
  deriving (Prelude.Eq, Show)

mi_left :: MaximisationInterval -> Rational
mi_left (SearchInterval   l _ _ _ _ _) = l
mi_left (CriticalInterval l _ _ _ _ _) = l
mi_left (FinalInterval l _ _ ) = l

mi_right :: MaximisationInterval -> Rational
mi_right (SearchInterval   _ r _ _ _ _) = r
mi_right (CriticalInterval _ r _ _ _ _) = r
mi_right (FinalInterval _ r _ ) = r

mi_isFinal :: MaximisationInterval -> Bool
mi_isFinal FinalInterval{} = True
mi_isFinal _ = False

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

mi_isAccurate :: MaximisationInterval -> Accuracy -> Integer -> Bool
mi_isAccurate FinalInterval{} _ _ = True
{-mi_isAccurate mi@(CriticalInterval {}) bts _ =
  getAccuracy (mi_value mi) >= bts
  || (not (isMoreAccurate (mi_value mi) (mi_oldValue mi)))
mi_isAccurate mi bts maxKey =
  getAccuracy (mi_value mi) >= bts
  || (snd (mi_derivative mi) == maxKey
  && not (isMoreAccurate (mi_value mi) (mi_oldValue mi)))-}
mi_isAccurate mi bts maxKey =
  getAccuracy (mi_value mi) >= bts
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

computeMidpoint :: Rational -> Rational -> Rational
computeMidpoint l r = 0.5*(l + r)

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

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational
