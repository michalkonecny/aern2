module AERN2.Poly.Power.MaximumIntAlt
(
genericMaximum,
genericMaximumWithBounds
)
where

import MixedTypesNumPrelude hiding (maximum, minimum)
import AERN2.Poly.Power.Type
import AERN2.Poly.Power.Eval
import AERN2.Poly.Power.RootsInt
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import Data.List hiding (maximum, minimum)
import qualified Prelude
import Data.Maybe
import AERN2.Poly.Power.SizeReduction
import AERN2.Interval

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

genericMaximum
  :: (MPBall -> MPBall)
    -> Map Integer (MPBall -> MPBall, (ErrorBound, PowPoly Integer))
    -> Accuracy
    -> MPBall -> MPBall
    -> MPBall
genericMaximum f df bts lBall rBall =
  genericMaximumWithBounds f df bts lBall rBall lower upper
  where
  rangeBall = f (hullMPBall lBall rBall)
  Interval lower' upper' = dyadicInterval rangeBall
  lower = mpBall lower'
  upper = mpBall upper'

genericMaximumWithBounds
  :: (MPBall -> MPBall)
      -> Map Integer (MPBall -> MPBall, (ErrorBound, PowPoly Integer))
      -> Accuracy
      -> MPBall -> MPBall
      -> MPBall -> MPBall
      -> MPBall
genericMaximumWithBounds f dfs bts lBall rBall lower' upper' =
  let
    l   = rational $ dyadic (ball_value lBall) - dyadic (ball_error lBall)
    r   = rational $ dyadic (ball_value rBall) + dyadic (ball_error rBall)
    (e0, df0) = snd $ fromJust $ Map.lookup 0 dfs
    bsI       = initialBernsteinCoefs df0 e0 l r
    (d0 , Just sgnL) = tryFindSign l (getPrecision lBall) 0 -- TODO: Just sgnL assumes exact poly
  in
    case signVars bsI of
       Just 1 -> splitUntilAccurate lower' upper' $
            (let fx = evalfOnBall lBall lower' upper' in Q.insert (FinalInterval l l fx)) $
            (let fx = evalfOnBall rBall lower' upper' in Q.insert (FinalInterval r r fx)) $
            Q.singleton $
            let fx = evalfOnInterval l r lower' upper' in
              CriticalInterval l r fx Nothing d0 sgnL
       Just 0 -> splitUntilAccurate lower' upper' $
            (let fx = evalfOnBall lBall lower' upper' in Q.insert (FinalInterval l l fx)) $
            Q.singleton $
            let fx = evalfOnBall rBall lower' upper' in FinalInterval r r fx
       _ -> splitUntilAccurate lower' upper' $
            (let fx = evalfOnBall lBall lower' upper' in Q.insert (FinalInterval l l fx)) $
            (let fx = evalfOnBall rBall lower' upper' in Q.insert (FinalInterval r r fx)) $
            Q.singleton $
            let fx = evalfOnInterval l r lower' upper' in SearchInterval l r fx Nothing 0 {-d0-} bsI
  where
  maxKey = fst $ Map.findMax dfs
  evalfOnBall :: MPBall -> MPBall -> MPBall -> MPBall
  evalfOnBall x lower upper =
    let
      aux p q ac =
        let
          try  = min upper $ max lower $ f (setPrecision p x)
        in
          if getAccuracy try <= ac then
            try
          else
            aux (p + q) p (getAccuracy try)
    in
      aux (getPrecision lBall)
          (getPrecision lBall) NoInformation
  evalfOnInterval :: Rational -> Rational -> MPBall -> MPBall -> MPBall
  evalfOnInterval a b lower upper =
    let
      aux p q ac =
        let
          x    = (hullMPBall (mpBallP p a) (mpBallP p b))
          try  = min upper $ max lower (f x)
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
  splitUntilAccurate ::  MPBall -> MPBall -> PQueue MaximisationInterval -> MPBall
  splitUntilAccurate lower upper q =
    let
      ul = hullMPBall lower upper
      Just (mi, q') = {-trace("q size: "++(show $ Q.size q)) $-} Q.minView q
    in
      maybeTrace (
      "minimal interval " ++ (show mi)++
      "\nminimal interval value: "++(show $ mi_value mi)
      ) $
      if getAccuracy ul >= bts then
        ul
      else if mi_isAccurate mi maxKey bts then
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
                splitUntilAccurate lower upper $
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
                fm = evalfOnInterval m m lower upper
                newLow = max fm lower
                miL = if varsL == Just 1 then
                        CriticalInterval a m (evalfOnInterval a m newLow upper) (Just v) (max dl dm') (fromJust sgnL) -- TODO: fromJust assumes that poly is exact
                      else
                        SearchInterval a m (evalfOnInterval a m newLow upper) (Just v) k bsL
                miR = if varsR == Just 1 then
                        CriticalInterval m b (evalfOnInterval m b newLow upper) (Just v) (max dl dm') (fromJust sgnM)
                      else
                        SearchInterval m b (evalfOnInterval m b newLow upper) (Just v) k bsR
              in
                case (varsL == Just 0, varsR == Just 0) of
                  (True, True)   -> splitUntilAccurate newLow upper q'
                  (False, False) -> splitUntilAccurate newLow upper
                                      $ Q.insert miL $ Q.insert miR q'
                  (False, True) -> splitUntilAccurate newLow upper
                                      $ Q.insert miL q'
                  (True, False) -> splitUntilAccurate newLow upper
                                      $ Q.insert miR q'
          CriticalInterval a b v _ k sgnL ->
            let
              m = computeMidpoint a b --0.5*(a + b)
              mBall = (mpBallP (getPrecision lBall) m)
              fm = evalfOnBall mBall lower upper
              newLow = max lower fm
              (dm, sgnM)  = tryFindSign m (getPrecision lBall) k
              (dl, nSgnL) = tryFindSign a (getPrecision lBall) dm
            in
              if sgnM == Just 0 then
                splitUntilAccurate newLow upper $ Q.insert (CriticalInterval m m fm (Just v) dl sgnL) q'
              else if isJust sgnM then
                if sgnL /= fromJust sgnM then
                  splitUntilAccurate newLow upper $ Q.insert (CriticalInterval a m (evalfOnInterval a m newLow upper) (Just v) dl $ fromJust nSgnL) q'
                else
                  splitUntilAccurate newLow upper $ Q.insert (CriticalInterval m b (evalfOnInterval m b newLow upper) (Just v) dl $ fromJust sgnM) q'
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
    (_, u0) = endpointsAsIntervals $ mi_value mi0
    (_, u1) = endpointsAsIntervals $ mi_value mi1

{- auxiliary functions -}

isMoreAccurate :: MPBall -> Maybe MPBall -> Bool
isMoreAccurate _ Nothing  = True
isMoreAccurate x (Just y) =
  radius x < radius y

computeMidpoint :: Rational -> Rational -> Rational
computeMidpoint l r = 0.5*(l + r)

{-evalDirectAccurately ::
 (Ring t, CanAddSubMulDivCNBy t Dyadic, CanDivCNBy t Integer,
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
