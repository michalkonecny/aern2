module AERN2.Poly.Cheb.MaximumPrime(
maximum,
maximumOptimised,
maximumOptimisedWithAccuracy,
minimum,
minimumOptimised,
minimumOptimisedWithAccuracy
)
where

import MixedTypesNumPrelude hiding (maximum, minimum)

--import Text.Printf

import AERN2.MP.Ball
import AERN2.MP.Dyadic
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map

-- import AERN2.Poly.Basics (terms_updateConst)

import AERN2.Poly.Power (PowPoly (..))
import AERN2.Poly.Power.RootsInt
import qualified AERN2.Poly.Power as Pow hiding (genericMaximum)
import qualified AERN2.Poly.Power.MaximumInt as Pow

import AERN2.RealFun.Operations

import AERN2.Poly.Basics hiding (Terms)
import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Conversion
import AERN2.Interval

import qualified Prelude as P
import AERN2.PQueue (PQueue)
import qualified AERN2.PQueue as Q
import Data.Maybe

import Debug.Trace


maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum f l r = maximumOptimised f l r (degree f) 1

minimum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r)

maximumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f =
  maximumOptimisedWithAccuracy (getFiniteAccuracy f) f

minimumOptimisedWithAccuracy :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimisedWithAccuracy acc f l r iDeg steps = -(maximumOptimisedWithAccuracy acc (-f) l r iDeg steps)

minimumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f = minimumOptimisedWithAccuracy (getFiniteAccuracy f) f


intify :: ChPoly MPBall -> Poly Integer
intify (ChPoly _ p _) =
  pInt
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  --err = termsError * termsDenominator
  --termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational

maximumOptimisedWithAccuracy ::
  Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimisedWithAccuracy cutoff f l r iDeg steps =
  genericMaximum fs (min cutoff (getFiniteAccuracy f)) l r
  where
  {-df  = (derivativeExact . centre) f
  df0 = reduceToEvalDirectAccuracy df (bits 0)-}
  maxKey = max 0 (ceiling ((degree f - iDeg) / steps))
  fs = Map.fromList [(k, fk k) | k <- [0 .. maxKey + 1]]
  fk k =
    let
      g   = reduceDegree (iDeg + k*steps) f
      dg  = (derivativeExact . centre) g
      dg0 = reduceToEvalDirectAccuracy dg (bits 0)
    in
      (evalIRRAM g dg0, (min cutoff (getFiniteAccuracy g)), cheb2Power $ intify dg)
      --(evalIRRAM f df0, (min cutoff (getFiniteAccuracy g)), cheb2Power $ intify dg) -- TODO: sound?

evalIRRAM :: ChPoly MPBall -> ChPoly MPBall -> MPBall -> MPBall
evalIRRAM f df x =
  aux (2*initPrec) initPrec fx
  where
  initPrec = max (getPrecision f) (getPrecision x)
  fx = evalDf f df x
  fAcc = getAccuracy f
  xAcc = getAccuracy x
  aux p q ofx =
    let
      try = evalDf (setPrecision p f) df (setPrecision p x)
    in
      if getAccuracy try >= min xAcc fAcc then
        try
      else if getAccuracy try <= getAccuracy ofx then
        ofx
      else
        aux (p + q) p try

genericMaximum ::
  Map Integer (MPBall -> MPBall, Accuracy, PowPoly Integer) ->
  Accuracy -> MPBall -> MPBall -> MPBall
genericMaximum fs cutoff lBall rBall =
  splitUntilAccurate initialQueue
  where
  l   = dyadic (ball_value lBall) - dyadic (ball_error lBall)
  r   = dyadic (ball_value rBall) + dyadic (ball_error rBall)
  f0  = ffst $ fromJust $ Map.lookup 0 fs
  df0 = thrd $ fromJust $ Map.lookup 0 fs
  bsI = initialBernsteinCoefs df0 (errorBound 0) (rational l) (rational r)
  maxKey = fst $ Map.findMax fs
  initialQueue =
    Q.singleton $
      SearchInterval l r (f0 (fromEndpoints lBall rBall :: MPBall)) Nothing 0 bsI

  splitUntilAccurate q =
    let
      Just (mi, q') = Q.minView q
    in
      --trace("mi value: "++(show $ mi_value mi)) $
      if mi_isAccurate cutoff maxKey mi then
        --trace("mi accurate: "++(show mi)) $
        mi_value mi
      else if mi_revisited fs maxKey mi then
        let
        ml = mi_left mi
        mr = mi_right mi
        d   = mi_degree mi + 1
        fd  = (ffst . fromJust . Map.lookup d) fs
        dfd = (thrd . fromJust . Map.lookup d) fs
        bs  = initialBernsteinCoefs dfd (errorBound 0) (rational ml) (rational mr)
        (val , vars) =
          mi_newValue fs maxKey (SearchInterval ml mr (mpBall 0) Nothing d bs)

        {- Critical stuff -}
        sgnL = polySgn dfd ml
        in
        if d == maxKey then
          case vars of
            0 -> splitUntilAccurate $
                  Q.insert
                  (FinalInterval ml ml (fd $ mpBall ml)) $
                  Q.insert
                  (FinalInterval mr mr (fd $ mpBall mr))
                  q'
            1 -> splitUntilAccurate $
                   Q.insert
                   (CriticalInterval ml mr val (Just $ mi_value mi) sgnL)
                   q'
            _ -> splitUntilAccurate  $
                  Q.insert
                  (SearchInterval ml mr val (Just $ mi_value mi) d bs)
                  q'
        else
          splitUntilAccurate  $
            Q.insert
            (SearchInterval ml mr val (Just $ mi_value mi) d bs)
            q'
      else
        case mi of
          SearchInterval a b v ov d' cfs ->
            let
              d = if d' == maxKey then d' else d' + 1
              m  = (dyadic 0.5) * (a + b)
              (cls, crs) =
                bernsteinCoefs (rational a) (rational b) (rational m) cfs
              (vl, varsL) =
                mi_newValue fs maxKey (SearchInterval a m v ov d cls)
              (vr, varsR) =
                mi_newValue fs maxKey (SearchInterval m b v ov d crs)

              {- Critical stuff: -}
              fd = (ffst . fromJust . Map.lookup d) fs
              df = (thrd . fromJust . Map.lookup d) fs
              sgnL = polySgn df l
              sgnM = polySgn df m

              leftSearch  = (SearchInterval a m vl (Just v) d cls)
              rightSearch = (SearchInterval m b vr (Just v) d crs)

              leftCrit  = (CriticalInterval a m vl (Just v) sgnL)
              rightCrit = (CriticalInterval m b vr (Just v) sgnM)

              leftFinal   = (FinalInterval a a (fd $ mpBall a))
              middleFinal = (FinalInterval m m (fd $ mpBall m))
              rightFinal  = (FinalInterval b b (fd $ mpBall b))

            in
              if d == maxKey then
                splitUntilAccurate $
                  (case varsL of
                    0 -> Q.insert leftFinal .
                         Q.insert middleFinal
                    1 -> Q.insert leftCrit
                    _ -> Q.insert leftSearch) $
                  (case varsR of
                      0 -> Q.insert middleFinal .
                           Q.insert rightFinal
                      1 -> Q.insert rightCrit
                      _ -> Q.insert rightSearch)
                      q'
              else
                splitUntilAccurate $
                  Q.insert
                  leftSearch $
                  Q.insert
                  rightSearch
                  q'

            {-splitUntilAccurate $
              Q.insert
                (if d == maxKey && varsL == 1 then
                  (CriticalInterval a m vl (Just v) sgnL)
                else
                  (SearchInterval a m vl (Just v) d cls)) $
              Q.insert
                (if d == maxKey && varsR == 1 then
                  (CriticalInterval m b vr (Just v) sgnM)
                else
                  (SearchInterval m b vr (Just v) d crs))
              q'-}
          CriticalInterval a b v ov ls ->
            let
              m = (dyadic 0.5)*(a + b)
              --mBall = (mpBallP (getPrecision lBall) m)
              --fm = f mBall -- TODO: precision?
              --(dm, sgnM)  = tryFindSign m (getPrecision lBall) k
              --(dl, nSgnL) = tryFindSign a (getPrecision lBall) dm
              df = thrd $ fromJust $ Map.lookup maxKey fs
              f  = ffst $ fromJust $ Map.lookup maxKey fs
              sgnM = polySgn df m
              (vl, _) = mi_newValue fs maxKey (CriticalInterval a m v ov 0)
              (vr, _) = mi_newValue fs maxKey (CriticalInterval m b v ov 0)
            in
              if sgnM == 0 then
                splitUntilAccurate $
                  Q.insert
                    (FinalInterval m m (f $ mpBall m)) -- TODO: precision?
                    q'
              else if sgnM*ls < 0 then
                splitUntilAccurate $
                  Q.insert
                    (CriticalInterval a m vl (Just v) ls)
                    q'
              else
                splitUntilAccurate $
                  Q.insert
                    (CriticalInterval m b vr (Just v) sgnM)
                    q'
          FinalInterval _a _b _v ->
            error "this point should never be reached"

data MaximisationInterval =
    SearchInterval {
        mi_left     :: Dyadic
      , mi_right    :: Dyadic
      , mi_value    :: MPBall
      , mi_oldValue :: Maybe MPBall
      , mi_degree   :: Integer
      , mi_coefs    :: Terms
    }
  | CriticalInterval {
        mi_left     :: Dyadic -- critical interval is only for "final" stage
      , mi_right    :: Dyadic
      , mi_value    :: MPBall
      , mi_oldValue :: Maybe MPBall
      , mi_leftSign :: Integer
    }
  | FinalInterval {
        mi_left  :: Dyadic
      , mi_right :: Dyadic
      , mi_value :: MPBall
    }
  deriving (P.Eq, Show)

mi_deg :: Integer -> MaximisationInterval -> Integer
mi_deg _ FinalInterval{} = undefined
mi_deg maxKey CriticalInterval{} = maxKey
mi_deg _ mi@SearchInterval{} = mi_degree mi

mi_revisited ::
  Map Integer (MPBall -> MPBall, Accuracy, PowPoly Integer) ->
  Integer -> MaximisationInterval -> Bool
mi_revisited _ _ FinalInterval{}    = False
mi_revisited _ _ CriticalInterval{} = False
mi_revisited fs maxKey mi =
  let
    d   = mi_degree mi
    acc = (ssnd . fromJust . Map.lookup d) fs
  in
    d /= maxKey
    &&
    (getAccuracy (mi_value mi) >= acc - 1
     || (not $ isMoreAccurate (mi_value mi) (mi_oldValue mi)))


mi_isAccurate :: Accuracy -> Integer -> MaximisationInterval -> Bool
mi_isAccurate _ _ FinalInterval{} = True
mi_isAccurate cutoff maxKey mi =
  getAccuracy (mi_value mi) >= cutoff
  {-- || (mi_deg maxKey mi == maxKey
  && not (isMoreAccurate (mi_value mi) (mi_oldValue mi)))-}


splitCritical ::
  Accuracy -> (MPBall -> MPBall) -> PowPoly Integer -> Dyadic -> Dyadic -> Integer -> MPBall
splitCritical cutoff f df l r sgnL =
  if getAccuracy fivl >= cutoff - 1 then
    fivl
  else if sgnM == 0 then
    splitCritical cutoff f df m m 0
  else if sgnM * sgnL < 0 then
    splitCritical cutoff f df l m sgnL
  else
    splitCritical cutoff f df m r sgnM
  where
  ivl  = (fromEndpoints (mpBall l) (mpBall r) :: MPBall)
  fivl = f ivl
  m    = (dyadic 0.5) * (l + r)
  sgnM  = polySgn df m

mi_newValue ::
  Map Integer (MPBall -> MPBall, Accuracy, PowPoly Integer) ->
  Integer -> MaximisationInterval -> (MPBall, Integer)
mi_newValue fs _maxKey (SearchInterval l r _ _ d ts) =
  let
    f      = (ffst . fromJust . Map.lookup d) fs
    df     = (thrd . fromJust . Map.lookup d) fs
    cutoff = (ssnd . fromJust . Map.lookup d) fs
    sgnL  = polySgn df l
    lBall = mpBall l
    rBall = mpBall r
  in
  case signVars ts of
    Just 0 -> (max (f lBall) (f rBall), 0)
    Just 1 ->
      (splitCritical cutoff f df l r sgnL, 1)
    Just n -> (f (fromEndpoints lBall rBall :: MPBall), n)
    _ -> error "fff"

mi_newValue fs maxKey (CriticalInterval l r _ _ _) =
  let
    f = (ffst . fromJust . Map.lookup maxKey) fs
    lBall = mpBall l
    rBall = mpBall r
  in
    (f (fromEndpoints lBall rBall :: MPBall), 1)

mi_newValue _ _ FinalInterval{} =
  error "trying to compute new value of final interval."

instance P.Ord MaximisationInterval where
  (<=) mi0 mi1 =
    fromJust $ u0 >= u1
    where
    (_, u0 :: MPBall) = endpoints $ mi_value mi0
    (_, u1 :: MPBall) = endpoints $ mi_value mi1

ffst :: (a,b,c) -> a
ffst (a,_,_) = a

ssnd :: (a,b,c) -> b
ssnd (_,b,_) = b

thrd :: (a,b,c) -> c
thrd (_,_,c) = c

isMoreAccurate :: MPBall -> Maybe MPBall -> Bool
isMoreAccurate _ Nothing  = True
isMoreAccurate x (Just y) =
  --getAccuracy x >= bits 0 && -- TODO :needed?
  radius x < radius y

polySgn :: PowPoly Integer -> Dyadic -> Integer
polySgn f = (signum . evalOnDyadic f)

evalOnDyadic :: PowPoly Integer -> Dyadic -> Integer
evalOnDyadic (PowPoly (Poly ts)) x =
  evalHornerAcc (terms_degree ts) 1 (convertExactly 0)
  where
  evalHornerAcc :: Integer -> Integer -> Integer -> Integer
  evalHornerAcc 0 pw sm = xNum*sm + pw*terms_lookupCoeff ts 0
  evalHornerAcc k pw sm = evalHornerAcc (k - 1) (xDen*pw) $ xNum*sm + pw*terms_lookupCoeff ts k
  xRat   = rational x
  xDen   = denominator xRat
  xNum   = numerator xRat

signum :: Integer -> Integer
signum x
  | x == 0    = 0
  | x >  0    = 1
  | otherwise = -1

instance CanMinimiseOverDom (ChPoly MPBall) DyadicInterval where
  type MinimumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  minimumOverDom f (Interval l r) =
    minimumOptimised f (mpBall l) (mpBall r) 5 5

instance CanMaximiseOverDom (ChPoly MPBall) DyadicInterval where
  type MaximumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  maximumOverDom f (Interval l r) =
    maximumOptimised f (mpBall l) (mpBall r) 5 5
