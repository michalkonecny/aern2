module AERN2.Local.Maximum
(
GenericMaximum
, genericise
, maximum
, minimum
)
where

import Numeric.MixedTypes hiding (maximum , minimum)
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Poly.Cheb hiding (maximum, minimum, degree)
--import AERN2.Local.Poly
import AERN2.Poly.Power.RootsInt
import AERN2.Interval
import AERN2.PQueue (PQueue)
import qualified AERN2.PQueue as Q

import Data.Maybe

import qualified Prelude

--import Debug.Trace

type GenFun =
  Dyadic -> Dyadic -> Accuracy ->
    [(Dyadic, Dyadic, MPBall -> MPBall, Accuracy, Rational -> Rational, DyadicInterval, Terms)] -- TODO: enforce constraint that domain is [l, r]?

class GenericMaximum a where
  genericise :: a -> GenFun

minimum :: (GenericMaximum a, CanNegSameType a) => a -> MPBall -> MPBall -> Accuracy -> MPBall
minimum f lBall rBall targetAcc = -(maximum (-f) lBall rBall targetAcc)

maximum :: (GenericMaximum a) => a -> MPBall -> MPBall -> Accuracy -> MPBall
maximum f = genericMaximum (genericise f)

{- generic maximum: -}

genericMaximum :: GenFun -> MPBall -> MPBall -> (Accuracy -> MPBall )
genericMaximum f lBall rBall targetAcc =
  splitUntilAccurate $
    insertAll initialIntervals Q.empty
  where
  l   = dyadic (ball_value lBall) - dyadic (ball_error lBall)
  r   = dyadic (ball_value rBall) + dyadic (ball_error rBall)
  n = min 16 (fromAccuracy targetAcc) -- TODO there is a bug where the computation seems to diverge for some subdivisions
  ps = [l + k*(r - l)/n | k <- [0 .. n]]
  dyPs = map (centre . mpBallP (prec $ fromAccuracy targetAcc)) ps
  dyIntervals = zip dyPs (tail dyPs)
  initialIntervals = concat [mi_searchIntervals f a b ac0 | (a,b) <- dyIntervals]
  ac0 = bits 10
  updateAccuracy ac =
    if ac >= targetAcc
    || ac < bits 20 then
      ac + 5
    else
      max targetAcc (bits $ ceiling $ 1.3*(fromAccuracy ac))

  splitUntilAccurate :: PQueue MaximisationInterval -> MPBall
  splitUntilAccurate q =
    let
      Just (mi, q') = Q.minView q
    in
    {-trace("mi: "++(show $ mpBall $ mi_left mi)++ " " ++ (show $ mpBall $ mi_right mi)) $
    trace("mi value: "++ (show $ mi_value mi)) $
    trace("mi accuracy: "++ (show $ getAccuracy $ mi_value mi)) $-}
    if mi_isAccurate mi targetAcc then
      mi_value mi
    else
      case mi of
        SearchInterval a b uA uB v tac aac g dg bs ->
          if getAccuracy v >= aac then -- TODO: maybe also if accuracy doesn't change over 2-3 iterations
            splitUntilAccurate $
              insertAll (mi_searchIntervals f a b (updateAccuracy tac))
              q'
          else
            case signVars bs of
              Nothing ->
                splitUntilAccurate $
                  insertAll (mi_searchIntervals f a b (updateAccuracy tac))
                  q'
              Just 0 ->
                splitUntilAccurate $
                 Q.insert (mi_monotoneInterval a b g tac aac)
                 q'
              Just 1 ->
                splitUntilAccurate $
                  Q.insert (mi_criticalInterval a b g dg tac aac)
                  q'
              _  ->
                let
                  m  = (dyadic 0.5)*(a + b)
                  uM = 0.5*(uA + uB)
                  vl = evalOnInterval g a m aac
                  vr = evalOnInterval g m b aac
                  (bsL, bsR)  = bernsteinCoefs uA uB uM bs
                  nq =
                      Q.insert (SearchInterval a m uA uM vl tac aac g dg bsL) $
                      Q.insert (SearchInterval m b uM uB vr tac aac g dg bsR)
                      q'
                in
                  splitUntilAccurate
                  nq
        CriticalInterval a b _v tac _aac ->
          splitUntilAccurate $
            insertAll (mi_searchIntervals f a b (updateAccuracy tac))
            q'

{- maximisation interval -}

data MaximisationInterval =
  SearchInterval {
      mi_left   :: Dyadic
    , mi_right  :: Dyadic
    , mi_unitRight :: Rational
    , mi_unitLeft :: Rational
    , mi_value  :: MPBall
    , mi_targetAccuracy :: Accuracy
    , mi_actualAccuracy :: Accuracy
    , mi_evaluator :: MPBall -> MPBall
    , mi_derivative :: Rational -> Rational -- TODO: Rational -> Rational sufficiently efficient?
    , mi_terms :: Terms
  }
  | CriticalInterval {
      mi_left  :: Dyadic
    , mi_right :: Dyadic
    , mi_value :: MPBall
    , mi_targetAccuracy :: Accuracy
    , mi_actualAccuracy :: Accuracy
  }

mi_mononoteValue :: Dyadic -> Dyadic -> (MPBall -> MPBall) -> Accuracy -> MPBall
mi_mononoteValue l r f ac =
  max (evalOnDyadic f l ac) (evalOnDyadic f r ac)

mi_criticalValue :: Dyadic -> Dyadic -> (MPBall -> MPBall) -> (Rational -> Rational) -> Accuracy -> MPBall
mi_criticalValue l r f df ac =
  max (evalOnDyadic f l ac) $
    max (evalOnDyadic f r ac)  $
    aux l r (sign $ df (rational l)) (sign $ df (rational r))
  where
  sign x
    | x == 0    = 0
    | x < 0     = -1
    | otherwise = 1
  aux :: Dyadic -> Dyadic -> Integer -> Integer -> MPBall
  aux a b sgA sgB =
    let
      v = evalOnInterval f a b ac
    in
      if getAccuracy v >= ac then
        v
      else
        let
          m   = (dyadic 0.5)*(a + b)
          dfm = df (rational m)
          sgM = sign dfm
        in
          if sgM == 0 then
            evalOnDyadic f m ac
          else if sgM * sgA < 0 then
            aux a m sgA sgM
          else
            aux m b sgM sgB

mi_criticalInterval :: Dyadic -> Dyadic -> (MPBall -> MPBall) -> (Rational -> Rational) -> Accuracy -> Accuracy -> MaximisationInterval
mi_criticalInterval l r f df tac aac =
  CriticalInterval l r cv tac aac
  where
  cv = mi_criticalValue l r f df (min tac aac)

mi_monotoneInterval :: Dyadic -> Dyadic -> (MPBall -> MPBall) -> Accuracy -> Accuracy -> MaximisationInterval
mi_monotoneInterval l r f tac aac =
    CriticalInterval l r mv tac aac
    where
    mv = mi_mononoteValue l r f (min tac aac)

mi_searchIntervals :: GenFun -> Dyadic -> Dyadic -> Accuracy ->  [MaximisationInterval]
mi_searchIntervals f l r ac =
  [
    let
    v = evalOnInterval evalF a b ac
    aI = fromDomToUnitInterval dom (rational a)
    bI = fromDomToUnitInterval dom (rational b)
    in
    SearchInterval a b aI bI v ac aac evalF evalDF bsI
    |
    (a, b, evalF, aac ,evalDF, dom, bsI) <- sis
  ]
  where
  sis = f l r ac

mi_isAccurate :: MaximisationInterval -> Accuracy -> Bool
mi_isAccurate mi ac =
  getAccuracy (mi_value mi) >= ac

instance Prelude.Eq MaximisationInterval where
  (==) mi0 mi1 =
      mi_left mi0 == mi_left mi1
    && mi_right mi0 == mi_right mi1

instance Prelude.Ord MaximisationInterval where
  (<=) mi0 mi1 =
    fromJust $ u0 >= u1
    where
    (_, u0 :: MPBall) = endpoints $ mi_value mi0
    (_, u1 :: MPBall) = endpoints $ mi_value mi1


{- auxiliary functions -}

insertAll :: (Prelude.Ord a) => [a] -> PQueue a -> PQueue a
insertAll [] q = q
insertAll (x:xs) q = insertAll xs (Q.insert x q)

evalOnDyadic :: (MPBall -> MPBall) -> Dyadic -> Accuracy -> MPBall
evalOnDyadic f x bts =
  let
    aux p q ac =
      let
        xBall = setPrecision p (mpBall x)
        try   = f xBall
      in
        if getAccuracy try >= bts
        || getAccuracy try <= ac then
          try
        else
          aux (p + q) p (getAccuracy try)
  in
    aux (prec 100)
         (prec 50) NoInformation

evalOnInterval :: (MPBall -> MPBall) -> Dyadic -> Dyadic -> Accuracy -> MPBall
evalOnInterval f a b bts =
  let
    result = aux (prec 100) (prec 50) NoInformation
    aux p q ac =
      let
        x    = setPrecision p (fromEndpoints (mpBall a) (mpBall b))
        try  = f x
      in
        if getAccuracy try >= bts
        || getAccuracy try <= ac then
          try
        else
          aux (p + q) p (getAccuracy try)
  in
    result
