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
import AERN2.Local.Poly
import AERN2.Poly.Basics hiding (Terms)
import AERN2.Poly.Power (degree)
import AERN2.Poly.Power.RootsInt
import AERN2.Poly.Power.SignedSubresultant
import Data.Ratio
import AERN2.Interval
import AERN2.Poly.Conversion
import AERN2.PQueue (PQueue)
import qualified AERN2.PQueue as Q

import qualified Data.Map as Map
import Data.Maybe

import qualified Prelude

type GenFun =
  Dyadic -> Dyadic -> Accuracy ->
    (MPBall -> MPBall, Accuracy, Rational -> Rational, DyadicInterval, Terms) -- TODO: enforce constraint that domain is [l, r]?

class GenericMaximum a where
  genericise :: a -> GenFun


debug_useSeparablePart :: Bool
debug_useSeparablePart = True

instance GenericMaximum (LocalPoly MPBall) where
  genericise f l r ac =
    (evalF, fAcc, evalDF, dom, bsI)
    where
    evalF = evalDf fI (2/(r - l) * dfI) -- TODO: remove workaround for bug in poly derivative
    fAcc = getAccuracy fI
    evalDF = evalDirect dfRat
    fI = f l r ac
    ch2Power (e, p) = (e, cheb2Power p)
    dfI    = (derivativeExact . centre) fI -- TODO: derivativeExact?
    dfRat = makeRational dfI
    (eI, dfIPow) = (ch2Power . intify) dfI
    dom = chPoly_dom fI
    lI = fromDomToUnitInterval dom (rational l)
    rI = fromDomToUnitInterval dom (rational r)
    dfIPow' =
      if debug_useSeparablePart
        && dyadic eI == 0
        && degree dfIPow < 50
      then
        separablePart dfIPow
      else
        dfIPow
    bsI = initialBernsteinCoefs dfIPow' eI lI rI

minimum :: (GenericMaximum a, CanNegSameType a) => a -> MPBall -> MPBall -> Accuracy -> MPBall
minimum f lBall rBall targetAcc = -(maximum (-f) lBall rBall targetAcc)

maximum :: (GenericMaximum a) => a -> MPBall -> MPBall -> Accuracy -> MPBall
maximum f = genericMaximum (genericise f)

{- generic maximum: -}

genericMaximum :: GenFun -> MPBall -> MPBall -> (Accuracy -> MPBall )
genericMaximum f lBall rBall targetAcc =
  case signVars bsI of
    Just 1 -> splitUntilAccurate $
         Q.singleton $
           mi_criticalInterval l r evalf0 evaldf0 ac0 f0Acc
    Just 0 -> splitUntilAccurate $
         Q.singleton $
          mi_monotoneInterval l r evalf0 ac0 f0Acc
    _ -> splitUntilAccurate $
         Q.singleton $
         let fx = evalOnInterval evalf0 l r ac0 in
          SearchInterval l r lI rI fx  ac0 f0Acc evalf0 evaldf0 bsI
  where
  l   = dyadic (ball_value lBall) - dyadic (ball_error lBall)
  r   = dyadic (ball_value rBall) + dyadic (ball_error rBall)
  lI = fromDomToUnitInterval dom (rational l)
  rI = fromDomToUnitInterval dom (rational r)
  ac0 = bits 5
  (evalf0, f0Acc, evaldf0, dom, bsI) = f l r ac0
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
    {-trace("mi: "++(show $ mpBallP (prec 100) $ mi_left mi)++ " " ++ (show $ mpBallP (prec 100) $ mi_right mi)) $
    trace("mi value: "++ (show $ mi_value mi)) $
    trace("mi accuracy: "++ (show $ getAccuracy $ mi_value mi)) $-}
    if mi_isAccurate mi targetAcc then
      mi_value mi
    else
      case mi of
        SearchInterval a b uA uB v tac aac g dg bs ->
          if getAccuracy v >= aac then -- TODO: maybe also if accuracy doesn't change over 2-3 iterations
            splitUntilAccurate $
              Q.insert (mi_searchInterval f a b (updateAccuracy tac))
              q'
          else
            case signVars bs of
              Nothing ->
                splitUntilAccurate $
                  Q.insert (mi_searchInterval f a b (updateAccuracy tac))
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
            Q.insert (mi_searchInterval f a b (updateAccuracy tac))
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

mi_searchInterval :: GenFun -> Dyadic -> Dyadic -> Accuracy -> MaximisationInterval
mi_searchInterval f l r ac =
  SearchInterval l r lI rI v ac aac evalF evalDF bsI
  where
  v = evalOnInterval evalF l r ac
  lI = fromDomToUnitInterval dom (rational l)
  rI = fromDomToUnitInterval dom (rational r)
  (evalF, aac ,evalDF, dom, bsI) = f l r ac

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

makeRational :: ChPoly MPBall -> ChPoly Rational
makeRational (ChPoly dom (Poly ts) _) =
  ChPoly dom (Poly $ terms_map (rational . centre) ts) Nothing

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

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational
