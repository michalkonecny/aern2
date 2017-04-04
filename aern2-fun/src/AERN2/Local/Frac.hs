module AERN2.Local.Frac
(
  LocalFrac
  , fromPoly
)
where

import Numeric.MixedTypes
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import Data.Ratio
import AERN2.Local.Basics
import AERN2.Local.Poly hiding (fromPoly)
import AERN2.Local.Maximum
import AERN2.Poly.Basics
import AERN2.Poly.Conversion
import AERN2.Poly.Cheb as Cheb
import AERN2.Poly.Power.RootsInt
import AERN2.Frac as Frac hiding (fromPoly)
import qualified AERN2.Frac as Frac (fromPoly)
import AERN2.Frac.Field

import qualified Data.Map as Map

import Debug.Trace

type LocalFrac a = Local (Frac a)

fromPoly :: (ConvertibleExactly Integer a) => LocalPoly a -> LocalFrac a
fromPoly = liftLocal1 Frac.fromPoly

instance GenericMaximum (LocalFrac MPBall) where
  genericise f l r ac =
    [(l, r, evalF, fAcc, evalDF, dom, bsI)]
    where
    evalF = Frac.evalDf fI (2/(r - l) * dNum) (2/(r - l) * dDenom)
    fAcc = getAccuracy fI
    evalDF = Cheb.evalDirect dfRat
    fI@(Frac num denom _dIM) = f l r ac
    ch2Power (e, p) = (e, cheb2Power p)
    dNum     = derivativeExact cNum
    dDenom   = derivativeExact cDenom
    cNum   = centre num
    cDenom = centre denom
    dfINum = computeNum (prec 100) (prec 50) --num*dDenom - dNum*denom
    --dfI = dfINum * (inverseWithLowerBound (denom*denom) (dIM*dIM))
    dfRat = makeRational dfINum
    (eI, dfIPow) = (ch2Power . intify) dfINum
    dom = chPoly_dom num
    lI = fromDomToUnitInterval dom (rational l)
    rI = fromDomToUnitInterval dom (rational r)
    bsI = initialBernsteinCoefs dfIPow eI lI rI
    computeNum p q =
      let
        try =
          (setPrecision p cNum)*(setPrecision p dDenom)
          - (setPrecision p dNum)*(setPrecision p cDenom)
      in
        if getAccuracy try == Exact then
          try
        else
          computeNum (p + q) p

{- auxiliary functions -}

makeRational :: ChPoly MPBall -> ChPoly Rational
makeRational (ChPoly dom (Poly ts) _) =
  ChPoly dom (Poly $ terms_map (rational . centre) ts) Nothing

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational
