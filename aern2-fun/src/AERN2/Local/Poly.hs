module AERN2.Local.Poly
(
  variable
, genericisePoly
, LocalPoly
)
where

import MixedTypesNumPrelude
import AERN2.Interval
import AERN2.MP.Ball
import AERN2.Poly.Cheb
import AERN2.RealFun.Operations
import AERN2.Local.Basics

import AERN2.MP.Dyadic
import AERN2.Poly.Power.RootsInt
import AERN2.Poly.Power.SignedSubresultant
import AERN2.Poly.Basics hiding (Terms)
import AERN2.Poly.Conversion
import Data.Ratio
import qualified Data.Map as Map
import AERN2.Local.Maximum

type LocalPoly a = Local (ChPoly a)

variable :: LocalPoly MPBall
variable l r ac =
  setPrecision ((prec . fromAccuracy) ac) x
  where
  x :: ChPoly MPBall
  x = varFn (dom, ac) ()
  dom = Interval l r

debug_useSeparablePart :: Bool
debug_useSeparablePart = False

instance GenericMaximum (LocalPoly MPBall) where
  genericise f l r ac =
    [genericisePoly fI l r]
    where
    fI = f l r ac

genericisePoly :: ChPoly MPBall -> Dyadic -> Dyadic -> (Dyadic, Dyadic, MPBall -> MPBall, Accuracy, Rational -> Rational, DyadicInterval, Terms)
genericisePoly fI a b =
  (a, b, evalF, fAcc, evalDF, dom, bsI)
  where
  Interval l r = getDomain fI
  evalF = evalDf fI (2/!(r - l) * dfI)
  fAcc = getAccuracy fI
  evalDF = evalDirect dfRat :: Rational -> Rational
  ch2Power (e, p) = (e, cheb2Power p)
  dfI    = (derivativeExact . centre) fI
  dfRat = makeRational dfI
  (eI, dfIPow) = (ch2Power . intify) dfI
  dom = chPoly_dom fI
  aI = fromDomToUnitInterval dom (rational l)
  bI = fromDomToUnitInterval dom (rational r)
  dfIPow' =
    if debug_useSeparablePart
      && dyadic eI == 0
      && degree dfI < 50
    then
      separablePart dfIPow
    else
      dfIPow
  bsI = initialBernsteinCoefs dfIPow' eI aI bI

makeRational :: ChPoly MPBall -> ChPoly Rational
makeRational (ChPoly dom (Poly ts) acG _) =
  ChPoly dom (Poly $ terms_map (rational . centre) ts) acG Nothing

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _ _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational
