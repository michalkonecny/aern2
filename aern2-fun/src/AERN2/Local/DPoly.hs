module AERN2.Local.DPoly
(
  DPoly(..)
)
where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Interval
import AERN2.Local.Maximum
import AERN2.Local.Poly
import AERN2.Poly.Cheb

import AERN2.Poly.Power.RootsInt
import AERN2.Poly.Power.SignedSubresultant
import AERN2.Poly.Basics hiding (Terms)
import AERN2.Poly.Conversion

import qualified Data.Map as Map
import Data.Ratio

data DPoly = DPoly (LocalPoly MPBall) (MPBall -> MPBall) (MPBall -> MPBall)

instance GenericMaximum DPoly where
  genericise dp a b ac =
    [genericiseDPoly dp a b ac]

-- currently not using local Lipschitz information
genericiseDPoly :: DPoly -> Dyadic -> Dyadic -> Accuracy -> (Dyadic, Dyadic, MPBall -> MPBall, Accuracy, Rational -> Rational, DyadicInterval, Terms)
genericiseDPoly (DPoly fLoc evalLoc _loclip) a b ac =
  (a, b, evalF, fAcc, evalDF, dom, bsI)
  where
  fI = fLoc a b ac
  evalF x =
    {-let lip = loclip x in
    if lip !<! dyadic 0.125 then
      let
      (l :: MPBall, r :: MPBall) = endpoints x
      m     = (dyadic 0.5)*(l + r)
      fm    = evalLoc m
      errBall = lip*((r-l)*(dyadic 0.5))*unitBall
      unitBall = mpBall (0,1)
      in
        fm + errBall
      else-}
        evalLoc x
    {-let
    (l :: MPBall, r :: MPBall) = endpoints x
    m     = (dyadic 0.5)*(l + r)
    fm    = evalLoc m
    errBall = (loclip x)*((r-l)*(dyadic 0.5))*unitBall
    unitBall = mpBall (0,1)
    in
    fm + errBall-}
  fAcc = getAccuracy fI
  evalDF = evalDirect dfRat :: Rational -> Rational
  ch2Power (e, p) = (e, cheb2Power p)
  dfI    = (derivativeExact . centre) fI
  dfRat  = makeRational dfI
  (eI, dfIPow) = (ch2Power . intify) dfI
  dom = chPoly_dom fI
  aI = fromDomToUnitInterval dom (rational a)
  bI = fromDomToUnitInterval dom (rational b)
  bsI = initialBernsteinCoefs (separablePart dfIPow) eI aI bI

makeRational :: ChPoly MPBall -> ChPoly Rational
makeRational (ChPoly dom (Poly ts) acG _) =
  ChPoly dom (Poly $ terms_map (rational . centre) ts) acG
    (error "makeRational does not define bounds")

intify :: ChPoly MPBall -> (ErrorBound, Poly Integer)
intify (ChPoly _ p _ _) =
  (err, pInt)
  where
  termsRational = terms_map (rational . ball_value) (poly_terms p)
  err = termsError * termsDenominator
  termsError = ball_error $ terms_lookupCoeff (poly_terms p) 0
  termsDenominator = Map.foldl' lcm 1 $ terms_map denominator termsRational
  pInt = Poly $ terms_map (numerator . (* termsDenominator)) termsRational
