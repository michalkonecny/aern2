{-|
    Module      :  AERN2.Poly.Cheb.Type
    Description :  Chebyshev basis unary sparse polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Chebyshev basis unary sparse polynomials
-}

module AERN2.Poly.Cheb.Type
(
  ChPoly(..), chPoly_terms, CanBeChPoly, chPoly, chPolyMPBall
, showInternals, fromDomToUnitInterval
, Degree, degree, reduceDegree, reduceDegreeWithLostAccuracyLimit
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.Normalize

import AERN2.Norm

import AERN2.MP.Accuracy
import AERN2.MP.Precision
import AERN2.MP.ErrorBound
import AERN2.MP.Ball hiding (ball_value)
import AERN2.MP.Dyadic

-- import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics
import AERN2.Poly.Conversion
import qualified AERN2.Poly.Power as Pow

chPolyMPBall :: (ConvertibleExactly t (ChPoly MPBall)) => t -> ChPoly MPBall
chPolyMPBall = convertExactly

{- Chebyshev polynomials with domain translation -}

data ChPoly c =
  ChPoly
  { chPoly_dom :: DyadicInterval
  , chPoly_poly :: Poly c
  , chPoly_lip :: c
  , chPoly_lowerBnd :: c
  , chPoly_upperBnd :: c
  }

chPoly_terms :: ChPoly c -> Terms c
chPoly_terms = poly_terms . chPoly_poly

instance Show (ChPoly MPBall) where
  show (ChPoly dom poly _ _ _) = show ppDom
    where
    pp = cheb2Power poly
    ppDom =
      Pow.translate ((rB+lB)/2) $
        Pow.contract (2/(rB-lB)) pp
    lB = mpBall l
    rB = mpBall r
    Interval l r = dom



showInternals :: (Show c) => ChPoly c -> String
showInternals (ChPoly dom (Poly terms) lip lb ub) =
  "ChPoly: dom = " ++ show dom ++
  ", lip = " ++ show lip ++
  ", lowerBbd = " ++ show lb ++
  ", upperBbd = " ++ show ub ++
  ", terms = " ++ show terms

fromDomToUnitInterval ::
  (CanAddSubMulDivBy t Dyadic) =>
  DyadicInterval -> t -> t
fromDomToUnitInterval (Interval l r) xInDom =
  (xInDom - m)/(0.5*(r-l))
  where
  m = (r+l)*0.5

instance HasDomain (ChPoly c) where
  type Domain (ChPoly c) = DyadicInterval
  getDomain = chPoly_dom

instance (IsBall c, HasIntegers c) => IsBall (ChPoly c) where
  type CentreType (ChPoly c) = ChPoly c
  radius (ChPoly _dom (Poly terms) _ _ _) =
    List.foldl' (+) (errorBound 0) $ map radius $ terms_coeffs terms
  centre cp@(ChPoly dom (Poly terms) lip lb ub) =
    ChPoly dom (Poly (terms_map centreAsBall terms)) (updateRadius (+r) lip) lb ub
    where
    r = radius cp
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)
  updateRadius updateFn (ChPoly dom (Poly terms)) =
    ChPoly dom (Poly $ terms_updateConst (updateRadius updateFn) terms)

instance
  (PolyCoeff c) =>
  CanNormalize (ChPoly c) where
  normalize = makeExactCentre . sweepUsingAccuracy

sweepUsingAccuracy ::
  (PolyCoeff c) =>
  ChPoly c -> ChPoly c
sweepUsingAccuracy (ChPoly dom poly@(Poly ts)) =
  ChPoly dom (Poly ts')
  where
  ts' = reduceTerms shouldKeep ts
  shouldKeep deg coeff =
    normLog2Accuracy (getNormLog coeff) <= deg + thresholdAcc
      -- prefer to remove terms with higher degree
  thresholdAcc = getFiniteAccuracy poly

{- constructors -}

instance (HasDyadics c) => HasVars (ChPoly c) where
  type Var (ChPoly c) = ()
  varFn sampleFn () =
    ChPoly dom (Poly terms)
    where
    dom@(Interval l r) = getDomain sampleFn
    terms = terms_fromList [(0, c0), (1, c1)]
    c0 = coeff $ (r + l) * 0.5
    c1 = coeff $ (r - l) * 0.5
    coeff = convertExactly

type CanBeChPoly c t = ConvertibleExactly t (ChPoly c)
chPoly :: (CanBeChPoly c t) => t -> (ChPoly c)
chPoly = convertExactly

instance (ConvertibleExactly t c) => ConvertibleExactly (DyadicInterval, t) (ChPoly c)
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly x of
      Right c -> Right $ ChPoly dom (Poly $ terms_fromList [(0,c)])
      Left e -> Left e

instance (ConvertibleExactly t c) => ConvertibleExactly (ChPoly c, t) (ChPoly c)
  where
  safeConvertExactly (ChPoly dom _, x) =
    case safeConvertExactly x of
      Right c -> Right $ ChPoly dom (Poly $ terms_fromList [(0,c)])
      Left e -> Left e

degree :: ChPoly c -> Integer
degree (ChPoly _ (Poly ts)) = terms_degree ts

{- precision -}

instance (HasPrecision c) => HasPrecision (ChPoly c) where
  getPrecision (ChPoly _ poly) = getPrecision poly

instance (PolyCoeff c) => CanSetPrecision (ChPoly c) where
  setPrecision p (ChPoly dom poly) = normalize $ ChPoly dom $ setPrecision p poly

{- accuracy -}

instance (HasAccuracy c, HasIntegers c, IsBall c) => HasAccuracy (ChPoly c) where
  getAccuracy = getAccuracy . radius

{-|
    Drop all terms that whose degree is above the given limit or whose norm is at or below the threshold.
    Compensate for the drops in the constant term.
-}
reduceDegree ::
  (PolyCoeff c) =>
  Degree -> ChPoly c -> ChPoly c
reduceDegree maxDegree p =
    p { chPoly_poly = Poly terms' }
    where
    (Poly terms) = chPoly_poly p
    terms' =
      reduceDegreeTerms maxDegree terms

{-|
    Drop all terms that whose degree is above the given limit or whose norm is at or below the threshold.
    Compensate for the drops in the constant term.
-}
reduceDegreeTerms ::
  (PolyCoeff c) =>
  Degree -> Terms c -> Terms c
reduceDegreeTerms maxDegree =
  reduceTerms shouldKeep
  where
  shouldKeep deg _coeff =
      deg <= maxDegree

reduceTerms ::
  (PolyCoeff c) =>
  (Degree -> c -> Bool) -> Terms c -> Terms c
reduceTerms shouldKeepPre terms
    | terms_size termsToRemove == 0 = terms
    | otherwise =
        terms_insertWith (+) 0 errorBall (terms_filter shouldKeep terms)
    where
    shouldKeep deg coeff = deg == 0 || shouldKeepPre deg coeff
    errorBall =
        sum $ map plusMinus $ terms_coeffs termsToRemove
        where
        plusMinus c = fromEndpoints (-c) c
    termsToRemove = terms_filter shouldRemove terms
    shouldRemove deg coeff =
        not $ shouldKeep deg coeff

instance
  (PolyCoeff c) =>
  CanReduceSizeUsingAccuracyGuide (ChPoly c)
  where
  reduceSizeUsingAccuracyGuide = reduceDegreeWithLostAccuracyLimit

reduceDegreeWithLostAccuracyLimit ::
  (PolyCoeff c) =>
  Accuracy -> ChPoly c -> ChPoly c
reduceDegreeWithLostAccuracyLimit accuracyLossLimit p =
    p { chPoly_poly = Poly terms' }
    where
    (Poly terms) = chPoly_poly p
    terms' =
      reduceDegreeWithLostAccuracyLimitTerms accuracyLossLimit terms

reduceDegreeWithLostAccuracyLimitTerms ::
  (PolyCoeff c) =>
  Accuracy -> Terms c -> Terms c
reduceDegreeWithLostAccuracyLimitTerms accuracyLossLimit (termsMap :: Terms c) =
  terms_updateConst (+ err) (terms_fromList termsToKeep)
  where
  termsDescList = terms_toDescList termsMap
  (err, termsToKeep) = dropTermsUntilLimit (convertExactly 0 :: c) [] termsDescList
  dropTermsUntilLimit _errSoFar _termsSoFar _remaingTerms@[] =
    error "reduceDegreeWithLostAccuracyLimitTerms: missing constant term"
  dropTermsUntilLimit errSoFar termsSoFar remaingTerms@[_constTerm] =
    (errSoFar, termsSoFar ++ remaingTerms)
  dropTermsUntilLimit errSoFar termsSoFar _remaingTerms@(term@(_d,cf):rest)
    | getAccuracy errNew >= accuracyLossLimit =
      dropTermsUntilLimit errNew termsSoFar rest
    | otherwise =
      dropTermsUntilLimit errSoFar (term : termsSoFar) rest
    where
    errNew = errSoFar + (plusMinus cf)
    plusMinus :: (Ring c, IsInterval c c) => c -> c
    plusMinus c = fromEndpoints (-c) c
