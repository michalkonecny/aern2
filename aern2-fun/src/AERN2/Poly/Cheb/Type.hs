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
-- (
-- )
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.Norm

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

type PolyBall = ChPoly MPBall

polyBall :: (ConvertibleExactly t PolyBall) => t -> PolyBall
polyBall = convertExactly

{- Chebyshev polynomials with domain translation -}

data ChPoly c = ChPoly { chPoly_dom :: DyadicInterval, chPoly_poly :: Poly c }

instance Show PolyBall where
  show (ChPoly dom poly) = show ppDom
    where
    pp = cheb2Power poly
    ppDom =
      Pow.translate ((rB+lB)/2) $
        Pow.contract (2/(rB-lB)) pp
    lB = mpBall l
    rB = mpBall r
    Interval l r = dom

instance HasDomain (ChPoly c) where
  type Domain (ChPoly c) = DyadicInterval
  getDomain = chPoly_dom

instance (IsBall c, HasIntegers c) => IsBall (ChPoly c) where
  type CentreType (ChPoly c) = ChPoly c
  radius (ChPoly _dom (Poly terms)) =
    List.foldl' (+) (errorBound 0) $ map radius $ terms_coeffs terms
  centre (ChPoly dom (Poly terms)) =
    ChPoly dom (Poly (terms_map centreAsBall terms))
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)
  updateRadius updateFn (ChPoly dom (Poly terms)) =
    ChPoly dom (Poly $ terms_updateConst (updateRadius updateFn) terms)

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


{- precision -}

instance (HasPrecision c) => HasPrecision (ChPoly c) where
  getPrecision (ChPoly _ poly) = getPrecision poly

instance (CanSetPrecision c) => CanSetPrecision (ChPoly c) where
  setPrecision p (ChPoly dom poly) = ChPoly dom $ setPrecision p poly

{- accuracy -}

instance (HasAccuracy c, HasIntegers c, IsBall c) => HasAccuracy (ChPoly c) where
  getAccuracy = getAccuracy . radius

{-|
    Drop all terms that whose degree is above the given limit or whose norm is at or below the threshold.
    Compensate for the drops in the constant term.
-}
reduceDegreeAndSweep ::
  (Ring c, IsInterval c c, HasNorm c) =>
  Degree -> NormLog -> ChPoly c -> ChPoly c
reduceDegreeAndSweep maxDegree thresholdNormLog p =
    p { chPoly_poly = Poly terms' }
    where
    (Poly terms) = chPoly_poly p
    terms' =
      reduceDegreeAndSweepTerms maxDegree thresholdNormLog terms

{-|
    Drop all terms that whose degree is above the given limit or whose norm is at or below the threshold.
    Compensate for the drops in the constant term.
-}
reduceDegreeAndSweepTerms ::
  (Ring c, IsInterval c c, HasNorm c) =>
  Degree -> NormLog -> Terms c -> Terms c
reduceDegreeAndSweepTerms maxDegree thresholdNormLog terms
    | terms_size koTerms == 0 = terms
    | otherwise =
        terms_insertWith (+) 0 errorBall (terms_filter isOK terms)
    where
    errorBall =
        sum $ map plusMinus $ terms_coeffs koTerms
        where
        plusMinus c = fromEndpoints (-c) c
    koTerms = terms_filter isNotOK terms
    isOK deg coeff =
        deg <= maxDegree && (deg == 0 || getNormLog coeff > thresholdNormLog)
    isNotOK deg coeff =
        not $ isOK deg coeff
