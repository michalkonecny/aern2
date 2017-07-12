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
  ChPolyMB, ChPoly(..), chPoly_terms
, ChPolyBounds(..), chPoly_maybeLip, chPoly_setLip
, CanBeChPoly, chPoly, chPolyMPBall
, showInternals, fromDomToUnitInterval
, serialise, deserialise
, Degree, degree, reduceDegree, reduceDegreeWithLostAccuracyLimit
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
import Text.Printf

import Text.Regex.TDFA

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import Control.CollectErrors

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
-- import AERN2.RealFun.UnaryBallFun

import AERN2.Poly.Basics
import AERN2.Poly.Conversion
import qualified AERN2.Poly.Power as Pow

chPolyMPBall :: (ConvertibleExactly t (ChPoly MPBall)) => t -> ChPoly MPBall
chPolyMPBall = convertExactly

{- Chebyshev polynomials with domain translation -}

type ChPolyMB = ChPoly MPBall

data ChPoly c =
  ChPoly
  { chPoly_dom :: DyadicInterval
  , chPoly_poly :: Poly c
  , chPoly_acGuide :: Accuracy
  , chPoly_maybeBounds :: Maybe (ChPolyBounds c)
  }

instance (SuitableForCE es) => CanEnsureCE es (ChPoly c)

data ChPolyBounds c =
  ChPolyBounds
  { chPolyBounds_lip :: c
  }
  deriving (Show)

instance (SuitableForCE es) => CanExtractCE es ChPoly
  where
  extractCE sample_es (ChPoly dom poly acG _mb) =
    fmap (\p -> ChPoly dom p acG Nothing) (extractCE sample_es poly)

chPoly_maybeLip :: ChPoly c -> Maybe c
chPoly_maybeLip = fmap chPolyBounds_lip . chPoly_maybeBounds

chPoly_setLip :: c -> ChPoly c -> ChPoly c -- TODO: use Maybe c instead?
chPoly_setLip lip (ChPoly dom poly acG _) =  ChPoly dom poly acG (Just $ ChPolyBounds lip)

chPoly_terms :: ChPoly c -> Terms c
chPoly_terms = poly_terms . chPoly_poly

chPoly_map_terms :: (Terms c -> Terms c) -> ChPoly c -> ChPoly c
chPoly_map_terms f cp =
  cp { chPoly_poly = (chPoly_poly cp) { poly_terms = f (chPoly_terms cp)} }

instance Show (ChPoly MPBall) where
  show (ChPoly dom poly _acG _) = show ppDom
    where
    pp = cheb2Power poly
    ppDom =
      Pow.translate ((rB+lB)/!2) $
        Pow.contract (2/!(rB-lB)) pp
    lB = mpBall l
    rB = mpBall r
    Interval l r = dom

showInternals :: (Show c) => ChPoly c -> String
showInternals (ChPoly dom (Poly terms) acG bnd) =
  "ChPoly: dom = " ++ show dom ++
  ", acG = " ++ show acG ++
  ", bounds = " ++ show bnd ++
  ", terms = " ++ show terms

serialise :: ChPolyMB -> String
serialise (ChPoly dom (Poly terms) acG _) =
  printf "(ChPoly (%s{--}) (Poly (terms_fromList %s{--})) (%s{--}) Nothing)"
    (show dom) (show $ terms_toList $ terms_map dyadicInterval terms) (show $ fromAccuracy acG)

deserialise :: BS.ByteString -> Maybe ChPolyMB
deserialise polyS =
  case groups of
    [domS,termsS, acG_S] ->
      case (reads (BS.unpack domS), reads (BS.unpack termsS), reads (BS.unpack acG_S)) of
        ([(dom,"")],[(terms,"")], [(acG_I :: Integer, "")]) ->
          Just $ ChPoly dom
                  (Poly $ terms_map mpBall $ terms_fromList (terms :: [(Integer, DyadicInterval)]))
                  (bits acG_I) Nothing
        _ -> Nothing
    _ -> Nothing
  where
  pat = "\\(ChPoly \\(([^{]*){--}\\) \\(Poly \\(terms_fromList ([^{]*){--}\\)\\) \\(([^{]*){--}\\) Nothing\\)"
  (_before,_whole,_after,groups) = polyS =~ pat
    :: (BS.ByteString,BS.ByteString,BS.ByteString,[BS.ByteString])
    -- :: (String,String,String,[String])

fromDomToUnitInterval ::
  (CanAddSubMulDivCNBy t Dyadic) =>
  DyadicInterval -> t -> t
fromDomToUnitInterval (Interval l r) xInDom =
  (xInDom - m)/!(half*(r-l))
  where
  m = (r+l)*half
  half = dyadic 0.5

instance HasDomain (ChPoly c) where
  type Domain (ChPoly c) = DyadicInterval
  getDomain = chPoly_dom

instance (IsBall c, HasIntegers c) => IsBall (ChPoly c) where
  type CentreType (ChPoly c) = ChPoly c
  radius (ChPoly _dom (Poly terms) _acG _) =
    List.foldl' (+) (errorBound 0) $ map radius $ terms_coeffs terms
  centre (ChPoly dom (Poly terms) acG _bnd) =
    ChPoly dom (Poly (terms_map centreAsBall terms)) acG Nothing
  centreAsBall = centre
  centreAsBallAndRadius cp = (centre cp, radius cp)
  updateRadius updateFn (ChPoly dom (Poly terms) acG _) =
    ChPoly dom (Poly $ terms_updateConst (updateRadius updateFn) terms) acG Nothing

instance CanNormalize (ChPoly MPBall) where
  normalize p =
    case chPoly_maybeLip p of
      Nothing  -> (makeExactCentre . sweepUsingAccuracy) p
      Just lip -> (chPoly_setLip lip . makeExactCentre . sweepUsingAccuracy) p

instance CanNormalize (ChPoly Integer) where
  normalize = chPoly_map_terms (terms_filterKeepConst (\_d c -> c /= 0))

instance CanNormalize (ChPoly Rational) where
  normalize = chPoly_map_terms (terms_filterKeepConst (\_d c -> c /= 0))

instance CanNormalize (ChPoly Dyadic) where
  normalize = chPoly_map_terms (terms_filterKeepConst (\_d c -> c /= 0))

sweepUsingAccuracy ::
  (PolyCoeffBall c) =>
  ChPoly c -> ChPoly c
sweepUsingAccuracy (ChPoly dom _poly@(Poly ts) acG bnd) =
  ChPoly dom (Poly ts') acG bnd
  where
  ts' = reduceTerms shouldKeep ts
  shouldKeep deg coeff =
    bits (getNormLog coeff) <= deg + thresholdAcc
      -- prefer to remove terms with higher degree
  thresholdAcc = acG -- getFiniteAccuracy poly

{- constructors -}

instance HasFnConstructorInfo (ChPoly c) where
  type FnConstructorInfo (ChPoly c) = (DyadicInterval, Accuracy)
  getFnConstructorInfo (ChPoly dom _ acG _) = (dom, acG)

instance (HasDyadics c, HasIntegers c) => HasVars (ChPoly c) where
  type Var (ChPoly c) = ()
  varFn (dom@(Interval l r), acG) () =
    ChPoly dom (Poly terms) acG Nothing
    where
    terms = terms_fromList [(0, c0), (1, c1)]
    c0 = coeff $ (r + l) * half
    c1 = coeff $ (r - l) * half
    half = dyadic 0.5
    coeff = convertExactly

type CanBeChPoly c t = ConvertibleExactly t (ChPoly c)
chPoly :: (CanBeChPoly c t) => t -> (ChPoly c)
chPoly = convertExactly

instance (ConvertibleExactly t c, HasIntegers c) => ConvertibleExactly ((DyadicInterval, Accuracy), t) (ChPoly c)
  where
  safeConvertExactly ((dom, acG), x) =
    case safeConvertExactly x of
      Right c -> Right $ ChPoly dom (Poly $ terms_fromList [(0,c)]) acG Nothing
      Left e -> Left e

instance (ConvertibleExactly t c, HasIntegers c) => ConvertibleExactly (ChPoly c, t) (ChPoly c)
  where
  safeConvertExactly (ChPoly dom _ acG _, x) =
    case safeConvertExactly x of
      Right c -> Right $ ChPoly dom (Poly $ terms_fromList [(0,c)]) acG Nothing
      Left e -> Left e

degree :: ChPoly c -> Integer
degree (ChPoly _ (Poly ts) _ _) = terms_degree ts

{- precision -}

instance (HasPrecision c) => HasPrecision (ChPoly c) where
  getPrecision (ChPoly _ poly _ _) = getPrecision poly

instance (CanSetPrecision c, CanNormalize (ChPoly c)) => CanSetPrecision (ChPoly c) where
  setPrecision p (ChPoly dom poly acG bnd) = normalize $ ChPoly dom (setPrecision p poly) acG bnd

{- accuracy -}

instance (HasAccuracy c, HasIntegers c, IsBall c) => HasAccuracy (ChPoly c) where
  getAccuracy = getAccuracy . radius

{-|
    Drop all terms that whose degree is above the given limit or whose norm is at or below the threshold.
    Compensate for the drops in the constant term.
-}
reduceDegree ::
  (PolyCoeffBall c) =>
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
  (PolyCoeffBall c) =>
  Degree -> Terms c -> Terms c
reduceDegreeTerms maxDegree =
  reduceTerms shouldKeep
  where
  shouldKeep deg _coeff =
      deg <= maxDegree

reduceTerms ::
  (PolyCoeffBall c) =>
  (Degree -> c -> Bool) -> Terms c -> Terms c
reduceTerms shouldKeep terms
    | terms_size termsToRemove == 0 = terms
    | otherwise =
        terms_insertWith (+) 0 errorBall (terms_filterKeepConst shouldKeep terms)
    where
    errorBall =
        sum $ map plusMinus $ terms_coeffs termsToRemove
        where
        plusMinus c = fromEndpoints (-c) c
    termsToRemove = terms_filterMayLoseConst shouldRemove terms
    shouldRemove deg coeff =
        deg /= 0 && (not $ shouldKeep deg coeff)

instance
  (PolyCoeffBall c) =>
  CanReduceSizeUsingAccuracyGuide (ChPoly c)
  where
  reduceSizeUsingAccuracyGuide = reduceDegreeWithLostAccuracyLimit

reduceDegreeWithLostAccuracyLimit ::
  (PolyCoeffBall c) =>
  Accuracy -> ChPoly c -> ChPoly c
reduceDegreeWithLostAccuracyLimit accuracyLossLimit p =
    p { chPoly_poly = Poly terms' }
    where
    (Poly terms) = chPoly_poly p
    terms' =
      reduceDegreeWithLostAccuracyLimitTerms accuracyLossLimit terms

reduceDegreeWithLostAccuracyLimitTerms ::
  (PolyCoeffBall c) =>
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
