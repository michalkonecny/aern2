module AERN2.MPoly.Power.Type where

import MixedTypesNumPrelude

import Data.Function ((&))

import Data.Map (Map)
import qualified Data.Map as Map
import AERN2.Util.Vector (Vector, (!), (!.), vlength)
import qualified AERN2.Util.Vector as V

import AERN2.MPoly.Type (MPoly)
import AERN2.MPoly.MultiIndex
import qualified AERN2.MPoly.Type as MPoly
import AERN2.Interval
import AERN2.MP.Accuracy
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Box (Box, DyadicBox)
import qualified AERN2.Box as Box

import AERN2.Poly.Cheb (ChPoly)
import AERN2.Poly.Power (PowPoly(..))
import AERN2.Poly.Basics (Poly(..))
import qualified AERN2.Poly.Cheb as Cheb
import AERN2.Poly.Conversion
import Debug.Trace

import Data.Maybe

variable :: Box Dyadic -> Integer -> PowMPoly MPBall
variable bx k =
  PowMPoly
    bx
    (MPoly.MPoly n ts)
    (bits 53)
    where
    n = Box.dim bx
    delta i j
      | i == j    = 1
      | otherwise = 0
    ts = Map.fromList [(zeroIndex n, mpBall 0), (V.generate (int n) (delta k), mpBall 1)]

zero :: Box Dyadic -> PowMPoly MPBall
zero bx =
  PowMPoly
    bx
    (MPoly.MPoly n ts)
    (bits 53)
  where
  n = Box.dim bx
  ts = Map.fromList [(V.generate (int n) (const 0), mpBall 0)]

data PowMPoly a =
  PowMPoly {
      doms    :: DyadicBox
    , poly    :: MPoly a
    , acGuide :: Accuracy
  }

terms :: PowMPoly a -> Map MultiIndex a
terms (PowMPoly _ds (MPoly.MPoly _d ts) _ac) = ts

constantTerm :: (HasIntegers a) => PowMPoly a -> a
constantTerm p =
  fromJust $ Map.lookup (zeroIndex $ dim p) (terms p)

instance IsBall (PowMPoly MPBall) where
  type CentreType (PowMPoly MPBall) = PowMPoly MPBall
  centre p = updateTerms centreAsBall p
  radius p = radius $ fromJust $ Map.lookup (zeroIndex $ dim p) (terms p)
  centreAsBallAndRadius p = (centre p, radius p)
  updateRadius f (PowMPoly ds (MPoly.MPoly d ts) ac) =
    (PowMPoly ds (MPoly.MPoly d ts') ac)
    where
    ts' = Map.update (\x -> Just (updateRadius f x)) (zeroIndex d) ts

instance (HasAccuracy a, HasIntegers a) => HasAccuracy (PowMPoly a) where
  getAccuracy = getAccuracy . constantTerm

updateTerms :: (a -> a) -> PowMPoly a -> PowMPoly a
updateTerms f (PowMPoly ds (MPoly.MPoly d ts) ac) =
  PowMPoly ds (MPoly.MPoly d ts') ac
  where
  ts' = Map.map f ts

filterTermsWithKey :: (MultiIndex -> a -> Bool) -> PowMPoly a -> PowMPoly a
filterTermsWithKey f (PowMPoly ds (MPoly.MPoly d ts) ac) =
  PowMPoly ds (MPoly.MPoly d ts') ac
  where
  ts' = Map.filterWithKey f ts

filterTerms :: (a -> Bool) -> PowMPoly a -> PowMPoly a
filterTerms f (PowMPoly ds (MPoly.MPoly d ts) ac) =
  PowMPoly ds (MPoly.MPoly d ts') ac
  where
  ts' = Map.filter f ts


type MPolyCoeff a =
  (Show a
  ,IsBall a, HasIntegers a,
   CanAbsSameType a, HasOrderAsymmetric a Dyadic,
   HasEqAsymmetric a a, EqCompareType a a ~ Maybe Bool,
   OrderCompareType a Dyadic ~ Maybe Bool,
   CanBeErrorBound a, CanAddSameType a, CanMulSameType a,
   CanDivCNBy a Integer, CanPowCNBy a Integer,
   CanSubSameType a, ConvertibleExactly Dyadic a)

sweepAndNormalise ::
  (IsBall a, HasIntegers a,
   CanAbsSameType a, HasOrderAsymmetric a Dyadic,
   HasEqAsymmetric a a, EqCompareType a a ~ Maybe Bool,
   OrderCompareType a Dyadic ~ Maybe Bool,
   CanBeErrorBound a)
   => PowMPoly a -> PowMPoly a
sweepAndNormalise ((PowMPoly ds (MPoly.MPoly d ts) ac) :: PowMPoly a) = -- TODO: error does not take into account domain yet.
  PowMPoly ds p' ac
  where
  zeroBall r = updateRadius (+ r) (convertExactly 0) :: a
  sweepingThreshold = (dyadic 0.5)^!(fromAccuracy ac)
  sweep x = zeroBall (errorBound $ abs x)
  tsSweeped =
    Map.map
      (\x -> if (abs x) !<! sweepingThreshold then sweep x else x)
      ts
  err = Map.foldl' (\x y -> x + (radius y)) (errorBound 0) tsSweeped
  p'  = MPoly.MPoly d tsNormalised
  tsNormalised =
     tsSweeped
     & Map.map centreAsBall
     & Map.filter (\x -> not $ x !==! (convertExactly 0 :: a))
     & Map.insertWith (\b x -> updateRadius (+ (radius b)) x) (zeroIndex d) (zeroBall err)

coef :: (HasIntegers a) => PowMPoly a -> MultiIndex -> a
coef (PowMPoly _ p _) i = MPoly.coef p i

dim :: PowMPoly a -> Integer
dim (PowMPoly _ds p _acG) = MPoly.dim p

fromCheb ::
  (CanMulSameType (PowPoly a), HasIntegers a, CanAddSameType a)
  => ChPoly a -> Integer -> Integer -> PowMPoly a -- TODO: support arbitrary domains
fromCheb f n k =
  PowMPoly (Box.unitCube n) (MPoly.MPoly n ts') (getAccuracyGuide f)
  where
  PowPoly (Poly ts) = cheb2Power (Cheb.chPoly_poly f)
  ts' =
    Map.mapKeys
    (\i -> V.generate (int n) (\j -> if k == j then i else 0))
    ts


instance (Show a) => (Show (PowMPoly a)) where
  show p
    | dim p <= 9 = showSmallDim p
    | otherwise  = showLargeDim p

showSmallDim :: (Show a) => (PowMPoly a) -> String
showSmallDim  (PowMPoly _ (MPoly.MPoly _n ts) _) =
  concatMap showterm (Map.toList ts)
  where
  showterm (i, a) =
    (show a) ++ "*" ++ (showMonomial i) ++ " + "
  showMonomial i =
    concat
      [
      if i !. k == 0 then
        ""
      else if i !.k == 1 then
        (varName k)
      else
        (varName k)++"^"++(show $ i !. k) | k <- [0 .. (vlength i) - 1]
      ]
  varName k
    | k == 0 = "x"
    | k == 1 = "y"
    | k == 2 = "z"
    | k == 3 = "w"
    | k == 4 = "u"
    | k == 5 = "v"
    | k == 6 = "s"
    | k == 7 = "t"
    | k == 8 = "r"
    | otherwise = error "showSmallDim: dimension too high"

showLargeDim :: (Show a) => (PowMPoly a) -> String
showLargeDim (PowMPoly _ (MPoly.MPoly _n ts) _) =
  concatMap showterm (Map.toList ts)
  where
  showterm (i, a) =
    (show a) ++ "*" ++ (showMonomial i) ++ " + "
  showMonomial i =
    concat
      [
      if i !. k == 0 then
        ""
      else
        "x_"++(show k)++"^"++(show $ i !. k) | k <- [0 .. (vlength i) - 1]
      ]

degree :: PowMPoly a -> Integer
degree f =
  let
    ts = terms f
  in
  if Map.null ts then
    -1
  else
    (size . fst . Map.findMax) (terms f)

reduceDegree :: Integer -> PowMPoly MPBall -> PowMPoly MPBall
reduceDegree d p =
  updateRadius (+ err) $ filterTermsWithKey (\k _ -> size k <= d) p
  where
  ts = terms p
  highDegreeTerms = Map.filterWithKey (\k _ -> size k > d) ts
  err =
    Map.foldl' (\e x -> e + (errorBound $ abs x)) (errorBound 0) highDegreeTerms -- TODO this does not take into account the contribution from the variables

reduceDegreeToAccuracy :: PowMPoly MPBall -> PowMPoly MPBall
reduceDegreeToAccuracy p =
  aux 1 (degree p) p
  where
  ac = getAccuracyGuide p `min` getAccuracy p
  aux i j q =
    let
      m = (i + j) `div` 2
      q' = reduceDegree m p
    in
    if m == i || m == j then
      q
    else if getAccuracy q' >= ac then
      aux i m q'
    else
      aux m j q

instance HasAccuracyGuide (PowMPoly a) where
  getAccuracyGuide = acGuide

instance (HasPrecision a) => HasPrecision (PowMPoly a) where
  getPrecision (PowMPoly _ds p _acG) = getPrecision p

instance (CanSetPrecision a) => CanSetPrecision (PowMPoly a) where
  setPrecision prc (PowMPoly ds p acG) =
    PowMPoly ds (setPrecision prc p) acG

instance (CanSetPrecision a) => CanSetAccuracyGuide (PowMPoly a) where
  setAccuracyGuide ac f@(PowMPoly ds p _) =
    setPrecisionAtLeastAccuracy (ac + (degree f)) $
      PowMPoly ds p ac

liftDimensions :: PowMPoly a -> PowMPoly a -> (PowMPoly a, PowMPoly a)
liftDimensions c0@(PowMPoly ds0 p0 acG0) c1@(PowMPoly ds1 p1 acG1) =
  if dim c0 == dim c1 then
    if domainsMatch ds0 ds1 then
      (c0,c1)
    else
      error "PowMPoly liftDimensions: domains don't match."
  else if dim c0 < dim c1 then
    let
      ds0' = ds0 `addDomains` ds1
    in
    if domainsMatch ds0' ds1 then
      ((PowMPoly ds0' p0 acG0), c1)
    else
      error "PowMPoly liftDimensions: domains don't match."
  else
    let
      ds1' = ds1 `addDomains` ds0
    in
    if domainsMatch ds0 ds1' then
      (c0, (PowMPoly ds1' p1 acG1))
    else
      error "PowMPoly liftDimensions: domains don't match."
  where
  domainsMatch dsA dsB =
    auxD (vlength dsA - 1) dsA dsB
  auxD (-1) _ _  = True
  auxD n dsA dsB =
    if (dsA !. n) /= (dsB !. n) then False else auxD (n - 1) dsA dsB
  addDomains dsA dsB =
    V.generate
      (V.length dsB)
      (\k -> if k < V.length dsA then (dsA V.! k) else (dsB V.! k))
