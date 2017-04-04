{-# LANGUAGE TemplateHaskell #-}
module AERN2.Poly.Power.Type
where

import Numeric.MixedTypes
import AERN2.Poly.Basics
import AERN2.Utils.TH
import qualified Data.Map as Map
import AERN2.MP.Ball hiding (iterateUntilAccurate)
import AERN2.MP.Dyadic
import Data.List
import Data.Maybe

iterateUntilAccurate
  :: (Precision -> a -> b) -> (b -> Bool) -> Precision -> a -> b
iterateUntilAccurate f isAccurate pr x =
  let
    y = f pr x
  in
    case isAccurate y of
      True  -> y
      False -> iterateUntilAccurate f isAccurate (prec $ 2 * integer pr) x -- TODO 1.5* instead of 2*

iterateUntilDefined
  :: (Precision -> a -> Maybe b) -> Precision -> a -> b
iterateUntilDefined f pr x = y
  where
  Just y = iterateUntilAccurate f isJust pr x

_powPoly_geomPoly :: Integer -> PowPoly MPBall
_powPoly_geomPoly n = PowPoly $ Poly $ terms_fromList [(i, mpBall 1) | i <- [0..n]]

_powPoly_truncatedSin :: Integer -> PowPoly MPBall
_powPoly_truncatedSin n =
  PowPoly $ Poly $ terms_fromList
    ((0, mpBall 0) :
    [ (2*k + 1, convertP (prec 55) $ ((-1)^k) /(fac (2*k + 1)))  | k <- [0..n]])
  where
  fac k = foldl' (*) 1 [1..k]

_powPoly_truncatedSin_rat :: Integer -> PowPoly Rational
_powPoly_truncatedSin_rat n =
  PowPoly $ Poly $ terms_fromList
    [ (2*k + 1, ((-1)^k) /(fac (2*k + 1)))  | k <- [0..n]]
  where
  fac k = foldl' (*) 1 [1..k]

{- -}

data PowPoly c = PowPoly {powPoly_poly :: Poly c}

powPoly_centre :: PowPoly MPBall -> PowPoly MPBall
powPoly_centre (PowPoly (Poly ts)) =
  PowPoly $ Poly $ Map.map centreAsBall ts

{- distance from centre on interval [-1,1] -}
powPoly_radius :: PowPoly MPBall -> ErrorBound
powPoly_radius (PowPoly (Poly ts)) =
  Map.foldl' (+) (errorBound 0) (Map.map ball_error ts)

instance IsBall (PowPoly MPBall) where
  type CentreType (PowPoly MPBall) = PowPoly MPBall
  centre = powPoly_centre
  radius = powPoly_radius
  centreAsBallAndRadius p = (centre p, radius p)
  updateRadius updateFn p =
     let
     nr = mpBall $ dyadic $ updateFn (radius p)
     in
     (fromEndpoints (-nr) nr :: MPBall) + (powPoly_centre p)

instance (CanAddSameType c) => CanAddAsymmetric (PowPoly c) (PowPoly c) where
  type AddType (PowPoly c) (PowPoly c) = PowPoly c
  add (PowPoly p0) (PowPoly p1) = PowPoly (p0 + p1)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric $t (PowPoly c) where
      type AddType $t (PowPoly c) = PowPoly c
      add c (PowPoly p) = PowPoly $ add c p
  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
    instance (CanAddThis c $t, HasIntegers c) => CanAddAsymmetric (PowPoly c) $t where
      type AddType (PowPoly c) $t = PowPoly c
      add (PowPoly p) c = PowPoly $ add c p
  |]))

instance (CanSubSameType c, CanNegSameType c, CanAddSameType c)
  => CanSub (PowPoly c) (PowPoly c) where
  type SubType (PowPoly c) (PowPoly c) = PowPoly c
  sub (PowPoly p) (PowPoly q) = PowPoly $ p - q

instance (CanNegSameType c) => CanNeg (PowPoly c) where
  type NegType (PowPoly p) = PowPoly p
  negate (PowPoly p) = PowPoly $ -p

{- multiplication -}

$(declForTypes
  [[t| Integer |], [t| Rational |], [t| Dyadic |], [t| MPBall |]]
  (\ t -> [d|
      instance CanMulAsymmetric $t (PowPoly $t) where
        type MulType $t (PowPoly $t) = PowPoly $t
        mul c (PowPoly (Poly ts)) = PowPoly (Poly $ terms_map (c*) ts)
    |]))

instance
  (CanMulSameType c, CanAddSameType c, HasIntegers c,
   CanMulAsymmetric c (Poly c),
   MulType c (Poly c) ~ Poly c) =>
  CanMulAsymmetric (PowPoly c) (PowPoly c) where
  type MulType (PowPoly c) (PowPoly c) = PowPoly c
  mul (PowPoly (Poly ts0)) (PowPoly (Poly ts1)) =
    PowPoly $
    Map.foldl' (+)
    (Poly $ terms_fromList [(0, convertExactly 0)]) $
    Map.mapWithKey (\p c -> c*(Poly $ Map.mapKeys (+ p) ts1)) ts0

{- derivative -}

derivative :: (HasIntegers c, CanMulAsymmetric Integer c, MulType Integer c ~ c)
  => PowPoly c -> PowPoly c
derivative (PowPoly (Poly ts)) =
  PowPoly $ Poly $
    Map.mapKeys (\k -> k - 1) $ Map.mapWithKey (*) (Map.delete 0 ts)

-- should only be applied to exact polynomials
derivative_exact :: PowPoly MPBall -> PowPoly MPBall
derivative_exact (PowPoly (Poly ts)) =
  PowPoly $ Poly $
    Map.mapKeys (\k -> k - 1) $
     Map.mapWithKey
      (\k c -> iterateUntilAccurate (mwp k) isExact (getPrecision c) c) $
     Map.delete 0 ts
  where
  mwp i pr x = i * setPrecision pr x
  isExact x = dyadic (ball_error x) == dyadic 0

{- auxiliary functions and instances -}

instance (HasPrecision c) => HasPrecision (PowPoly c) where
  getPrecision (PowPoly (Poly ts)) =
    Map.foldl' (\p x -> min p (getPrecision x)) maximumPrecision ts

instance (CanSetPrecision c) => CanSetPrecision (PowPoly c) where
  setPrecision p (PowPoly (Poly ts)) =
    PowPoly $ Poly $ Map.map (setPrecision p) ts

instance (HasAccuracy c) => HasAccuracy (PowPoly c) where
  getAccuracy (PowPoly p) = getAccuracy p

instance (Show (Poly c)) => Show (PowPoly c) where
  show (PowPoly p) = show p

fromIntegerList :: (HasIntegers c) => [(Integer, Integer)] -> PowPoly c
fromIntegerList ts =
  PowPoly $ Poly $ terms_fromList $ map (\(i,c) -> (i, convertExactly c)) ts

fromList :: (HasIntegers c) => [(Integer, c)] -> PowPoly c
fromList ts = PowPoly $ Poly $ terms_fromList ts

degree :: PowPoly c -> Integer
degree (PowPoly (Poly ts)) = terms_degree ts


{- auxiliary functions -}
shiftRight :: Integer -> PowPoly c -> PowPoly c
shiftRight n (PowPoly (Poly ts)) = PowPoly $ Poly $ Map.mapKeys (+ n) ts
shiftLeft ::  Integer -> PowPoly c -> PowPoly c
shiftLeft n (PowPoly (Poly ts)) =
  PowPoly $ Poly $ Map.filterWithKey (\k _ -> k >= 0)  $ Map.mapKeys (\k -> k - n) ts
