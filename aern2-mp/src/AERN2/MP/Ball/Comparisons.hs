{-|
    Module      :  AERN2.MP.Ball.Comparisons
    Description :  Comparisons of arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Comparisons of arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Comparisons
(
  -- * Auxiliary types
  module AERN2.Norm
  -- * Ball operations (see also instances)
  , reducePrecionIfInaccurate
  , intersect
  -- * Helpers for constructing ball functions
  , byEndpointsMP
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.Norm
import AERN2.MP.Dyadic (Dyadic)
import AERN2.MP.Float (MPFloat)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions (integerBounds)

{- comparisons -}

instance HasEqAsymmetric MPBall MPBall where
  type EqCompareType MPBall MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Integer where
  type EqCompareType MPBall Integer = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Integer MPBall where
  type EqCompareType Integer MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Int where
  type EqCompareType MPBall Int = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Int MPBall where
  type EqCompareType Int MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Rational where
  type EqCompareType MPBall Rational = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Rational MPBall where
  type EqCompareType Rational MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Dyadic where
  type EqCompareType MPBall Dyadic = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Dyadic MPBall where
  type EqCompareType Dyadic MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasOrderAsymmetric MPBall MPBall where
  type OrderCompareType MPBall MPBall = Maybe Bool
  lessThan b1 b2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    (l2, r2) = endpointsMP b2
  leq b1 b2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    (l2, r2) = endpointsMP b2

instance HasOrderAsymmetric Integer MPBall where
  type OrderCompareType Integer MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Integer where
  type OrderCompareType MPBall Integer = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Int MPBall where
  type OrderCompareType Int MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Int where
  type OrderCompareType MPBall Int = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Dyadic MPBall where
  type OrderCompareType Dyadic MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Dyadic where
  type OrderCompareType MPBall Dyadic = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric MPBall Rational where
  type OrderCompareType MPBall Rational = Maybe Bool
  lessThan b1 q2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    l2 = q2
    r2 = q2
  leq b1 q2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpointsMP b1
    l2 = q2
    r2 = q2

instance HasOrderAsymmetric Rational MPBall where
  type OrderCompareType Rational MPBall = Maybe Bool
  lessThan q1 b2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l2, r2) = endpointsMP b2
    l1 = q1
    r1 = q1
  leq q1 b2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l2, r2) = endpointsMP b2
    l1 = q1
    r1 = q1

instance CanTestZero MPBall
instance CanTestPosNeg MPBall

instance CanTestInteger MPBall where
  certainlyNotInteger b =
    (rN - lN) == 1 && lN !<! b && b !<! rN
    where
      (lN, rN) = integerBounds b
  certainlyIntegerGetIt b
    | rN == lN = Just lN
    | otherwise = Nothing
    where
      (lN, rN) = integerBounds b

instance CanMinMaxAsymmetric MPBall MPBall where
  min = byEndpointsMP min
  max = byEndpointsMP max

instance CanMinMaxAsymmetric MPBall Integer where
  type MinMaxType MPBall Integer = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Integer MPBall where
  type MinMaxType Integer MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Int where
  type MinMaxType MPBall Int = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Int MPBall where
  type MinMaxType Int MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Dyadic where
  type MinMaxType MPBall Dyadic = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Dyadic MPBall where
  type MinMaxType Dyadic MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Rational where
  type MinMaxType MPBall Rational = MPBall
  min = convertPSecond min
  max = convertPSecond max
instance CanMinMaxAsymmetric Rational MPBall where
  type MinMaxType Rational MPBall = MPBall
  min = convertPFirst min
  max = convertPFirst max

{- intersection -}

intersect :: MPBall -> MPBall -> MPBall
intersect a b
  | rL > rR = error $ "intersect: empty intersection: " ++ show a ++ "; " ++ show b
  | otherwise = fromEndpointsMP rL rR
  where
  rL = max aL bL
  rR = min aR bR
  (aL,aR) = endpointsMP a
  (bL,bR) = endpointsMP b

{-|
  Computes an *increasing* ball fucntion @f@ from *exact* MPFR operations.
-}
byEndpointsMP ::
    (MPFloat -> MPFloat -> MPFloat) ->
    (MPBall -> MPBall -> MPBall)
byEndpointsMP op b1 b2 =
    fromEndpointsMP (l1 `op` l2) (r1 `op` r2)
    where
    (l1,r1) = endpointsMP b1
    (l2,r2) = endpointsMP b2

{-  random generation -}
