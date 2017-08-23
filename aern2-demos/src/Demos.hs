{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Demos where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Test.QuickCheck
import qualified Data.List as List

import AERN2.QA.Protocol
import AERN2.Real

-- import Debug.Trace

{- type classes -}

-- toStrings :: (Show a) => [a] -> [String]
toStrings xs = map show xs

-- toStrings_bad = toStrings [io_run1, io_run2]

{- real numbers -}

type R = CauchyReal
-- Real numbers represented by fast converging Cauchy sequences of dyadic intervals
type RA to = CauchyRealA to
-- Real numbers to which one can send an accuracy query in any QAArrow

-- internally:
-- CauchyReal = Sequence MPBall
-- Sequence t = ... (AccuracySG -> t)

{- accuracy guide -}

sumSines1 n = sum [sin i | i <- [1..n]]

sumSines1_run1 = sumSines1 100
sumSines1_run2 = (sumSines1 100) ? (bitsSG 100 200)
sumSines1_run3 = (sumSines1 100) ? (bitsSG 100 100)

{- comparisons -}

pi100 = pi?(bitsS 100)

compare_run1 = pi100 > 0
compare_run2 = pi100 == pi100

compare_run3 = pi > 0
compare_run4 = pi == pi + 0.5^!1000
compare_run5 = (pi == pi + 0.5^!1000) ? (bitsS 100)
compare_run6 = real 0 == 0

{- partial function checking -}

-- partialfn_run0 = sqrt (-1) :: R
partialfn_run1 = sqrt (-1)
partialfn_run2 = sqrt 0
partialfn_run3 = sqrt (pi-pi)
partialfn_run4 = (~!) (sqrt (pi-pi))

{- pif -}

-- myabs0 :: R -> R
myabs0 x = if x < 0 then -x else x

type RCN = CauchyRealCN

myabs1 :: R -> RCN
myabs1 x = if x < 0 then -x else x

myabs2 :: R -> R
myabs2 x = (~!) $ if x < 0 then -x else x

{- lim -}

-- TODO


-----------------------------------------
-- Cauchy reals vs iRRAM style execution
-----------------------------------------

logistic1 :: _ => Rational -> Integer -> t -> t
logistic1 c n x0 =
  (foldl1 (.) (replicate n lg)) x0
  where
  lg x = c * x * (1-x)

logisticR_run1 = logistic1 3.82 1000 (real 0.5)

logisticMB1_run1 =
  logistic1 3.82 1000 (mpBallP (prec 1000) 0.5)
logisticMB1_run2 =
  logistic1 3.82 1000 (mpBallP (prec 2000) 0.5)

logistic2 :: _ => Rational -> Integer -> t -> Maybe t
logistic2 c n x0 =
  (foldl1 (.) (replicate n lg)) (Just x0)
  where
  lg Nothing = Nothing
  lg (Just x)
    | getAccuracy res > 0 = Just res
    | otherwise = Nothing
    where
    res = c * x * (1-x)

logisticMB2_run1 =
  logistic2 3.82 10000 (mpBallP (prec 10000) 0.5)
logisticMB2_run2 =
  logistic2 3.82 10000 (mpBallP (prec 20000) 0.5)

logisticIter :: Rational -> Integer -> R -> R
logisticIter c n =
  irramEval (logistic2 c n)

irramEval :: (MPBall -> Maybe MPBall) -> (R -> R)
irramEval f x =
  newCR "irramEval" [] getAnswer
  where
  getAnswer _ (AccuracySG acS _acG) =
    case (last $ iterateUntilAccurate acS withPrecision) of
      (_,Just res) -> res
      _ -> error "irramEval failed"
    where
    withPrecision p = f xWithP
      where
      xWithP = (setPrecision p (x ? (accuracySG $ bits p)))


----------------------------------
-- Finding the smallest distance within a set of real numbers
----------------------------------

closestPairDist_naive ::
  _ => [t] -> t
closestPairDist_naive pts
  | length pts < 2 = error "closestPairDist_naive: too few points"
  | otherwise =
      (foldl1 min (map distance (distinctPairs pts)))

distance :: (CanSubSameType t, CanAbsSameType t) => (t, t) -> t
distance (a,b) = abs (a-b)

closestPairDist_run ::
  _ =>
  ([t] -> t) ->
  Integer -> t
closestPairDist_run (closestPairDist :: [t] -> t) n =
  closestPairDist [sin (convertExactly i :: t) | i <- [1..n]]

{- specification and randomised tests -}

closestPairDist_spec closestPairDist (getFinite :: r -> t) numbers =
  (length numbers) < 2
  .||.
  (getFinite (closestPairDist numbersR)) ?==?$ (closestPairDist_naive numbers)
  where
  numbersR = map convertExactly numbers :: [r]
  a ?==?$ b = printArgsIfFails2 "?==?" (?==?) a b

closestPairDist_runTests1 =
  quickCheck (closestPairDist_spec (closestPairDist_split compRApprox) (?acStd) :: [Integer] -> Property)
closestPairDist_runTests2 =
  quickCheck (closestPairDist_spec (closestPairDist_split compMPBall) id :: [Integer] -> Property)

acStd = bitsSG 100 120

sample_integers = sample' (arbitrary :: Gen [Integer]) >>= mapM_ print
sample_rationals = sample' (arbitrary :: Gen [Rational]) >>= mapM_ print

{- a version that splits, recurses and merges the results -}
closestPairDist_split ::
  _ => (t -> t -> Bool) -> [t] -> t
closestPairDist_split (.<) pts
  | length ptsL < 2 || length ptsR < 2 =
      closestPairDist_naive pts
  | otherwise =
      recurseAndMerge
  where
  (ptsL,ptsR) = List.partition isCertainlyLeft pts
    where
    isCertainlyLeft x = x .< average pts
  recurseAndMerge =
    -- foldl1 min [dL, dLR, dR]
    foldl1 min [dL, dR]
    where
    dL = closestPairDist_split (.<) ptsL
    -- dLR = distance (largest ptsL, smallest ptsR)
    dR = closestPairDist_split (.<) ptsR

compRApprox :: R -> R -> Bool
compRApprox a b = (a?ac) !<! (b?ac)
  where
  ac = bitsS 100

compMPBall :: MPBall -> MPBall -> Bool
compMPBall = (!<!)

{- auxiliary functions -}

hull :: MPBall -> MPBall -> MPBall
hull = fromEndpoints

average :: (HasIntegers t, CanAddSameType t, CanDivCNBy t Integer) => [t] -> t
average xs = (sum xs) /! (length xs)

largest :: (CanMinMaxSameType t) => [t] -> t
largest pts = foldl1 max pts

smallest :: (CanMinMaxSameType t) => [t] -> t
smallest pts = foldl1 min pts

distinctPairs :: [t] -> [(t,t)]
distinctPairs xs = [(x,y) | (x:rest) <- tails1 xs, y <- rest]

{-| non-empty tails -}
tails1 :: [t] -> [[t]]
tails1 list =
  take (length list - 1) $ List.tails list
