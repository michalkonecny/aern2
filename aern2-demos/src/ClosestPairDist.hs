module ClosestPairDist where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Test.QuickCheck
import qualified Data.List as List

import AERN2.QA.Protocol
import AERN2.Real

-- import Debug.Trace

-- define a short name for the type of real numbers:
type R = CauchyReal

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
  quickCheck (closestPairDist_spec (closestPairDist_split compRApprox) (?bitsS 100) :: [Integer] -> Property)
closestPairDist_runTests2 =
  quickCheck (closestPairDist_spec (closestPairDist_split compMPBall) id :: [Integer] -> Property)

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
