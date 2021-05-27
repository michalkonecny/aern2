{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-|
    Module      :  AERN2.Real.Introduction
    Description :  aern2-real introductory examples
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    You can run the examples in this file in ghci.
    If you installed AERN2 using the official instructions,
    you can start ghci using the following command in the base
    folder:

    @
    stack repl aern2-real/examples/AERN2/Real/Examples/ClosestPairDist.hs
    @
-}
module AERN2.Real.Examples.ClosestPairDist where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Test.QuickCheck
import qualified Data.List as List

import AERN2.MP
import AERN2.Real

-- import Debug.Trace

-- define a short name for the type of real numbers:
type R = CReal

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

closestPairDist_run_naive :: Integer -> R
closestPairDist_run_naive =
  closestPairDist_run closestPairDist_naive 

closestPairDist_run_split :: Integer -> R
closestPairDist_run_split =
  closestPairDist_run $ closestPairDist_split compRApprox

{- Example runs:

*AERN2.Real.Examples.ClosestPairDist> closestPairDist_run_naive 1000 ? (prec 1000)
[0.00000013295546744391165086... ± ~0.0000 ~2^(-1221)]
(13.80 secs, 12,017,593,904 bytes)

*AERN2.Real.Examples.ClosestPairDist> closestPairDist_run_split 1000 ? (prec 1000)
[0.00000013295546744391165086... ± ~0.0000 ~2^(-1221)]
(4.95 secs, 9,979,768,504 bytes)

-}

{- specification and randomised tests -}

closestPairDist_spec closestPairDist (getFinite :: r -> t) numbers =
  (length numbers) < 2
  .||.
  (getFinite (closestPairDist numbersR)) ?==?$ (closestPairDist_naive numbers)
  where
  numbersR = map convertExactly numbers :: [r]
  a ?==?$ b = printArgsIfFails2 "?==?" (?==?) a b

closestPairDist_runTests1 =
  quickCheck (closestPairDist_spec (closestPairDist_split compRApprox) (?bits 100) :: [Integer] -> Property)
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
    foldl1 min [dL, dLR, dR]
    where
    dL = closestPairDist_split (.<) ptsL
    dLR = distance (largest ptsL, smallest ptsR)
    dR = closestPairDist_split (.<) ptsR

compRApprox :: R -> R -> Bool
compRApprox a b = (a?ac) !<! (b?ac)
  where
  ac = bits 100

compMPBall :: MPBall -> MPBall -> Bool
compMPBall = (!<!)

{- auxiliary functions -}

-- hull :: MPBall -> MPBall -> MPBall
-- hull = hullMPBall

average :: (HasIntegers t, CanAddSameType t, CanDivBy t Integer) => [t] -> t
average xs = (sum xs) / (length xs)

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
