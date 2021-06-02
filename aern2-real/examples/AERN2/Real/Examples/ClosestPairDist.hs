{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-|
    Module      :  AERN2.Real.Examples..ClosestPairDist
    Description :  Example: Computing shortest distance among a set of 1D points
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Example: Computing shortest distance among a set of 1D points.

    You can run this file in ghci.
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

import AERN2.Real

----------------------------------
-- Finding the smallest distance within a set of real numbers
----------------------------------

distance :: 
  (CanAbsSameType t, CanSubSameType t)
  =>
  (t, t) -> t
distance (a,b) = abs (a-b)

closestPairDist_naive :: 
  (CanMinMaxSameType t, CanAbsSameType t, CanSubSameType t) 
  =>
  [t] -> t
closestPairDist_naive pts
  | length pts < 2 = error "closestPairDist_naive: too few points"
  | otherwise =
      (foldl1 min (map distance (distinctPairs pts)))
  where
  distinctPairs :: [t] -> [(t,t)]
  distinctPairs xs = [(x,y) | (x:rest) <- tails1 xs, y <- rest]

  {-| non-empty tails -}
  tails1 :: [t] -> [[t]]
  tails1 list =
    take (length list - 1) $ List.tails list

{- a version that splits, recurses and merges the results -}
closestPairDist_split ::
  (RealNumber r, CanMinMaxSameType r, CanAbsSameType r) 
  => 
  [r] -> r
closestPairDist_split pts
  | length ptsL < 2 || length ptsR < 2 =
      closestPairDist_naive pts
  | otherwise =
      recurseAndMerge
  where
  (ptsL,ptsR) = List.partition isCertainlyLeft pts
    where
    isCertainlyLeft x = 
      isCertainlyTrue $ select (x < a) (x > a - 0.5^100)
    a = average pts
  recurseAndMerge =
    foldl1 min [dL, dLR, dR]
    where
    dL = closestPairDist_split ptsL
    dLR = distance (largest ptsL, smallest ptsR)
    dR = closestPairDist_split ptsR
  
average :: (HasIntegers t, CanAddSameType t, CanDivBy t Integer) => [t] -> t
average xs = (sum xs) / (length xs)

largest :: (CanMinMaxSameType t) => [t] -> t
largest pts = foldl1 max pts

smallest :: (CanMinMaxSameType t) => [t] -> t
smallest pts = foldl1 min pts

{-
  Helper functions for running tests by hand.
  -}

closestPairDist_run ::
  (RealNumber r, CanSinCosSameType r, CanMinMaxSameType r, CanAbsSameType r)
  =>
  ([r] -> r) -> 
  Integer -> r
closestPairDist_run (closestPairDist :: [t] -> t) n =
  closestPairDist [sin (convertExactly i :: t) | i <- [1..n]]

closestPairDist_run_naive :: 
  (RealNumber r, CanSinCosSameType r, CanMinMaxSameType r, CanAbsSameType r) 
  => 
  Integer -> r
closestPairDist_run_naive =
  closestPairDist_run closestPairDist_naive 

closestPairDist_run_split ::
  (RealNumber r, CanSinCosSameType r, CanMinMaxSameType r, CanAbsSameType r) 
  => 
  Integer -> r
closestPairDist_run_split =
  closestPairDist_run $ closestPairDist_split

closestPairDist_run_naive_CReal :: Integer -> CReal
closestPairDist_run_naive_CReal = closestPairDist_run_naive

closestPairDist_run_naive_WCP :: Integer -> CReal
closestPairDist_run_naive_WCP n = crealFromWithCurrentPrec $ closestPairDist_run_naive n

closestPairDist_run_split_CReal :: Integer -> CReal
closestPairDist_run_split_CReal = closestPairDist_run_split

closestPairDist_run_split_WCP :: Integer -> CReal
closestPairDist_run_split_WCP n = crealFromWithCurrentPrec $ closestPairDist_run_split n


{- Example runs:

*AERN2.Real.Examples.ClosestPairDist> closestPairDist_run_naive_CReal 1000 ? (prec 1000)
[0.00000013295546744391165086... ± ~0.0000 ~2^(-1221)]
(13.80 secs, 12,017,593,904 bytes)

*AERN2.Real.Examples.ClosestPairDist> closestPairDist_run_naive_WCP 1000 ? (prec 1000)
[0.00000013295546744391165086... ± ~0.0000 ~2^(-1221)]
(7.12 secs, 9,187,727,688 bytes)

*AERN2.Real.Examples.ClosestPairDist> closestPairDist_run_split_CReal 1000 ? (prec 1000)
[0.00000013295546744391165086... ± ~0.0000 ~2^(-1221)]
(2.59 secs, 4,659,949,752 bytes)

*AERN2.Real.Examples.ClosestPairDist> closestPairDist_run_split_WCP 1000 ? (prec 1000)
[0.00000013295546744391165086... ± ~0.0000 ~2^(-1221)]
(1.11 secs, 2,245,453,016 bytes)

-}

{- specification and randomised tests -}

closestPairDist_spec :: 
  _ =>
  ([r] -> r) -> (r -> t) -> [b] -> Property
closestPairDist_spec closestPairDist (getFinite :: r -> t) numbers =
  (length numbers) < 2
  .||.
  (getFinite (closestPairDist numbersR)) ?==?$ (closestPairDist_naive numbers)
  where
  numbersR = map convertExactly numbers :: [r]
  a ?==?$ b = printArgsIfFails2 "?==?" (?==?) a b

closestPairDist_runTests1 :: IO ()
closestPairDist_runTests1 =
  quickCheck (closestPairDist_spec (closestPairDist_split :: [CReal] -> CReal) (?bits 100) :: [Integer] -> Property)

sample_integers = sample' (arbitrary :: Gen [Integer]) >>= mapM_ print
sample_rationals = sample' (arbitrary :: Gen [Rational]) >>= mapM_ print
