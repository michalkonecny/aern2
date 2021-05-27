{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Tasks.ClosestPairA where

import MixedTypesNumPrelude hiding (Real)
-- import qualified Prelude as P
-- import Text.Printf

import Control.Arrow

import qualified Data.List as List

import AERN2.QA.Protocol
import AERN2.Real

type Real to = CauchyRealA to

closestPairDist1D_naive ::
  (CanMinMaxSameType t, CanSubSameType t, CanAbsSameType t) =>
  [t] -> Maybe t
closestPairDist1D_naive pts
  | length pts < 2 = Nothing
  | otherwise =
      Just (foldl1 min [ distance (p1,p2) | (p1,p2) <- distinctPairs pts])

distinctPairs :: [t] -> [(t,t)]
distinctPairs xs = [(x,y) | (x:rest) <- tails1 xs, y <- rest]

{-| non-empty tails -}
tails1 :: [t] -> [[t]]
tails1 = tail . reverse . List.tails

distance :: (CanSubSameType t, CanAbsSameType t) => (t, t) -> t
distance (a,b) = abs (a-b)

average :: (HasIntegers t, CanAddSameType t, CanDivCNBy t Int) => [t] -> t
average xs = (sum xs) /! (length xs)

largest :: (CanMinMaxSameType t) => [t] -> t
largest pts = foldl1 max pts

smallest :: (CanMinMaxSameType t) => [t] -> t
smallest pts = foldl1 min pts

task_closestPairA ::
  (QAArrow to
  , CanSinCosSameType t
  , CanMinMaxSameType t, CanSubSameType t, CanAbsSameType t
  , HasIntegers t, CanAddSameType t, CanDivCNBy t Int)
  =>
  (t `to` t) -> ((t, t) `to` Bool) -> Integer -> () `to` Maybe t
task_closestPairA (reg :: t `to` t) comp n =
  proc () ->
    closestPairDistA reg comp -< [sin (convertExactly i :: t) | i <- [1..n]]

closestPairDistA ::
  (QAArrow to
  , CanMinMaxSameType t, CanSubSameType t, CanAbsSameType t
  , HasIntegers t, CanAddSameType t, CanDivCNBy t Int)
  =>
  (t `to` t) -> ((t, t) `to` Bool) -> [t] `to` Maybe t
closestPairDistA (reg :: (t `to` t)) comp =
  proc pts ->
    do
    (ptsL,ptsR) <- partitionByComp -< pts
    if (length ptsL < 2 || length ptsR < 2)
      then returnA -< closestPairDist1D_naive pts
      else splitAndMerge -< (ptsL,ptsR)
  where
  partitionByComp =
    proc pts ->
      case pts of
        [] -> returnA -< ([], [])
        (pt:rest) ->
          do
          let avg = average pts
          (l1,l2) <- partitionByComp -< rest
          b <- comp -< (pt, avg)
          returnA -< if b then (pt:l1,l2) else (l1,pt:l2)
  splitAndMerge :: ([t], [t]) `to` (Maybe t)
  splitAndMerge =
    proc (ptsL, ptsR) ->
      do
      (Just dL_pre) <- closestPairDistA reg comp -< ptsL
      dL <- reg -< dL_pre
      dLR <- reg -< distance (largest ptsL, smallest ptsR)
      (Just dR_pre) <- closestPairDistA reg comp -< ptsR
      dR <- reg -< dR_pre
      returnA -< Just (foldl1 min [dL, dLR, dR])

compApproxA :: (QAArrow to) => (Real to, Real to) `to` Bool
compApproxA =
  proc (a,b) ->
    do
    aB <- (-?-) -< (a,ac)
    bB <- (-?-) -< (b,ac)
    returnA -< aB !<! bB
  where
  ac = bitsS 10

closestPairDistA_Real :: (QAArrow to) => [Real to] `to` Maybe (Real to)
closestPairDistA_Real = closestPairDistA (-:-||) compApproxA


{- auxiliary functions -}

hull :: MPBall -> MPBall -> MPBall
hull = fromEndpoints
