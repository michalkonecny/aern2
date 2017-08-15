{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Demos where

import MixedTypesNumPrelude hiding (replicate)
import qualified Prelude as P
-- import Text.Printf

import Control.Arrow

import Control.Concurrent
import Control.Concurrent.STM

import Test.QuickCheck

import qualified Data.List as List

import AERN2.QA.Protocol
import AERN2.Real

-- import Debug.Trace

----------------------------------
-- A nano-introduction to Haskell
----------------------------------

{- lists, types -}

-- types are inferred and checked
squareList :: [Integer] -> _
squareList list = map (^!2) list

fact1 n = product [1..n]

fact2 n = foldl1 (*) [1..n]

allPairs list = [(i,j) | i<-list, j<-list, i<j ]

{- IO -}

io_run1 = print (fact1 100)
io_run2 = do { line <- getLine; print (reverse line) }
io_run3 = sequence_ [print (fact1 i) | i <- [1..10]]


{- imperative multithreading -}

fork_run =
  do
  sequence_ [forkIO (print (fact1 i)) | i <- [1..10]]
  threadDelay (int 100000)

sync_run =
  do
  busyVar <- atomically (newTVar False)
  sequence_ [forkIO $ do {set busyVar; print (fact1 i); unset busyVar} | i <- [1..10]]
  threadDelay (int 100000)
  where
  set busyVar = atomically $
    do
    busy <- readTVar busyVar
    if busy
      then retry
      else writeTVar busyVar True
  unset busyVar = atomically $ writeTVar busyVar False

{- type classes -}

-- toStrings :: (Show a) => [a] -> [String]
toStrings xs = map show xs

-- toStrings_bad = toStrings [io_run1, io_run2]


----------------------------------
-- Introduction to aern2-real
----------------------------------

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

task_closestPairA ::
  (QAArrow to
  , CanSinCosSameType t
  , CanMinMaxSameType t, CanSubSameType t, CanAbsSameType t
  , HasIntegers t, CanAddSameType t, CanDivCNBy t Int)
  =>
  (t `to` t) -> ((t, t) `to` Bool) -> Integer -> () `to` t
task_closestPairA (reg :: t `to` t) comp n =
  proc () ->
    closestPairDistA reg comp -< [sin (convertExactly i :: t) | i <- [1..n]]

closestPairDistA ::
  (QAArrow to
  , CanMinMaxSameType t, CanSubSameType t, CanAbsSameType t
  , HasIntegers t, CanAddSameType t, CanDivCNBy t Int)
  =>
  (t `to` t) -> ((t, t) `to` Bool) -> [t] `to` t
closestPairDistA (reg :: (t `to` t)) comp =
  proc pts ->
    do
    (ptsL,ptsR) <- partitionByComp -< pts
    if (length ptsL < 2 || length ptsR < 2)
      then returnA -< closestPairDist_naive pts
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
  splitAndMerge :: ([t], [t]) `to` t
  splitAndMerge =
    proc (ptsL, ptsR) ->
      do
      dL_pre <- closestPairDistA reg comp -< ptsL
      dL <- reg -< dL_pre
      dLR <- reg -< distance (largest ptsL, smallest ptsR)
      dR_pre <- closestPairDistA reg comp -< ptsR
      dR <- reg -< dR_pre
      returnA -< foldl1 min [dL, dLR, dR]

compApproxA :: (QAArrow to) => (RA to, RA to) `to` Bool
compApproxA =
  proc (a,b) ->
    do
    aB <- (-?-) -< (a,ac)
    bB <- (-?-) -< (b,ac)
    returnA -< aB !<! bB
  where
  ac = bitsS 10

closestPairDistA_Real :: (QAArrow to) => [RA to] `to` (RA to)
closestPairDistA_Real = closestPairDistA (-:-||) compApproxA


{- auxiliary functions -}

hull :: MPBall -> MPBall -> MPBall
hull = fromEndpoints

average :: (HasIntegers t, CanAddSameType t, CanDivCNBy t Int) => [t] -> t
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
  take (int $ length list - 1) $ List.tails list

replicate :: Integer -> a -> [a]
replicate = P.replicate . int
-- TODO: move this to mixed-types-num
