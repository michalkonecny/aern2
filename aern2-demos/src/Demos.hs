module Demos where

import MixedTypesNumPrelude

import AERN2.Real

-- import Debug.Trace

------------------------------
-- real numbers
------------------------------

-- define a short name for the type of real numbers:
type R = CauchyReal
-- Real numbers are represented by fast converging sequences of dyadic intervals:
--    CauchyReal = Sequence MPBall
-- A sequence is essentially a function from accuracy, ie number of bits:
--    Sequence t = ... (AccuracySG -> t)

-- Start with a simple real number expression:
sumSines1 n = sum [sin i | i <- [1..n]]

{-
  You can run many of the following yourself in ghci.
  If you installed AERN2 using the official instructions,
  you can start ghci using the following command in the base
  folder:

  stack repl aern2/aern2-demos/src/Demos.hs
-}

-- Request accuracy
--    strict limit: 100 bits
--    and guide: 120 bits
sumSines1_run1 :: MPBall
sumSines1_run1 = (sumSines1 100) ? (bitsSG 100 120)
{- output:
*Demos> sumSines1_run1
[-0.127171013660420115436752171427 ± <2^(-112)]
(0.02 secs, 0 bytes)

Note that the result accuracy is somewhat below
the guide accuracy target of 120.  This is because the intermediate results are
computed with an accuracy that is not entirely sufficient for the target accuracy
of the final result.  When propagating requests to sub-expressions, we
avoid raising the accuracy requests unnecessarily so that in deeply nested
expressions the accuracy would not grow too fast. The price is that typically
we miss the accuracy target.
-}

-- Request 100 bits of the result of the above expression with n = 100:
sumSines1_run2 = (sumSines1 100) ? (bitsSG 100 100)
{- output:
  *Demos> sumSines1_run2
  [-0.127171013660420115436752171426 ± <2^(-100)]
  (1.60 secs, 2,382,797,736 bytes)

This is very slow because there is a lot of backtracking when target accuracy
is not reached.  Thus one should avoid making strict limit and guide too close.

The following query will automatically set the guide accuracy 20 bits above the strict limit:
-}
sumSines1_run3 = (sumSines1 100) ? (bitsS 100)
-- which is the default request when printing a CauchyReal:
sumSines1_run4 = print (sumSines1 100)


------------------------------
-- real number comparisons
------------------------------

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

{-
  Recommended further reading:  ClosestPairDist.hs
-}
