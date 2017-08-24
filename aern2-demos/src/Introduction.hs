module Introduction where

import MixedTypesNumPrelude

import AERN2.Real
import AERN2.Sequence

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

  stack repl aern2/aern2-demos/src/Introduction.hs
-}

-- Request accuracy
--    strict limit: 100 bits
--    and guide: 120 bits
sumSines1_run1 :: MPBall
sumSines1_run1 = (sumSines1 100) ? (bitsSG 100 120)
{- ghci log:
*Introduction> sumSines1_run1
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
{- ghci log:
  *Introduction> sumSines1_run2
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

{-
  First consider comparisons of real number approximations.
  These may be decided or undecided, using Maybe.
-}

pi100 :: MPBall
pi100 = pi?(bitsS 100)

compare_run1 :: Maybe Bool
compare_run1 = pi100 > 0
-- returns: Just True

compare_run2 = pi100 == pi100
-- returns: Nothing

{-
  Comparison of real numbers are not decidable.
  Nevertheless, the result of such a comparison
  can be computably approximated using a sequence of Booleans.
-}

compare_run3 :: Sequence (Maybe Bool)
compare_run3 = pi > 0
-- in ghci prints: Just True
-- (evaluated using default accuracy bitsS 100)

compare_run4 = pi == pi + 0.5^!1000
-- in ghci prints: Nothing
-- (evaluated using default accuracy bitsS 100)

compare_run5 = (pi == pi + 0.5^!1000) ? (bitsS 1000)
-- returns: Just False

compare_run6 = real 0 == 0
-- in ghci prints: Just True
-- this is decided in finite time because 0 is represented exactly

compare_run7 = pi == pi ? (bitsS 10000)
-- returns: Nothing
-- (cannot confirm pi=pi in finite time)

------------------------------
-- checking partial functions
------------------------------

{-
  Normally in Haskell computation such as 1/0 or sqrt (-1) result
  in a run-time exception.  When using AERN2 with mixed-types-num
  (the default), these expressions do not throw exceptions, but
  return special values that describe the error.  These values are
  propagated through expressions and can be caught at any level of the expression.

  Please see
-}

partialfn_run1 :: CN MPBall
partialfn_run1 = sqrt (-1) ? (bitsS 100)
{- ghci log:
*Introduction> partialfn_run1
{[(ERROR,out of range: sqrt: argument must be >= 0: [-1.0 ± 0])]}
-}

{- Now let us catch/inspect collected errors: -}
partialfn_run2 :: Maybe MPBall
partialfn_run2 =
  let x = -1 in
  case ensureNoCN (sqrt x ? (bitsS 100)) of
    (Just r, _errors) -> Just r
    (Nothing, _errors) -> Nothing

partialfn_run3 = sqrt 0
{- ghci log:
*Introduction> partialfn_run3
[0.0 ± 0]
-}

{-
 When computing on approximations which do not have enough information
 to check whether an error occurs, we get a *potential* error:
-}

partialfn_run4 = sqrt (pi-pi)
{- ghci log:
*Introduction> partialfn_run4
[2.82118644197349001912496768933e-37 ± <2^(-121)]{[(POTENTIAL ERROR,out of range: sqrt: argument must be >= 0: [0.0 ± <2^(-240)])]}
-}

{-
  When we are sure the potential error will not occur, we can erase it,
  risking a run-time exception if we get it wrong:
-}

partialfn_run5 = (~!) (sqrt (pi-pi))
{- ghci log:
*Introduction> partialfn_run5
[2.82118644197349001912496768933e-37 ± <2^(-121)]
-}

partialfn_run6 = (~!) (sqrt (pi-pi-1))
{- ghci log:
*Introduction> partialfn_run6
*** Exception: WithGlobalParam ensureNoCE: []

FIXME: the above should print a description of the exception...
-}

---------------------------------
-- "parallel" branching for real numbers
--------------------------------

myabs0 :: Rational -> Rational
myabs0 x = if x < 0 then -x else x

type RCN = CauchyRealCN

myabs1 :: R -> RCN
myabs1 x = if x < 0 then -x else x

myabs2 :: R -> R
myabs2 x = (~!) $ if x < 0 then -x else x

---------------------------------
-- Computing limits
--------------------------------

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
