{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-|
    Module      :  AERN2.Real.Examples.Introduction
    Description :  aern2-real introductory examples
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Introductory examples for packages aern2-mp and aern2-real.

    Please see aern2-real/README.md for explanations.

    You can run the following examples in ghci.
    If you installed AERN2 using the official instructions,
    you can start ghci using the following command in the base
    folder:

    @
    stack repl aern2-real/examples/AERN2/Real/Examples/Introduction.hs
    @
-}
module AERN2.Real.Examples.Introduction where

import MixedTypesNumPrelude

import qualified Numeric.CollectErrors as CN

import AERN2.MP
import AERN2.Real

-- import Debug.Trace

------------------------------
-- real numbers
------------------------------

-- Start with a simple real number:

sine1 = sin 1

sine1_run1 = sine1 ? (prec 120)
-- result: [0.84147098480789650665250232... ± ~4.6644e-35 ~2^(-114)]
sine1_run2 = sine1 ? (bits 120)
-- result: [0.84147098480789650665250232... ± ~2.2431e-55 ~2^(-181)]

-- Next, do a bit more work:

sumSines1 :: Integer -> CReal
sumSines1 n = sum [sin i | i <- [1..n]]

-- Request the above expression with n = 100 using roughly 100 significant binary digits:
sumSines1_run1 :: CN MPBall
sumSines1_run1 = (sumSines1 100) ? (prec 120)
{- ghci log:

*AERN2.Real.Introduction> sumSines1_run1
[-0.12717101366042011543675217... ± ~2.8393e-33 ~2^(-108)]
(0.03 secs, 26,203,776 bytes)
-}

-- Same as above but request guaranteed 100 bits of accuracy:
sumSines1_run2 = (sumSines1 100) ? (bits 100)
{- ghci log:

*AERN2.Real.Introduction> sumSines1_run2
[-0.12717101366042011543675217... ± ~2.8393e-33 ~2^(-108)]
(0.19 secs, 319,789,600 bytes)

This is considetably slower because there is some backtracking when target accuracy is not reached.  
-}

------------------------------
-- real number comparisons
------------------------------

{-
  First consider comparisons of real number approximations.
  These may be decided or undecided, using a 'Kleenean'.
-}

pi100 :: CN MPBall
pi100 = pi?(bits 100)

compare_run1 :: CN Kleenean
compare_run1 = pi100 > 0
-- returns: CertainTrue

compare_run2 :: CN Kleenean
compare_run2 = pi100 == pi100
-- returns: TrueOrFalse

compare_run3 :: CKleenean
compare_run3 = pi > 0
-- in ghci prints: {?(prec 36): CertainTrue}
-- (evaluated using default precision 36)

compare_run4 = pi == pi + 2^(-100)
-- in ghci prints: {?(prec 36): TrueOrFalse}

compare_run5 = (pi == pi + 2^(-100)) ? (prec 1000)
-- returns: CertainFalse

compare_run6 = (creal 0) == 0
-- in ghci prints: {?(prec 36): CertainTrue}
-- this is decided in finite time because 0 is represented exactly

compare_run7 = pi == pi ? (prec 10000)
-- returns: TrueOrFalse
-- (cannot confirm pi=pi in finite time)

------------------------------
-- checking partial functions
------------------------------

partialfn_bad1 = sqrt (-1)
{- ghci log:

*AERN2.Real.Introduction> partialfn_bad1 ? (bits 100)
{{ERROR: out of domain: negative sqrt argument}}

-}

a_third = creal (1/3)

partialfn_bad2 = 1/(a_third-a_third)
{- ghci log:

*AERN2.Real.Introduction> partialfn_bad2 ? (prec 100)
{{POTENTIAL ERROR: division by 0}}

*AERN2.Real.Introduction> partialfn_bad2 ? (bits 100)
{{POTENTIAL ERROR: numeric error: failed to find an approximation with sufficient accuracy}}

-}

partialfn_bad3 = 1/(pi-pi)
{- ghci log:

*AERN2.Real.Introduction> partialfn_bad3 ? (prec 100)
{{POTENTIAL ERROR: division by 0}}

*AERN2.Real.Introduction> partialfn_bad3 ? (bits 100)
-- TAKES A VERY LONG TIME

-}

{-
 When computing on approximations which do not have enough information
 to check whether an error occurs, we get a *potential* error:
-}

partialfn_ok4 = sqrt (pi-pi)
{- ghci log:

*AERN2.Real.Introduction> partialfn_ok4 ? (prec 100)
[0.00000000000000000061331736... ± ~6.1332e-19 ~2^(-60)]{{POTENTIAL ERROR: out of domain: negative sqrt argument}}
-}

partialfn_ok5 = clearPotentialErrors (sqrt (pi-pi))
{- ghci log:

*AERN2.Real.Introduction> partialfn_ok5 ? (prec 100)
[0.00000000000000000061331736... ± ~6.1332e-19 ~2^(-60)]

-}

partialfn_bad6 = clearPotentialErrors (sqrt (pi-pi-1))
{- ghci log:

*AERN2.Real.Introduction> partialfn_bad6 ? (prec 100) 
{{ERROR: out of domain: negative sqrt argument}}

-}

partialfn_bad7 = clearPotentialErrors (sqrt (pi-pi-2^(-1000)))
{- ghci log:

*AERN2.Real.Introduction> partialfn_bad7 ? (prec 100)
[0.00000000000000000061331736... ± ~6.1332e-19 ~2^(-60)]

*AERN2.Real.Introduction> partialfn_bad7 ? (prec 1000)
{{ERROR: out of domain: negative sqrt argument}}

-}

detectCN :: CN.CanTestErrorsPresent a => a -> Maybe a
detectCN r = if not (CN.hasError r) then Just r else Nothing
{- ghci log:

*AERN2.Real.Introduction> detectCN (sqrt (-1) ? (prec 100))
Nothing

*AERN2.Real.Introduction> detectCN (sqrt 0 ? (prec 100))
Just [0 ± 0]

-}

---------------------------------
-- Computing limits
--------------------------------

fact :: Integer -> CReal
fact n = creal $ product [1..n]

e_sum :: Integer -> CReal
e_sum n = sum $ map (recip . fact) [0..n]

my_e :: CReal
my_e = limit $ \(n :: Integer) -> e_sum (n+2)

{- ghci log:

*AERN2.Real.Introduction> my_e ? (prec 1000)
[2.71828182845904523536028747... ± ~0.0000 ~2^(-1217)]

-}

-- a faster version:

e_sum2 :: Integer -> CReal
e_sum2 n = foldl aux (creal 1) $ reverse [1..n]
  where aux x m = 1 + x / m

my_e2 :: CReal
my_e2 = limit $ \(n :: Integer) -> e_sum2 (n+2)

---------------------------------
-- "parallel" branching for real numbers
--------------------------------

absQ :: Rational -> Rational
absQ x = if x < 0 then -x else x

absR1 :: CReal -> CReal
absR1 x = if x < 0 then -x else x


pif_run1 = absR1 (pi-pi)
{- ghci log:

*AERN2.Real.Introduction> pif_run1
{?(prec 36): [0 ± ~2.9104e-11 ~2^(-35)]}

-}

-- pif_run2 = foldl1 (.) (replicate 100 (absR1 . (100*))) (pi-pi)

absR2_approx x (q :: Rational) = if select (x > -q) (x < q) then x else -x

absR2 :: CReal -> CReal
absR2 x = limit $ absR2_approx x

select_run1 = absR2 (pi-pi)
{- ghci log:

*AERN2.Real.Introduction> select_run1
{?(prec 36): [0 ± ~4.3656e-11 ~2^(-34)]}

-}

-----------------------------------------
-- Cauchy reals vs iRRAM style execution
-----------------------------------------

logistic1 :: _ => Rational -> Integer -> t -> t
logistic1 c n x0 =
  (foldl1 (.) (replicate n lg)) x0
  where
  lg x = c * x * (1-x)

logistic1_CReal_run :: Integer -> CReal
logistic1_CReal_run n = logistic1 3.82 n (creal 0.5)

-- TODO: define logistic1_iter

{-  Example uses:

*AERN2.Real.Examples.Introduction> logistic1_CReal_run 100 ? (bits 100)
[0.95087585116480286419338875... ± ~2.9792e-32 ~2^(-104)]
  
*AERN2.Real.Examples.Introduction> logistic1_CReal_run 10000 ? (bits 100)
[0.20775682944252359241450861... ± ~0.0000 ~2^(-2566)]
(2.06 secs, 2,970,188,704 bytes)

-}

{-
  Recommended further reading:  ClosestPairDist.hs
-}
