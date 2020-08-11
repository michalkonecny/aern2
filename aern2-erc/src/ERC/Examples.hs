{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Examples where

import Prelude hiding ((<*),pi)

import GHC.TypeLits
import Data.Proxy

import AERN2.MP (MPBall)

import ERC.Monad
import ERC.Variables
import ERC.Logic
import ERC.Integer
import ERC.Statements
import ERC.Real
import ERC.Pair
import ERC.Array

--------------------------------------------------
-- JMMuller
--------------------------------------------------

erc_JMMuller :: ERC s INTEGER -> ERC s REAL
erc_JMMuller param_n =
  do
  n <- declareINTEGER $ param_n -- copy-in parameter passing
  a <- declareREAL $ 11 / 2
  b <- declareREAL $ 61 / 11
  c <- declareREAL $ 0
  while_ ((n?) ># 0) $ do
    -- ____traceREAL "a = " (a?)
    c .= 111 - (1130 - 3000/(a?))/(b?)
    a .= (b?)
    b .= (c?)
    n .= (n?) - 1
  return_ (a?)

run_erc_JMMuller :: Integer -> Integer -> MPBall
run_erc_JMMuller n ac = runERC_REAL ac (erc_JMMuller (pure n))

--------------------------------------------------
-- HeronSqrt
--------------------------------------------------

erc_HeronSqrt'_p :: ERC s INTEGER -> ERC s REAL -> ERC s REAL
erc_HeronSqrt'_p p param_x =
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  y <- declareREAL $ (x?)
  z <- declareREAL $ (x?)/(y?)
  while_ (choose [iota(p) >* (y?) - (z?), (y?) - (z?) >* iota(p-1)] ==# 1) $ do
    y .= ((y?) + (z?))/2
    z .= (x?)/(y?)
  return_ (y?)

erc_HeronSqrt' :: ERC s REAL -> ERC s REAL
erc_HeronSqrt' x = 
  return_ $ limit (\p -> erc_HeronSqrt'_p p x)

run_erc_HeronSqrt' :: Rational -> Integer -> MPBall
run_erc_HeronSqrt' x ac = runERC_REAL ac (erc_HeronSqrt' (fromRational x))

erc_HeronSqrt :: ERC s REAL -> ERC s REAL
erc_HeronSqrt x = 
  return_ $ parallelIfThenElse (x >* 1) (erc_HeronSqrt' x) (1/(erc_HeronSqrt' (1/x)))

run_erc_HeronSqrt :: Rational -> Integer -> MPBall
run_erc_HeronSqrt x ac = runERC_REAL ac (erc_HeronSqrt (fromRational x))

--------------------------------------------------
-- Exp using Taylor formula
--------------------------------------------------

erc_exp'_p :: ERC s INTEGER -> ERC s REAL -> ERC s REAL
erc_exp'_p p param_x =
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  j <- declareINTEGER $ 1
  rj <- declareREAL $ 1
  jf <- declareREAL $ 1
  y <- declareREAL $ 1
  z <- declareREAL $ (x?)
  while_ ((j?) <=# -p) $ do
    y .= (y?) + (z?)/(jf?)
    j .= (j?) + 1
    rj .= (rj?) + 1
    z .= (z?)*(x?)
    jf .= (jf?) * (rj?)
  return_ (y?)

erc_exp' :: ERC s REAL -> ERC s REAL
erc_exp' x = 
  return_ $ limit (\p -> erc_exp'_p p x)

run_erc_exp' :: Rational -> Integer -> MPBall
run_erc_exp' x ac = runERC_REAL ac (erc_exp' (fromRational x))

erc_exp :: ERC s REAL -> ERC s REAL
erc_exp param_x =
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  z <- declareREAL $ erc_exp' (1/2)
  y <- declareREAL $ 1
  while_ (choose [(x?) <* 1, (x?) >* 1/2] ==# 1) $ do
    y .= (y?) * (z?)
    x .= (x?) - 1/2
  return_ $ (y?)*(erc_exp' (x?))
  
run_erc_exp :: Rational -> Integer -> MPBall
run_erc_exp x ac = runERC_REAL ac (erc_exp (fromRational x))

--------------------------------------------------
-- Exp using iteration
--------------------------------------------------

erc_exp2_p :: ERC s INTEGER -> ERC s REAL -> ERC s REAL
erc_exp2_p p param_x = -- precondition: 0 <= x <= 2
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  n <- declareINTEGER $ 1
  rn <- declareREAL $ 1
  c <- declareREAL $ 1 + (x?)
  a <- declareREAL $ (c?)
  b <- declareREAL $ (a?) * (c?)
  while_ (choose [iota(p) >* (b?)-(a?), (b?)-(a?) >* iota(p-1)] ==# 1) $ do
    -- ____tracePrecision
    -- ____traceINTEGER "p=" p
    -- ____traceREAL "iota(p-1)=" (iota(p-1))
    -- ____traceREAL "b-a=" ((b?)-(a?))
    -- ____traceREAL "a=" (a?)
    n .= (n?)+(n?)
    rn .= 2*(rn?)
    c .= 1 + (x?)/(rn?)
    a .= (c?)^*(n?)
    b .= (a?)*(c?)
  return_ (a?)

erc_exp2 :: ERC s REAL -> ERC s REAL
erc_exp2 x = 
  return_ $ limit (\p -> erc_exp2_p p x)

run_erc_exp2 :: Rational -> Integer -> MPBall
run_erc_exp2 x ac = runERC_REAL ac (erc_exp2 (fromRational x))

--------------------------------------------------
-- Rounding to a nearby integer
--------------------------------------------------

erc_round1 :: ERC s REAL -> ERC s INTEGER
erc_round1 param_x =
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  k <- declareINTEGER $ 0
  while_ (choose [(x?) <* 1, (x?) >* 1/2] ==# 1) $ do
    k .= (k?) + 1
    x .= (x?) - 1
  while_ (choose [(x?) >* (-1), (x?) <* (-1/2)] ==# 1) $ do
    k .= (k?) - 1
    x .= (x?) + 1
  return_ (k?)

run_erc_round1 :: Rational -> Integer
run_erc_round1 x = runERC (const True) (erc_round1 (fromRational x))

erc_round1_sqrt :: ERC s REAL -> ERC s INTEGER
erc_round1_sqrt x = erc_round1 (erc_HeronSqrt x)

run_erc_round1_sqrt :: Rational -> Integer
run_erc_round1_sqrt x = runERC (const True) (erc_round1_sqrt (fromRational x))

erc_round2 :: ERC s REAL -> ERC s INTEGER
erc_round2 param_x =
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  k <- declareINTEGER $ 0
  j <- declareINTEGER $ 0
  b <- declareINTEGER $ 0
  y <- declareREAL $ (x?)
  while_ (choose [absREAL (y?) <* 1, absREAL (y?) >* 1/2] ==# 1) $ do
    j .= (j?) + 1
    y .= (y?) / 2
  while_ ((j?) ># 0) $ do
    y .= (y?) * 2
    b .= choose [(y?) <* 0, -1 <* (y?) &&? (y?) <* 1, (y?) >* 0] - 1
    -- y .= (y?) - (b?)
    ifThen_ ((b?) ==# -1) $ y .= (y?) + 1
    ifThen_ ((b?) ==# 1) $ y .= (y?) - 1
    k .= 2*(k?) + (b?)
    j .= (j?) - 1
  return_ (k?)

run_erc_round2 :: Rational -> Integer
run_erc_round2 x = runERC (const True) (erc_round2 (fromRational x))

--------------------------------------------------
-- Determinant function using Gaussian elimination
--------------------------------------------------

erc_pivot :: (KnownNat d) => (ERC s (REALnm s d d), ERC s INTEGER) -> ERC s (INTEGER, INTEGER)
erc_pivot (a, k) = 
  let d = array2DLength1 a in
  do
  i0 <- declareINTEGER $ k
  j0 <- declareINTEGER $ k
  x <- declareREAL $ 0
  i <- declareINTEGER $ 0
  j <- declareINTEGER $ 0
  forNfromTo_ i k (d-1) $ do
    forNfromTo_ j k (d-1) $ do
      x .= maxREAL ((x?), absREAL (a ?!! [(i?), (j?)]))
  forNfromTo_ i k (d-1) $ do
    forNfromTo_ j k (d-1) $ do
      ifThen_ (choose [absREAL (a?!![(i?), (j?)]) <* (x?), absREAL (a?!![(i?), (j?)]) >* (x?)/2] ==# 1) $ do
        i0 .= (i?)
        j0 .= (j?)
  return_ $ pair_ (i0?) (j0?)

run_erc_pivot :: (KnownNat d) => Proxy d -> [[Rational]] -> Integer -> (Integer, Integer)
run_erc_pivot (_ :: Proxy d) aRational k = 
  runERC (const True) $ do
    (a :: REALnm s d d) <- array2D $ map (map fromRational) aRational
    erc_pivot (pure a, pure k)

run_erc_pivot_test1 :: (Integer, Integer)
run_erc_pivot_test1 =
  run_erc_pivot (Proxy::Proxy 2) [[1,0],[-2,1]] 0

erc_det :: (KnownNat d) => ERC s (REALnm s d d) -> ERC s REAL
erc_det a = -- precondition: a is invertible
  let d = array2DLength1 a in
  do
  i <- declareINTEGER $ 0
  j <- declareINTEGER $ 0
  k <- declareINTEGER $ 0
  pi <- declareINTEGER $ 0
  pj <- declareINTEGER $ 0
  det <- declareREAL $ 1
  forNfromTo_ k 0 (d-2) $ do
    ____traceREALnm "a=" a
    ____traceINTEGER "k=" (k?)
    (pi, pj) ..= erc_pivot (a, (k?))
    ____traceINTEGER "pi=" (pi?)
    ____traceINTEGER "pj=" (pj?)
    det .= (det?) * a?!![(pi?), (pj?)]
    forNfromTo_ j 0 (d-1) $ do
      array2DSwap (a, [(k?),(j?)], a, [(pi?), (j?)])
    ____traceREALnm "after swap 1: a=" a
    ifThen_ ((k?) /=# (pi?)) $ do
      det .= - (det?)
    forNfromTo_ i 0 (d-1) $ do
      array2DSwap (a, [(i?),(k?)], a, [(i?), (pj?)])
    ____traceREALnm "after swap 2: a=" a
    ifThen_ ((k?) /=# (pj?)) $ do
      det .= - (det?)
    forNfromTo_ j ((k?)+1) (d-1) $ do
      ____traceINTEGER "j=" (j?)
      (a, [(k?),(j?)]) .=!! a?!![(k?),(j?)] / a?!![(k?),(k?)]
      ____traceREALnm "a=" a
      forNfromTo_ i ((k?)+1) (d-1) $ do
        ____traceINTEGER "i=" (i?)
        (a, [(i?),(j?)]) .=!! a?!![(i?),(j?)] - a?!![(i?),(k?)] * a?!![(k?),(j?)]
        ____traceREALnm "a=" a
    (a, [(k?),(k?)]) .=!! 1
    forNfromTo_ i ((k?)+1) (d-1) $ do
      (a, [(i?),(k?)]) .=!! 0
  ____traceREALnm "a=" a
  det .= (det?) * a?!![d-1,d-1]
  return_ (det?)

run_erc_det :: (KnownNat d) => Proxy d -> [[Rational]] -> Integer -> MPBall
run_erc_det (_ :: Proxy d) aRational ac = 
  runERC_REAL ac $ do
    (a :: REALnm s d d) <- array2D $ map (map fromRational) aRational
    erc_det (pure a)

run_erc_det_test1 :: MPBall
run_erc_det_test1 =
  run_erc_det (Proxy::Proxy 2) [[1,0],[2,1]] 10

run_erc_det_test2 :: MPBall
run_erc_det_test2 =
  run_erc_det (Proxy::Proxy 3) [[1,2,1],[1,2,0],[2,2,1]] 10

run_erc_det_test3 :: MPBall
run_erc_det_test3 =
  run_erc_det (Proxy::Proxy 4) [[1,5,-2,1],[3,1,1,3],[3,2,1,3],[-7,2,1,5]] 10
  -- Alpha:  det({{1,5,-2,1},{3,1,1,3},{3,2,1,3},{-7,2,1,5}})

----------------------------------------------------------------------
-- Enclosing the unique root of a continuous function using trisection
----------------------------------------------------------------------

erc_Root_p :: ERC s INTEGER -> (ERC s REAL -> ERC s REAL) -> ERC s REAL
erc_Root_p p f =
  do
  a <- declareREAL $ 0
  b <- declareREAL $ 1
  while_ (choose [iota(p) >* (b?)-(a?), (b?)-(a?) >* iota(p-1)] ==# 1) $ do
    ifThenElse_ (choose [0 >* f((b?)/3 + 2*(a?)/3) * f((b?)), 0 >* f((a?))*f(2*(b?)/3 + (a?)/3)] ==# 1) $ do
      b .= 2*(b?)/3 + (a?)/3
      `else_` do
        a .= (b?)/3 + 2*(a?)/3
  return_ (a?)

erc_Root :: (ERC s REAL -> ERC s REAL) -> ERC s REAL
erc_Root f = 
  return_ $ limit (\p -> erc_Root_p p f)

erc_Root_sqrt :: ERC s REAL -> ERC s REAL
erc_Root_sqrt y =
  do
  erc_Root $ \ x -> do
    x^*2-y

run_erc_Root_sqrt :: Rational -> Integer -> MPBall
run_erc_Root_sqrt y ac 
  | 0 < y && y < 1 = 
    runERC_REAL ac (erc_Root_sqrt (fromRational y))
  | otherwise =
    error "run_erc_Root_sqrt y is defined only for 0<y<1"

erc_Root_log :: ERC s REAL -> ERC s REAL
erc_Root_log y =
  do
  erc_Root $ \ x -> do
    (erc_exp x) - y

run_erc_Root_log :: Rational -> Integer -> MPBall
run_erc_Root_log y ac 
  | 1 < y && (fromRational y) < exp(1::Double) = 
    runERC_REAL ac (erc_Root_log (fromRational y))
  | otherwise =
    error "run_erc_Root_log y is defined only for 1<y<e"

