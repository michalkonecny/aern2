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

{- Executing the above in GHCi:

(bash)$ cd aern2-erc
(bash)$ stack repl src/ERC/Examples.hs
...
Ok, 9 modules loaded.
Loaded GHCi configuration from ...

*ERC.Examples> run_erc_JMMuller 0 10 -- (0 iterations)
[5.5 ± 0]
(0.00 secs, 425,744 bytes)

*ERC.Examples> run_erc_JMMuller 100 10 -- (100 iterations)
[5.99999998792532667338407089... ± ~4.0254e-20 ~2^(-64)]
(0.03 secs, 18,425,768 bytes)

*ERC.Examples> run_erc_JMMuller 100 100 -- (100 iterations, higher accuracy)
[5.99999998792532667338407110... ± ~8.0941e-107 ~2^(-352)]
(0.03 secs, 24,286,408 bytes)

-}

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

{- Executing the above in GHCi:

*ERC.Examples> run_erc_HeronSqrt 2 10
[1.414215564727783203125 ± ~2.4557e-4 ~2^(-11)]
(0.01 secs, 1,396,352 bytes)

*ERC.Examples> run_erc_HeronSqrt 2 100
[1.41421356237309504880168872... ± ~7.5892e-37 ~2^(-119)]
(0.02 secs, 7,905,680 bytes)

*ERC.Examples> run_erc_HeronSqrt 1000000000000 10
[1000000 ± ~3.4939e-4 ~2^(-11)]
(0.04 secs, 33,559,048 bytes)

*ERC.Examples> run_erc_HeronSqrt 1000000000000 100
[999999.99999999999999999999999999... ± ~7.5386e-37 ~2^(-119)]
(0.20 secs, 214,332,664 bytes)

-}
--------------------------------------------------
-- Exp using Taylor formula
--------------------------------------------------

erc_Exp'_p :: ERC s INTEGER -> ERC s REAL -> ERC s REAL
erc_Exp'_p p param_x =  -- precondition: 0 <= x <= 1
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

erc_Exp' :: ERC s REAL -> ERC s REAL
erc_Exp' x =  -- precondition: 0 <= x <= 1
  return_ $ limit (\p -> erc_Exp'_p p x)

run_erc_Exp' :: Rational -> Integer -> MPBall
run_erc_Exp' x ac = runERC_REAL ac (erc_Exp' (fromRational x))

erc_Exp :: ERC s REAL -> ERC s REAL
erc_Exp param_x = -- precondition: 0 <= x
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  z <- declareREAL $ erc_Exp' (1/2)
  y <- declareREAL $ 1
  while_ (choose [(x?) <* 1, (x?) >* 1/2] ==# 1) $ do
    y .= (y?) * (z?)
    x .= (x?) - 1/2
  return_ $ (y?)*(erc_Exp' (x?))
  
run_erc_Exp :: Rational -> Integer -> MPBall
run_erc_Exp x ac = runERC_REAL ac (erc_Exp (fromRational x))

{- Executing the above in GHCi:

*ERC.Examples> run_erc_Exp 1 10
[2.71827851035800449608359485... ± ~8.2085e-4 ~2^(-10)]
(0.01 secs, 2,712,928 bytes)

*ERC.Examples> run_erc_Exp 1 100
[2.71828182845904523536028747... ± ~3.0555e-36 ~2^(-117)]
(0.05 secs, 37,553,128 bytes)

*ERC.Examples> run_erc_Exp 100 10
[26881171418161354484126255515800135873611118.77374192241486461352195647... ± ~1.1351e-11 ~2^(-36)]
(0.16 secs, 169,098,648 bytes)

*ERC.Examples> run_erc_Exp 100 100
[26881171418161354484126255515800135873611118.77374192241519160861528028... ± ~1.0120e-44 ~2^(-146)]
(0.20 secs, 207,570,408 bytes)

-}

--------------------------------------------------
-- Exp using iteration
--------------------------------------------------

erc_Exp2_p :: ERC s INTEGER -> ERC s REAL -> ERC s REAL
erc_Exp2_p p param_x = -- precondition: 0 <= x <= 2
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

erc_Exp2 :: ERC s REAL -> ERC s REAL
erc_Exp2 x = 
  return_ $ limit (\p -> erc_Exp2_p p x)

run_erc_Exp2 :: Rational -> Integer -> MPBall
run_erc_Exp2 x ac = runERC_REAL ac (erc_Exp2 (fromRational x))

{- Executing the above in GHCi:

*ERC.Examples> run_erc_Exp2 1 10
[2.71819095406681299209594726... ± ~2.5998e-4 ~2^(-11)]
(1.35 secs, 1,639,356,128 bytes)

*ERC.Examples> run_erc_Exp2 1 12
[2.71826096599397715181112289... ± ~6.1283e-5 ~2^(-13)]
(6.85 secs, 8,445,653,544 bytes)

*ERC.Examples> run_erc_Exp2 1 14
[2.71827651994226471288129687... ± ~1.5506e-5 ~2^(-15)]
(19.40 secs, 23,358,341,240 bytes)

-}

--------------------------------------------------
-- Rounding to a nearby integer
--------------------------------------------------

erc_Round1 :: ERC s REAL -> ERC s INTEGER
erc_Round1 param_x =
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

run_erc_Round1 :: Rational -> Integer
run_erc_Round1 x = runERC (const True) (erc_Round1 (fromRational x))

erc_Round1_sqrt :: ERC s REAL -> ERC s INTEGER
erc_Round1_sqrt x = erc_Round1 (erc_HeronSqrt x)

run_erc_Round1_sqrt :: Rational -> Integer
run_erc_Round1_sqrt x = runERC (const True) (erc_Round1_sqrt (fromRational x))

erc_Round2 :: ERC s REAL -> ERC s INTEGER
erc_Round2 param_x =
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

run_erc_Round2 :: Rational -> Integer
run_erc_Round2 x = runERC (const True) (erc_Round2 (fromRational x))

{- Executing the above in GHCi:

*ERC.Examples> run_erc_Round1 (-10.5)
-10
(0.01 secs, 769,048 bytes)

*ERC.Examples> run_erc_Round1 10.5
10
(0.01 secs, 748,384 bytes)

*ERC.Examples> run_erc_Round1 1000.5
1000
(0.04 secs, 33,558,024 bytes)

*ERC.Examples> run_erc_Round1 100000.5
100000
(3.04 secs, 3,090,943,160 bytes)

*ERC.Examples> run_erc_Round2 (-10.5)
-11
(0.01 secs, 837,312 bytes)

*ERC.Examples> run_erc_Round2 10.5
10
(0.01 secs, 823,768 bytes)

*ERC.Examples> run_erc_Round2 1000.5
1000
(0.01 secs, 1,429,648 bytes)

*ERC.Examples> run_erc_Round2 100000.5
100000
(0.01 secs, 2,099,120 bytes)

*ERC.Examples> run_erc_Round2 100000.8
100001
(0.01 secs, 2,106,736 bytes)

-}

--------------------------------------------------
-- Determinant function using Gaussian elimination
--------------------------------------------------

erc_Pivot :: (KnownNat d) => (ERC s (REALnm s d d), ERC s INTEGER) -> ERC s (INTEGER, INTEGER)
erc_Pivot (a, k) = 
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

run_erc_Pivot :: (KnownNat d) => Proxy d -> [[Rational]] -> Integer -> (Integer, Integer)
run_erc_Pivot (_ :: Proxy d) aRational k = 
  runERC (const True) $ do
    (a :: REALnm s d d) <- array2D $ map (map fromRational) aRational
    erc_Pivot (pure a, pure k)

run_erc_Pivot_test1 :: (Integer, Integer)
run_erc_Pivot_test1 =
  run_erc_Pivot (Proxy::Proxy 2) [[1,0],[-2,1]] 0

erc_Det :: (KnownNat d) => ERC s (REALnm s d d) -> ERC s REAL
erc_Det a = -- precondition: a is invertible
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
    (pi, pj) ..= erc_Pivot (a, (k?))
    ____traceINTEGER "  pi=" (pi?)
    ____traceINTEGER "  pj=" (pj?)
    det .= (det?) * a?!![(pi?), (pj?)]
    forNfromTo_ j 0 (d-1) $ do
      array2DSwap (a, [(k?),(j?)], a, [(pi?), (j?)])
    ____traceREALnm "  after swap 1: a=" a
    ifThen_ ((k?) /=# (pi?)) $ do
      det .= - (det?)
    forNfromTo_ i 0 (d-1) $ do
      array2DSwap (a, [(i?),(k?)], a, [(i?), (pj?)])
    ____traceREALnm "  after swap 2: a=" a
    ifThen_ ((k?) /=# (pj?)) $ do
      det .= - (det?)
    forNfromTo_ j ((k?)+1) (d-1) $ do
      ____traceINTEGER "  j=" (j?)
      (a, [(k?),(j?)]) .=!! a?!![(k?),(j?)] / a?!![(k?),(k?)]
      ____traceREALnm "    a=" a
      forNfromTo_ i ((k?)+1) (d-1) $ do
        ____traceINTEGER "    i=" (i?)
        (a, [(i?),(j?)]) .=!! a?!![(i?),(j?)] - a?!![(i?),(k?)] * a?!![(k?),(j?)]
        ____traceREALnm "      a=" a
    (a, [(k?),(k?)]) .=!! 1
    forNfromTo_ i ((k?)+1) (d-1) $ do
      (a, [(i?),(k?)]) .=!! 0
  ____traceREALnm "a=" a
  det .= (det?) * a?!![d-1,d-1]
  return_ (det?)

run_erc_Det :: (KnownNat d) => Proxy d -> [[Rational]] -> Integer -> MPBall
run_erc_Det (_ :: Proxy d) aRational ac = 
  runERC_REAL ac $ do
    (a :: REALnm s d d) <- array2D $ map (map fromRational) aRational
    erc_Det (pure a)

run_erc_Det_test1 :: MPBall
run_erc_Det_test1 =
  run_erc_Det (Proxy::Proxy 2) [[1,0],[2,1]] 10

run_erc_Det_test2 :: MPBall
run_erc_Det_test2 =
  run_erc_Det (Proxy::Proxy 3) [[1,2,1],[1,2,0],[2,2,1]] 10

run_erc_Det_test3 :: MPBall
run_erc_Det_test3 =
  run_erc_Det (Proxy::Proxy 4) [[1,5,-2,1],[3,1,1,3],[3,2,1,3],[-7,2,1,5]] 10
  -- Alpha:  det({{1,5,-2,1},{3,1,1,3},{3,2,1,3},{-7,2,1,5}})

{- Executing the above in GHCi:

*ERC.Examples> run_erc_Pivot (Proxy::Proxy 2) [[1,0],[-2,1]] 0
(0,1)
(0.01 secs, 856,064 bytes)

*ERC.Examples> run_erc_Pivot (Proxy::Proxy 2) [[1,3],[-2,1]] 0
(1,0)
(0.01 secs, 867,248 bytes)

*ERC.Examples> run_erc_Det (Proxy::Proxy 3) [[1,2,1],[1,2,0],[2,2,1]] 10
a=[[[1 ± 0],[2 ± 0],[1 ± 0]],[[1 ± 0],[2 ± 0],[0 ± 0]],[[2 ± 0],[2 ± 0],[1 ± 0]]]
k=0
  pi=1
  pj=2
  after swap 1: a=[[[2 ± 0],[1 ± 0],[1 ± 0]],[[2 ± 0],[1 ± 0],[0 ± 0]],[[2 ± 0],[2 ± 0],[1 ± 0]]]
  after swap 2: a=[[[2 ± 0],[2 ± 0],[1 ± 0]],[[2 ± 0],[1 ± 0],[0 ± 0]],[[2 ± 0],[1 ± 0],[1 ± 0]]]
  j=1
    a=[[[2 ± 0],[2 ± 0],[1 ± 0]],[[1 ± 0],[1 ± 0],[0 ± 0]],[[2 ± 0],[1 ± 0],[1 ± 0]]]
    i=1
      a=[[[2 ± 0],[2 ± 0],[1 ± 0]],[[1 ± 0],[-1 ± 0],[0 ± 0]],[[2 ± 0],[1 ± 0],[1 ± 0]]]
    i=2
      a=[[[2 ± 0],[2 ± 0],[1 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[2 ± 0],[1 ± 0],[1 ± 0]]]
  j=2
    a=[[[2 ± 0],[2 ± 0],[1 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[1 ± 0],[1 ± 0]]]
    i=1
      a=[[[2 ± 0],[2 ± 0],[1 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[-1 ± 0],[1 ± 0]]]
    i=2
      a=[[[2 ± 0],[2 ± 0],[1 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[-1 ± 0],[0 ± 0]]]
a=[[[1 ± 0],[0 ± 0],[0 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[-1 ± 0],[0 ± 0]]]
k=1
  pi=2
  pj=1
  after swap 1: a=[[[1 ± 0],[0 ± 0],[0 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[0 ± 0],[-1 ± 0]]]
  after swap 2: a=[[[1 ± 0],[0 ± 0],[0 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[0 ± 0],[-1 ± 0]]]
  j=2
    a=[[[1 ± 0],[0 ± 0],[0 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[0 ± 0],[-1 ± 0]]]
    i=2
      a=[[[1 ± 0],[0 ± 0],[0 ± 0]],[[1 ± 0],[-1 ± 0],[-1 ± 0]],[[1 ± 0],[0 ± 0],[-1 ± 0]]]
a=[[[1 ± 0],[0 ± 0],[0 ± 0]],[[1 ± 0],[1 ± 0],[0 ± 0]],[[1 ± 0],[0 ± 0],[-1 ± 0]]]
[-2 ± 0]
(0.01 secs, 4,121,360 bytes)

-}

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
    (erc_Exp x) - y

run_erc_Root_log :: Rational -> Integer -> MPBall
run_erc_Root_log y ac 
  | 1 < y && (fromRational y) < exp(1::Double) = 
    runERC_REAL ac (erc_Root_log (fromRational y))
  | otherwise =
    error "run_erc_Root_log y is defined only for 1<y<e"

{- Executing the above in GHCi:

*ERC.Examples> run_erc_Root_sqrt 0.5 10
[0.707033634185791015625 ± ~2.4796e-4 ~2^(-11)]
(0.02 secs, 16,232,144 bytes)

*ERC.Examples> run_erc_Root_sqrt 0.5 20
[0.70710677938768640160560607... ± ~1.5417e-8 ~2^(-25)]
(0.09 secs, 79,931,160 bytes)

*ERC.Examples> run_erc_Root_log 2 10
[0.69301664829254150390625 ± ~2.4533e-4 ~2^(-11)]
(0.36 secs, 352,681,184 bytes)

*ERC.Examples> run_erc_Root_log 2 20
[0.69314717282759374938905239... ± ~1.5089e-8 ~2^(-25)]
(3.10 secs, 3,187,074,320 bytes)

-}
