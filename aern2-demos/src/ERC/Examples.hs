{-# LANGUAGE PostfixOperators #-}
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Examples where

import Prelude hiding ((<*))

import AERN2.MP

import ERC.Monad
import ERC.Variables
import ERC.Logic
import ERC.Integer
import ERC.Real
import ERC.Statements

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
  while ((n?) ># 0) $ do
    -- traceREAL "a = " (a?)
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
  while (choose [iota(p) >* (y?) - (z?), (y?) - (z?) >* iota(p-1)] ==# 1) $ do
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
  while ((j?) <=# -p) $ do
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
  while (choose [(x?) <* 1, (x?) >* 1/2] ==# 1) $ do
    y .= (y?) * (z?)
    x .= (x?) - 1/2
  return_ $ (y?)*(erc_exp' (x?))
  
run_erc_exp :: Rational -> Integer -> MPBall
run_erc_exp x ac = runERC_REAL ac (erc_exp (fromRational x))

--------------------------------------------------
-- Exp using iteration
--------------------------------------------------

erc_exp2_p :: ERC s INTEGER -> ERC s REAL -> ERC s REAL
erc_exp2_p p param_x =
  do
  x <- declareREAL $ param_x -- copy-in parameter passing
  n <- declareINTEGER $ 1
  rn <- declareREAL $ 1
  c <- declareREAL $ 1 + (x?)
  a <- declareREAL $ (c?)
  b <- declareREAL $ (a?) * (c?)
  while (choose [iota(p) >* (b?)-(a?), (b?)-(a?) >* iota(p-1)] ==# 1) $ do
    -- tracePrecision
    -- traceINTEGER "p=" p
    -- traceREAL "iota(p-1)=" (iota(p-1))
    -- traceREAL "b-a=" ((b?)-(a?))
    -- traceREAL "a=" (a?)
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
