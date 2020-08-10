{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Pair where

import Prelude

import Control.Monad.ST.Trans

import ERC.Monad
import ERC.Variables

pair_ :: ERC s a -> ERC s b -> ERC s (a,b)
pair_ a b = (,) <$> a <*> b

fst_ :: ERC s (a,b) -> ERC s a
fst_ ab = fst <$> ab

snd_ :: ERC s (a,b) -> ERC s b
snd_ ab = snd <$> ab

assignPair,(..=) :: (Var s t1, Var s t2) -> ERC s (t1,t2) -> ERC s ()
assignPair (v1,v2) pairERC =
  do
  (val1, val2) <- pairERC
  ifInvalidUseDummy () $ writeSTRef v1 val1
  ifInvalidUseDummy () $ writeSTRef v2 val2

(..=) = assignPair
