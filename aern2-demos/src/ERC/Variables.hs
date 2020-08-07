{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Variables where

import Prelude

import Control.Monad.ST.Trans

import ERC.Monad

type Var s = STRef s

(?) :: Var s t -> ERC s t
(?) = readSTRef

assign :: Var s t -> ERC s t -> ERC s ()
assign v valERC =
  do
  val <- valERC
  writeSTRef v val

(.=) :: Var s t -> ERC s t -> ERC s ()
(.=) = assign

infixl 0 .=
