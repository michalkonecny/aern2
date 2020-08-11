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
  ifInvalidUseDummy () $ writeSTRef v val

(.=) :: Var s t -> ERC s t -> ERC s ()
(.=) = assign

infixl 1 .=
