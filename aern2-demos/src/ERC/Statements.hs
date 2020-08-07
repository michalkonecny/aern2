{-# LANGUAGE PostfixOperators #-}
{-|

ERC shallow embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Statements where

import Prelude

import ERC.Monad
import ERC.Variables
import ERC.Logic
import ERC.Integer

return_ :: ERC s a -> ERC s a
return_ = id

while :: ERC s KLEENEAN -> ERC s a -> ERC s ()
while condERC doAction = aux
  where
  aux = 
    do
    cond <- condERC
    case cond of
      Just True -> do { _ <- doAction; aux } 
      Just False -> pure ()
      Nothing -> insufficientPrecision

forNfromTo :: Var s INTEGER -> ERC s INTEGER -> ERC s INTEGER -> ERC s a -> ERC s ()
forNfromTo n k l c =
  do
  n .= k
  while ((n?) <=# l) $ do
    _ <- c
    n .= (n?) + 1
