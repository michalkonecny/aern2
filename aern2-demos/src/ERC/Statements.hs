{-|

ERC shallow embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Statements where

import Prelude

import ERC.Monad
import ERC.Logic

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
