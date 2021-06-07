module ERC.Statements where

import Prelude

import AERN2.Kleenean

import ERC.Monad
import ERC.Variables
import ERC.Logic
import ERC.Integer

return_ :: ERC s a -> ERC s a
return_ = id

while_ :: ERC s KLEENEAN -> ERC s a -> ERC s ()
while_ condERC doAction = aux
  where
  aux =
    do
    cond <- condERC
    case cond of
      CertainTrue  -> do { _ <- doAction; aux } 
      CertainFalse -> pure ()
      TrueOrFalse  -> insufficientPrecision ()

ifThenElse_ :: ERC s KLEENEAN -> (ERC s (), ERC s ()) -> ERC s ()
ifThenElse_ condERC (thenAction, elseAction) =
  do
  cond <- condERC
  case cond of
    CertainTrue  -> thenAction
    CertainFalse -> elseAction
    TrueOrFalse  -> insufficientPrecision ()

else_ :: ERC s () -> ERC s () -> (ERC s (), ERC s ())
else_ = (,)

ifThen_ :: ERC s KLEENEAN -> ERC s () -> ERC s ()
ifThen_ condERC thenAction =
  do
  cond <- condERC
  case cond of
    CertainTrue  -> thenAction
    CertainFalse -> pure ()
    TrueOrFalse  -> insufficientPrecision ()

forNfromTo_ :: Var s INTEGER -> ERC s INTEGER -> ERC s INTEGER -> ERC s a -> ERC s ()
forNfromTo_ n k l c =
  do
  n .= k
  while_ ((n?) <=# l) $ do
    _ <- c
    n .= (n?) + 1
