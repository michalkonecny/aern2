{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Logic where

import Prelude

import Control.Monad.ST.Trans

import Data.List

import ERC.Monad

--------------------------------------------------
-- Elements of the ERC language
--------------------------------------------------

type KLEENEAN = Maybe Bool

kTrue, kFalse, kUnknown :: KLEENEAN
kTrue = Just True
kFalse = Just False
kUnknown = Nothing

boolToKleenean :: Bool -> KLEENEAN
boolToKleenean True = kTrue
boolToKleenean False = kFalse

declareKLEENEAN :: ERC s KLEENEAN -> ERC s (STRef s KLEENEAN)
declareKLEENEAN k = k >>= newSTRef

choose :: [ERC s KLEENEAN] -> ERC s Integer
choose options =
  do
  options2 <- sequence options
  case all (== kFalse) options2 of
    True -> error "ERC choose: all options failed"
    _ -> 
      case elemIndex kTrue options2 of
        Just i -> pure $ fromIntegral i
        _ -> insufficientPrecision
