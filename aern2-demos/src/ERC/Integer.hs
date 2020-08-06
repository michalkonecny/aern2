{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Integer where

import Prelude

import Control.Monad.ST.Trans

import ERC.Monad
import ERC.Logic

type INTEGER = Integer

declareINTEGER :: ERC s INTEGER -> ERC s (STRef s INTEGER)
declareINTEGER i = i >>= newSTRef

eqINTEGER, ltINTEGER, leqINTEGER, geqINTEGER, gtINTEGER :: ERC s INTEGER -> ERC s INTEGER -> ERC s KLEENEAN
eqINTEGER a b = boolToKleenean <$> ((==) <$> a <*> b)
ltINTEGER a b = boolToKleenean <$> ((<) <$> a <*> b)
leqINTEGER a b = boolToKleenean <$> ((<=) <$> a <*> b)
geqINTEGER a b = boolToKleenean <$> ((>=) <$> a <*> b)
gtINTEGER a b = boolToKleenean <$> ((>) <$> a <*> b)

(==#), (<#),(<=#),(>#),(>=#) :: ERC s INTEGER -> ERC s INTEGER -> ERC s KLEENEAN
(==#) = eqINTEGER
(<#) = ltINTEGER
(<=#) = leqINTEGER
(>=#) = geqINTEGER
(>#) = gtINTEGER
infix 4 ==#, <#, <=#, >=#, >#

instance Num (ERC s INTEGER) where
  fromInteger = pure
  negate a = negate <$> a
  abs a = abs <$> a
  signum a = signum <$> a
  a + b = (+) <$> a <*> b
  a - b = (-) <$> a <*> b
  a * b = (*) <$> a <*> b

