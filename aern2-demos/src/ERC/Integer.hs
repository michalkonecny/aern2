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

import Debug.Trace (trace)

import Control.Monad.ST.Trans

import ERC.Monad
import ERC.Variables
import ERC.Logic

type INTEGER = Integer

declareINTEGER :: ERC s INTEGER -> ERC s (Var s INTEGER)
declareINTEGER iERC =
  do
  i <- checkI $ iERC
  newSTRef i

traceINTEGER :: String -> ERC s INTEGER -> ERC s ()
traceINTEGER label iERC =
  do
  i <- iERC
  trace (label ++ show i) $ pure ()

eqINTEGER, ltINTEGER, leqINTEGER, geqINTEGER, gtINTEGER :: ERC s INTEGER -> ERC s INTEGER -> ERC s KLEENEAN
eqINTEGER a b = checkK $ boolToKleenean <$> ((==) <$> a <*> b)
ltINTEGER a b = checkK $ boolToKleenean <$> ((<) <$> a <*> b)
leqINTEGER a b = checkK $ boolToKleenean <$> ((<=) <$> a <*> b)
geqINTEGER a b = checkK $ boolToKleenean <$> ((>=) <$> a <*> b)
gtINTEGER a b = checkK $ boolToKleenean <$> ((>) <$> a <*> b)

(==#), (<#),(<=#),(>#),(>=#) :: ERC s INTEGER -> ERC s INTEGER -> ERC s KLEENEAN
(==#) = eqINTEGER
(<#) = ltINTEGER
(<=#) = leqINTEGER
(>=#) = geqINTEGER
(>#) = gtINTEGER
infix 4 ==#, <#, <=#, >=#, >#

instance Num (ERC s INTEGER) where
  fromInteger = pure
  negate a = checkI $ negate <$> a
  abs a = checkI $ abs <$> a
  signum a = checkI $ signum <$> a
  a + b = checkI $ (+) <$> a <*> b
  a - b = checkI $ (-) <$> a <*> b
  a * b = checkI $ (*) <$> a <*> b
    
checkI :: ERC s INTEGER -> ERC s INTEGER
checkI = ifInvalidUseDummy (-1)
