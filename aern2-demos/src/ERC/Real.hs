{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Real where

import Debug.Trace (trace)

import Prelude

import Control.Monad.ST.Trans
import Control.Monad.Except (lift)

import Control.Monad.Trans.State

import AERN2.MP
import AERN2.MP.Ball (hullMPBall)
import qualified Numeric.MixedTypes.Ord as MixOrd

import ERC.Monad
import ERC.Logic
import ERC.Integer

--------------------------------------------------
-- Elements of the ERC language
--------------------------------------------------


type REAL = MPBall

declareREAL :: ERC s REAL -> ERC s (STRef s REAL)
declareREAL r = 
  r >>= newSTRef

traceREAL :: String -> ERC s REAL -> ERC s ()
traceREAL label rComp =
  do
  r <- rComp
  trace (label ++ show r) $ pure ()

runERC_REAL :: Accuracy -> (forall s. ERC s REAL) -> MPBall
runERC_REAL ac = runERC (\result -> getAccuracy result >= ac)

ltREAL, leqREAL, geqREAL, gtREAL :: ERC s REAL -> ERC s REAL -> ERC s KLEENEAN
ltREAL a b = (MixOrd.<) <$> a <*> b
leqREAL a b = ((MixOrd.<=) <$> a <*> b)
geqREAL a b = ((MixOrd.>=) <$> a <*> b)
gtREAL a b = ((MixOrd.>) <$> a <*> b)

(<*),(<=*),(>*),(>=*) :: ERC s REAL -> ERC s REAL -> ERC s KLEENEAN
(<*) = ltREAL
(<=*) = leqREAL
(>=*) = geqREAL
(>*) = gtREAL
infix 4 <*, <=*, >=*, >*

iota :: ERC s INTEGER -> ERC s REAL
iota i = (2^^) <$> i

limit :: (ERC s INTEGER -> ERC s REAL) -> ERC s REAL
limit x_ = 
  do
  precision <- lift get
  let p = negate $ fromIntegral precision - 10
  x_p <- x_ (pure p)
  iotaP <- iota (pure p)
  pure (x_p + (hullMPBall (- iotaP) iotaP))

instance Num (ERC s REAL) where
  fromInteger i = 
    lift $
      do
      precision <- get
      pure $ mpBallP precision i
  negate a = negate <$> a
  abs a = abs <$> a
  signum a = signum <$> a
  a + b = (+) <$> a <*> b
  a - b = (-) <$> a <*> b
  a * b = (*) <$> a <*> b

instance Fractional (ERC s REAL) where
  fromRational q = 
    lift $
      do
      precision <- get
      pure $ mpBallP precision q
  a / b = 
    do
    a_ <- a
    b_ <- b
    case b_ MixOrd.!>! (0 :: MPBall) || b_ MixOrd.!<! (0 :: MPBall) of
      True -> pure $ a_ / b_
      _ -> insufficientPrecision

instance CanHull (ERC s REAL) where
  hull a b = hullMPBall <$> a <*> b