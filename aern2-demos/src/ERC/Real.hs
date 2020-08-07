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
import ERC.Variables
import ERC.Logic
import ERC.Integer
import ERC.Statements

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

runERC_REAL :: Integer -> (forall s. ERC s REAL) -> MPBall
runERC_REAL ac = runERC (\result -> getAccuracy result >= bits ac)

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

maxREAL :: (ERC s REAL, ERC s REAL) -> ERC s REAL
maxREAL (x, y) = parallelIfThenElse (x >* y) x y

instance CanHull (ERC s REAL) where
  hull a b = hullMPBall <$> a <*> b

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

powerREALtoINTEGER :: ERC s REAL -> ERC s INTEGER -> ERC s REAL
powerREALtoINTEGER param_x j =
  do
  x <- declareREAL $ param_x
  y <- declareREAL $ 1
  n <- declareINTEGER $ 0
  forNfromTo n 1 j $ do
    y .= (y?) * (x?)
  forNfromTo n 1 (-j) $ do
    y .= (y?) / (x?)
  return_ (y?)

(^*) :: ERC s REAL -> ERC s INTEGER -> ERC s REAL
(^*) = powerREALtoINTEGER

infix 8 ^*

iota :: ERC s INTEGER -> ERC s REAL
iota i = (2^^) <$> i

limit :: (ERC s INTEGER -> ERC s REAL) -> ERC s REAL
limit x_ = 
  do
  precision <- lift get
  let p = negate $ fromIntegral precision - 10
  let precisions = take 100 $ standardPrecisions precision
  x_p <- raisePrecisionUntilAccurate precisions p $ x_ (pure p)
  iotaP <- iota (pure p)
  pure (x_p + (hullMPBall (- iotaP) iotaP))
  where
  raisePrecisionUntilAccurate [] _ _ = error "limit: no more precisions to try"
  raisePrecisionUntilAccurate (precision:precs) accuracy action =
    do
    lift $ put precision
    result <- action
    case getAccuracy result >= (bits accuracy) of
      True -> pure result
      False -> raisePrecisionUntilAccurate precs accuracy action
