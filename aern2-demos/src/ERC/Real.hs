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

import AERN2.MP
import AERN2.MP.Ball (hullMPBall)
import qualified Numeric.MixedTypes.Ord as MixOrd

import ERC.Monad
import ERC.Variables
import ERC.Logic
import ERC.Integer
import ERC.Statements

type REAL = MPBall

declareREAL :: ERC s REAL -> ERC s (Var s REAL)
declareREAL rERC = 
  do
  r <- checkR $ rERC
  newSTRef r

____traceREAL :: String -> ERC s REAL -> ERC s ()
____traceREAL label rERC =
  do
  r <- rERC
  trace (label ++ show r) $ pure ()

runERC_REAL :: Integer -> (forall s. ERC s REAL) -> MPBall
runERC_REAL ac = runERC (\result -> getAccuracy result >= bits ac)

ltREAL, leqREAL, geqREAL, gtREAL :: ERC s REAL -> ERC s REAL -> ERC s KLEENEAN
ltREAL a b = checkK $ (MixOrd.<) <$> a <*> b
leqREAL a b = checkK $ ((MixOrd.<=) <$> a <*> b)
geqREAL a b = checkK $ ((MixOrd.>=) <$> a <*> b)
gtREAL a b = checkK $ ((MixOrd.>) <$> a <*> b)

(<*),(<=*),(>*),(>=*) :: ERC s REAL -> ERC s REAL -> ERC s KLEENEAN
(<*) = ltREAL
(<=*) = leqREAL
(>=*) = geqREAL
(>*) = gtREAL
infix 4 <*, <=*, >=*, >*

maxREAL :: (ERC s REAL, ERC s REAL) -> ERC s REAL
maxREAL (x, y) = parallelIfThenElse (x >* y) x y

absREAL :: ERC s REAL -> ERC s REAL
absREAL x = parallelIfThenElse (x >* 0) x (- x)

instance CanHull (ERC s REAL) where
  hull a b = hullMPBall <$> a <*> b

instance Num (ERC s REAL) where
  fromInteger i = 
    do
    precision <- getPrecisionERC
    pure $ mpBallP precision i
  negate a = checkR $ negate <$> a
  abs a = checkR $ abs <$> a
  signum a = signum <$> a
  a + b = checkR $ (+) <$> a <*> b
  a - b = checkR $ (-) <$> a <*> b
  a * b = checkR $ (*) <$> a <*> b

instance Fractional (ERC s REAL) where
  fromRational q = 
    do
    precision <- getPrecisionERC
    pure $ mpBallP precision q
  a / b =
    checkR $ do
    a_ <- a
    b_ <- b
    case b_ MixOrd.!>! (0 :: MPBall) || b_ MixOrd.!<! (0 :: MPBall) of
      True -> pure $ a_ / b_
      _ -> insufficientPrecision dummyReal

powerREALtoINTEGER :: ERC s REAL -> ERC s INTEGER -> ERC s REAL
powerREALtoINTEGER param_x j =
  checkR $ do
  x <- declareREAL $ param_x
  y <- declareREAL $ 1
  n <- declareINTEGER $ 0
  forNfromTo_ n 1 j $
    y .= (y?) * (x?)
  forNfromTo_ n 1 (-j) $
    y .= (y?) / (x?)
  return_ (y?)

(^*) :: ERC s REAL -> ERC s INTEGER -> ERC s REAL
(^*) = powerREALtoINTEGER

infix 8 ^*

iota :: ERC s INTEGER -> ERC s REAL
iota i = checkR $ (2^^) <$> i

limit :: (ERC s INTEGER -> ERC s REAL) -> ERC s REAL
limit x_ = checkR $ do
  precision <- getPrecisionERC
  let p = negate $ fromIntegral precision - 10
  let precisions = take 100 $ standardPrecisions precision
  x_p <- raisePrecisionUntilAccurate precisions p $ x_ (pure p)
  setPrecisionERC precision
  iotaP <- iota (pure p)
  pure (x_p + (hullMPBall (- iotaP) iotaP))
  where
  raisePrecisionUntilAccurate [] _ _ = insufficientPrecision dummyReal
  raisePrecisionUntilAccurate (precision:precs) accuracy action = checkR $ do
    setPrecisionERC precision
    result <- action
    isValid <- getIsValidERC
    -- traceREAL "result" (pure result)
    case isValid && getAccuracy result >= (bits accuracy) of
      True -> pure result
      False -> 
        do
        setValidERC
        raisePrecisionUntilAccurate precs accuracy action

checkR :: ERC s REAL -> ERC s REAL
checkR = ifInvalidUseDummy dummyReal

dummyReal :: REAL
dummyReal = 1
