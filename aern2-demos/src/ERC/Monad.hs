{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Monad where

import Prelude

import Debug.Trace (trace)

import Control.Monad.ST.Trans
import Control.Monad.Except
import Data.Functor.Identity

import Control.Monad.Trans.State

import AERN2.MP

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif


{-| ERC is a monad of stateful computations that maintain a global precision and may fail. -}
type ERC s =  (STT s (State (Precision, Bool)))

getPrecisionERC :: ERC s Precision
getPrecisionERC = lift $ fst <$> get

setPrecisionERC :: Precision -> ERC s ()
setPrecisionERC precision =
  lift $ do 
    (_, isValid) <- get
    put (precision, isValid)

getIsValidERC :: ERC s Bool
getIsValidERC = lift $ snd <$> get

setInvalidERC :: ERC s ()
setInvalidERC =
  lift $ do 
    (precision, _) <- get
    put (precision, False)

setValidERC :: ERC s ()
setValidERC =
  lift $ do 
    (precision, _) <- get
    put (precision, True)

insufficientPrecision :: a -> ERC s a
insufficientPrecision dummy = setInvalidERC >> pure dummy

ifInvalidUseDummy :: a -> ERC s a -> ERC s a
ifInvalidUseDummy dummy iERC =
  do
  isValid <- getIsValidERC
  case isValid of
    True -> iERC
    False -> pure dummy

runERC :: (Show t) => (t -> Bool) -> (forall s. ERC s t) -> t
runERC isOK rComp =
  tryWithPrecision $ take 100 $ standardPrecisions 20
  where
  tryWithPrecision [] = error "runERC: no more precisions to try"
  tryWithPrecision (p:rest) = 
    case rComp2 (p, True) of
      Identity (result, (_, isValid)) | isValid && isOK result -> result
      _ -> tryWithPrecision rest
    where
    (StateT rComp2) = runSTT rComp

tracePrecision :: ERC s ()
tracePrecision =
  do
  precision <- getPrecisionERC
  trace ("precision = " ++ show precision) $ pure ()
