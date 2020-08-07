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
type ERC s =  (STT s (StateT Precision Maybe))

-- instance PrimMonad (STT s m) where
--   type PrimState (STT s m) = s
--   primitive = ST
--   {-# INLINE primitive #-}

insufficientPrecision :: ERC s a
insufficientPrecision = throwError ()

runERC :: (Show t) => (t -> Bool) -> (forall s. ERC s t) -> t
runERC isOK rComp =
  tryWithPrecision $ take 100 $ standardPrecisions 20
  where
  tryWithPrecision [] = error "runERC: no more precisions to try"
  tryWithPrecision (p:rest) = 
    case rComp2 p of
      Just (result, _) | isOK result -> result
      _ -> tryWithPrecision rest
    where
    (StateT rComp2) = runSTT rComp

tracePrecision :: ERC s ()
tracePrecision =
  do
  precision <- lift get
  trace ("precision = " ++ show precision) $ pure ()
