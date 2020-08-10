{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
module ERC.Array where

import Debug.Trace (trace)
#ifdef DEBUG
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import Prelude

-- import MixedTypesNumPrelude
-- import Numeric.CollectErrors
-- import qualified Prelude as P

import Control.Monad.ST.Trans
import Control.Monad.Except
-- import Data.STRef

-- import qualified  Data.Vector.Mutable.Sized as VM
-- import qualified  Data.Vector.Sized as V
-- import Control.Monad.Primitive
-- import Data.Finite
-- import GHC.TypeNats

import Control.Monad.ST.Trans

import AERN2.MP
import AERN2.MP.Ball (hullMPBall)
import qualified Numeric.MixedTypes.Ord as MixOrd

import ERC.Monad
import ERC.Variables
import ERC.Logic
import ERC.Integer
import ERC.Real


type REALn s = STArray s Integer REAL

declareREALn :: ERC s (REALn s) -> ERC s (Var s (REALn s))
declareREALn aERC = 
  do
  a <- checkA $ aERC
  newSTRef a

-- array :: (KnownNat n) => [ERC s REAL] -> ERC s (REALn n s)
array :: [ERC s REAL] -> ERC s (REALn s)
array itemsERC = 
  do
  items <- sequence itemsERC
  let n = fromIntegral (length items) :: Integer
  a <- newSTArray (0, n - 1) 0
  zipWithM_ (writeSTArray a) [0..] items -- initialise the array using items
  return a

-- arrayLookup :: (KnownNat n) => (REALn n s) -> INTEGER -> ERC s REAL
arrayLookup, (?!) :: ERC s (REALn s) -> [ERC s INTEGER] -> ERC s REAL
arrayLookup aERC [iERC] =
  do
  a <- checkA aERC
  i <- checkI iERC
  checkR $ readSTArray a i
arrayLookup _ _ = error "arrayLookup: supporting only 1-dimensional arrays"

(?!) = arrayLookup
infix 1 ?!

-- arrayUpdate :: (KnownNat n) => (REALn n s) -> INTEGER -> REAL -> ERC s ()
arrayUpdate :: ERC s (REALn s) -> [ERC s INTEGER] -> ERC s REAL -> ERC s ()
arrayUpdate aERC [iERC] rERC =
  do
  a <- checkA aERC
  i <- checkI iERC
  r <- checkR rERC
  ifInvalidUseDummy () $ writeSTArray a i r
arrayUpdate _ _ _ = error "arrayUpdate: supporting only 1-dimensional arrays"

(.=!) = arrayLookup
infix 1 .=!

checkA :: ERC s (REALn s) -> ERC s (REALn s)
checkA aERC = 
  do
  dummyA <- error "accessing a non-existent dummy array"
  ifInvalidUseDummy dummyA aERC
