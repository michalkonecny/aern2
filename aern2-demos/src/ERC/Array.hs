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

import Control.Monad.Trans.State

import AERN2.MP
import AERN2.MP.Ball (hullMPBall)
import qualified Numeric.MixedTypes.Ord as MixOrd

import ERC.Monad
import ERC.Logic
import ERC.Integer
import ERC.Real


-- type REALn n s = VM.MVector n s REAL
-- -- type REALnm n m s = VM.MVector (n*m) s REAL

-- declareREALn :: p n -> REALn n s -> ERC s (STRef s (REALn n s))
-- declareREALn _ = newSTRef

-- -- declareREALnm :: REALnm n m s -> ERC s (STRef s (REALnm n m s))
-- -- declareREALnm = lift . lift . newSTRef

-- array :: (KnownNat n) => [ERC s REAL] -> ERC s (REALn n s)
-- array itemsERC = 
--   do
--   items <- sequence itemsERC
--   case V.fromList items of
--     Just vector -> V.unsafeThaw vector
--     _ -> error "ERC array literal of incorrect size"

-- arrayLookup :: (KnownNat n) => (REALn n s) -> INTEGER -> ERC s REAL
-- arrayLookup a index = VM.read a (finite index)

-- arrayUpdate :: (KnownNat n) => (REALn n s) -> INTEGER -> REAL -> ERC s ()
-- arrayUpdate a index = VM.write a (finite index)
