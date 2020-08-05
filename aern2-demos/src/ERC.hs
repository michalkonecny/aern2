{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import Control.CollectErrors

import Control.Monad.ST
import Data.STRef
import qualified  Data.Vector.Mutable.Sized as V
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Finite
import GHC.TypeNats

import Data.List

import AERN2.MP

-- import AERN2.Limit

--------------------------------------------------
-- Elements of the ERC language
--------------------------------------------------

{-| ERC is a monad of stateful computations that maintain a global precision and may fail. -}
type ERC s = MaybeT (StateT Precision (ST s))

insufficientPrecision :: ERC s a
insufficientPrecision = MaybeT $ pure Nothing

type KLEENEAN = Maybe Bool

kTrue, kFalse, kUnknown :: KLEENEAN
kTrue = Just True
kFalse = Just False
kUnknown = Nothing

type INTEGER = Integer

type REAL = CN MPBall -- TODO: REAL should be an abstract type

type REALn n s = V.MVector n s REAL
-- type REALnm n m s = V.MVector (n*m) s REAL

arrayLookup :: (KnownNat n) => (REALn n s) -> INTEGER -> ERC s REAL
arrayLookup array index = V.read array (finite index)

arrayUpdate :: (KnownNat n) => (REALn n s) -> INTEGER -> REAL -> ERC s ()
arrayUpdate array index = V.write array (finite index)

declareKLEENEAN :: KLEENEAN -> ERC s (STRef s KLEENEAN)
declareKLEENEAN = lift . lift . newSTRef

declareINTEGER :: INTEGER -> ERC s (STRef s INTEGER)
declareINTEGER = lift . lift . newSTRef

declareREAL :: REAL -> ERC s (STRef s REAL)
declareREAL = lift . lift . newSTRef

declareREALn :: REALn n s -> ERC s (STRef s (REALn n s))
declareREALn = lift . lift . newSTRef

-- declareREALnm :: REALnm n m s -> ERC s (STRef s (REALnm n m s))
-- declareREALnm = lift . lift . newSTRef

choose :: [KLEENEAN] -> ERC s (CN Integer)
choose options
  | all (== kFalse) options =
      pure $ noValueNumErrorCertainECN (Nothing :: Maybe Integer) $
        NumError "choose: all options failed"
  | otherwise =
    case elemIndex kTrue options of
      Just i -> pure $ cn (integer i)
      _ -> insufficientPrecision


