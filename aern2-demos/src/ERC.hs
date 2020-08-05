{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PostfixOperators #-}
-- #define DEBUG
{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC where

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
import Control.Monad.Error
-- import Data.STRef

import qualified  Data.Vector.Mutable.Sized as VM
import qualified  Data.Vector.Sized as V
import Control.Monad.Primitive

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Finite
import GHC.TypeNats

import Data.List

import AERN2.MP
import AERN2.MP.Ball
import Numeric.MixedTypes.Ord ((!<!),(!>!))

-- import AERN2.Limit

--------------------------------------------------
-- Elements of the ERC language
--------------------------------------------------

{-| ERC is a monad of stateful computations that maintain a global precision and may fail. -}
type ERC s =  (STT s (StateT Precision Maybe))

-- instance PrimMonad (STT s m) where
--   type PrimState (STT s m) = s
--   primitive = ST
--   {-# INLINE primitive #-}

insufficientPrecision :: ERC s a
insufficientPrecision = throwError ()

type Var s = STRef s

(?) :: Var s t -> ERC s t
(?) = readSTRef

assign :: Var s t -> ERC s t -> ERC s ()
assign v valERC =
  do
  val <- valERC
  writeSTRef v val

(.=) :: Var s t -> ERC s t -> ERC s ()
(.=) = assign

infixl 0 .=

type KLEENEAN = Maybe Bool

kTrue, kFalse, kUnknown :: KLEENEAN
kTrue = Just True
kFalse = Just False
kUnknown = Nothing

boolToKleenean :: Bool -> KLEENEAN
boolToKleenean True = kTrue
boolToKleenean False = kFalse

declareKLEENEAN :: ERC s KLEENEAN -> ERC s (STRef s KLEENEAN)
declareKLEENEAN k = k >>= newSTRef

choose :: [KLEENEAN] -> ERC s INTEGER
choose options
  | all (== kFalse) options =
      error "ERC choose: all options failed"
  | otherwise =
    case elemIndex kTrue options of
      Just i -> pure $ fromIntegral i
      _ -> insufficientPrecision

type INTEGER = Integer

declareINTEGER :: ERC s INTEGER -> ERC s (STRef s INTEGER)
declareINTEGER i = i >>= newSTRef

ltINTEGER, leqINTEGER, geqINTEGER, gtINTEGER :: ERC s INTEGER -> ERC s INTEGER -> ERC s KLEENEAN
ltINTEGER a b = boolToKleenean <$> ((<) <$> a <*> b)
leqINTEGER a b = boolToKleenean <$> ((<=) <$> a <*> b)
geqINTEGER a b = boolToKleenean <$> ((>=) <$> a <*> b)
gtINTEGER a b = boolToKleenean <$> ((>) <$> a <*> b)

(<#),(<=#),(>#),(>=#) :: ERC s INTEGER -> ERC s INTEGER -> ERC s KLEENEAN
(<#) = ltINTEGER
(<=#) = leqINTEGER
(>=#) = geqINTEGER
(>#) = gtINTEGER
infix 4 <#, <=#, >=#, >#

instance Num (ERC s INTEGER) where
  fromInteger = pure
  negate a = negate <$> a
  a + b = (+) <$> a <*> b
  a - b = (-) <$> a <*> b
  a * b = (*) <$> a <*> b
  -- TODO

type REAL = MPBall -- TODO: REAL should be an abstract type

declareREAL :: ERC s REAL -> ERC s (STRef s REAL)
declareREAL r = 
  r >>= newSTRef

traceREAL :: String -> ERC s REAL -> ERC s ()
traceREAL label rComp =
  do
  r <- rComp
  trace (label ++ show r) $ pure ()


runERC_REAL :: Accuracy -> (forall s. ERC s REAL) -> MPBall
runERC_REAL ac rComp =
  tryWithPrecision $ standardPrecisions 20
  where
  tryWithPrecision [] = error "runERC_REAL: no more precisions to try"
  tryWithPrecision (p:rest) = 
    maybeTrace ("p = " ++ show p) $
    maybeTrace ("rComp2 p = " ++ show (rComp2 p)) $
    case rComp2 p of
      Just (result, _) | getAccuracy result >= ac -> result
      _ -> tryWithPrecision rest
    where
    (StateT rComp2) = runSTT rComp
  

instance Num (ERC s REAL) where
  fromInteger = real
  negate a = negate <$> a
  a + b = (+) <$> a <*> b
  a - b = (-) <$> a <*> b
  a * b = (*) <$> a <*> b
  -- TODO

real :: Integer -> ERC s REAL
real i = 
  lift $
    do
    precision <- get
    pure $ mpBallP precision i

instance Fractional (ERC s REAL) where
  a / b = 
    do
    a_ <- a
    b_ <- b
    case b_ !>! (0 :: MPBall) || b_ !<! (0 :: MPBall) of
      True -> pure $ a_ / b_
      _ -> insufficientPrecision
  -- TODO

-- instance CanDiv (ERC s REAL) (ERC s REAL) where
--   type DivTypeNoCN (ERC s REAL) (ERC s REAL) = (ERC s REAL)
--   type DivType (ERC s REAL) (ERC s REAL) = (ERC s REAL)
--   divide a b = divide <$> a <*> b
--   divideNoCN a b = divideNoCN <$> a <*> b

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

while :: ERC s KLEENEAN -> ERC s a -> ERC s ()
while condERC doAction = aux
  where
  aux = 
    do
    cond <- condERC
    case cond of
      Just True -> do { _ <- doAction; aux } 
      Just False -> pure ()
      Nothing -> insufficientPrecision

--------------------------------------------------
-- Example JMMuller
--------------------------------------------------

erc_example_JMMuller :: INTEGER -> ERC s REAL
erc_example_JMMuller n0 =
  do
  n <- declareINTEGER $ fromInteger n0
  a <- declareREAL $ 11 / 2
  b <- declareREAL $ 61 / 11
  c <- declareREAL $ 0
  while ((n?) ># 0) $ do
    -- traceREAL "a = " (a?)
    c .= 111 - (1130 - 3000/(a?))/(b?)
    a .= (b?)
    b .= (c?)
    n .= (n?) - 1
  (a?)

run_JMMuller :: Integer -> Integer -> MPBall
run_JMMuller n ac = runERC_REAL (bits ac) (erc_example_JMMuller n)