{-|

Experimenting with programming in ERC shallow
embedding in Haskell/AERN2.

ERC is an experimental core language for exact real computation
developed within the CID EU project in 2017-2020.

-}
module ERC.Logic where

import Prelude
import qualified MixedTypesNumPrelude as MX

import Control.Monad.ST.Trans

import Data.List

import AERN2.Kleenean

import ERC.Monad

type KLEENEAN = Kleenean

kTrue, kFalse, kUnknown :: KLEENEAN
kTrue = CertainTrue 
kFalse = CertainFalse 
kUnknown = TrueOrFalse

boolToKleenean :: Bool -> KLEENEAN
boolToKleenean True = kTrue
boolToKleenean False = kFalse

declareKLEENEAN :: ERC s KLEENEAN -> ERC s (STRef s KLEENEAN)
declareKLEENEAN k = k >>= newSTRef

(&&?) :: ERC s KLEENEAN -> ERC s KLEENEAN -> ERC s KLEENEAN
a &&? b = checkK $ (MX.&&) <$> a <*> b

infix 3 &&?

choose :: [ERC s KLEENEAN] -> ERC s Integer
choose optionsERC =
  do
  options <- sequence optionsERC
  case all (== kFalse) options of
    True -> error "ERC choose: all options failed"
    _ -> 
      case elemIndex kTrue options of
        Just i -> pure $ fromIntegral i
        _ -> insufficientPrecision (-1)

parallelIfThenElse :: (CanHull (ERC s a)) => ERC s KLEENEAN -> ERC s a -> ERC s a -> ERC s a
parallelIfThenElse condERC branch1 branch2 =
  do
  cond <- condERC
  case cond of
    CertainTrue  -> branch1
    CertainFalse -> branch2
    _ -> do
      branch1 `hull` branch2

class CanHull a where
  hull :: a -> a -> a

checkK :: ERC s KLEENEAN -> ERC s KLEENEAN
checkK = ifInvalidUseDummy kUnknown
