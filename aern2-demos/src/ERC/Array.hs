{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
module ERC.Array where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import Prelude

import Control.Monad.ST.Trans
import Control.Monad.Except

import GHC.TypeLits

import Text.Printf

import ERC.Monad
import ERC.Variables
import ERC.Integer
import ERC.Real


newtype REALn s (n :: Nat) =
  REALn { unREALn :: STArray s Integer REAL }

newtype REALnm s (n :: Nat) (m :: Nat) =
  REALnm { unREALnm :: REALn s n }

declareREALn :: ERC s (REALn s n) -> ERC s (Var s (REALn s n))
declareREALn aERC = 
  do
  a <- checkA $ aERC
  newSTRef a

declareREALnm :: ERC s (REALnm s n m) -> ERC s (Var s (REALnm s n m))
declareREALnm aERC = 
  do
  a <- checkAA $ aERC
  newSTRef a

array :: (KnownNat n) => [ERC s REAL] -> ERC s (REALn s n)
array itemsERC = 
  do
  items <- sequence itemsERC
  let n = fromIntegral (length items) :: Integer
  a <- newSTArray (0, n - 1) 0
  let aa = REALn a
  let an = natVal aa
  case an == n of
    True -> pure ()
    False -> error $ printf "array literal length (%d) does not match the array size (%d)" n an
  zipWithM_ (writeSTArray a) [0..] items -- initialise the array using items
  return aa

array2D :: (KnownNat n, KnownNat m) => [[ERC s REAL]] -> ERC s (REALnm s n m)
array2D [] = error "2D array literal must not be empty"
array2D itemsERC
  | not allEqualLength = error "2D array literal should have rows of equal lenths"
  | otherwise =
    do
    items <- sequence (concat itemsERC)
    a <- newSTArray (0, n*m - 1) 0
    let aa = REALn a
    let an = natVal aa
    let aaa = REALnm aa
    let am = natVal aaa
    case am == m && an == n of
      True -> pure ()
      False -> error $ printf "2D array literal length (%d * %d) does not match the array size (%d * %d)" n m an am
    zipWithM_ (writeSTArray a) [0..] items -- initialise the array using items
    return aaa
  where
  n_ = length (head itemsERC)
  m_ = length itemsERC
  allEqualLength = all (== n_) (map length itemsERC)
  n = fromIntegral n_ :: Integer
  m = fromIntegral m_ :: Integer

arrayLength :: (KnownNat n) => ERC s (REALn s n) -> ERC s INTEGER
arrayLength aERC =
  do
  aa <- checkA aERC
  pure (natVal aa)

array2DLength1 :: (KnownNat n) => ERC s (REALnm s n m) -> ERC s INTEGER
array2DLength1 aERC =
  do
  (REALnm aa) <- checkAA aERC
  pure (natVal aa)

array2DLength2 :: (KnownNat m) => ERC s (REALnm s n m) -> ERC s INTEGER
array2DLength2 aERC =
  do
  aa <- checkAA aERC
  pure (natVal aa)

arrayLookup, (?!) :: (KnownNat n) => ERC s (REALn s n) -> [ERC s INTEGER] -> ERC s REAL
arrayLookup _ [] =
  error "arrayLookup: missing index"
arrayLookup aERC [iERC] =
  do
  aa@(REALn a) <- aERC
  i <- checkI iERC
  case 0 <= i && i < natVal aa of
    True -> pure ()
    False -> error "arrayLookup: index out of bounds"
  checkR $ readSTArray a i
arrayLookup _ _ = error "arrayLookup: supporting only 1-dimensional arrays"

(?!) = arrayLookup
infix 1 ?!

array2DLookup, (?!!) :: (KnownNat n, KnownNat m) => ERC s (REALnm s n m) -> [ERC s INTEGER] -> ERC s REAL
array2DLookup _ [] =
  error "array2DLookup: missing index"
array2DLookup _ [_] =
  error "array2DLookup: missing second index"
array2DLookup aERC [iERC, jERC] =
  do
  aaa@(REALnm aa@(REALn a)) <- aERC
  i <- checkI iERC
  j <- checkI jERC
  let di = natVal aa
  let dj = natVal aaa
  case 0 <= i && i < di && 0 <= j && j < dj of
    True -> pure ()
    False -> error "array2DLookup: index out of bounds"
  checkR $ readSTArray a (i + j*di)
array2DLookup _ _ = error "array2DLookup: supporting only 2-dimensional arrays"

(?!!) = array2DLookup
infix 1 ?!!

arrayUpdate, (.=!) :: (KnownNat n) => ERC s (REALn s n) -> [ERC s INTEGER] -> ERC s REAL -> ERC s ()
arrayUpdate _ [] _ =
  error "arrayUpdate: missing index"
arrayUpdate aERC [iERC] rERC =
  do
  aa@(REALn a) <- aERC
  i <- checkI iERC
  case 0 <= i && i < natVal aa of
    True -> pure ()
    False -> error "arrayUpdate: index out of bounds"
  r <- checkR rERC
  ifInvalidUseDummy () $ writeSTArray a i r
arrayUpdate _ _ _ = error "arrayUpdate: supporting only 1-dimensional arrays"

(.=!) = arrayUpdate
infix 1 .=!


array2DUpdate, (.=!!) :: (KnownNat n, KnownNat m) => ERC s (REALnm s n m) -> [ERC s INTEGER] -> ERC s REAL -> ERC s ()
array2DUpdate _ [] _ =
  error "array2DUpdate: missing index"
array2DUpdate _ [_] _ =
  error "array2DUpdate: missing second index"
array2DUpdate aERC [iERC,jERC] rERC =
  do
  aaa@(REALnm aa@(REALn a)) <- aERC
  i <- checkI iERC
  j <- checkI jERC
  let di = natVal aa
  let dj = natVal aaa
  case 0 <= i && i < di && 0 <= j && j < dj of
    True -> pure ()
    False -> error "array2DUpdate: index out of bounds"
  r <- checkR rERC
  ifInvalidUseDummy () $ writeSTArray a (i + j*di) r
array2DUpdate _ _ _ = error "array2DUpdate: supporting only 2-dimensional arrays"

(.=!!) = array2DUpdate
infix 1 .=!!

checkA :: ERC s (REALn s n) -> ERC s (REALn s n)
checkA aERC = 
  do
  dummyA <- error "accessing a non-existent dummy array"
  ifInvalidUseDummy dummyA aERC

checkAA :: ERC s (REALnm s n m) -> ERC s (REALnm s n m)
checkAA aERC = 
  ifInvalidUseDummy dummyA aERC
  where
  dummyA = error "accessing a non-existent dummy array"
