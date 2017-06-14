{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
module Tasks.Fourier where

import MixedTypesNumPrelude
import Text.Printf

import Control.Arrow
import AERN2.Utils.Arrows

import qualified Data.Map as Map

import Data.Complex

import AERN2.QA.Protocol
import AERN2.Real

taskFFTDescription :: Integer -> Integer -> Integer -> String
taskFFTDescription k acS acG =
  printf "taskFFT: FFT on a vector of size %s with strict accuracy %s and guide accuracy %s"
    (show (2^k)) (show acS) (show acG)

taskFFT ::
  (FFTOps c, HasIntegers c)
  =>
  (CauchyReal -> c) ->
  Integer -> [c]
taskFFT cr2c k = r
  where
  (Just r) = taskFFTWithHook cr2c Just k

taskFFTWithHook ::
  (FFTOps c, HasIntegers c)
  =>
  (CauchyReal -> c) ->
  (c -> Maybe c) -> Integer -> Maybe [c]
taskFFTWithHook cr2c hook k =
  ditfft2 cr2c hook n x
  where
  x = [ convertExactly i | i <- [1..n]]
  n = 2^!k

taskFFTWithHookA ::
  (FFTOpsA to c, QAArrow to, HasIntegers c)
  =>
  (Integer -> String -> c `to` Maybe c) ->
  Integer -> () `to` Maybe [c]
taskFFTWithHookA hookA k =
  proc () ->
    do
    mxR <- mapWithIndexA reg -< x
    let Just xR = sequence mxR
    ditfft2A hookA n -< xR
  where
  reg i = hookA 0 ("x" ++ show i)
  x = [ convertExactly i | i <- [1..n]]
  n = 2^!k

type FFTOpsA to c =
  (CanAddSubMulBy c c, CanMulBy c (Complex (CauchyRealA to)))

type FFTOps c =
  (CanMulBy c Rational, CanMulBy c (Complex Integer)
  , CanAddSubMulBy c c, CanNegSameType c, CanExpSameType c)


{-| Cooley-Tukey FFT closely following
    https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Pseudocode
-}
ditfft2 ::
  (FFTOps c)
  =>
  (CauchyReal -> c) ->
  (c -> Maybe c) ->
  Integer -> [c] -> Maybe [c]
ditfft2 cr2c (hook :: c -> Maybe c) nI x = aux 0 nI 1
  where
  aux i n s
    | n == 1 = Just [x !! i]
    | otherwise = convLR
    where
    convLR =
      do
      yEven <- aux i (n `div` 2) (2*s)
      yOdd <- aux (i+s) (n `div` 2) (2*s)
      yOddTw <- hookOnList $ zipWith (*) yOdd [tw k n | k <- [0..]]
      let yL = zipWith (+) yEven yOddTw
      let yR = zipWith (-) yEven yOddTw
      Just (yL ++ yR)
  hookOnList :: [c] -> Maybe [c]
  hookOnList list = sequence (map hook list)
  tw k n =
    case Map.lookup (k%n) twsNI of -- memoisation
      Just v -> v
      _ -> error "ditfft2: tw: internal error"
  twsNI = tws cr2c nI nI

{-| Cooley-Tukey FFT closely following
    https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Pseudocode
-}
ditfft2A ::
  (FFTOpsA to c, QAArrow to)
  =>
  (Integer -> String -> c `to` Maybe c) ->
  Integer -> [c] `to` Maybe [c]
ditfft2A (hookA :: Integer -> String -> c `to` Maybe c) nI = aux 0 nI 1
  where
  aux i n s
    | n == 1 =
        proc x -> returnA -< Just [x !! i]
    | otherwise = convLR
    where
    nHalf = n `div` 2
    -- nodeName = printf "node(%d,%d,%d)" i n s
    convLR :: [c] `to` Maybe [c]
    convLR =
      proc x ->
        do
        yEven_m <- aux i nHalf (2*s) -< x
        yOdd_m <- aux (i+s) nHalf (2*s) -< x
        case (yEven_m, yOdd_m) of
          (Just yEven, Just yOdd) ->
            do
            yOddTw_ms <- mapA twiddleA -< zip yOdd [0..]
            case sequence yOddTw_ms of
              Just yOddTw ->
                do
                yLm <- mapA (binReg (+)) -< zip yEven yOddTw
                yRm <- mapA (binReg (-)) -< zip yEven yOddTw
                returnA -< do { yL <- sequence yLm; yR <- sequence yRm; return (yL ++ yR) }
              _ -> returnA -< Nothing
          _ -> returnA -< Nothing
    binReg :: (t1 -> t2 -> c) -> ((t1,t2) `to` Maybe c)
    binReg op =
      proc (a,b) -> hookA n "node?" -< op a b
    twiddleA =
      proc (a,k) ->
        binReg (*) -< (a, tw k n)
  tw :: Integer -> Integer -> (Complex (CauchyRealA to))
  tw k n =
    case Map.lookup (k%n) twsNI of -- memoisation
      Just v -> convertExactly v
      _ -> error "ditfft2A: tw: internal error"
  twsNI = twsCR nI nI

twsCR :: Integer -> Integer -> Map.Map Rational (Complex CauchyReal)
twsCR n nN = foldl insertTw Map.empty [ k%nN | k <- [0..n]]
  where
  insertTw twsPrev r =
    case Map.lookup r twsPrev of
      Nothing -> Map.insert r (twInternal r) twsPrev
      _ -> twsPrev
  twInternal :: Rational -> Complex CauchyReal
  twInternal r =
    exp (-2*r*pi*complex_i)
    where
    complex_i = 0 :+ 1

tws ::
  (FFTOps c)
  =>
  (CauchyReal -> c) ->
  Integer -> Integer -> Map.Map Rational c
tws cr2a n nN =
  foldl insertTw Map.empty [ k % nN | k <- [0..n]]
  where
  insertTw twsPrev r =
    case Map.lookup r twsPrev of
      Nothing -> Map.insert r (twInternal r) twsPrev
      _ -> twsPrev
  twInternal r =
    exp (-2*r*(cr2a pi)*i)
    where
    i = 0 :+ 1


taskDFTDescription :: Integer -> Integer -> Integer -> String
taskDFTDescription k acS acG =
  printf "taskDFT: DFT on a vector of size %s with strict accuracy %s and guide accuracy %s"
    (show (2^k)) (show acS) (show acG)

taskDFT ::
  (FFTOps c, HasIntegers c)
  =>
  (CauchyReal -> c) ->
  Integer -> [c]
taskDFT cr2c k = r
  where
  (Just r) = taskDFTWithHook cr2c Just k

taskDFTWithHook ::
  (FFTOps c, HasIntegers c)
  =>
  (CauchyReal -> c) ->
  (c -> Maybe c) -> Integer -> Maybe [c]
taskDFTWithHook cr2c hook k =
  dft cr2c hook x
  where
  x = [ convertExactly i | i <- [1..n]]
  n = 2^!k

taskDFTWithHookA ::
  (FFTOpsA to c, QAArrow to, HasIntegers c)
  =>
  (String -> c `to` Maybe c) -> Integer -> () `to` Maybe [c]
taskDFTWithHookA hookA k =
  proc () ->
    do
    mxR <- mapWithIndexA reg -< x
    let Just xR = sequence mxR
    dftA hookA -< xR
  where
  reg i = hookA $ "x" ++ show i
  x = [ convertExactly i | i <- [1..n]]
  n = 2^!k

dft ::
  (FFTOps c, HasIntegers c)
  =>
  (CauchyReal -> c) ->
  (c -> Maybe c) -> [c] -> Maybe [c]
dft cr2c hook x =
  sequence $ map hook [(sumForK k) | k <- [0..nN-1]]
  where
  nN = integer (length x)
  sumForK k =
    sum $ map tw (zip [0..] x)
    where
    tw (n,xn) =
    --   xn * (exp (-2*((n*k) % nN)*(cr2c pi)*(0:+1)))
      case Map.lookup ((n*k) % nN) twsNN of -- memoisation
        Just v -> v * xn
        _ -> error "dft: tw: internal error"
    twsNN = tws cr2c ((nN-1)*(nN-1)) nN


dftA ::
  (FFTOpsA to c, QAArrow to, HasIntegers c)
  =>
  (String -> c `to` Maybe c) -> [c] `to` Maybe [c]
dftA (hookA :: String -> c `to` Maybe c) =
  proc x ->
    do
    mrX <- mapWithIndexA reg -< [sumForK k x | k <- [0..(length x)-1]]
    returnA -< sequence mrX
  where
  reg k = hookA $ "X" ++ show k
  sumForK k x =
    sum $ map tw (zip [0..] x)
    where
    nN = integer (length x)
    tw :: (Integer, c) -> c
    tw (n,xn) =
      case Map.lookup ((n*k) % nN) twsNN of -- memoisation
        Just v -> (convertExactly v :: Complex (CauchyRealA to)) * xn
        _ -> error "dft: tw: internal error"
    twsNN = twsCR ((nN-1)*(nN-1)) nN


_testFFT :: Integer -> IO ()
_testFFT k =
  do
  putStrLn "x = "
  sequence_ $ map print x
  putStrLn "z = "
  sequence_ $ map print z
  where
  n = 2^!k
  x = [complex i | i <- [1..n]]
  Just y = ditfft2A (\_ _ l -> Just l) n x
  y' = map (/n) $ head y : (reverse $ tail y)
  Just z = ditfft2A (\_ _ l -> Just l) n y'
