{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
module Tasks.Fourier where

import Numeric.MixedTypes
-- import Text.Printf

import Control.Arrow
import AERN2.Utils.Arrows

import qualified Data.Map as Map

import Data.Complex

import AERN2.QA.Protocol
import AERN2.Real

taskFFTDescription :: Integer -> Integer -> String
taskFFTDescription k ac = "taskFFT: FFT on a vector of size " ++ show (2^k) ++ " to accuracy " ++ show ac

taskFFT ::
  (FFTOpsA (->) c, HasIntegers c)
  =>
  Integer -> [c]
taskFFT k = r
  where
  (Just r) = taskFFTWithHook k (\ _s l -> Just l)

taskFFTWithHook ::
  (FFTOpsA (->) c, HasIntegers c)
  =>
  Integer -> (String -> c -> Maybe c) -> Maybe [c]
taskFFTWithHook k hook = taskFFTWithHookA k hook ()

taskFFTWithHookA ::
  (FFTOpsA to c, QAArrow to, HasIntegers c)
  =>
  Integer -> (String -> c `to` Maybe c) -> () `to` Maybe [c]
taskFFTWithHookA k hookA =
  proc () ->
    do
    mxR <- mapWithIndexA reg -< x
    let Just xR = sequence mxR
    ditfft2 hookA n 1 -< xR
  where
  reg i = hookA $ "x" ++ show i
  x = [ convertExactly i | i <- [1..n]]
  n = 2^k

type FFTOpsA to c = (CanAddSubMulBy c c, CanMulBy c (Complex (CauchyRealA to)))

{-| Cooley-Tukey FFT closely following
    https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Pseudocode
-}
ditfft2 ::
  (FFTOpsA to c, QAArrow to)
  =>
  (String -> c `to` Maybe c) -> Integer -> Integer -> [c] `to` Maybe [c]
ditfft2 (hookA :: String -> c `to` Maybe c) nI sI = aux 0 nI sI
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
      proc (a,b) -> hookA "node?" -< op a b
    twiddleA =
      proc (a,k) ->
        binReg (*) -< (a, tw k n)
  tw :: Integer -> Integer -> (Complex (CauchyRealA to))
  tw k n =
    case Map.lookup (k/n) twsNI of -- memoisation
      Just v -> convertExactly v
      _ -> error "ditfft2: tw: internal error"
  twsNI = tws nI nI

tws :: Integer -> Integer -> Map.Map Rational (Complex CauchyReal)
tws n nN = foldl insertTw Map.empty [k/nN | k <- [0..n]]
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

_testFFT :: Integer -> IO ()
_testFFT k =
  do
  putStrLn "x = "
  sequence_ $ map print x
  putStrLn "z = "
  sequence_ $ map print z
  where
  n = 2^k
  x = [complex i | i <- [1..n]]
  Just y = ditfft2 (\_ l -> Just l) n 1 x
  y' = map (/n) $ head y : (reverse $ tail y)
  Just z = ditfft2 (\_ l -> Just l) n 1 y'


taskDFTDescription :: Integer -> Integer -> String
taskDFTDescription k ac = "taskDFT: DFT on a vector of size " ++ show (2^k) ++ " to accuracy " ++ show ac

taskDFT ::
  (FFTOpsA (->) c, HasIntegers c)
  =>
  Integer -> [c]
taskDFT k = r
  where
  (Just r) = taskFFTWithHook k (\ _s l -> Just l)

taskDFTWithHook ::
  (FFTOpsA (->) c, HasIntegers c)
  =>
  Integer -> (String -> c -> Maybe c) -> Maybe [c]
taskDFTWithHook k hook = taskDFTWithHookA k hook ()

taskDFTWithHookA ::
  (FFTOpsA to c, QAArrow to, HasIntegers c)
  =>
  Integer -> (String -> c `to` Maybe c) -> () `to` Maybe [c]
taskDFTWithHookA k hookA =
  proc () ->
    do
    mxR <- mapWithIndexA reg -< x
    let Just xR = sequence mxR
    dft hookA -< xR
  where
  reg i = hookA $ "x" ++ show i
  x = [ convertExactly i | i <- [1..n]]
  n = 2^k

dft ::
  (FFTOpsA to c, QAArrow to, HasIntegers c)
  =>
  (String -> c `to` Maybe c) -> [c] `to` Maybe [c]
dft (hookA :: String -> c `to` Maybe c) =
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
      case Map.lookup (n*k/nN) twsNN of -- memoisation
        Just v -> (convertExactly v :: Complex (CauchyRealA to)) * xn
        _ -> error "dft: tw: internal error"
    twsNN = tws ((nN-1)*(nN-1)) nN
