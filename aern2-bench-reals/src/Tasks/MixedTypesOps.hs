{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
module Tasks.MixedTypesOps where

import Numeric.MixedTypes

import Control.Arrow

import Text.Printf

import qualified Data.Map as Map

import Data.Complex

import AERN2.QA
import AERN2.Real

import Tasks.PreludeOps (taskLogistic_c)

type HasLogisticOps r =
  (CanMulSameType r,
   CanSub Integer r, SubType Integer r ~ r,
   CanMulBy r Rational)

taskLogistic :: (HasLogisticOps r) => Integer -> r -> r
taskLogistic n x = r
  where
  (Just r) = taskLogisticWithHook n (const Just) x

taskLogisticWithHook ::
  (HasLogisticOps r)
  =>
  Integer -> (Integer -> r -> Maybe r) -> r -> Maybe r
taskLogisticWithHook = taskLogisticWithHookA

taskLogisticWithHookA ::
  (ArrowChoice to, HasLogisticOps r)
  =>
  Integer -> (Integer -> r `to` Maybe r) -> (r `to` Maybe r)
taskLogisticWithHookA n hookA =
  proc r ->
    logisticWithHookA hookA taskLogistic_c n -< (Just r)

logisticWithHook ::
  (HasLogisticOps r)
  =>
  (Integer -> r -> Maybe r) -> Rational -> Integer -> Maybe r -> Maybe r
logisticWithHook = logisticWithHookA

logisticWithHookA ::
  (ArrowChoice to, HasLogisticOps r)
  =>
  (Integer -> r `to` Maybe r) -> Rational -> Integer -> (Maybe r `to` Maybe r)
logisticWithHookA hookA c n =
    foldl1 (<<<) (take (int n) (map step [1..]))
    where
    step i = proc mx ->
      do
      case mx of
        Just x ->
          hookA i -< c*x*(1-x)
        Nothing ->
          returnA -< Nothing


{-
logisticA ::
  (ArrowChoice to, HasLogisticOps r)
  =>
  Rational -> Integer -> (r `to` r)
logisticA c n =
    foldl1 (<<<) (take (int n) (repeat l))
    where
    l = arr (\x -> (c * x * (1 - x)))

executeLogistic c n =
  irramEval (logistic c n)

irramEval :: (MPBall -> MPBall) -> (CR -> CR)
irramEval f rIn = rOut
  where
  rOut = newCR ... getAnswer
  getAnswer ac =
    ...
    where

logistic :: (HasLogisticOps t) => Rational -> Integer -> t -> t
logistic c n x0 =
  foldl1 (.) (take (int n) (repeat l)) x0
  where
  l x = c * x * (1-x)
-}
{- FFT -}

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
  ditfft2 hookA x n 1
  where
  x = [ convertExactly i | i <- [0..n-1]]
  n = 2^k

type FFTOpsA to c = (CanAddSubMulBy c c, CanMulBy c (Complex (CauchyRealA to)))

ditfft2 ::
  (FFTOpsA to c, QAArrow to)
  =>
  (String -> c `to` Maybe c) -> [c] -> Integer -> Integer -> () `to` Maybe [c]
ditfft2 (hookA :: String -> c `to` Maybe c) x nI sI = aux 0 nI sI
  where
  aux i n s
    | n == 1 =
        proc () ->
          do
          x0m <- hookA nodeName -< x !! i
          returnA -< do { x0 <- x0m; return [x0] }
    | otherwise = convLR
    where
    nHalf = n `div` 2
    nodeName = printf "node(%d,%d,%d)" i n s
    convLR :: () `to` Maybe [c]
    convLR =
      proc () ->
        do
        yEven_m <- aux i nHalf (2*s) -< ()
        yOdd_m <- aux (i+s) nHalf (2*s) -< ()
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
    case Map.lookup (k/n) tws of -- memoisation
      Just v -> convertExactly v
      _ -> error "ditfft2: tw: internal error"
  tws :: Map.Map Rational (Complex CauchyReal)
  tws = foldl insertTw Map.empty [k/nI | k <- [0..nI-1]]
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
  Just y = ditfft2 (\_ l -> Just l) x n 1 ()
  y' = map (/n) $ head y : (reverse $ tail y)
  Just z = ditfft2 (\_ l -> Just l) y' n 1 ()
