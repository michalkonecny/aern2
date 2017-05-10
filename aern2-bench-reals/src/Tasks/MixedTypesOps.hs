{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
module Tasks.MixedTypesOps where

import Numeric.MixedTypes

import Control.Arrow

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
        Just x -> hookA i -< c * x * (1 - x)
        Nothing -> returnA -< Nothing

taskFFTWithHookA ::
  (ConvertibleExactly (Complex CauchyReal) c, CanAddSubMulBy c c
  , QAArrow to, HasIntegers c)
  =>
  Integer -> (c `to` Maybe c) -> () `to` Maybe [c]
taskFFTWithHookA k hookA =
  ditfft2 hookA x n 1
  where
  x = [ convertExactly i | i <- [0..n-1]]
  n = 2^k

ditfft2 ::
  (ConvertibleExactly (Complex CauchyReal) c, CanAddSubMulBy c c
  , QAArrow to)
  =>
  (c `to` Maybe c) -> [c] -> Integer -> Integer -> () `to` Maybe [c]
ditfft2 (hookA :: c `to` Maybe c) xI nI sI = aux xI nI sI
  where
  aux x n s
    | n == 1 =
        proc () ->
          do
          x0m <- hookA -< head x
          returnA -< do { x0 <- x0m; return [x0] }
    | otherwise = convLR
    where
    nHalf = n `div` 2
    convLR :: () `to` Maybe [c]
    convLR =
      proc () ->
        do
        yEven_m <- aux x nHalf (2*s) -< ()
        yOdd_m <- aux (drop (int s) x) nHalf (2*s) -< ()
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
      proc (a,b) -> hookA -< op a b
    twiddleA =
      proc (a,k) ->
        binReg (*) -< (a, tw k n)
  tw :: Integer -> Integer -> c
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
      exp (-2*r*pi*(0 :+ 1))

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
  Just y = ditfft2 Just x n 1 ()
  y' = map (/n) $ head y : (reverse $ tail y)
  Just z = ditfft2 Just y' n 1 ()
