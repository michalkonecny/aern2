{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
module BenchTasks.Fourier where

import MixedTypesNumPrelude
import Text.Printf

import Control.Arrow
import AERN2.Utils.Arrows

import qualified Data.Map as Map
import qualified Data.Sequence as SQ
import GHC.Exts (toList)

import Data.Complex
import Data.Monoid

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
  r = ditfft2 cr2c n x
  x = [ convertExactly i | i <- [1..n]]
  n = 2^!k

taskFFTWithHook ::
  (FFTOps c, HasIntegers c)
  =>
  (CauchyReal -> c) ->
  (c -> Maybe c) -> Integer -> Maybe [c]
taskFFTWithHook cr2c hook k =
  ditfft2Hook cr2c hook n x
  where
  x = [ convertExactly i | i <- [1..n]]
  n = 2^!k

taskFFTA ::
  (FFTOpsA r c, QAArrow to, HasIntegers (QA to p)
  , c ~ Complex (QA to p), QAProtocolCacheable p, ConvertibleExactly CauchyReal (QA to p))
  =>
  Integer -> () `to` [c]
taskFFTA k =
  proc () ->
    do
    xR <- mapA (regComplex False) -< x
    ditfft2A n -< xR
  where
  x = [ complexGiveName ("x" ++ show i) $ convertExactly i | i <- [1..n]]
  n = 2^!k

-- taskFFTWithHookA ::
--   (FFTOpsA r c, QAArrow to, HasIntegers c)
--   =>
--   (Integer -> String -> c `to` Maybe c) ->
--   Integer -> () `to` Maybe [c]
-- taskFFTWithHookA hookA k =
--   proc () ->
--     do
--     mxR <- mapWithIndexA reg -< x
--     let Just xR = sequence mxR
--     ditfft2AHook hookA n -< xR
--   where
--   reg i = hookA 0 ("x" ++ show i)
--   x = [ convertExactly i | i <- [1..n]]
--   n = 2^!k

type FFTOpsA r c =
  (CanAddSubMulBy r r, c ~ Complex r)

type FFTOps c =
  (CanMulBy c Rational, CanMulBy c (Complex Integer)
  , CanAddSubMulBy c c, CanNegSameType c, CanExpSameType c)


{-| Cooley-Tukey FFT closely following
    https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Pseudocode

    A vanilla version using Data.Sequence.
-}
ditfft2 ::
  (FFTOps c)
  =>
  (CauchyReal -> c) ->
  Integer -> [c] -> [c]
ditfft2 cr2c nI x =
  toList $ aux 0 nI 1
  where
  xSQ = SQ.fromList x
  aux i n s
    | n == 1 = SQ.singleton $ xSQ `SQ.index` (int i)
    | otherwise =
      let
        nHalf = n `divINoCN` 2
        yEven = aux i nHalf (2*s)
        yOdd = aux (i+s) nHalf (2*s)
        yOddTw = SQ.zipWith (*) yOdd $ SQ.fromList [tw k n | k <- [0..nHalf-1]]
        yL = SQ.zipWith (+) yEven yOddTw
        yR = SQ.zipWith (-) yEven yOddTw
      in
      (yL <> yR)
  tw k n =
    case Map.lookup (k%n) twsNI of -- memoisation
      Just v -> v
      _ -> error "ditfft2Hook: tw: internal error"
  twsNI = tws cr2c nI nI

{-| Cooley-Tukey FFT closely following
    https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Pseudocode

    A version with a hook that allows one to stop the computation at any point
    and propagate Nothing.
-}
ditfft2Hook ::
  (FFTOps c)
  =>
  (CauchyReal -> c) ->
  (c -> Maybe c) ->
  Integer -> [c] -> Maybe [c]
ditfft2Hook cr2c (hook :: c -> Maybe c) nI x =
  fmap toList $ aux 0 nI 1
  where
  xSQ = SQ.fromList x
  aux i n s
    | n == 1 = Just $ SQ.singleton $ xSQ `SQ.index` (int i)
    | otherwise = convLR
    where
    nHalf = n `divINoCN` 2
    convLR =
      do
      yEven <- aux i nHalf (2*s)
      yOdd <- aux (i+s) nHalf (2*s)
      yOddTw <- hookOnList $ SQ.zipWith (*) yOdd $ SQ.fromList [tw k n | k <- [0..nHalf-1]]
      let yL = SQ.zipWith (+) yEven yOddTw
      let yR = SQ.zipWith (-) yEven yOddTw
      Just (yL <> yR)
  hookOnList :: SQ.Seq c -> Maybe (SQ.Seq c)
  hookOnList list = sequence (fmap hook list)
  tw k n =
    case Map.lookup (k%n) twsNI of -- memoisation
      Just v -> v
      _ -> error "ditfft2Hook: tw: internal error"
  twsNI = tws cr2c nI nI

{-| Cooley-Tukey FFT closely following
    https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Pseudocode
-}
ditfft2A ::
  (FFTOpsA r c, QAArrow to
  , c ~ Complex (QA to p), QAProtocolCacheable p, ConvertibleExactly CauchyReal (QA to p))
  =>
  Integer -> [c] `to` [c]
ditfft2A nI = aux 0 nI 1
  where
  aux i n s
    | n == 1 = arr (\ x -> [x !! i])
    | otherwise = convLR
    where
    nHalf = n `divINoCN` 2
    isParallel = (nI == n)
    convLR =
      proc x -> do
        yEven <- aux i nHalf (2*s) -< x
        yOdd <- aux (i+s) nHalf (2*s) -< x
        yOddTw <- mapWithIndexA twiddleA -< yOdd
        let yL = map (complexGiveName "+ node") (zipWith (+) yEven yOddTw)
        let yR = map (complexGiveName "- node") (zipWith (-) yEven yOddTw)
        mapA (regComplex isParallel) -< yL ++ yR
    twiddleA k =
      proc (a :: c) ->
        regComplex isParallel -< complexGiveName opName (a * (tw k n :: c))
      where
      opName = printf "*(tw %d/%d) node" k n
  tw k n =
    case Map.lookup (k%n) twsNI of -- memoisation
      Just v -> convertExactly v
      _ -> error "ditfft2A: tw: internal error"
  twsNI = twsCR nI nI

regComplex ::
  (QAArrow to, QAProtocolCacheable p) =>
  Bool -> (Complex (QA to p)) `to` (Complex (QA to p))
regComplex isParallel
  | isParallel =
    proc (a:+b) -> do
      a' <- (-:-||) -< a
      b' <- (-:-||) -< b
      returnA -< (a':+b')
  | otherwise =
    proc (a:+b) -> do
      a' <- (-:-) -< a
      b' <- (-:-) -< b
      returnA -< (a':+b')

complexGiveName :: String -> Complex (QA to p) -> Complex (QA to p)
complexGiveName name (a:+b) = (a':+b')
  where
  a' = qaRename (\_ -> name ++ " (R)") a
  b' = qaRename (\_ -> name ++ " (I)") b

-- {-| Cooley-Tukey FFT closely following
--     https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Pseudocode
-- -}
-- ditfft2AHook ::
--   (FFTOpsA r c, QAArrow to)
--   =>
--   (Integer -> String -> c `to` Maybe c) ->
--   Integer -> [c] `to` Maybe [c]
-- ditfft2AHook (hookA :: Integer -> String -> c `to` Maybe c) nI = aux 0 nI 1
--   where
--   aux i n s
--     | n == 1 =
--         proc x -> returnA -< Just [x !! i]
--     | otherwise = convLR
--     where
--     nHalf = n `div` 2
--     -- nodeName = printf "node(%d,%d,%d)" i n s
--     convLR :: [c] `to` Maybe [c]
--     convLR =
--       proc x ->
--         do
--         yEven_m <- aux i nHalf (2*s) -< x
--         yOdd_m <- aux (i+s) nHalf (2*s) -< x
--         case (yEven_m, yOdd_m) of
--           (Just yEven, Just yOdd) ->
--             do
--             yOddTw_ms <- mapWithIndexA twiddleA -< yOdd
--             case sequence yOddTw_ms of
--               Just yOddTw ->
--                 do
--                 yLm <- mapWithIndexA (binHook "+" (+)) -< zip yEven yOddTw
--                 yRm <- mapWithIndexA (binHook "-" (-)) -< zip yEven yOddTw
--                 returnA -< do { yL <- sequence yLm; yR <- sequence yRm; return (yL ++ yR) }
--               _ -> returnA -< Nothing
--           _ -> returnA -< Nothing
--     binHook :: String -> (t1 -> t2 -> c) -> (Integer -> (t1,t2) `to` Maybe c)
--     binHook opName op _k =
--       proc (a,b) -> hookA n nodeName -< op a b
--       where
--       nodeName =
--         opName
--         -- printf "%s(i=%d,n=%d,s=%d,k=%d)" opName i n s k
--     twiddleA k =
--       proc a -> binHook opName (*) k -< (a, tw k n)
--       where
--       opName = printf "*(tw %d/%d)" k n
--   tw :: Integer -> Integer -> (Complex (CauchyRealA to))
--   tw k n =
--     case Map.lookup (k%n) twsNI of -- memoisation
--       Just v -> convertExactly v
--       _ -> error "ditfft2AHook: tw: internal error"
--   twsNI = twsCR nI nI

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

-- taskDFTWithHookA ::
--   (FFTOpsA r c, QAArrow to, HasIntegers c)
--   =>
--   (String -> c `to` Maybe c) -> Integer -> () `to` Maybe [c]
-- taskDFTWithHookA hookA k =
--   proc () ->
--     do
--     mxR <- mapWithIndexA reg -< x
--     let Just xR = sequence mxR
--     dftA hookA -< xR
--   where
--   reg i = hookA $ "x" ++ show i
--   x = [ convertExactly i | i <- [1..n]]
--   n = 2^!k

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
  (FFTOpsA r c, QAArrow to, HasIntegers c, CanAddSubMulBy r (CauchyRealA to))
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


-- _testFFT :: Integer -> IO ()
-- _testFFT k =
--   do
--   putStrLn "x = "
--   sequence_ $ map print x
--   putStrLn "z = "
--   sequence_ $ map print z
--   where
--   n = 2^!k
--   x = [complex i | i <- [1..n]]
--   y = ditfft2A n x
--   y' = map (/n) $ head y : (reverse $ tail y)
--   z = ditfft2A n y'
