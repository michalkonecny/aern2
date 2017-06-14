{-# LANGUAGE DataKinds, Arrows, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
module Main where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
-- import Data.String (fromString)

import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Control.Arrow
import AERN2.Utils.Arrows

import Data.Complex

import AERN2.MP

import AERN2.QA.Protocol
import AERN2.QA.NetLog
import AERN2.QA.Strategy.Cached
import AERN2.QA.Strategy.Parallel

import AERN2.Real

import qualified Tasks.LogisticPreludeOps as TP
import Tasks.Logistic
import Tasks.Fourier

main :: IO ()
main =
    do
    (implName : benchName : benchParamsS) <- getArgs
    let (benchDecription, resultDecription) = bench implName benchName benchParamsS
    putStrLn benchDecription
    putStrLn resultDecription


bench :: String -> String -> [String] -> (String, String)
bench implName benchName benchParamsS =
    (benchDecription, implName ++ ": " ++ resultDecription)
    where
    benchParams :: [Integer]
    benchParams = map read benchParamsS
    benchDecription =
        case benchName of
            "logistic" -> logisticAux benchParams
            "fft" -> fftAux benchParams
            "dft" -> dftAux benchParams
            _ ->
                error $ "unknown benchmark: " ++ benchName
        where
        logisticAux [n,_acS, _acG] = TP.taskLogisticDescription n
        logisticAux _ = error "logistic requires 3 integer parameters \"n\", \"acS\" and \"acG\""
        fftAux [k,acS, acG] = taskFFTDescription k acS acG
        fftAux _ = error "fft requires 3 integer parameters \"k\", \"acS\" and \"acG\""
        dftAux [k,acS, acG] = taskDFTDescription k acS acG
        dftAux _ = error "dft requires 3 integer parameters \"k\", \"acS\" and \"acG\""
    resultDecription =
        case (benchName, benchParams) of
            ("logistic", [n,acS, acG]) ->
                case implName of
                    "MP_preludeOps" -> show (logistic_MP_preludeOps n)
                    "MP" -> show (logistic_MP n)
                    -- "CR_AC_plain" -> show (logistic_CR_AC_plain n)
                    "CR_cachedUnsafe" -> show (logistic_CR_cachedUnsafe n (bitsSG acS acG))
                    "CR_cachedArrow" -> show (logistic_CR_cachedArrow n (bitsSG acS acG))
                    _ -> error $ "unknown implementation: " ++ implName
            ("fft", [k,acS, acG]) -> fourier True k acS acG
            ("dft", [k,acS, acG]) -> fourier False k acS acG
            _ -> error ""
        where
        fourier isFFT k acS acG =
          case implName of
              "Double" -> showL (fft_FP isFFT k)
              "MP" -> showL (case fft_MP isFFT k (bitsSG acS acG) of Just rs -> rs; _ -> error "no result")
              "CR_cachedUnsafe" -> showL (fft_CR_cachedUnsafe isFFT k (bitsSG acS acG))
              "CR_cachedArrow" -> showL (fft_CR_cachedArrow isFFT k (bitsSG acS acG))
              "CR_parArrow" -> showL (fft_CR_parArrow isFFT k (bitsSG acS acG))
              _ -> error $ "unknown implementation: " ++ implName
          where
          -- n = 2^k
          -- acHalf = ac `div` 2
          showL xs = "\n" ++ (unlines $ map show xs)

logistic_CR_cachedUnsafe :: Integer -> AccuracySG -> MPBall
logistic_CR_cachedUnsafe n acSG =
  (taskLogistic n $ real (TP.taskLogistic_x0 :: Rational)) ? acSG

logistic_CR_cachedArrow ::  Integer -> AccuracySG -> MPBall
logistic_CR_cachedArrow n acSG =
  maybeTrace (formatQALog 0 netlog) $
  result
  where
  (netlog, result) =
    executeQACachedA $
      proc () ->
        do
        x0R <- (-:-)-< realA x0
        (Just x) <-taskLogisticWithHookA n hookA -< x0R
        realWithAccuracyA -< (x, acSG)
  x0 = TP.taskLogistic_x0 :: Rational
  hookA i =
    proc r ->
      do
      rNext <- (-:-)-< (rename r)
      returnA -< Just rNext
    where
    rename = realRename (\_ -> "x_" ++ show i)

logistic_MP_preludeOps :: Integer -> Maybe MPBall
logistic_MP_preludeOps n =
    snd $ last $ iterateUntilAccurate (bits (50 :: Integer)) $ withP
    where
    withP p =
        TP.taskLogisticWithHook n checkAccuracy c x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)
        c = mpBallP p (TP.taskLogistic_c :: Rational)

logistic_MP :: Integer -> Maybe MPBall
logistic_MP n =
    snd $ last $ iterateUntilAccurate (bits (50 :: Integer)) $ withP
    where
    withP p =
        (taskLogisticWithHook n (const checkAccuracy)) x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)


checkAccuracy :: MPBall -> Maybe MPBall
checkAccuracy ball
    | getAccuracy ball < (bits 50) = Nothing
    | otherwise = Just ball


fft_CR_cachedUnsafe :: Bool -> Integer -> AccuracySG -> [Complex MPBall]
fft_CR_cachedUnsafe isFFT k acSG =
  map approx $ task
  where
  task
    | isFFT = taskFFT (\r -> r :+ (real 0)) k
    | otherwise = taskDFT (\r -> r :+ (real 0)) k
  approx :: Complex CauchyReal -> Complex MPBall
  approx (a :+ i) = (a ? acSG) :+ (i ? acSG)

fft_CR_cachedArrow :: Bool -> Integer -> AccuracySG -> [Complex MPBall]
fft_CR_cachedArrow isFFT k acSG =
  maybeTrace (formatQALog 0 netlog) $
  results
  where
  approxA =
    proc (aR :+ iR) ->
      do
      a <- (-?-) -< (aR, acSG)
      i <- (-?-) -< (iR, acSG)
      returnA -< a :+ i
  (netlog, results) =
    executeQACachedA $
      proc () ->
        do
        (Just resultRs) <- task -< ()
        mapA approxA -< resultRs
  task
    | isFFT = taskFFTWithHookA hookA k
    | otherwise = taskDFTWithHookA (hookA 0) k
  hookA _ name =
    proc (a :+ i) ->
      do
      aNext <- (-:-)-< (rename a)
      iNext <- (-:-)-< (rename i)
      returnA -< Just (aNext :+ iNext)
    where
    rename = realRename (\_ -> name)

fft_CR_parArrow :: Bool -> Integer -> AccuracySG -> [Complex MPBall]
fft_CR_parArrow isFFT k acSG =
  unsafePerformIO $
    do
    results <-
      executeQAParA $
        proc () ->
          do
          (Just resultRs) <- task -< ()
          promises <- mapA getPromiseComplexA -< resultRs
          mapA fulfilPromiseComplex -< promises
    return results
  where
  getPromiseComplexA =
    proc (aR :+ iR) ->
      do
      aProm <- (-?..-) -< (aR, acSG)
      iProm <- (-?..-) -< (iR, acSG)
      returnA -< aProm :+ iProm
  fulfilPromiseComplex =
    proc (aProm :+ iProm) ->
      do
      a <- qaFulfilPromiseA -< aProm
      i <- qaFulfilPromiseA -< iProm
      returnA -< a :+ i
  task
    | isFFT = taskFFTWithHookA hookA k
    | otherwise = taskDFTWithHookA (hookA 0) k
  n = 2^!k
  hookA nH name =
    proc (a :+ i) ->
      do
      aNext <- reg -< (rename a)
      iNext <- reg -< (rename i)
      returnA -< Just (aNext :+ iNext)
    where
    rename = realRename (\_ -> name)
    reg
      | nH < n = (-:-|)
      | otherwise = (-:-||)


fft_MP :: Bool -> Integer -> AccuracySG -> Maybe [Complex MPBall]
fft_MP isFFT k _acSG@(AccuracySG acS _) =
    snd $ last $ iterateUntilAccurate acS $ withP
    where
    withP p =
        task
        where
        task
          | isFFT = taskFFTWithHook (\ r -> r * c1) checkCAccuracy k
          | otherwise = taskDFTWithHook (\ r -> r * c1) checkCAccuracy k
        c1 = (mpBallP p 1 :+ mpBallP p 0)
        checkCAccuracy (a :+ i) =
          do
          a2 <- checkAccuracy a
          i2 <- checkAccuracy i
          return $ setPrecision p (a2 :+ i2)

fft_FP :: Bool -> Integer -> [Complex Double]
fft_FP True k = taskFFT (\r -> (double r :+ double 0)) k
fft_FP False k = taskDFT (\r -> (double r :+ double 0)) k
