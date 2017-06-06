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

import Numeric.MixedTypes
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
        logisticAux [n] = TP.taskLogisticDescription n
        logisticAux _ = error "logistic requires 1 integer parameter \"n\""
        fftAux [k,ac] = taskFFTDescription k ac
        fftAux _ = error "fft requires 2 integer parameters \"k\" and \"ac\""
        dftAux [k,ac] = taskDFTDescription k ac
        dftAux _ = error "dft requires 2 integer parameters \"k\" and \"ac\""
    resultDecription =
        case (benchName, benchParams) of
            ("logistic", [n]) ->
                case implName of
                    "MP_preludeOps" -> show (logistic_MP_preludeOps n)
                    "MP" -> show (logistic_MP n)
                    -- "CR_AC_plain" -> show (logistic_CR_AC_plain n)
                    "CR_AC_cachedUnsafe" -> show (logistic_CR_cachedUnsafe n (bitsSG 100 100))
                    "CR_AC_cachedArrow" -> show (logistic_CR_cachedArrow n (bitsSG 100 100))
--                     "CR_AG_plain" -> show (logistic_CR_AG_plain n)
                    "CR_AG_cachedUnsafe" -> show (logistic_CR_cachedUnsafe n (bitsSG 10 100))
                    "CR_AG_cachedArrow" -> show (logistic_CR_cachedArrow n (bitsSG 10 100))
                    _ -> error $ "unknown implementation: " ++ implName
            ("fft", [k,ac]) -> fourier True k ac
            ("dft", [k,ac]) -> fourier False k ac
            _ -> error ""
        where
        fourier isFFT k ac =
          case implName of
              "Double" -> showL (fft_FP isFFT k)
              "MP" -> showL (case fft_MP isFFT k (bitsSG ac ac) of Just rs -> rs; _ -> error "no result")
              "CR_AC_cachedUnsafe" -> showL (fft_CR_cachedUnsafe isFFT k (bitsSG ac ac))
              "CR_AC_cachedArrow" -> showL (fft_CR_cachedArrow True isFFT k (bitsSG ac ac))
              "CR_AC_noncachedArrow" -> showL (fft_CR_cachedArrow False isFFT k (bitsSG ac ac))
              "CR_AC_parArrow" -> showL (fft_CR_parArrow isFFT k (bitsSG ac ac))
              "CR_AG_cachedUnsafe" -> showL (fft_CR_cachedUnsafe isFFT k (bitsSG acHalf ac))
              "CR_AG_cachedArrow" -> showL (fft_CR_cachedArrow True isFFT k (bitsSG acHalf ac))
              "CR_AG_noncachedArrow" -> showL (fft_CR_cachedArrow False isFFT k (bitsSG acHalf ac))
              "CR_AG_parArrow" -> showL (fft_CR_parArrow isFFT k (bitsSG acHalf ac))
              _ -> error $ "unknown implementation: " ++ implName
          where
          -- n = 2^k
          acHalf = ac `div` 2
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
    | isFFT = taskFFT k
    | otherwise = taskDFT k
  approx :: Complex CauchyReal -> Complex MPBall
  approx (a :+ i) = (a ? acSG) :+ (i ? acSG)

fft_CR_cachedArrow :: Bool -> Bool -> Integer -> AccuracySG -> [Complex MPBall]
fft_CR_cachedArrow shouldCache isFFT k acSG =
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
    executeA $
      proc () ->
        do
        (Just resultRs) <- task -< ()
        mapA approxA -< resultRs
  executeA
    | shouldCache = executeQACachedA
    | otherwise = executeQAUncachedA
  task
    | isFFT = taskFFTWithHookA k hookA
    | otherwise = taskDFTWithHookA k hookA
  hookA name =
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
    | isFFT = taskFFTWithHookA k hookA
    | otherwise = taskDFTWithHookA k hookA
  hookA name =
    proc (a :+ i) ->
      do
      aNext <- (-:-)-< (rename a)
      iNext <- (-:-)-< (rename i)
      returnA -< Just (aNext :+ iNext)
    where
    rename = realRename (\_ -> name)


fft_MP :: Bool -> Integer -> AccuracySG -> Maybe [Complex MPBall]
fft_MP isFFT k _acSG@(AccuracySG acS _) =
    snd $ last $ iterateUntilAccurate acS $ withP
    where
    withP p =
        task ()
        where
        task
          | isFFT = taskFFTWithHookA k (const checkCAccuracy)
          | otherwise = taskDFTWithHookA k (const checkCAccuracy)
        checkCAccuracy (a :+ i) =
          do
          a2 <- checkAccuracy a
          i2 <- checkAccuracy i
          return $ setPrecision p (a2 :+ i2)

fft_FP :: Bool -> Integer -> [Complex Double]
fft_FP True k = taskFFT k
fft_FP False k = taskDFT k
