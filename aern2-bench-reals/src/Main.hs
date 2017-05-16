{-# LANGUAGE DataKinds, Arrows, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
module Main where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import Numeric.MixedTypes
-- import Data.String (fromString)

import System.Environment (getArgs)

import Control.Arrow

import Data.Complex

import AERN2.MP

import AERN2.QA.Protocol
import AERN2.QA.NetLog
import AERN2.QA.Strategy.Cached
import AERN2.Real

import qualified Tasks.PreludeOps as TP
import qualified Tasks.MixedTypesOps as TM

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
            _ ->
                error $ "unknown benchmark: " ++ benchName
        where
        logisticAux [n] = TP.taskLogisticDescription n
        logisticAux _ = error "logistic requires 1 integer parameter \"n\""
        fftAux [k,ac] = TM.taskFFTDescription k ac
        fftAux _ = error "fft requires 2 integer parameters \"k\" and \"ac\""
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
            ("fft", [k,ac]) ->
                case implName of
                    "MP" -> showL (case fft_MP k (bitsSG ac ac) of Just rs -> rs; _ -> error "no result")
                    "CR_AC_cachedUnsafe" -> showL (fft_CR_cachedUnsafe k (bitsSG ac ac))
                    "CR_AC_cachedArrow" -> showL (fft_CR_cachedArrow k (bitsSG ac ac))
                    "CR_AG_cachedUnsafe" -> showL (fft_CR_cachedUnsafe k (bitsSG acHalf ac))
                    "CR_AG_cachedArrow" -> showL (fft_CR_cachedArrow k (bitsSG acHalf ac))
                    _ -> error $ "unknown implementation: " ++ implName
                where
                -- n = 2^k
                acHalf = ac `div` 2
                showL xs = "\n" ++ (unlines $ map show xs)
            _ -> error ""

logistic_CR_cachedUnsafe :: Integer -> AccuracySG -> MPBall
logistic_CR_cachedUnsafe n acSG =
  (TM.taskLogistic n $ real (TP.taskLogistic_x0 :: Rational)) ? acSG

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
        (Just x) <-TM.taskLogisticWithHookA n hookA -< x0R
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
        (TM.taskLogisticWithHook n (const checkAccuracy)) x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)


checkAccuracy :: MPBall -> Maybe MPBall
checkAccuracy ball
    | getAccuracy ball < (bits 50) = Nothing
    | otherwise = Just ball


fft_CR_cachedUnsafe :: Integer -> AccuracySG -> [Complex MPBall]
fft_CR_cachedUnsafe k acSG =
  map approx $ TM.taskFFT k
  where
  approx :: Complex CauchyReal -> Complex MPBall
  approx (a :+ i) = (a ? acSG) :+ (i ? acSG)

fft_CR_cachedArrow :: Integer -> AccuracySG -> [Complex MPBall]
fft_CR_cachedArrow k acSG =
  maybeTrace (formatQALog 0 netlog) $
  results
  where
  approxA =
    proc (aR :+ iR) ->
      do
      a <- qaMakeQueryA -< (aR, acSG)
      i <- qaMakeQueryA -< (iR, acSG)
      returnA -< a :+ i
  (netlog, results) =
    executeQACachedA $
      proc () ->
        do
        (Just resultRs) <-TM.taskFFTWithHookA k hookA -< ()
        mapA approxA -< resultRs
  hookA name =
    proc (a :+ i) ->
      do
      aNext <- (-:-)-< (rename a)
      iNext <- (-:-)-< (rename i)
      returnA -< Just (aNext :+ iNext)
    where
    rename = realRename (\_ -> name)

fft_MP :: Integer -> AccuracySG -> Maybe [Complex MPBall]
fft_MP k _acSG@(AccuracySG acS _) =
    snd $ last $ iterateUntilAccurate acS $ withP
    where
    withP p =
        (TM.taskFFTWithHookA k (const checkCAccuracy)) ()
        where
        checkCAccuracy (a :+ i) =
          do
          a2 <- checkAccuracy a
          i2 <- checkAccuracy i
          return $ setPrecision p (a2 :+ i2)
