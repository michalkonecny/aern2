{-# LANGUAGE DataKinds, Arrows #-}
module Main where

import Numeric.MixedTypes
-- import Data.String (fromString)

import System.Environment (getArgs)

import Control.Arrow

import qualified AERN2.MP as MPBall
    (
      -- CauchyReal, cauchyReal,
     bits, getAccuracy,
     iterateUntilAccurate)
import AERN2.MP (MPBall, mpBallP)

import AERN2.Real
import AERN2.QA

import qualified Tasks.PreludeOps as TP
import qualified Tasks.MixedTypesOps as TM

main :: IO ()
main =
    do
    [benchArg, implArg] <- getArgs
    let (resultDecription, benchDecription) = bench benchArg implArg
    putStrLn benchDecription
    putStrLn resultDecription


bench :: String -> String -> (String, String)
bench benchArg implArg =
    (implArg ++ ": " ++ resultDecription, benchDecription)
    where
    (benchName, benchParams, benchDecription) =
        case benchArg of
            "logistic0" -> logisticAux 100
            "logistic1" -> logisticAux 1000
            "logistic2" -> logisticAux 10000
            "logistic3" -> logisticAux 100000
            _ ->
                error $ "unknown benchmark: " ++ benchArg
        where
        logisticAux n = ("logistic"  :: String, [n],  TP.taskLogisticDescription n)
    resultDecription =
        case (benchName, benchParams) of
            ("logistic", [n]) ->
                case implArg of
                    "MP_preludeOps" -> show (logistic_MP_preludeOps n)
                    "MP" -> show (logistic_MP n)
                    -- "CR_AC_plain" -> show (logistic_CR_AC_plain n)
                    "CR_AC_cachedUnsafe" -> show (logistic_CR_AC_cachedUnsafe n)
                    "CR_AC_cachedArrow" -> show (logistic_CR_AC_cachedArrow n)
--                     "CR_AG_plain" -> show (logistic_CR_AG_plain n)
--                     "CR_AG_cachedUnsafe" -> show (logistic_CR_AG_cachedUnsafe n)
--                     "CR_AG_cachedArrow" -> show (logistic_CR_AG_cachedArrow n)
                    _ -> error $ "unknown implementation: " ++ implArg
            _ -> error ""

logistic_CR_AC_cachedUnsafe :: Integer -> CauchyReal
logistic_CR_AC_cachedUnsafe n =
  TM.taskLogistic n $ real (TP.taskLogistic_x0 :: Rational)

logistic_CR_AC_cachedArrow :: Integer -> MPBall
logistic_CR_AC_cachedArrow n =
  snd $ executeQACachedA $
    proc () ->
      do
      x0R <- (-:-)-< realA x0
      (Just x) <-TM.taskLogisticWithHookA n hookA -< x0R
      realWithAccuracyA -< (x, bitsSG 100)
  where
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
    snd $ last $ MPBall.iterateUntilAccurate (MPBall.bits (50 :: Integer)) $ withP
    where
    withP p =
        TP.taskLogisticWithHook n checkAccuracy c x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)
        c = mpBallP p (TP.taskLogistic_c :: Rational)

logistic_MP :: Integer -> Maybe MPBall
logistic_MP n =
    snd $ last $ MPBall.iterateUntilAccurate (MPBall.bits (50 :: Integer)) $ withP
    where
    withP p =
        (TM.taskLogisticWithHook n (const checkAccuracy)) x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)


checkAccuracy :: MPBall -> Maybe MPBall
checkAccuracy ball
    | MPBall.getAccuracy ball < (MPBall.bits 50) = Nothing
    | otherwise = Just ball
