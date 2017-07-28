{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import System.Environment

-- import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq

-- import qualified Data.List as List

import AERN2.Real

-- import Debug.Trace

sumCos_parallel n =
  sum $
    concat $
      parMap rdeepseq (map cos) $
        chopUp k numbers
  where
  numbers = map (mpBallP (prec 200)) [1..n]
  k = int $ 1 + (n `div` 4)

chopUp k list
  | length list < k = [list]
  | otherwise = take k list : (chopUp k (drop k list))

sumCos_serial n =
  sum $
    concat $
      map (map cos) $
        chopUp k numbers
  where
  numbers = map (mpBallP (prec 200)) [1..n]
  k = int $ 1 + (n `div` 4)

main =
  do
  args <- getArgs -- read command-line args
  case args of -- switch
    [nS, "P"] -> print (sumCos_parallel (read nS))
    [nS, "S"] -> print (sumCos_serial (read nS))
    _ -> pure ()


{- auxiliary -}

instance NFData MPBall where
  rnf b = rnf (getAccuracy b)

instance NFData Accuracy where
  rnf Exact = ()
  rnf NoInformation = ()
  rnf b = rnf $ fromAccuracy b
