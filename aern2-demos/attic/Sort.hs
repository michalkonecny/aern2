{-# LANGUAGE Arrows, FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tasks.Demos where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import qualified Data.List as List

import AERN2.Real

-- import Debug.Trace


{-
  Sorting a list of real numbers using merge sort with parallel if.
-}

task_sortR :: Integer -> [R]
task_sortR n = sortR rs
  where
  rs = [real i | i <- [1..n]]
  -- rs = [real (i/!7) | i <- [1..n]]
  -- rs = [real (((i^!2) `mod` m)/!7) | i <- [1..n]]
  -- m = 7+(n `div` 2)

sortR :: [R] -> [R]
sortR rs
  | length rs < 2 = rs
  | otherwise =
    let
      (rsL, rsR) = splitAtMiddle rs
      sL = sortR rsL
      sR = sortR rsR
    in
    merge sL sR

merge :: [R] -> [R] -> [R]
merge [] rsR = rsR
merge rsL [] = rsL
merge l@(rL:rsL) r@(rR:rsR) =
  trace (printf "merge %s %s" (show l) (show r)) $
  map (~!) $
    if rL <= rR
      then rL : merge rsL r
      else rR : merge l rsR

splitAtMiddle :: [a] -> ([a],[a])
splitAtMiddle list =
  let
    middle = (length list) `div` (int 2)
  in
    splitAt middle list
