{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-|
    Module      :  AERN2.Real.Limit
    Description :  limits of CReal sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Limits of Cauchy Real sequences.
-}
module AERN2.Real.Limit where

import MixedTypesNumPrelude

-- import qualified Numeric.CollectErrors as CN

-- import Math.NumberTheory.Logarithms (integerLog2)

import AERN2.Real.Type
import AERN2.Limit
import AERN2.MP ( (+-) )

---------
-- limit
---------

instance HasLimits Rational CReal where
  type LimitType Rational CReal = CReal
  limit s = crealFromPrecFunction withPrec
    where
    withPrec p = ((s epsilon) ? p) +- epsilon
      where
      epsilon = 0.5 ^ (integer p)
    
instance HasLimits Integer CReal where
  type LimitType Integer CReal = CReal
  limit s = crealFromPrecFunction withPrec
    where
    withPrec p = ((s (integer p)) ? p) +- epsilon
      where
      epsilon = 0.5 ^ (integer p)

instance HasLimits Int CReal where
  type LimitType Int CReal = CReal
  limit s = limit (s . c)
    where
    c :: Integer -> Int
    c = int

instance HasLimits Rational (CReal -> CReal) where
  type LimitType Rational (CReal -> CReal) = (CReal -> CReal)
  limit fs x = crealFromPrecFunction withPrec
    where
    withPrec p = ((fs epsilon x) ? p) +- epsilon
      where
      epsilon = 0.5 ^ (integer p)
