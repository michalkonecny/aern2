{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Float
    Description :  Arbitrary precision floating point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers with up/down-rounded operations.
-}

module AERN2.MP.Float
  (
   -- * Precision operations
   module AERN2.MP.Precision
   -- * Helper structure
   , BoundsCEDU(..)
   -- * The type definition and basic operations
   , module Type
   -- * Arithmetic operations
   , module Arithmetic
   , distUp, distDown, avgUp, avgDown
   -- * Conversions, comparisons and norm, constants such as NaN, infinity
   , module Conversions
   -- * Infix operators for up/down-rounded operations
   , module Operators
   -- * Tests
   , module Tests
   )
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import AERN2.MP.Precision
import AERN2.MP.Float.BoundsCEDU

#ifdef USE_CDAR

import AERN2.MP.Float.UseCDAR.Type as Type
import AERN2.MP.Float.UseCDAR.Arithmetic as Arithmetic
import AERN2.MP.Float.UseCDAR.Conversions as Conversions

#else

import AERN2.MP.Float.UseRounded.Type as Type
import AERN2.MP.Float.UseRounded.Arithmetic as Arithmetic
import AERN2.MP.Float.UseRounded.Conversions as Conversions

#endif

import AERN2.MP.Float.Operators as Operators
import AERN2.MP.Float.Tests as Tests

-- | Computes an upper bound to the distance @|x - y|@ of @x@ and @y@.
distUp :: MPFloat -> MPFloat -> MPFloat
distUp x y = if x >= y then x -^ y else y -^ x

-- | Computes a lower bound to the distance @|x - y|@ of @x@ and @y@.
distDown :: MPFloat -> MPFloat -> MPFloat
distDown x y = if x >= y then x -. y else y -. x

avgUp :: MPFloat -> MPFloat -> MPFloat
avgUp x y = (x +^ y) /^ (mpFloat 2)

avgDown :: MPFloat -> MPFloat -> MPFloat
avgDown x y = (x +. y) /. (mpFloat 2)
