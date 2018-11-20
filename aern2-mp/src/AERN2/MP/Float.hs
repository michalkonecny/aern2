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

    Currently, we use hmpfr when compiling with ghc 7.10 and higher
    and haskell-mpfr when compiling with ghc 7.8.
-}

module AERN2.MP.Float
  (
   -- * Precision operations
   module AERN2.MP.Precision
   -- * The type definition and basic operations
#ifdef UseCDAR
   , module AERN2.MP.Float.UseCDAR.Type
   -- * Arithmetic operations
   , module AERN2.MP.Float.UseCDAR.Arithmetic
   , distUp, distDown, avgUp, avgDown
   -- * Conversions, comparisons and norm
   , module AERN2.MP.Float.UseCDAR.Conversions
#else
    , module AERN2.MP.Float.UseRounded.Type
    -- * Arithmetic operations
    , module AERN2.MP.Float.UseRounded.Arithmetic
    , distUp, distDown, avgUp, avgDown
    -- * Conversions, comparisons and norm
    , module AERN2.MP.Float.UseRounded.Conversions
#endif
   -- * Infix operators for up/down-rounded operations
   , module AERN2.MP.Float.Operators
   -- * Constants such as NaN, infinity
   , module AERN2.MP.Float.Constants
   -- * Tests
   , module AERN2.MP.Float.Tests
   )
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import AERN2.MP.Precision

#ifdef UseCDAR
import AERN2.MP.Float.UseCDAR.Type
import AERN2.MP.Float.UseCDAR.Arithmetic
import AERN2.MP.Float.UseCDAR.Conversions
#else
import AERN2.MP.Float.UseRounded.Type
import AERN2.MP.Float.UseRounded.Arithmetic
import AERN2.MP.Float.UseRounded.Conversions
#endif

import AERN2.MP.Float.Operators
import AERN2.MP.Float.Constants
import AERN2.MP.Float.Tests

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
