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
   module Precision
   -- * Helper structure
   , module Aux
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

import AERN2.MP.Precision as Precision
import AERN2.MP.Float.Aux as Aux

import AERN2.MP.Float.Type as Type
import AERN2.MP.Float.Arithmetic as Arithmetic
import AERN2.MP.Float.Conversions as Conversions

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
