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
   -- * The type definition and its operations
   , module AERN2.MP.Float.Type
   -- * Infix operators for up/down-rounded operations
   , module AERN2.MP.Float.Operators
   )
where

-- import Numeric.MixedTypes
-- import qualified Prelude as P

import AERN2.MP.Precision
import AERN2.MP.Float.Type
import AERN2.MP.Float.Operators
