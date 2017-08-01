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
#ifdef USEMPFR
   module AERN2.MP.UseMPFR.Float
#endif
  )
where

-- import MixedTypesNumPrelude
-- import qualified Prelude as P

#ifdef USEMPFR
import AERN2.MP.UseMPFR.Float
#endif
