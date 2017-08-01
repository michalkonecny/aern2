{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Dyadic
    Description :  Dyadics with exact ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision floating-point numbers with exact ring operations.
-}

module AERN2.MP.Dyadic
  (
#ifdef USEMPFR
   module AERN2.MP.UseMPFR.Dyadic
#endif
  )
where

-- import MixedTypesNumPrelude
-- import qualified Prelude as P

#ifdef USEMPFR
import AERN2.MP.UseMPFR.Dyadic
#endif
