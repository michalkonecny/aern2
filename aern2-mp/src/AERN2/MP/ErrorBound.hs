{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.ErrorBound
    Description :  Fixed precision non-negative up-rounded floating-point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Fixed precision non-negative up-rounded floating-point numbers.
-}

module AERN2.MP.ErrorBound
#ifdef USEMPFR
  (
   module AERN2.MP.UseMPFR.ErrorBound
  )
#endif
where

-- import MixedTypesNumPrelude
-- import qualified Prelude as P

#ifdef USEMPFR
import AERN2.MP.UseMPFR.ErrorBound
#endif
