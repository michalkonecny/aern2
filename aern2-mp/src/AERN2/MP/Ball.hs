{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Ball
    Description :  Arbitrary precision ball arithmetic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Arbitrary precision ball arithmetic
-}

module AERN2.MP.Ball
  (
#ifdef USEMPFR
   module AERN2.MP.UseMPFR.Ball
#endif
  )
where

-- import MixedTypesNumPrelude
-- import qualified Prelude as P

#ifdef USEMPFR
import AERN2.MP.UseMPFR.Ball
#endif
