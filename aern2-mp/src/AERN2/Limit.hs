{-|
    Module      :  AERN2.Limit
    Description :  limit operation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Generic limit operation
-}
module AERN2.Limit where

-- import MixedTypesNumPrelude

-- import qualified Numeric.CollectErrors as CN

---------
-- limit
---------

class HasLimits ix s where
  type LimitType ix s
  limit :: (ix -> s) -> LimitType ix s

type HasLimitsSameType ix s = (HasLimits ix s, LimitType ix s ~ s)
