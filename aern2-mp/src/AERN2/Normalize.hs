{-|
    Module      :  AERN2.Normalize
    Description :  Overloadable "normalize" function
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module AERN2.Normalize
(
    CanNormalize(..)
)
where

-- import Numeric.MixedTypes
-- import qualified Prelude as P

class CanNormalize t where
  normalize :: t -> t
