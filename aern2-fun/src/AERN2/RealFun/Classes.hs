{-|
    Module      :  AERN2.RealFun.Classes
    Description :  Classes for real number function operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Classes for real number function operations
-}

module AERN2.RealFun.Classes
(
  HasDomain(..), CanApply(..), HasVars(..),
  HasConstFunctions
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

-- import AERN2.MP.Dyadic
-- import AERN2.MP.Ball

class HasDomain f where
  type Domain f
  getDomain :: f -> Domain f

class CanApply f d where
  type ApplyType f d
  apply :: f -> d -> ApplyType f d

class HasVars f where
  type Var f
  varF :: Var f -> f

type HasConstFunctions t f = ConvertibleExactly t f
