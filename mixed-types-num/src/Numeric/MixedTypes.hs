{-|
    Module      :  Numeric.MixedType
    Description :
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    This module provides a version of Prelude where
    binary and unary operations such as +, ==, not
    with the result type being derived from the parameter
    types, allowing eg:

      * adding integer and rational, giving a rational

      * comparing an integer with a real number, giving a Maybe Bool

-}

module Numeric.MixedTypes
(
  module Prelude,
  module Numeric.MixedTypes.Literals,
  module Numeric.MixedTypes.Bool,
  module Numeric.MixedTypes.EqOrd
)
where

import Prelude hiding
  (
    fromInteger, fromRational, (!!),
    negate, not, (&&), (||), and, or,
    (==),(/=),(<),(>),(<=),(>=)
  )

-- import qualified Prelude as P

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
