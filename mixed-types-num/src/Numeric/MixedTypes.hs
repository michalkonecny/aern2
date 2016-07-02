{-|
    Module      :  Numeric.MixedType
    Description :  Mixed-type numeric expressions
    Copyright   :  (c) Michal Konecny, Pieter Collins
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

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.EqOrd
