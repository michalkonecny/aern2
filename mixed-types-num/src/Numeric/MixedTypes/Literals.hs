{-|
    Module      :  Numeric.MixedType.Literals
    Description :  Fixed-type numeric literals + conversions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    This module defines fixed-type integer and rational literals.
    This is useful when deriving the type of an expression bottom-up.
    Eg we would not be able to write @1 < x@
    when the type of @<@ does not force the two sides to be of the
    same type.  We would need to write eg @(1::Integer) < x@ with
    Prelude's generic literals.

    Moreover, convenient conversion functions are provided for
    the most common numeric types.  Thus one can say eg @take (int 1)@
    or @integer (length list)@.
-}

module Numeric.MixedTypes.Literals
(
    -- * Fixed-type literals
    fromInteger, fromRational
    , ifThenElse
    -- * Generic list index
    , (!!)
    -- * Convenient conversions
    , CanBeInteger, integer, integers
    , CanBeInt, int, ints
    , CanBeRational, rational, rationals
    , CanBeDouble, double, doubles
    , Convertible(..), convert
)
where

import Prelude hiding (fromInteger, fromRational, (!!))
-- import qualified Prelude as P

import qualified Data.Convertible as CVT

import qualified Data.List as List

{-| Replacement for 'Prelude.fromInteger' using the RebindableSyntax extension.
    This version of fromInteger arranges that integer literals
    are always of type 'Integer'.
-}
fromInteger :: Integer -> Integer
fromInteger = id

{-| Replacement for 'Prelude.fromRational' using the RebindableSyntax extension.
    This version of fromRational arranges that rational literals are
    always of type 'Rational'. -}
fromRational :: Rational -> Rational
fromRational = id

{-|
  Restore if-then-else with RebindableSyntax
-}
ifThenElse :: Bool -> t -> t -> t
ifThenElse b e1 e2
  | b = e1
  | otherwise = e2

_testIf1 :: String
_testIf1 = if True then "yes" else "no"

{---- Numeric conversions -----}

type CanBeInteger t = Convertible t Integer
integer :: (CanBeInteger t) => t -> Integer
integer = convert
integers :: (CanBeInteger t) => [t] -> [Integer]
integers = map convert

type CanBeInt t = Convertible t Int
int :: (CanBeInt t) => t -> Int
int = convert
ints :: (CanBeInt t) => [t] -> [Int]
ints = map convert

(!!) :: (CanBeInteger t) => [a] -> t -> a
list !! ix = List.genericIndex list (integer ix)

type CanBeRational t = Convertible t Rational
rational :: (CanBeRational t) => t -> Rational
rational = convert
rationals :: (CanBeRational t) => [t] -> [Rational]
rationals = map convert

type CanBeDouble t = Convertible t Double
double :: (CanBeDouble t) => t -> Double
double = convert
doubles :: (CanBeDouble t) => [t] -> [Double]
doubles = map convert

{-|
Define our own Convertible since convertible is too relaxed for us.
For example, convertible allows conversion from Rational to Integer,
rounding to nearest integer.  We prefer to allow only exact conversions.
-}
class Convertible t1 t2 where
  safeConvert :: t1 -> CVT.ConvertResult t2
  default safeConvert :: (CVT.Convertible t1 t2) => t1 -> CVT.ConvertResult t2
  safeConvert = CVT.safeConvert

convert :: (Convertible t1 t2) => t1 -> t2
convert a =
  case safeConvert a of
    Right v -> v
    Left err -> error (show err)

instance Convertible Integer Integer -- use CVT instance by default
instance Convertible Int Integer

instance Convertible Int Int where
  safeConvert n = Right n
instance Convertible Rational Rational where
  safeConvert q = Right q

instance Convertible Integer Int
instance Convertible Int Rational
instance Convertible Integer Rational

instance Convertible Int Double
instance Convertible Integer Double
instance Convertible Rational Double
instance Convertible Double Double where
  safeConvert d = Right d

{-- we deliberately do not allow converions from Double to any other type --}
