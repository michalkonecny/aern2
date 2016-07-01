{-|
    Module      :  Numeric.MixedType.Literals
    Description :  Fixed-type numeric literals + conversions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    This module defines fixed-type integer and rational literals.

    Moreover, convenient conversion functions are provided for
    the most common numeric types.  Thus one can say eg @take (int 1)@
    or @integer (length list)@.
-}

module Numeric.MixedTypes.Literals
(
    -- * Fixed-type literals
    fromInteger, fromRational
    -- * Convenient conversions
    , CanBeInteger, integer, integers
    , CanBeInt, int, ints
    , CanBeRational, rational, rationals
    , CanBeDouble, double, doubles
    , Convertible(..), convert
    -- * generic list index
    , (!!)
)
where

import Prelude hiding (fromInteger, fromRational, (!!))
-- import qualified Prelude as P

import qualified Data.Convertible as CVT

import qualified Data.List as List

fromInteger :: Integer -> Integer
fromInteger = id

fromRational :: Rational -> Rational
fromRational = id

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
