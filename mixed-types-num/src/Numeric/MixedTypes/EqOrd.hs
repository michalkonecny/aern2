{-|
    Module      :  Numeric.MixedType.EqOrd
    Description :  Mixed-type comparisons
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.EqOrd
(
    HasEq(..)
    -- TODO
    -- HasOrder, OrderCompareType, lessThan, leq, greaterThan, geq,
    -- (==), (/=), (>), (<), (<=), (>=)
)
where

import Prelude hiding
  (negate,not,(&&),(||),and,or,
  (==), (/=), (>), (<), (<=), (>=))
import qualified Prelude as P

-- import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool

-- infix  4  ==, /=, <, <=, >=, >

class (IsBool (EqCompareType a b)) => HasEq a b where
    type EqCompareType a b
    type EqCompareType a b = Bool -- default
    equalTo :: a -> b -> (EqCompareType a b)
    -- default equalToA via Prelude for (->) and Bool:
    default equalTo :: (EqCompareType a b ~ Bool, a~b, P.Eq a) => a -> b -> Bool
    equalTo = (P.==)
    notEqualTo :: a -> b -> (EqCompareType a b)
    -- default notEqualToA via equalToA for Bool:
    default notEqualTo ::
        (CanNegSameType (EqCompareType a b)) =>
        a -> b -> (EqCompareType a b)
    notEqualTo a b = not $ equalTo a b
