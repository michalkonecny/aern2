{-|
    Module      :  Numeric.MixedType.EqOrdTolerant
    Description :  Operations (eg comparisons) with a certain tolerance
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Operations (eg comparisons) with a certain tolerance
-}

module AERN2.Tolerant
(
  -- * Tolerant types, especially Booleans
  Tolerant(..)
  -- * Equality checks modulo a given tolerance
  , HasTolerantEq,  HasTolerantEqAsymmetric(..), (==~), (/=~), (//+-)
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf
-- import Data.Ratio

-- import Test.Hspec
-- import Test.QuickCheck as QC
-- import Control.Exception (evaluate)

import Control.Applicative

import AERN2.MP.ErrorBound
import AERN2.MP.Ball

{---- Tolerant types -----}

data Tolerant b
  = TolerantExact b
  | Tolerant ErrorBound b

instance Functor Tolerant where
  fmap f (TolerantExact v) = TolerantExact (f v)
  fmap f (Tolerant er v) = Tolerant er (f v)

instance Applicative Tolerant where
  pure = TolerantExact
  (TolerantExact f) <*> (TolerantExact x) = TolerantExact (f x)
  (Tolerant er1 f) <*> (TolerantExact x) = Tolerant er1 (f x)
  (TolerantExact f) <*> (Tolerant er2 x) = Tolerant er2 (f x)
  (Tolerant er1 f) <*> (Tolerant er2 x) = Tolerant (max er2 er1) (f x)

instance ConvertibleExactly t1 t2 => ConvertibleExactly t1 (Tolerant t2) where
  safeConvertExactly = fmap TolerantExact . safeConvertExactly

instance (HasBools b) => HasBools (Tolerant b) where
  isCertainlyTrue (TolerantExact b) = isCertainlyTrue b
  isCertainlyTrue (Tolerant _eb _) = False
  isCertainlyFalse (TolerantExact b) = isCertainlyFalse b
  isCertainlyFalse (Tolerant _eb _) = False

instance (CanNegSameType b) => CanNeg (Tolerant b) where
  negate = fmap negate

instance (CanAndOrAsymmetric b1 b2) => CanAndOrAsymmetric (Tolerant b1) (Tolerant b2) where
  type AndOrType (Tolerant b1) (Tolerant b2) = Tolerant (AndOrType b1 b2)
  and2 = liftA2 and2
  or2 = liftA2 or2

{---- Tolerant equality tests -----}

infix  3  //+-
infix  4  ==~, /=~

type HasTolerantEq t1 t2 =
    (HasTolerantEqAsymmetric t1 t2,  HasTolerantEqAsymmetric t2 t1,
     TolerantEqCompareType t1 t2 ~ TolerantEqCompareType t2 t1)

class (CanNegSameType (TolerantEqCompareType a b)) => HasTolerantEqAsymmetric a b where
    type TolerantEqCompareType a b
    type TolerantEqCompareType a b = Tolerant Bool -- default
    {-|
      @tolerantEqualTo a (e,b)@

      alternatively written @a ==~ b //+- e@

      may return @Tolerant e' True@ if @a@ and @b@ differ by no more than @e'@;
      may return @TolerantExact False@ if @a@ and @b@ encode different values.

      Whenever possible, @e' <= e@.

    -}
    tolerantEqualTo :: a -> (ErrorBound,b) -> (TolerantEqCompareType a b)
    {-|
      @tolerantNotEqualTo a (e,b)@

      alternatively written @a /=~ b //+- e@

      may return @Tolerant e' False@ if @a@ and @b@ differ by no more than @e'@;
      may return @TolerantExact True@ if @a@ and @b@ encode different values.

      Whenever possible, @e' <= e@.
-}
    tolerantNotEqualTo :: a -> (ErrorBound,b) -> (TolerantEqCompareType a b)
    tolerantNotEqualTo a (eb,b) = negate $ tolerantEqualTo a (eb,b)

(==~) :: (HasTolerantEqAsymmetric a b) => a -> (ErrorBound,b) -> TolerantEqCompareType a b
(==~) = tolerantEqualTo

(/=~) :: (HasTolerantEqAsymmetric a b) => a -> (ErrorBound,b) -> TolerantEqCompareType a b
(/=~) = tolerantNotEqualTo

(//+-) :: b -> e -> (e,b)
(//+-) = flip (,)

instance HasTolerantEqAsymmetric MPBall MPBall where
  tolerantEqualTo a (_e,b) =
    case a == b of
      Just False -> TolerantExact False
      _ -> Tolerant e True
    where
    e = errorBound (a - b)
