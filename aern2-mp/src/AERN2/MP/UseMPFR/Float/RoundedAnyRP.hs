{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, ExistentialQuantification, RankNTypes #-}
-- {-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.UseMPFR.Float.RoundedAnyRP
    Description :  Numeric.Rounded + variable precision
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Numeric.Rounded + variable precision
-}
module AERN2.MP.UseMPFR.Float.RoundedAnyRP

-- #ifndef MPFRRounded
-- ()
-- #else
-- (
--   module AERN2.MP.UseMPFR.Float.RoundedAnyRP
-- )
where

#ifdef MPFRRounded

import Prelude hiding (div, pi)
import qualified Prelude as P

import Data.Coerce
import Data.Proxy


import Numeric.Rounded hiding (Precision)
import qualified Numeric.Rounded as R
-- import Numeric.Rounded.Precision

data RoundedAnyRP = forall p r . (R.Precision p, Rounding r) => RoundedAnyRP (Rounded r p)

instance Show RoundedAnyRP where
  show (RoundedAnyRP a) = show a

getExp :: RoundedAnyRP -> Int
getExp (RoundedAnyRP a) = P.exponent a

data RoundMode = Up | Down

type Precision = Int

defaultPrecision :: Precision
defaultPrecision = 10

getPrec :: RoundedAnyRP -> Precision
getPrec (RoundedAnyRP a) =
  precision (proxyPrec a)
  where
  proxyPrec :: Rounded r p -> Proxy p
  proxyPrec _ = Proxy

set :: RoundMode -> Precision -> RoundedAnyRP -> RoundedAnyRP
set r p (RoundedAnyRP a) =
  case r of
    Up ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          (precRound (forceUp a) :: Rounded 'TowardInf p)
    Down ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          (precRound (forceDown a) :: Rounded 'TowardNegInf p)

pi :: RoundMode -> Precision -> RoundedAnyRP
pi = lift0 P.pi

fromIntegerA :: RoundMode -> Precision -> Integer -> RoundedAnyRP
fromIntegerA r p n = lift0 (fromInteger n) r p

zero, one :: RoundedAnyRP
zero = lift0 0 Up defaultPrecision
one = lift0 1 Up defaultPrecision

toDoubleA :: RoundMode -> RoundedAnyRP -> Double
toDoubleA r (RoundedAnyRP a) =
  case r of
    Up -> toDouble (forceUp a)
    Down -> toDouble (forceDown a)

fromRationalA :: RoundMode -> Precision -> Rational -> RoundedAnyRP
fromRationalA r p q = lift0 (fromRational q) r p

toRationalA :: RoundedAnyRP -> Rational
toRationalA (RoundedAnyRP a) = toRational a

add, sub, mul, div :: RoundMode -> Precision -> RoundedAnyRP -> RoundedAnyRP -> RoundedAnyRP
add = lift2 (!+!)
sub = lift2 (!-!)
mul = lift2 (!*!)
div = lift2 (!/!)
-- atan2 = lift2 (atan2_)

neg, sqrt, exp, log, sin, cos :: RoundMode -> Precision -> RoundedAnyRP -> RoundedAnyRP
neg = lift1 negate_
sqrt = lift1 sqrt_
exp = lift1 exp_
log = lift1 log_
sin = lift1 sin_
cos = lift1 cos_
-- TODO: add more ops

instance Eq RoundedAnyRP where
  (==) = liftRel (!==!)
  (/=) = liftRel (!/=!)

instance Ord RoundedAnyRP where
  compare = liftRel compare_
  (<) = liftRel (!<!)
  (<=) = liftRel (!<=!)
  (>) = liftRel (!>!)
  (>=) = liftRel (!>=!)
  max = lift2autoRP max_
  min = lift2autoRP min_

forceUp :: (Rounding r, R.Precision p) => Rounded r p -> Rounded 'TowardInf p
forceUp a = coerce a
forceDown :: (Rounding r, R.Precision p) => Rounded r p -> Rounded 'TowardNegInf p
forceDown a = coerce a
{-# INLINE forceUp #-}
{-# INLINE forceDown #-}

lift0 ::
  (forall r p . (Rounding r, R.Precision p) => (Rounded r p)) ->
  RoundMode -> Precision -> RoundedAnyRP
lift0 x r p =
  case r of
    Up ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          (x :: Rounded 'TowardInf p)
    Down ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          (x :: Rounded 'TowardNegInf p)

lift1 ::
  (forall r p1 p2 . (Rounding r, R.Precision p1, R.Precision p2) => (Rounded r p1 -> Rounded r p2)) ->
  RoundMode -> Precision ->
  RoundedAnyRP -> RoundedAnyRP
lift1 op r p (RoundedAnyRP a) =
  case r of
    Up ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          (op (precRound (forceUp a) :: Rounded 'TowardInf p)  :: Rounded 'TowardInf p)
    Down ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          (op (precRound (forceDown a) :: Rounded 'TowardNegInf p) :: Rounded 'TowardNegInf p)


lift2 ::
  (forall r p1 p2 p3 . (Rounding r, R.Precision p1, R.Precision p2, R.Precision p3) => (Rounded r p1 -> Rounded r p2 -> Rounded r p3)) ->
  RoundMode -> Precision ->
  RoundedAnyRP -> RoundedAnyRP -> RoundedAnyRP
lift2 op r p (RoundedAnyRP a) (RoundedAnyRP b) =
  case r of
    Up ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          ((precRound (forceUp a) :: Rounded 'TowardInf p)
          `op`
          (precRound (forceUp b) :: Rounded 'TowardInf p)
          :: Rounded  'TowardInf p)
    Down ->
      reifyPrecision p $ \(_ :: Proxy p) ->
        RoundedAnyRP $
          ((precRound (forceDown a) :: Rounded 'TowardNegInf p)
          `op`
          (precRound (forceDown b) :: Rounded 'TowardNegInf p)
          :: Rounded  'TowardNegInf p)

lift2autoRP ::
  (forall r p1 p2 p3 . (Rounding r, R.Precision p1, R.Precision p2, R.Precision p3) => (Rounded r p1 -> Rounded r p2 -> Rounded r p3)) ->
  RoundedAnyRP -> RoundedAnyRP -> RoundedAnyRP
lift2autoRP op aA bA =
  let p = (getPrec aA) `P.max` (getPrec bA) in
  lift2 op Up p aA bA

liftRel ::
  (forall r p1 p2 . (Rounding r, R.Precision p1, R.Precision p2) => (Rounded r p1 -> Rounded r p2 -> t)) ->
  RoundedAnyRP -> RoundedAnyRP -> t
liftRel rel (RoundedAnyRP a) (RoundedAnyRP b) =
  (forceUp a) `rel` (forceUp b)
{-# INLINE liftRel #-}

#endif
