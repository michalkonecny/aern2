{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec
    Description :  Type wrapper setting default precision
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Type wrapper setting default precision.

    Not suitable for use with MixedTypesNumPrelude since we need binary operators to enforce
    the same precision on both operands via the equality of their types.

    Borrowed some tricks from https://github.com/ekmett/rounded/blob/master/src/Numeric/Rounded/Precision.hs
-}
module AERN2.MP.WithCurrentPrec
(
    WithCurrentPrec(..), runWithPrec, HasCurrentPrecision(..)
    , WithAnyPrec(..)
    -- , _example1 , _example2 , _example3
)
where

import MixedTypesNumPrelude
import qualified Prelude as P
-- import Text.Printf

-- import Text.Printf
import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

import Data.Proxy
import Data.Reflection
import GHC.TypeLits

-- import Data.Complex

import AERN2.Limit
import AERN2.MP.Precision
import AERN2.MP.Ball

class HasCurrentPrecision p where
    getCurrentPrecision :: proxy p -> Precision

instance KnownNat n => HasCurrentPrecision n where
    getCurrentPrecision p = max (prec 2) . min maximumPrecision $ prec (natVal p)

{-|

An existential type wrapper for convenient conversions, eg using aern2-real:

> _x :: KnownNat p => WithCurrentPrec (CN MPBall) p
> _x = undefined
>
> _r_x :: CReal
> _r_x = creal $ WithAnyPrec _x

-}

newtype WithAnyPrec t = WithAnyPrec (forall p. (KnownNat p) => WithCurrentPrec t p)


-- data PrecAdd10 (p :: *)

-- instance (HasCurrentPrecision p) => HasCurrentPrecision (PrecAdd10 p) where
--     isPrecision (_ :: proxy _) = 10 + isPrecision (undefined :: proxy p)

newtype WithCurrentPrec t p = WithCurrentPrec { unWithCurrentPrec :: t }
    deriving (Show)

deriving instance (CanTakeErrors NumErrors t) => (CanTakeErrors NumErrors (WithCurrentPrec t p))

runWithPrec :: Precision -> (forall n. (KnownNat n) => WithCurrentPrec t n) -> t
runWithPrec p (wfp :: (forall n. (KnownNat n) => WithCurrentPrec t n)) = 
    reifyNat (integer p) withNat
    where
    withNat :: KnownNat n => Proxy n -> t
    withNat (_ :: Proxy n) = 
        unWithCurrentPrec (wfp :: WithCurrentPrec t n)

-- -- The following does not work:
-- instance (CanAddAsymmetric t1 t2) => (CanAddAsymmetric (WithCurrentPrec t1 p) (WithCurrentPrec t2 p)) where
--     type AddType (WithCurrentPrec t1 p) (WithCurrentPrec t2 p) = WithCurrentPrec (AddType t1 t2) p
--     add (WithCurrentPrec a1) (WithCurrentPrec a2) = WithCurrentPrec $ a1 + a2

instance 
    (HasOrderAsymmetric t1 t2)
    =>
    HasOrderAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) 
    where
    type OrderCompareType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = OrderCompareType t1 t2
    greaterThan (WithCurrentPrec v1) (WithCurrentPrec v2) = greaterThan v1 v2
    lessThan (WithCurrentPrec v1) (WithCurrentPrec v2) = lessThan v1 v2
    geq (WithCurrentPrec v1) (WithCurrentPrec v2) = geq v1 v2
    leq (WithCurrentPrec v1) (WithCurrentPrec v2) = leq v1 v2

instance
    (CanMinMaxAsymmetric t1 t2, p1 ~ p2)
    =>
    CanMinMaxAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) 
    where
    type MinMaxType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (MinMaxType t1 t2) p1
    min (WithCurrentPrec v1) (WithCurrentPrec v2) = WithCurrentPrec $ min v1 v2
    max (WithCurrentPrec v1) (WithCurrentPrec v2) = WithCurrentPrec $ max v1 v2

instance P.Eq t => P.Eq (WithCurrentPrec t p) where
    (==) = lift2P (P.==)
instance P.Ord t => P.Ord (WithCurrentPrec t p) where
    compare = lift2P P.compare

instance 
    (HasCurrentPrecision p, P.Num t, ConvertibleWithPrecision Integer t) 
    => 
    P.Num (WithCurrentPrec t p) 
    where
    fromInteger n = r
        where   
        r = WithCurrentPrec $ convertP (getCurrentPrecision r) n
    negate = lift1 P.negate
    abs = lift1 P.abs
    (+) = lift2 (P.+)
    (*) = lift2 (P.*)
    signum = lift1 P.signum

instance 
    (HasCurrentPrecision p, P.Fractional t
    , ConvertibleWithPrecision Integer t, ConvertibleWithPrecision Rational t) 
    => 
    P.Fractional (WithCurrentPrec t p) 
    where
    fromRational q = r
        where   
        r = WithCurrentPrec $ convertP (getCurrentPrecision r) q
    recip = lift1 P.recip
    (/) = lift2 (P./)

instance (HasCurrentPrecision p) => P.Floating (WithCurrentPrec (CN MPBall) p) where
    pi = r 
        where
        r = WithCurrentPrec $ cn $ piBallP (getCurrentPrecision r)
    sqrt = lift1 P.sqrt
    exp = lift1 P.exp
    log = lift1 P.log
    sin = lift1 P.sin
    cos = lift1 P.cos
    asin = lift1 P.asin
    acos = lift1 P.acos
    atan = lift1 P.atan
    sinh = lift1 P.sinh
    cosh = lift1 P.cosh
    asinh = lift1 P.asinh
    acosh = lift1 P.acosh
    atanh = lift1 P.atanh

instance 
    (HasLimits ix (CN MPBall -> CN MPBall)
    , LimitType ix (CN MPBall -> CN MPBall) ~ (CN MPBall -> CN MPBall)
    ,HasCurrentPrecision p)
    => 
    HasLimits ix (WithCurrentPrec (CN MPBall) p) 
    where
    type LimitType ix (WithCurrentPrec (CN MPBall) p) = WithCurrentPrec (CN MPBall) p
    limit (s :: ix -> (WithCurrentPrec (CN MPBall) p)) = 
        WithCurrentPrec $ limit (snop) $ sample
        where
        sample :: CN MPBall
        sample = setPrecision (getCurrentPrecision sampleP) $ cn $ mpBall 0 
        sampleP :: WithCurrentPrec MPBall p
        sampleP = error "sampleP is not defined, it is only a type proxy"
        snop :: ix -> (CN MPBall -> CN MPBall)
        snop ix _sample = unWithCurrentPrec $ s ix

lift1 :: (t1 -> t2) -> (WithCurrentPrec t1 p) -> (WithCurrentPrec t2 p)
lift1 f (WithCurrentPrec v1) = WithCurrentPrec (f v1)

lift2 :: (t1 -> t2 -> t3) -> (WithCurrentPrec t1 p) -> (WithCurrentPrec t2 p) -> (WithCurrentPrec t3 p)
lift2 f (WithCurrentPrec v1) (WithCurrentPrec v2) = WithCurrentPrec (f v1 v2)

lift2P :: (t1 -> t2 -> t3) -> (WithCurrentPrec t1 p) -> (WithCurrentPrec t2 p) -> t3
lift2P f (WithCurrentPrec v1) (WithCurrentPrec v2) = f v1 v2

_example1 :: CN MPBall
_example1 = runWithPrec (prec 1000) P.pi

_example2 :: CN MPBall
_example2 = runWithPrec (prec 1000) $ P.pi P.- P.pi

_example3 :: CN MPBall
_example3 = runWithPrec (prec 1000) $ P.sqrt (P.fromInteger 2)
