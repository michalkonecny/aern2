{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
    WithCurrentPrec(..), runWithPrec
    -- , _example1 , _example2 , _example3
)
where

import qualified MixedTypesNumPrelude as MxP
import Prelude
-- import Text.Printf

import Numeric.CollectErrors (CN, cn)
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

-- data PrecAdd10 (p :: *)

-- instance (HasCurrentPrecision p) => HasCurrentPrecision (PrecAdd10 p) where
--     isPrecision (_ :: proxy _) = 10 + isPrecision (undefined :: proxy p)

newtype WithCurrentPrec t p = WithCurrentPrec { unWithCurrentPrec :: t }
    deriving (Show)

runWithPrec :: Precision -> (forall n. (KnownNat n) => WithCurrentPrec t n) -> t
runWithPrec p (wfp :: (forall n. (KnownNat n) => WithCurrentPrec t n)) = 
    reifyNat (MxP.integer p) withNat
    where
    withNat :: KnownNat n => Proxy n -> t
    withNat (_ :: Proxy n) = 
        unWithCurrentPrec (wfp :: WithCurrentPrec t n)

-- -- The following does not work:
-- instance (CanAddAsymmetric t1 t2) => (CanAddAsymmetric (WithCurrentPrec t1 p) (WithCurrentPrec t2 p)) where
--     type AddType (WithCurrentPrec t1 p) (WithCurrentPrec t2 p) = WithCurrentPrec (AddType t1 t2) p
--     add (WithCurrentPrec a1) (WithCurrentPrec a2) = WithCurrentPrec $ a1 + a2

instance 
    (MxP.HasOrderAsymmetric t1 t2)
    =>
    MxP.HasOrderAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) 
    where
    type OrderCompareType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = MxP.OrderCompareType t1 t2
    greaterThan (WithCurrentPrec v1) (WithCurrentPrec v2) = MxP.greaterThan v1 v2
    lessThan (WithCurrentPrec v1) (WithCurrentPrec v2) = MxP.lessThan v1 v2
    geq (WithCurrentPrec v1) (WithCurrentPrec v2) = MxP.geq v1 v2
    leq (WithCurrentPrec v1) (WithCurrentPrec v2) = MxP.leq v1 v2

instance Eq t => Eq (WithCurrentPrec t p) where
    (==) = lift2P (==)
instance Ord t => Ord (WithCurrentPrec t p) where
    compare = lift2P compare

instance 
    (HasCurrentPrecision p, Num t, ConvertibleWithPrecision Integer t) 
    => 
    Num (WithCurrentPrec t p) 
    where
    fromInteger n = r
        where   
        r = WithCurrentPrec $ convertP (getCurrentPrecision r) n
    negate = lift1 negate
    abs = lift1 abs
    (+) = lift2 (+)
    (*) = lift2 (*)
    signum = lift1 signum

instance 
    (HasCurrentPrecision p, Fractional t
    , ConvertibleWithPrecision Integer t, ConvertibleWithPrecision Rational t) 
    => 
    Fractional (WithCurrentPrec t p) 
    where
    fromRational q = r
        where   
        r = WithCurrentPrec $ convertP (getCurrentPrecision r) q
    recip = lift1 recip
    (/) = lift2 (/)

instance (HasCurrentPrecision p) => Floating (WithCurrentPrec (CN MPBall) p) where
    pi = r 
        where
        r = WithCurrentPrec $ cn $ piBallP (getCurrentPrecision r)
    sqrt = lift1 sqrt
    exp = lift1 exp
    log = lift1 log
    sin = lift1 sin
    cos = lift1 cos
    asin = lift1 asin
    acos = lift1 acos
    atan = lift1 atan
    sinh = lift1 sinh
    cosh = lift1 cosh
    asinh = lift1 asinh
    acosh = lift1 acosh
    atanh = lift1 atanh

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
        sample = setPrecision (getCurrentPrecision sampleP) 0 
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
_example1 = runWithPrec (prec 1000) pi

_example2 :: CN MPBall
_example2 = runWithPrec (prec 1000) $ pi - pi

_example3 :: CN MPBall
_example3 = runWithPrec (prec 1000) $ sqrt 2
