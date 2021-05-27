{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec.PreludeInstances
    Description :  WithCurrentPrec instances of Prelude classes
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    WithCurrentPrec instances of Prelude classes
-}
module AERN2.MP.WithCurrentPrec.PreludeInstances
(
    -- _example1P , _example2P , _example3P
)
where

import Prelude
-- import Text.Printf

import Numeric.CollectErrors (cn, CN)

import AERN2.MP.Precision
import AERN2.MP.Ball

import AERN2.MP.WithCurrentPrec.Type

{-
********************************
Instances of Prelude classes
********************************
-}

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

_example1P :: CN MPBall
_example1P = runWithPrec (prec 1000) pi

_example2P :: CN MPBall
_example2P = runWithPrec (prec 1000) $ pi - pi

_example3P :: CN MPBall
_example3P = runWithPrec (prec 1000) $ sqrt (fromInteger 2)
