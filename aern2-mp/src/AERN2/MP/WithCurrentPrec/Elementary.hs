{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec.Elementary
    Description :  WithCurrentPrec elementary operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    WithCurrentPrec elementary operations
-}
module AERN2.MP.WithCurrentPrec.Elementary
(   
    piCP
    , _example1 , _example2 , _example3
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import GHC.TypeLits ( KnownNat )

-- import qualified Numeric.CollectErrors as CN

import AERN2.MP.Ball

import AERN2.MP.WithCurrentPrec.Type

import AERN2.MP.WithCurrentPrec.Field ()

piCP :: (KnownNat p) => WithCurrentPrec (CN MPBall) p
piCP = r 
    where
    r = WithCurrentPrec $ cn $ piBallP (getCurrentPrecision r)

instance
    (CanSinCos t)
    =>
    CanSinCos (WithCurrentPrec t p)
    where
    type SinCosType (WithCurrentPrec t p) = WithCurrentPrec (SinCosType t) p
    sin = lift1 sin
    cos = lift1 cos

instance
    (CanSqrt t)
    =>
    CanSqrt (WithCurrentPrec t p)
    where
    type SqrtType (WithCurrentPrec t p) = WithCurrentPrec (SqrtType t) p
    sqrt = lift1 sqrt

instance
    (CanExp t)
    =>
    CanExp (WithCurrentPrec t p)
    where
    type ExpType (WithCurrentPrec t p) = WithCurrentPrec (ExpType t) p
    exp = lift1 exp

instance
    (CanLog t)
    =>
    CanLog (WithCurrentPrec t p)
    where
    type LogType (WithCurrentPrec t p) = WithCurrentPrec (LogType t) p
    log = lift1 log

instance
    (CanPow t1 t2, p1~p2)
    =>
    (CanPow (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2)) where
    type PowType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (PowType t1 t2) p1
    pow = lift2 pow

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ e -> [d|

  instance 
    (CanPow b $e)
    =>
    CanPow (WithCurrentPrec b p) $e 
    where
    type PowType (WithCurrentPrec b p) $e = WithCurrentPrec (PowType b $e) p
    pow = lift1T pow

  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ b -> [d|

  instance 
    (CanPow $b e, HasOrderCertainly e Integer, CanTestInteger e)
    =>
    CanPow $b (WithCurrentPrec e p) 
    where
    type PowType $b (WithCurrentPrec e p) = WithCurrentPrec (PowType $b e) p
    pow = liftT1 pow
  |]))

_example1 :: CN MPBall
_example1 = runWithPrec (prec 1000) piCP

_example2 :: CN MPBall
_example2 = runWithPrec (prec 1000) $ piCP - piCP

_example3 :: CN MPBall
_example3 = runWithPrec (prec 1000) $ sqrt (mpBallCP 2)