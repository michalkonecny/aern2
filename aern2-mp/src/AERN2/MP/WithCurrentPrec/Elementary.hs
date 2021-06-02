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

piCP :: (KnownNat p) => WithCurrentPrec p (CN MPBall)
piCP = r 
    where
    r = WithCurrentPrec $ cn $ piBallP (getCurrentPrecision r)

instance
    (CanSinCos t)
    =>
    CanSinCos (WithCurrentPrec p t)
    where
    type SinCosType (WithCurrentPrec p t) = WithCurrentPrec p (SinCosType t)
    sin = lift1 sin
    cos = lift1 cos

instance
    (CanSqrt t)
    =>
    CanSqrt (WithCurrentPrec p t)
    where
    type SqrtType (WithCurrentPrec p t) = WithCurrentPrec p (SqrtType t)
    sqrt = lift1 sqrt

instance
    (CanExp t)
    =>
    CanExp (WithCurrentPrec p t)
    where
    type ExpType (WithCurrentPrec p t) = WithCurrentPrec p (ExpType t)
    exp = lift1 exp

instance
    (CanLog t)
    =>
    CanLog (WithCurrentPrec p t)
    where
    type LogType (WithCurrentPrec p t) = WithCurrentPrec p (LogType t)
    log = lift1 log

instance
    (CanPow t1 t2, p1~p2)
    =>
    (CanPow (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2)) where
    type PowType (WithCurrentPrec p1 t1) (WithCurrentPrec p2 t2) = WithCurrentPrec p1 (PowType t1 t2)
    pow = lift2 pow

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ e -> [d|

  instance 
    (CanPow b $e)
    =>
    CanPow (WithCurrentPrec p b) $e 
    where
    type PowType (WithCurrentPrec p b) $e = WithCurrentPrec p (PowType b $e)
    pow = lift1T pow

  |]))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ b -> [d|

  instance 
    (CanPow $b e, HasOrderCertainly e Integer, CanTestInteger e)
    =>
    CanPow $b (WithCurrentPrec p e) 
    where
    type PowType $b (WithCurrentPrec p e) = WithCurrentPrec p (PowType $b e)
    pow = liftT1 pow
  |]))

_example1 :: CN MPBall
_example1 = runWithPrec (prec 1000) piCP

_example2 :: CN MPBall
_example2 = runWithPrec (prec 1000) $ piCP - piCP

_example3 :: CN MPBall
_example3 = runWithPrec (prec 1000) $ sqrt (cnmpBallCP 2)