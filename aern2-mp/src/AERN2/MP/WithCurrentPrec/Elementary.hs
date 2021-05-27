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
    (CanSqrt t)
    =>
    CanSqrt (WithCurrentPrec t p)
    where
    type SqrtType (WithCurrentPrec t p) = WithCurrentPrec (SqrtType t) p
    sqrt = lift1 sqrt

_example1 :: CN MPBall
_example1 = runWithPrec (prec 1000) piCP

_example2 :: CN MPBall
_example2 = runWithPrec (prec 1000) $ piCP - piCP

_example3 :: CN MPBall
_example3 = runWithPrec (prec 1000) $ sqrt (mpBallCP 2)