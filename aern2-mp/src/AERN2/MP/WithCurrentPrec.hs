{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec
    Description :  Type wrapper setting default precision
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Type wrapper setting default precision.

    Borrowed some tricks from https://github.com/ekmett/rounded/blob/master/src/Numeric/Rounded/Precision.hs
-}
module AERN2.MP.WithCurrentPrec
(
    WithCurrentPrec(..), runWithPrec
    , WithAnyPrec(..)
    , cnmpBallCP
    , piCP
    -- , _example1 , _example2 , _example3
    -- , _example1P , _example2P , _example3P
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import GHC.TypeNats

import AERN2.MP.WithCurrentPrec.Type
import AERN2.MP.WithCurrentPrec.Comparisons ()
import AERN2.MP.WithCurrentPrec.Field ()
import AERN2.MP.WithCurrentPrec.Elementary (piCP)
import AERN2.MP.WithCurrentPrec.Limit ()
import AERN2.MP.WithCurrentPrec.PreludeInstances ()

import AERN2.MP.Ball

instance (KnownNat p) => Ring (WithCurrentPrec p (CN MPBall))
instance (KnownNat p) => Field (WithCurrentPrec p (CN MPBall))
instance (KnownNat p) => OrderedRing (WithCurrentPrec p (CN MPBall))
instance (KnownNat p) => OrderedField (WithCurrentPrec p (CN MPBall))
