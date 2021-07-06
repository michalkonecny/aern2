{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec.Limit
    Description :  WithCurrentPrec limits
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    WithCurrentPrec limits
-}
module AERN2.MP.WithCurrentPrec.Limit
()
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

-- import qualified Numeric.CollectErrors as CN

import GHC.TypeLits
import Control.Monad (join)

import AERN2.MP.Ball

import AERN2.Limit

import AERN2.MP.WithCurrentPrec.Type

instance 
    (HasLimits ix (CN MPBall -> CN MPBall)
    , LimitType ix (CN MPBall -> CN MPBall) ~ (CN MPBall -> CN MPBall)
    , KnownNat p)
    => 
    HasLimits ix (WithCurrentPrec p (CN MPBall)) 
    where
    type LimitType ix (WithCurrentPrec p (CN MPBall)) = WithCurrentPrec p (CN MPBall)
    limit s = limit $ cn . s

instance 
    (HasLimits ix (CN MPBall -> CN MPBall)
    , LimitType ix (CN MPBall -> CN MPBall) ~ (CN MPBall -> CN MPBall)
    , KnownNat p)
    => 
    HasLimits ix (CN (WithCurrentPrec p (CN MPBall)))
    where
    type LimitType ix (CN (WithCurrentPrec p (CN MPBall))) = WithCurrentPrec p (CN MPBall)
    limit (s :: ix -> CN (WithCurrentPrec p (CN MPBall))) = 
        WithCurrentPrec $ limit (snop) $ sample
        where
        sample :: CN MPBall
        sample = setPrecision (getCurrentPrecision sampleP) (cn $ mpBall 0)
        sampleP :: WithCurrentPrec p MPBall
        sampleP = error "sampleP is not defined, it is only a type proxy"
        snop :: ix -> (CN MPBall -> CN MPBall)
        snop ix _sample = join $ fmap unWithCurrentPrec $ s ix
