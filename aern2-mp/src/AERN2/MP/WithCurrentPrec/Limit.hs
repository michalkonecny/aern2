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

import AERN2.MP.Ball

import AERN2.Limit

import AERN2.MP.WithCurrentPrec.Type

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
        sample = setPrecision (getCurrentPrecision sampleP) (cn $ mpBall 0)
        sampleP :: WithCurrentPrec MPBall p
        sampleP = error "sampleP is not defined, it is only a type proxy"
        snop :: ix -> (CN MPBall -> CN MPBall)
        snop ix _sample = unWithCurrentPrec $ s ix