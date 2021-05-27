{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE EmptyDataDecls #-}
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
(piCP)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import GHC.TypeLits ( KnownNat )

-- import qualified Numeric.CollectErrors as CN

import AERN2.MP.Ball

import AERN2.MP.WithCurrentPrec.Type

piCP :: (KnownNat p) => WithCurrentPrec (CN MPBall) p
piCP = r 
    where
    r = WithCurrentPrec $ cn $ piBallP (getCurrentPrecision r)
