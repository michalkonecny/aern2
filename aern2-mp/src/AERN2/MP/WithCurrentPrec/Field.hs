{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec.Field
    Description :  WithCurrentPrec field operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    WithCurrentPrec field operations
-}
module AERN2.MP.WithCurrentPrec.Field
()
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

-- import qualified Numeric.CollectErrors as CN

import AERN2.MP.WithCurrentPrec.Type

instance
    (CanAddAsymmetric t1 t2, p1~p2)
    =>
    (CanAddAsymmetric (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2)) where
    type AddType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (AddType t1 t2) p1
    add = lift2 add

instance
    (CanSub t1 t2, p1~p2)
    =>
    (CanSub (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2)) where
    type SubType (WithCurrentPrec t1 p1) (WithCurrentPrec t2 p2) = WithCurrentPrec (SubType t1 t2) p1
    sub = lift2 sub

