{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Real.Ball where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
import qualified Prelude as P

import AERN2.Real.OperationsToBall
import AERN2.Real.Operations

instance 
    (CanAddB a a, AddType a a ~ a, 
     CanAdd (ErrorBoundType a) (ErrorBoundType a),
     AddType (ErrorBoundType a) (ErrorBoundType a) ~ ErrorBoundType a) 
        => (CanAdd (Ball a) (Ball a))  
    where
    type AddType (Ball a) (Ball a) = Ball a
    add (Ball x1 e1) (Ball x2 e2) =
        Ball x12 (e12 + e1 + e2)
        where
        (Ball x12 e12) = x1 +: x2
        

{- 
    TODO: Instances for other operations. 
-} 
