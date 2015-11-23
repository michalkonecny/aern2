{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Real.Ball where

import Prelude hiding ((+),(*),(/),(-),abs,fromInteger,fromRational)
--import qualified Prelude as P

import Data.Convertible

import AERN2.Real.OperationsToBall
import AERN2.Real.Operations

ballFromRational ::
    (Convertible Rational a, Convertible Rational (ErrorBoundType a))
    =>
    (Rational, Rational) -> Ball a
ballFromRational (x,e) = Ball (convert x) (convert e) 
    

instance
    (CanAddB a a, CanAddSameType a,
     CanAddSameType (ErrorBoundType a)) 
    => 
    (CanAdd (Ball a) (Ball a))  
    where
    type AddType (Ball a) (Ball a) = Ball a
    add (Ball x1 e1) (Ball x2 e2) =
        Ball x12 (e12 + e1 + e2)
        where
        (Ball x12 e12) = x1 +: x2
        
instance 
    (CanAddSameType a, CanAddB a a, 
     CanAbsSameType a,
     CanAddSameType (ErrorBoundType a)) 
    => 
    CanAddSameType (Ball a)

instance
    (CanMulB a a, 
     CanMulSameType a, 
     CanAbsSameType a,
     CanMulBy (ErrorBoundType a) a,
     CanAddSameType (ErrorBoundType a), 
     CanMulSameType (ErrorBoundType a)) 
    => 
    (CanMul (Ball a) (Ball a))  
    where
    type MulType (Ball a) (Ball a) = Ball a
    mul (Ball x1 e1) (Ball x2 e2) =
        Ball x12 (e12 + e1*(abs x2) + e2*(abs x1) + e1*e2)
        where
        (Ball x12 e12) = x1 *: x2

{- 
    TODO: Instances for other operations. 
-} 
