{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AERN2.Real.Ball (Ball(..), ballAccuracy) where

import Prelude hiding ((+),(*),(/),(-),abs,recip,fromInteger,fromRational)
--import qualified Prelude as P

import AERN2.Real.OperationsToBall
import AERN2.Real.Operations

ballAccuracy :: 
    CanMeasureError (ErrorBoundType a) 
    =>
    Ball a -> Integer
ballAccuracy (Ball _ e) = 
    errorIndex e

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
    CanAddThis (Ball a) (Ball a)

instance
    (CanAddSameType a, CanAddB a a, 
     CanAbsSameType a,
     CanAddSameType (ErrorBoundType a)) 
    => 
    CanAddSameType (Ball a)

instance
    (CanSubB a a, CanSubSameType a,
     CanAddSameType (ErrorBoundType a)) 
    => 
    (CanSub (Ball a) (Ball a))  
    where
    type SubType (Ball a) (Ball a) = Ball a
    sub (Ball x1 e1) (Ball x2 e2) =
        Ball x12 (e12 + e1 + e2)
        where
        (Ball x12 e12) = x1 -: x2
        
instance
    (CanSubSameType a, CanSubB a a, 
     CanAbsSameType a,
     CanAddSameType (ErrorBoundType a)) 
    => 
    CanSubThis (Ball a) (Ball a)

instance
    (CanSubSameType a, CanSubB a a, 
     CanAbsSameType a,
     CanAddSameType (ErrorBoundType a)) 
    => 
    CanSubSameType (Ball a)

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

instance
    (CanMulB a a,
     CanMulSameType a, 
     CanAbsSameType a,
     CanMulBy (ErrorBoundType a) a,
     CanAddSameType (ErrorBoundType a), 
     CanMulSameType (ErrorBoundType a)) 
    => 
    CanMulBy (Ball a) (Ball a)

instance
    (CanMulB a a,
     CanMulSameType a, 
     CanAbsSameType a,
     CanMulBy (ErrorBoundType a) a,
     CanAddSameType (ErrorBoundType a), 
     CanMulSameType (ErrorBoundType a)) 
    => 
    CanMulSameType (Ball a)

{- 
    TODO: Instances such as: 
        CanAddThis (Ball a) Integer 
        CanAddThis (Ball a) Rational 
        CanSubThis (Ball a) Integer 
        CanSubThis (Ball a) Rational 
        CanMulBy (Ball a) Integer 
        CanMulBy (Ball a) Rational 
        CanDivBy (Ball a) Integer 
        CanDivBy (Ball a) Rational
        CanCosine (Ball a) 
-} 
