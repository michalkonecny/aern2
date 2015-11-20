{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax, TypeFamilies, MultiParamTypeClasses #-}

module AERN2.Real.IntegerRational () where

{- imports -}

import Prelude (Integer,Rational)
import qualified Prelude as P

import Data.Ratio ((%))

import AERN2.Real.Operations

{- examples -}

_test1 :: Rational
_test1 = 2 * 3 + (1/2) ^ 2

_test2 :: Integer -- cannot be Int
_test2 = 2 * 3 + 2 ^ 2

{- operations on Integers -}

instance CanNeg Integer where
    type NegType Integer = Integer
    neg a = P.negate a
    
instance CanAdd Integer Integer where
    type AddType Integer Integer = Integer
    add a b = a P.+ b

instance CanSub Integer Integer -- the default implementation is fine
    
instance CanMul Integer Integer where
    type MulType Integer Integer = Integer
    mul a b = a P.* b
    
instance CanPow Integer Integer where
    type PowType Integer Integer = Integer
    pow a b = a P.^ b
    
instance CanRecip Integer where
    type RecipType Integer = Rational
    recip a = 1 % a

instance CanDiv Integer Integer -- the default implementation is fine
    
{- operations on Rationals -}
    
instance CanNeg Rational where
    type NegType Rational = Rational
    neg a = P.negate a
    
instance CanAdd Rational Rational where
    type AddType Rational Rational = Rational
    add a b = a P.+ b

instance CanSub Rational Rational -- the default implementation is fine
    
instance CanMul Rational Rational where
    type MulType Rational Rational = Rational
    mul a b = a P.* b
    
instance CanPow Rational Integer where
    type PowType Rational Integer = Rational
    pow a b = a P.^ b
    
instance CanRecip Rational where
    type RecipType Rational = Rational
    recip a = 1 / a


{- operations mixing Integer and Rational -}

instance CanAdd Integer Rational where
    type AddType Integer Rational = Rational
    add a b = (P.fromInteger a) P.+ b

instance CanAdd Rational Integer where
    type AddType Rational Integer = Rational
    add a b = a P.+ (P.fromInteger b)

instance CanMul Integer Rational where
    type MulType Integer Rational = Rational
    mul a b = (P.fromInteger a) P.* b

instance CanMul Rational Integer where
    type MulType Rational Integer = Rational
    mul a b = a P.* (P.fromInteger b)

instance CanDiv Integer Rational where
    type DivType Integer Rational = Rational
    div a b = (P.fromInteger a) P./ b

instance CanDiv Rational Integer where
    type DivType Rational Integer = Rational
    div a b = a P./ (P.fromInteger b)



