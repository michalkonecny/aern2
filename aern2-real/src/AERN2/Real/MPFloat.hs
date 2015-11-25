{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module AERN2.Real.MPFloat 
(MPFloat, rational2MPFloat, zeroMPFloat, Precision(..), defaultPrecision)
where

import Prelude hiding (fromInteger)
import qualified Prelude as P

import qualified Data.Approximate.MPFRLowLevel as M

import AERN2.Real.Operations


type MPFloat = M.Rounded
newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

rational2MPFloat :: Precision -> Rational -> MPFloat
rational2MPFloat (Precision p) r =
    M.fromRationalA M.Up (P.fromInteger p) r
    
zeroMPFloat :: MPFloat
zeroMPFloat = M.zero
    
defaultPrecision :: Precision
defaultPrecision = Precision 100
    
instance CanNeg MPFloat where
    type NegType MPFloat = MPFloat
    neg = unary M.neg

instance CanNegSameType MPFloat

instance CanAbs MPFloat where
    type AbsType MPFloat = MPFloat
    abs x 
        | x < M.zero = neg x
        | otherwise = x

instance CanAbsSameType MPFloat

instance CanAdd MPFloat MPFloat where
    type AddType MPFloat MPFloat = MPFloat
    add = binary M.add

instance CanAddThis MPFloat MPFloat
instance CanAddSameType MPFloat

instance CanSub MPFloat MPFloat where
    type SubType MPFloat MPFloat = MPFloat
    sub = binary M.sub

instance CanSubThis MPFloat MPFloat
instance CanSubSameType MPFloat

instance CanMul MPFloat MPFloat where
    type MulType MPFloat MPFloat = MPFloat
    mul = binary M.mul

instance CanMulBy MPFloat MPFloat
instance CanMulSameType MPFloat

instance CanDiv MPFloat MPFloat where
    type DivType MPFloat MPFloat = MPFloat
    div = binary M.div

instance CanDivBy MPFloat MPFloat
instance CanDivSameType MPFloat

{- auxiliary functions to automatically determine result precision from operand precisions -}

unary :: 
    (M.RoundMode -> M.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unary opRP x = opRP M.Up p x
    where
    p = M.getPrec x

binary :: 
    (M.RoundMode -> M.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binary opRP x y = opRP M.Up p x y
    where
    p = (M.getPrec x) `max` (M.getPrec y)
    