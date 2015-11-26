{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module AERN2.Real.MPFloat 
    (MPFloat, Precision(..), 
     fromRationalUp, fromRationalDown, zero, piUp, piDown, 
     cosUp, cosDown, sinUp, sinDown)
where

import Prelude hiding (fromInteger)
import qualified Prelude as P

import Math.NumberTheory.Logarithms

import qualified Data.Approximate.MPFRLowLevel as M

import AERN2.Real.OperationsToBall
import AERN2.Real.Operations


type MPFloat = M.Rounded
newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

fromRationalUp :: Precision -> Rational -> MPFloat
fromRationalUp (Precision p) r =
    M.fromRationalA M.Up (P.fromInteger p) r
    
fromRationalDown :: Precision -> Rational -> MPFloat
fromRationalDown (Precision p) r =
    M.fromRationalA M.Down (P.fromInteger p) r
    
zero :: MPFloat
zero = M.zero
    
piUp :: Precision -> MPFloat
piUp (Precision p) =
    M.pi M.Up (P.fromInteger p)
    
piDown :: Precision -> MPFloat
piDown (Precision p) =
    M.pi M.Down (P.fromInteger p)
    
instance CanMeasureError MPFloat where
    errorIndex x = 
        toInteger $ integerLog2 $ floor $ M.toRationalA $ M.div M.Down (M.getPrec x) one x
        where
        one = M.fromIntegerA M.Up (M.getPrec x) 1
    
instance CanNeg MPFloat where
    type NegType MPFloat = MPFloat
    neg = unaryUp M.neg

instance CanNegSameType MPFloat

instance CanAbs MPFloat where
    type AbsType MPFloat = MPFloat
    abs x 
        | x < M.zero = neg x
        | otherwise = x

instance CanAbsSameType MPFloat

instance CanAdd MPFloat MPFloat where
    type AddType MPFloat MPFloat = MPFloat
    add = binaryUp M.add

instance CanAddThis MPFloat MPFloat
instance CanAddSameType MPFloat

instance CanSub MPFloat MPFloat where
    type SubType MPFloat MPFloat = MPFloat
    sub = binaryUp M.sub

instance CanSubThis MPFloat MPFloat
instance CanSubSameType MPFloat

instance CanMul MPFloat MPFloat where
    type MulType MPFloat MPFloat = MPFloat
    mul = binaryUp M.mul

instance CanMulBy MPFloat MPFloat
instance CanMulSameType MPFloat

instance CanDiv MPFloat MPFloat where
    type DivType MPFloat MPFloat = MPFloat
    div = binaryUp M.div

instance CanDivBy MPFloat MPFloat
instance CanDivSameType MPFloat

cosUp :: MPFloat -> MPFloat
cosUp = unaryUp M.cos

cosDown :: MPFloat -> MPFloat
cosDown = unaryDown M.cos

sinUp :: MPFloat -> MPFloat
sinUp = unaryUp M.sin

sinDown :: MPFloat -> MPFloat
sinDown = unaryDown M.sin

{- auxiliary functions to automatically determine result precision from operand precisions -}

unaryUp :: 
    (M.RoundMode -> M.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryUp opRP x = opRP M.Up p x
    where
    p = M.getPrec x

unaryDown :: 
    (M.RoundMode -> M.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryDown opRP x = opRP M.Down p x
    where
    p = M.getPrec x

binaryUp :: 
    (M.RoundMode -> M.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat -> MPFloat)
binaryUp opRP x y = opRP M.Up p x y
    where
    p = (M.getPrec x) `max` (M.getPrec y)
    