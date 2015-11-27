{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AERN2.Real.DoubleBound 
    (DoubleBound(..), 
     rational2DoubleBound, errorIndex) 
where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
import qualified Prelude as P

import Numeric.IEEE.RoundMode (getRound, setRound, RoundMode(Upward))
import System.IO.Unsafe (unsafePerformIO)

import Data.Ratio (numerator,denominator)

import Data.Convertible

import Math.NumberTheory.Logarithms

import AERN2.Real.Operations

{- example -}

_example1 :: DoubleBound
_example1 = 2*((rational2DoubleBound 0.01) + 0.1*(rational2DoubleBound 0.01)/3)

{-| A non-negative Double value to serve as an error bound. Arithmetic is rounded towards +infinity. -}
newtype DoubleBound = DoubleBound Double

instance Show DoubleBound where
    show (DoubleBound d) = show d

rational2DoubleBound :: Rational -> DoubleBound
rational2DoubleBound = convert

instance Convertible Rational DoubleBound where
    safeConvert a
        | a >= 0.0 = Right $ DoubleBound $ rational2DoubleUp a
        | otherwise = error "Trying to construct a negative DoubleBound."

errorIndex :: DoubleBound -> Integer
errorIndex (DoubleBound d) = 
    toInteger $ integerLog2 $ floor $ P.recip d

instance CanAdd DoubleBound DoubleBound where
    type AddType DoubleBound DoubleBound = DoubleBound
    add (DoubleBound a) (DoubleBound b) = DoubleBound $ withUpwardsRounding $ a P.+ b

instance CanAddThis DoubleBound DoubleBound
instance CanAddSameType DoubleBound

instance CanMul DoubleBound DoubleBound where
    type MulType DoubleBound DoubleBound = DoubleBound
    mul (DoubleBound a) (DoubleBound b) = DoubleBound $ withUpwardsRounding $ a P.* b

instance CanMulBy DoubleBound DoubleBound
instance CanMulSameType DoubleBound

instance CanMul DoubleBound Double where
    type MulType DoubleBound Double = DoubleBound
    mul (DoubleBound a) b = DoubleBound $ withUpwardsRounding $ a P.* b

instance CanMul Double DoubleBound where
    type MulType Double DoubleBound = DoubleBound
    mul a (DoubleBound b) = DoubleBound $ withUpwardsRounding $ a P.* b

instance CanMulBy DoubleBound Double

instance CanMul DoubleBound Integer where
    type MulType DoubleBound Integer = DoubleBound
    mul (DoubleBound a) i
        | i >= 0 = DoubleBound $ withUpwardsRounding $ a P.* (P.fromInteger i)
        | otherwise = error "trying to multiply DoubleBound by a negative integer"

instance CanMul Integer DoubleBound where
    type MulType Integer DoubleBound = DoubleBound
    mul i (DoubleBound b)
        | i >= 0 = DoubleBound $ withUpwardsRounding $ (P.fromInteger i) P.* b
        | otherwise = error "trying to multiply DoubleBound by a negative integer"

instance CanMulBy DoubleBound Integer

instance CanDiv DoubleBound Integer where
    type DivType DoubleBound Integer = DoubleBound
    div (DoubleBound a) i
        | i > 0 = DoubleBound $ withUpwardsRounding $ a P./ (P.fromInteger i)
        | otherwise = error "trying to multiply DoubleBound by a non-positive integer"

instance CanMul DoubleBound Rational where
    type MulType DoubleBound Rational = DoubleBound
    mul (DoubleBound a) r
        | r >= 0.0 = DoubleBound $ withUpwardsRounding $ a P.* (rational2DoubleUp r)
        | otherwise = error "trying to multiply DoubleBound by a negative integer"

instance CanMul Rational DoubleBound where
    type MulType Rational DoubleBound = DoubleBound
    mul r (DoubleBound b)
        | r >= 0.0 = DoubleBound $ withUpwardsRounding $ (rational2DoubleUp r) P.* b
        | otherwise = error "trying to multiply DoubleBound by a negative integer"

instance CanMulBy DoubleBound Rational

rational2DoubleUp :: Rational -> Double
rational2DoubleUp r =
    withUpwardsRounding $
    (P.fromInteger (numerator r))
    P./
    (P.negate $ P.fromInteger (P.negate $ denominator r)) -- round the denominator downward!

{-| Try to set the FPU to rounding towards +infinity before evaluating the argument. -}
withUpwardsRounding :: a -> a
withUpwardsRounding a =
    unsafePerformIO $
        do
        setMachineRoundingModeUp
        return $! a

{-| Try to set the FPU to rounding towards +infinity. -}
setMachineRoundingModeUp :: IO ()
setMachineRoundingModeUp =
    do
    currentRndMode <- getRound
    case currentRndMode == Upward of
        True ->
            do
--            putStrLn "setMachineRoundingModeUp: already up"
            return ()
        False ->
            do
            success <- setRound Upward
            case success of
                True ->
                    do
--                    putStrLn $ "setMachineRoundingModeUp: switching up from " ++ show currentRndMode
                    return ()
                False ->
                    error "AERN2.Real.DoubleBound: failed to switch rounding mode"
        