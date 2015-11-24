{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
module AERN2.Real.Double (withUpwardsRounding, rational2DoubleUp) where

import Prelude
import qualified Prelude as P
import Data.Ratio (numerator,denominator)

import Numeric.IEEE.RoundMode (getRound, setRound, RoundMode(Upward))
import System.IO.Unsafe (unsafePerformIO)

import AERN2.Real.Operations

rational2DoubleUp :: Rational -> Double
rational2DoubleUp r =
    withUpwardsRounding $
    (P.fromInteger (numerator r))
    P./
    (P.negate $ P.fromInteger (P.negate $ denominator r)) -- round the denominator downward!

instance CanNeg Double where
    type NegType Double = Double
    neg = P.negate

instance CanNegSameType Double

instance CanAbs Double where
    type AbsType Double = Double
    abs = P.abs

instance CanAbsSameType Double

instance CanAdd Double Double where
    type AddType Double Double = Double
    add d1 d2 = d1 P.+ d2

instance CanAddThis Double Double
instance CanAddSameType Double

instance CanMul Double Double where
    type MulType Double Double = Double
    mul d1 d2 = d1 P.* d2

instance CanMulBy Double Double
instance CanMulSameType Double

instance CanDiv Double Double where
    type DivType Double Double = Double
    div d1 d2 = d1 P./ d2

instance CanDivBy Double Double
instance CanDivSameType Double

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
