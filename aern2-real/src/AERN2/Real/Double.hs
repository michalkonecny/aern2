{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
module AERN2.Real.Double (withUpwardsRounding) where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)
import qualified Prelude as P

import Numeric.IEEE.RoundMode (getRound, setRound, RoundMode(Upward))
import System.IO.Unsafe (unsafePerformIO)

import AERN2.Real.Operations

instance CanNeg Double where
    type NegType Double = Double
    neg = P.negate

instance CanAdd Double Double where
    type AddType Double Double = Double
    add d1 d2 = d1 P.+ d2

instance CanMul Double Double where
    type MulType Double Double = Double
    mul d1 d2 = d1 P.* d2

instance CanDiv Double Double where
    type DivType Double Double = Double
    div d1 d2 = d1 P./ d2

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
