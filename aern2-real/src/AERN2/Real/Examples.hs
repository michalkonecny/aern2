{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module AERN2.Real.Examples where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)

import AERN2.Real.Operations
import AERN2.Real.OperationsToBall (Ball(..))
import AERN2.Real.IntegerRational ()
import AERN2.Real.Ball (ballAccuracy)
import AERN2.Real.DoubleToBall (rationals2ballDouble)
import AERN2.Real.MPFloat (MPFloat, Precision(..))
import AERN2.Real.MPFloatToBall (rationals2ballMPFloat)
--import AERN2.Real.CauchyReal (CauchyReal(..))


ballD1 :: Ball Double
ballD1 = rationals2ballDouble (2.0,1/3) :: Ball Double

ballDadd :: Ball Double
ballDadd = ballD1 + ballD1

ballDmul :: Ball Double
ballDmul = ballD1 * ballD1

ballR1 :: Ball MPFloat
ballR1 = rationals2ballMPFloat (Precision 1000) (2.0,1/3) :: Ball MPFloat 

ballRadd :: Ball MPFloat
ballRadd = ballR1 + ballR1

ballRmul :: Ball MPFloat
ballRmul = ballR1 * ballR1

ballD1Accuracy :: Integer
ballD1Accuracy = ballAccuracy ballD1
--ballR1Accuracy :: Integer
--ballR1Accuracy = ballAccuracy ballR1

--cauchyThird :: CauchyReal 
