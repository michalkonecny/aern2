{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module AERN2.Real.Examples where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)

import AERN2.Real.Operations
import AERN2.Real.OperationsToBall (Ball(..))
import AERN2.Real.IntegerRational ()
import AERN2.Real.Ball (ballFromRational, ballAccuracy)
import AERN2.Real.DoubleToBall ()
--import AERN2.Real.Rounded (RoundedP)
--import AERN2.Real.RoundedToBall ()
--import AERN2.Real.CauchyReal (CauchyReal(..))


ballD1 :: Ball Double
ballD1 = ballFromRational (2.0,1/3) :: Ball Double

--ballR1 :: Ball (RoundedP 1000)
--ballR1 = ballFromRational (2.0,1/3) :: Ball (RoundedP 1000) 
--
--ballRadd :: Ball (RoundedP 1000)
--ballRadd = ballR1 + ballR1
--
--ballRmul :: Ball (RoundedP 1000)
--ballRmul = ballR1 * ballR1

ballD1Accuracy :: Integer
ballD1Accuracy = ballAccuracy ballD1
--ballR1Accuracy :: Integer
--ballR1Accuracy = ballAccuracy ballR1

--cauchyThird :: CauchyReal 
