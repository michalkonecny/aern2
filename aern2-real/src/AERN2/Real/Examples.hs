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
import AERN2.Real.MPFloatToBall (rationals2MPBall, MPBall)
import AERN2.Real.CauchyReal (CauchyReal(..), rational2CauchyReal, cauchyReal2ball)


ballD1 :: Ball Double
ballD1 = rationals2ballDouble (2.0,1/300) :: Ball Double

ballDadd :: Ball Double
ballDadd = ballD1 + ballD1

ballDmul :: Ball Double
ballDmul = ballD1 * ballD1

ballR1 :: MPBall
ballR1 = rationals2MPBall (Precision 1000) (2.0,1/300) :: Ball MPFloat 

ballRadd :: MPBall
ballRadd = ballR1 + ballR1

ballRmul :: MPBall
ballRmul = ballR1 * ballR1

ballD1Accuracy :: Integer
ballD1Accuracy = ballAccuracy ballD1
ballR1Accuracy :: Integer
ballR1Accuracy = ballAccuracy ballR1

cauchyThird :: CauchyReal
cauchyThird = rational2CauchyReal (1/3) 

cauchyThirdWithAccuracy :: Integer -> MPBall
cauchyThirdWithAccuracy = cauchyReal2ball cauchyThird
