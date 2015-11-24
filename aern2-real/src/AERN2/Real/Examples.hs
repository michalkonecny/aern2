{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module AERN2.Real.Examples where

import Prelude hiding ((+),(*),(/),(-),fromInteger,fromRational)

import AERN2.Real.Operations (fromRational)
import AERN2.Real.OperationsToBall (Ball(..))
import AERN2.Real.Ball (ballFromRational)
--import AERN2.Real.DoubleBound ()
import AERN2.Real.Rounded (RoundedP)
import AERN2.Real.RoundedToBall ()
--import AERN2.Real.Double ()
import AERN2.Real.DoubleToBall ()

ballD1 :: Ball Double
ballD1 = ballFromRational (2.0,0.1) :: Ball Double

ballR1 :: Ball (RoundedP 1000)
ballR1 = ballFromRational (2.0,0.1) :: Ball (RoundedP 1000) 
