{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module AERN2.Real.Examples 
    (module AERN2.Real.Examples,
     module AERN2.Real.Operations,
     module AERN2.Real.MPFloat,
     module AERN2.Real.MPBall,
     module AERN2.Real.CauchyReal,
     module Prelude)
where

import Prelude hiding ((+),(*),(/),(-),(^),abs,recip,div,negate,pi,fromInteger,fromRational,sqrt,cos,sin)

import AERN2.Real.Operations
import AERN2.Real.IntegerRational ()
import AERN2.Real.MPBall (rationals2MPBall, MPBall, ballAccuracy, piBallUsingPrecision)
import AERN2.Real.MPFloat (Precision, prec)
import AERN2.Real.CauchyReal (CauchyReal, rational2CauchyReal, cauchyReal2ball, pi)

ballR1 :: MPBall
ballR1 = rationals2MPBall (prec 1000) (2.0,1/300) :: MPBall 

ballRadd :: MPBall
ballRadd = ballR1 + ballR1

ballRmul :: MPBall
ballRmul = ballR1 * ballR1

ballR1Accuracy :: Integer
ballR1Accuracy = ballAccuracy ballR1

cauchyThird :: CauchyReal
cauchyThird = rational2CauchyReal (1/3) 

cauchyThirdWithAccuracy :: Integer -> MPBall
cauchyThirdWithAccuracy = cauchyReal2ball cauchyThird

ballPluscauchy :: MPBall
ballPluscauchy = ballR1 + pi 
