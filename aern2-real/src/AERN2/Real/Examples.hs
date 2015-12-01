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

import Prelude hiding ((+),(*),(/),(-),(^),abs,recip,div,negate,pi,toRational,fromInteger,fromRational,sqrt,cos,sin)

import AERN2.Real.Operations
import AERN2.Real.IntegerRational ()
import AERN2.Real.MPBall
import AERN2.Real.MPFloat hiding (abs, neg, getPrecision)
import AERN2.Real.CauchyReal

ballR1 :: MPBall
ballR1 = fromRationalBallP (prec 1000) (2.0,1/300) :: MPBall 

ballRadd :: MPBall
ballRadd = ballR1 + ballR1

ballRmul :: MPBall
ballRmul = ballR1 * ballR1

ballR1Accuracy :: Integer
ballR1Accuracy = getAccuracy ballR1

cauchyThird :: CauchyReal
cauchyThird = rational2CauchyReal (1/3) 

cauchyThirdWithAccuracy :: Integer -> MPBall
cauchyThirdWithAccuracy = cauchyReal2ball cauchyThird

ballPluscauchy :: MPBall
ballPluscauchy = ballR1 + pi 
