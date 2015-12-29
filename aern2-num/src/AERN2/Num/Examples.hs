module AERN2.Num.Examples 
    (module AERN2.Num.Examples,
     module AERN2.Num.Operations,
     module AERN2.Num.MPBall,
     module AERN2.Num.CauchyReal)
where

import AERN2.Num.Operations
import AERN2.Num.IntegerRational ()
import AERN2.Num.MPBall
import AERN2.Num.CauchyReal
import AERN2.Num.Accuracy

ball1 :: MPBall
ball1 = rationalBall2BallP (prec 1000) (2.0,1/300) 

ball2 :: MPBall
ball2 = integer (5^100)

balladd :: MPBall
balladd = ball1 + ball1

ballmul :: MPBall
ballmul = ball1 * ball1

ball1Accuracy :: Accuracy
ball1Accuracy = getAccuracy ball1

ballComp1 :: Maybe Bool
ballComp1 = ball1 < ballmul

ballComp2 :: Maybe Bool
ballComp2 = ball1 == ball1

cauchyThird :: CauchyReal
cauchyThird = rational (1/3) 

cauchyThirdWithAccuracy :: Accuracy -> MPBall
cauchyThirdWithAccuracy = cauchyReal2ball cauchyThird

cauchyArithmetic :: CauchyReal
cauchyArithmetic = 1 + pi + cos(pi/3)

ballPlusCauchy :: MPBall
ballPlusCauchy = ball1 + cauchyArithmetic
