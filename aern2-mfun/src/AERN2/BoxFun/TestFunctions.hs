module AERN2.BoxFun.TestFunctions where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import qualified Data.List as List

import AERN2.AD.Differential
import AERN2.Util.Util
import AERN2.BoxFun.Type
import AERN2.Linear.Vector.Type ((!), Vector)
import qualified AERN2.Linear.Vector.Type as V


symmetricDomain :: Integer -> Rational -> Rational -> Vector (CN MPBall)
symmetricDomain n l r = 
    V.map (\_ -> fromEndpoints (cn $ mpBallP (prec 53) $ l) (cn $  mpBallP (prec 53) r) :: CN MPBall) $ V.enumFromTo 1 n

griewank :: Integer -> BoxFun
griewank n =
    BoxFun
    n
    (\v ->
        let
            ord = order (v ! 0)
            sm  = List.foldl' (+) (differential ord (cn $ mpBall 0 :: CN MPBall)) [let x = (v ! k) in x^2 | k <- [0 .. V.length v - 1]]
            prd = List.foldl' (*) (differential ord (cn $ mpBall 1 :: CN MPBall)) [cos $ (v ! k) / (sqrt $ 1 + mpBallP p k) | k <- [0 .. V.length v - 1]]
            p   = getPrecision v
        in
        (pure $ mpBall 1 :: CN MPBall) + ((setPrecision p $ mpBall 1)/(setPrecision p $ mpBall 4000)) * sm - prd
    )
    (symmetricDomain n (-600.0) 600.0)

rosenbrock :: BoxFun
rosenbrock =
    BoxFun
    2
    (\v ->
        let
            p = getPrecision v
            x = v ! 0
            y = v ! 1
            a = pure $ mpBallP p 1   :: CN MPBall
            b = pure $ mpBallP p 100 :: CN MPBall
            amx  = a - x
            ymxs = y - x*x
        in
            amx^2 + b*ymxs^2
    )
    (symmetricDomain 2 (-1.2) 1.2)

himmelblau :: BoxFun
himmelblau =
    BoxFun
    2
    (\v ->
        let
            x = v ! 0
            y = v ! 1
            a = (x^2 + y - (pure 11 :: CN Integer))
            b = (x + y^2 - (pure 7  :: CN Integer))
        in
            a^2 + b^2
    )
    (symmetricDomain 2 (-600.0) 600.0)

shekel :: BoxFun
shekel = 
    BoxFun
    2
    (\v ->
        let
            x   = v ! 0
            y   = v ! 1
            c0  = pure 1  :: CN Integer
            c1  = pure 17 :: CN Integer
            a00 = pure 43 :: CN Integer
            a01 = pure 23 :: CN Integer
            a10 = pure 6  :: CN Integer 
            a11 = pure 9  :: CN Integer
        in
            - (pure 1 :: CN Integer)/(c0 + (x - a00)^2 + (y - a01)^2)
            - (pure 1 :: CN Integer)/(c1 + (x - a10)^2 + (y - a11)^2)
    )
    (symmetricDomain 2 (-600.0) 600.0)

siam4 :: BoxFun
siam4 = 
    BoxFun
    2
    (\v ->
        let
            x   = v ! 0
            y   = v ! 1
        in
        exp(sin((pure 50 :: CN Integer) * x)) + sin((pure 60 :: CN Integer) * exp y) 
        + sin((pure 70 :: CN Integer) * sin(x)) + sin(sin((pure 80 :: CN Integer) * y)) 
        - sin((pure 10 :: CN Integer) * (x + y)) 
        + (x^2 + y^2) / (pure 4 :: CN Integer)
    )
    (symmetricDomain 2 (-10.0) 10.0)

ratz4 :: BoxFun
ratz4 =
    BoxFun
    2
    (\v ->
        let 
            x  = v ! 0
            y  = v ! 0
            xs = x^2
            ys = y^2
        in 
            sin(xs + (cn 2) * ys) * exp (-xs - ys)
    )
    (symmetricDomain 2 (-3.0) 3.0)