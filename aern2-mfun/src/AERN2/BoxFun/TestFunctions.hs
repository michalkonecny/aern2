module AERN2.BoxFun.TestFunctions where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import qualified Data.List as List

import AERN2.AD.Differential
import AERN2.BoxFun.Type
import AERN2.Linear.Vector.Type ((!), Vector)
import qualified AERN2.Linear.Vector.Type as V

fromListDomain :: [(Rational, Rational)] -> Vector (CN MPBall)
fromListDomain [] = V.empty
fromListDomain [x] = V.singleton $ fromEndpointsAsIntervals (cn $ mpBallP (prec 53) $ (fst x)) (cn $  mpBallP (prec 53) (snd x))
fromListDomain (x:xs) = V.cons (fromEndpointsAsIntervals (cn $ mpBallP (prec 53) $ (fst x)) (cn $  mpBallP (prec 53) (snd x))) (fromListDomain xs)

symmetricDomain :: Integer -> Rational -> Rational -> Vector (CN MPBall)
symmetricDomain n l r = 
    V.map (\_ -> fromEndpointsAsIntervals (cn $ mpBallP (prec 53) $ l) (cn $  mpBallP (prec 53) r)) $ V.enumFromTo 1 n

griewank :: Integer -> BoxFun
griewank n =
    BoxFun
    n
    (\v ->
        let
            ord = order (v ! 0)
            sm  = List.foldl' (+) (differential ord (cn $ mpBall 0)) [let x = (v ! k) in x^2 | k <- [0 .. V.length v - 1]]
            prd = List.foldl' (*) (differential ord (cn $ mpBall 1)) [cos $ (v ! k) / (sqrt $ 1 + mpBallP p k) | k <- [0 .. V.length v - 1]]
            p   = getPrecision v
        in
        1 + ((setPrecision p $ mpBall 1)/(setPrecision p $ mpBall 4000)) * sm - prd
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
            a = mpBallP p 1
            b = mpBallP p 100
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
            a = (x^2 + y - 11)
            b = (x + y^2 - 7)
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
            c0  = 1;  a00 = 43; a01 = 23
            c1  = 17; a10 = 6;  a11 = 9
        in
            - 1/(c0 + (x - a00)^2 + (y - a01)^2)
            - 1/(c1 + (x - a10)^2 + (y - a11)^2)
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
        exp(sin(50 * x)) + sin(60 * exp y) 
        + sin(70 * sin(x)) + sin(sin(80 * y)) 
        - sin(10 * (x + y)) 
        + (x^2 + y^2) / 4
    )
    (symmetricDomain 2 (-10.0) 10.0)

ratz4 :: BoxFun
ratz4 =
    BoxFun
    2
    (\v ->
        let 
            x  = v ! 0
            y  = v ! 1
            xs = x^2
            ys = y^2
        in 
            sin(xs + 2 * ys) * exp (-xs - ys)
    )
    (symmetricDomain 2 (-3.0) 3.0)

-- global minimum at bukin(-10, 1) ~ 0
-- bukin :: BoxFun
-- bukin =
--     BoxFun
--     2
--     (\v ->
--         let
--             x = v!0
--             y = v!1
--         in
--             100 * sqrt (abs (y - x^2 / 100)) + abs(x + 10) / 100
--     )
--     (fromListDomain [(-15.0, 5.0), (-3.0, 3.0)])

ackley :: BoxFun
ackley =
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            p = getPrecision v
            pi = piBallP p
        in
            -20 * exp(sqrt((x^2 + y^2) / 2) / (-5)) - exp((cos (2 * pi * x) + cos(2 * pi * y)) / 2) + exp(mpBallP p 1) + 20
    )
    (symmetricDomain 2 (-5.0) 5.0)

-- eggholder :: BoxFun
-- eggholder =
--     BoxFun
--     2
--     (\v ->
--         let
--             x = v!0
--             y = v!1
--         in
--             -(y + 47) * sin (sqrt (abs (x / 2 + (y + 47)))) - x * sin (sqrt (abs (x - (y + 47))))
--     )
--     (symmetricDomain 2 (-512.0) 512.0)
    
-- heron :: BoxFun
-- heron = 
--     BoxFun
--     2
--     (\v ->
--         let
--             x = v!0
--             y = v!1
--             p = getPrecision v
--             eps = 1/2^(23)
--             i = 2
--         in
--             max
--                 min
--                     max
--                         (y - sqrt x)
--                         ((sqrt x - y) - (mpBallP p 1/2)^(2^(i-1)) - 6 * eps * (i-1))
--                     max
--                         (sqrt x - y)
--                         (- (sqrt x - y) - (mpBallP p 1/2)^(2^(i-1)) - 6 * eps * (i-1))
--                 min
--                     max
--                         ((y + x/y)/2 - sqrt x)
--                         (- (sqrt x - (y+x/y)/2) + (mpBallP p 1/2)^(2^i) + 6 * eps * (i-1))
--                     max
--                         (sqrt x - (y+x/y)/2)
--                         ((sqrt x - (y+x/y)/2) + (mpBallP p 1/2)^(2^i) + 6 * eps * (i-1))

--     )
--     (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

-- max (min (max 1p 1q) (max 2p 2q)) (min (max 3p 3q) (max 4p 4q))

i :: Integer
i = 3

heron1p :: BoxFun
heron1p = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            _p = getPrecision v
            _eps = 1/2^(23)
        in
            (y - sqrt x)
     )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

    
heron1q :: BoxFun
heron1q = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            p = getPrecision v
            eps = 1/2^(23)
        in
            ((sqrt x - y) - (mpBallP p 1/2)^(2^(i-1)) - (mpBallP p 6) * eps * (i-1))

    )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

    
heron2p :: BoxFun
heron2p = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            _p = getPrecision v
            _eps = 1/2^(23)
        in
            (sqrt x - y)
    )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

    
heron2q :: BoxFun
heron2q = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            p = getPrecision v
            eps = 1/2^(23)
        in
            (- (sqrt x - y) - (mpBallP p 1/2)^(2^(i-1)) - (mpBallP p 6) * eps * (i-1))

    )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

    
heron3p :: BoxFun
heron3p = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            _p = getPrecision v
            _eps = 1/2^(23)
        in
            ((y + x/y)/2 - sqrt x)
    )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])
    
heron3q :: BoxFun
heron3q = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            p = getPrecision v
            eps = 1/2^(23)
        in
            (- (sqrt x - (y+x/y)/2) + (mpBallP p 1/2)^(2^i) + (mpBallP p 6) * eps * (i-1))
    )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

    
heron3p2 :: BoxFun
heron3p2 = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            _p = getPrecision v
            _eps = 1/2^(23)
            _i = 4
        in
            ((y + x/y)/2 - sqrt x)
    )
    (fromListDomain [(0.633758544921875, 0.63385009765625), (0.79999999999999982236431605997495353221893310546875, 0.80006103515624982239142111428709114306911942549049854278564453125)])
    
heron3q2 :: BoxFun
heron3q2 = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            p = getPrecision v
            eps = 1/2^(23)
            _i = 4
        in
            (- (sqrt x - (y+x/y)/2) + (mpBallP p 1/2)^(2^i) + (mpBallP p 6) * eps * (i-1))
    )
    (fromListDomain [(0.633758544921875, 0.63385009765625), (0.79999999999999982236431605997495353221893310546875, 0.80006103515624982239142111428709114306911942549049854278564453125)])

heron4p :: BoxFun
heron4p = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            _p = getPrecision v
            _eps = 1/2^(23)
        in
            (sqrt x - (y+x/y)/2)
    )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

    
heron4q :: BoxFun
heron4q = 
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            p = getPrecision v
            eps = 1/2^(23)
        in
            ((sqrt x - (y+x/y)/2) + (mpBallP p 1/2)^(2^i) + (mpBallP p 6) * eps * (i-1))

    )
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])

-- heronFull :: BoxFun
-- heronFull =
--     BoxFun
--     3
--     (\v ->
--         let
--             x = v!0
--             y = v!1
--             i = v!2
--             p = getPrecision v
--             eps = 1/2^(23)
--         in
--             ((sqrt x - (y+x/y)/2) + (mpBallP p 1/2)^(2^i) + (mpBallP p 6) * eps * (i-1))

--     )
--     (fromListDomain [(0.5, 2.0), (0.8, 1.8), (1.0, 5.0)])


mxp1 :: BoxFun
mxp1 =
    BoxFun
    2
    (\v ->
        let
            x = v!0
            -- y = v!1
        in
            -x+1

    )
    (fromListDomain [(0.0, 2.0), (0.0, 2.0)])

xm1 :: BoxFun
xm1 =
    BoxFun
    2
    (\v ->
        let
            x = v!0
            -- y = v!1
        in
            x-1

    )
    (fromListDomain [(0.0, 2.0), (0.0, 2.0)])

xe2p1 :: BoxFun
xe2p1 =
    BoxFun
    2
    (\v ->
        let
            x = v!0
            -- y = v!1
        in
            x^2+1
    )
    (fromListDomain [(0.0, 2.0), (0.0, 2.0)])

xe2m1 :: BoxFun
xe2m1 =
    BoxFun
    2
    (\v ->
        let
            x = v!0
            -- y = v!1
        in
            x^2-1
    )
    (fromListDomain [(0.0, 2.0), (0.0, 2.0)])