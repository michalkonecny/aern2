module AERN2.BoxFun.Type where

import MixedTypesNumPrelude

import AERN2.Linear.Vector.Type (Vector, (!))
import qualified AERN2.Linear.Vector.Type as V
import AERN2.MP.Ball
import AERN2.AD.Differential
import qualified AERN2.Linear.Matrix.Type as M
import AERN2.Util.Util
import AERN2.BoxFun.Box
import Numeric.CollectErrors

data BoxFun =
    BoxFun
    {
            dimension :: Integer
        ,   bf_eval   :: Vector (Differential (CN MPBall)) -> Differential (CN MPBall)
        ,   domain    :: Vector (CN MPBall)
    }

{-
    `bf_eval`  encodes how the function changes values and how it composes functions computing its parameters
    in terms of the first and second differentials.

    Order 1:
    point x in Rn and direction dx in Rn 
    |-> 
    y and dy

    Order 2:
    order 1 +
    another direction dxt (usually equal to dx) and a direction showing how the direction is changing
    |->
    dyt and d2y
-}

instance Show BoxFun where
    show (BoxFun d _f b) = show d ++ " dimensional BoxFun, domain: " ++ show b

boundaryRestrictions :: BoxFun -> [BoxFun]
boundaryRestrictions (BoxFun d ev dom) =
    concat
    [
        [
            BoxFun 
                (d - 1)
                (
                \v ->
                    ev $ V.map (\j -> if j == i then (setPrecision $ getPrecision v) $ differential 2 $ upperBound (dom ! i) else if j < i then v ! j else v ! (j - 1)) $ V.enumFromTo 0 (d - 1)
                ) 
                (V.map (\j -> if j >= i then dom ! (j + 1) else dom ! j) $ V.enumFromTo 0 (d - 2))
            ,
            BoxFun 
                (d - 1)
                (
                    \v ->
                        ev $ V.map (\j -> if j == i then (setPrecision $ getPrecision v) $ differential 2 $ lowerBound (dom ! i) else if j < i then v ! j else v ! (j - 1)) $ V.enumFromTo 0 (d - 1)
                )
                (V.map (\j -> if j >= i then dom ! (j + 1) else dom ! j) $ V.enumFromTo 0 (d - 2))
        ]
        |
        i <- [0 .. d - 1]
    ]

valueGradientHessian :: BoxFun -> Vector (CN MPBall) -> (CN MPBall, Vector (CN MPBall), M.Matrix (CN MPBall))
valueGradientHessian (BoxFun d e _) v =
    (value, grad, hess)
    where
    vgh i j = e (w i j)

    triangle =  
        V.map (\i-> V.map (\j -> vgh i j) $ V.enumFromTo 0 i) $ V.enumFromTo 0 (d - 1)

    value = diff_x $ (triangle ! 0) ! 0
    grad  = V.map (\i -> diff_dx $ (triangle ! i) ! 0) $ V.enumFromTo 0 (d - 1)
    hess  = M.create d d (\i j -> diff_d2x $ if i > j then (triangle ! i) ! j else (triangle ! j) ! i)

    w i j = V.imap (\k x -> OrderTwo x (delta i k) (delta j k) (pure $ mpBall 0)) v
    delta :: Integer -> Integer -> CN MPBall
    delta i k = if i == k then (cn $ mpBall 1) else (cn $ mpBall 0)

valueGradient :: BoxFun -> Vector (CN MPBall) -> (CN MPBall, Vector (CN MPBall))
valueGradient (BoxFun d e _) v =
    aux (d - 1) [] (pure $ mpBall 0)
    where
    tangent k = 
        V.imap (\i x -> OrderOne x (delta i k)) v
    valgrad k =
        let
            etk = e (tangent k)
        in 
            (diff_x etk, diff_dx etk)
    aux k ret val =
        if k < 0 then
            (val, V.fromList ret)
        else 
            let
                (val2,g) = valgrad k
            in
            aux (k - 1) (g : ret) val2
    delta :: Integer -> Integer -> CN MPBall
    delta i k = if i == k then (cn $ mpBall 1) else (cn $ mpBall 0)

apply :: BoxFun -> Vector (CN MPBall) -> CN MPBall
apply (BoxFun _d e _) v = 
    diff_x (e v')
    where
    v' = V.map (\x -> differential 0 x) v

applyMinimum :: BoxFun -> CN MPBall
applyMinimum h = fst $ endpointsAsIntervals (apply h (domain h))

applyMinimumOnBox :: BoxFun -> Vector (CN MPBall) -> CN MPBall
applyMinimumOnBox h hbox = fst $ endpointsAsIntervals (apply h hbox)

applyMaximum :: BoxFun -> CN MPBall
applyMaximum h = snd $ endpointsAsIntervals (apply h (domain h))

applyMaximumOnBox :: BoxFun -> Vector (CN MPBall) -> CN MPBall
applyMaximumOnBox h hbox = snd $ endpointsAsIntervals (apply h hbox)

gradient :: BoxFun -> Vector (CN MPBall) -> Vector (CN MPBall)
gradient (BoxFun d e _) v =
    aux (d - 1) []
    where
    tangent k = 
        V.imap (\i x -> OrderOne x (delta i k)) v
    grad k =
        -- diff_dx $ e (tangent k)
        case e (tangent k) of
            OrderZero _ -> noValueNumErrorCertain (NumError "details")
            val -> diff_dx $ val
    aux k ret =
        if k < 0 then
            V.fromList ret
        else 
            aux (k - 1) (grad k : ret)
    delta :: Integer -> Integer -> CN MPBall
    delta i k = if i == k then (cn $ mpBall 1) else (cn $ mpBall 0)

hessian :: BoxFun -> Vector (CN MPBall) -> M.Matrix (CN MPBall)
hessian (BoxFun d e _) v = 
    M.create d d a
    where
    a i j = diff_d2x $ e (w i j)
    w i j = V.imap (\k x -> OrderTwo x (delta i k) (delta j k) (pure $ mpBall 0)) v
    delta :: Integer -> Integer -> CN MPBall
    delta i k = if i == k then (cn $ mpBall 1) else (cn $ mpBall 0)

jacobian :: [BoxFun] -> Vector (CN MPBall) -> M.Matrix (CN MPBall)
jacobian fs v =
    M.create (length fs) highestDiminseionInFs a
    where
    highestDiminseionInFs = maximum (map dimension fs)

    a i j = diff_dx $ bf_eval (fs!!i) (w j)
    w j = V.imap (\k x -> OrderOne x (delta j k)) v
    delta :: Integer -> Integer -> CN MPBall
    delta i k = if i == k then (cn $ mpBall 1) else (cn $ mpBall 0)

gradientUsingGradient :: BoxFun -> Box -> Box
gradientUsingGradient f v =
    V.zipWith fromEndpointsAsIntervals lowerBounds upperBounds
    where
        lowerBounds = firstDerivatives - secondDerivatives * V.map (fmap (mpBall . radius)) v
        upperBounds = firstDerivatives + secondDerivatives * V.map (fmap (mpBall . radius)) v
        firstDerivatives  = head $ M.rows $ jacobian [f] (centre v)
        secondDerivatives = fmap abs $ hessian f v
        p = getPrecision v
        