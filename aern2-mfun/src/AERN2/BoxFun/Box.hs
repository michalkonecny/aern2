{-# OPTIONS_GHC -Wno-orphans #-}
module AERN2.BoxFun.Box where

import qualified Prelude as Prelude
import MixedTypesNumPrelude
-- import AERN2.Kleenean
import AERN2.MP.Dyadic
import AERN2.MP.Ball
import AERN2.Linear.Vector.Type (Vector, (!))
import qualified AERN2.Linear.Vector.Type as V
-- import AERN2.MP.Float
-- import Data.Maybe
-- import Debug.Trace
-- import AERN2.MP.Enclosure

import AERN2.Util.Util

type Box = Vector (CN MPBall)

-- instance HasEqAsymmetric Box Box where
--     type EqCompareType Box Box = (CN Kleenean)
--     equalTo box0 box1 = 
--         V.foldl' (&&) (cn CertainTrue) $ V.zipWith equalTo box0 box1

-- intersection :: Box -> Box -> Maybe Box
-- intersection = undefined

intersectionCertainlyEmpty :: Box -> Box -> Bool
intersectionCertainlyEmpty vx vy =
    V.foldl' (||) False $ V.zipWith ballIntersectionCertainlyEmpty vx vy
    where 
    ballIntersectionCertainlyEmpty x y = 
        (lx !>! ry || rx !<! ly)
        where
        (lx, rx) = endpointsAsIntervals x
        (ly, ry) = endpointsAsIntervals y

nonEmptyIntersection :: Box -> Box -> Box
nonEmptyIntersection vx vy = 
    V.zipWith intersectCN vx vy

instance IsBall Box where
    type CentreType (Box) = Box
    centre v = V.map (\x -> (cnMPBallP (getPrecision x) . centre) x) v

inftyNorm :: Box -> CN MPBall
inftyNorm box = 
    V.foldl' (\n x -> max n (abs x)) (cn $ mpBall 0) box

ellOneNorm :: Box -> CN MPBall
ellOneNorm box = 
    V.foldl' (\n x -> n + (abs x)) (cn $ mpBall 0) box

width :: Box -> CN MPBall
width box = 
    V.foldl' (\n x -> max n (2 * ((fmap mpBall) $ (fmap radius) x) ) ) (cn $ mpBall 0) box

widestDirection :: Box -> Integer
widestDirection box =
    aux (V.length box - 1) (V.length box - 1) (dyadic 0)
    where
    aux k i lth =
        if k < 0 then 
            i
        else
            let 
                x    = box ! k
                lth' = 
                    2 * (dyadic $ radius $ unCN x) -- TODO: unsafe
            in
                if (lth' > lth) then 
                    aux (k - 1) k lth'
                else 
                    aux (k - 1) i lth


bisect :: Integer -> Box -> (Box, Box)
bisect k box =
    if exponent_ > 0
    && (abs $ mc - lbc) < (dyadic 0.5)^exponent_ then
        (setPrecision (increasePrecision p) lb, setPrecision (increasePrecision p) rb)
    else 
        (lb, rb)
    where
    exponent_      = integer p - ilog
    NormBits ilog = 2 + (getNormLog $  mpBall $ 1 + abs lbc )
    lbc = unCN (centre $ lb ! k)
    -- rbc = unCN (centre $ rb ! k)
    mc  = unCN (centre $ m)
    increasePrecision p1 =
        p1 + (prec $ (integer p1) `Prelude.div` 2)
    lb = setPrecision p leftBox
    rb = setPrecision p rightBox
    p   = getPrecision box
    interval = box ! k
    m = (fmap $ mpBallP p) $ (fmap centre) interval
    (l, r) = endpointsAsIntervals interval
    leftBox  = V.imap (\i x -> if i == k then fromEndpointsAsIntervals l m else x) box
    rightBox = V.imap (\i x -> if i == k then fromEndpointsAsIntervals m r else x) box

-- Bisects a into 2^d boxes of the same size, where d is the dimension of the given box
fullBisect :: Box -> [Box]
fullBisect b =
    case V.length b of
        0 -> [b]
        l ->
            -- y is the dimension bisected in the current iteration
            -- x is a bisection of the previous dimension (tail recursion)
            concatMap (\x -> map (\y -> x V.+++ V.singleton y) (bisectDimension (l-1))) (fullBisect (V.take (fromIntegral (l-1)) b))

            where
                bisectDimension n = [fst bn ! n, snd bn ! n]
                    where bn = bisect n b

-- Get the endpoints of a box as a list containing a pair of MPBalls for each dimension
getEndpoints :: Box -> [(MPBall, MPBall)]
getEndpoints b  = 
    case V.length b of
        0 -> []
        _ -> endpointsAsIntervals (unCN (b ! 0)) : getEndpoints (V.drop (int 1) b)

lowerBounds :: Box -> Box
lowerBounds = V.map lowerBound

upperBounds :: Box -> Box
upperBounds = V.map upperBound

instance 
    CanNeg Box
    where
    type NegType Box = Box
    negate box = V.map (\x -> -x) box
    
createEnclosingBox :: Box -> Box -> Box
createEnclosingBox box1 box2 =
    enclosingBox
    where
        indexedBox1 = V.zip (V.fromList [0 .. integer (V.length box1) - 1]) box1
        enclosingBox = 
            V.map 
            (\(i, x) -> 
                let
                    y = box2 V.! i
                    (l, r) = endpointsAsIntervals x
                    (l', r') = endpointsAsIntervals y
                    newL = min l l'
                    newR = max r r'
                in
                    fromEndpointsAsIntervals newL newR)
            indexedBox1

intersectList :: [Box] -> Box
intersectList []            = V.empty
intersectList [b]           = b
intersectList [b1,b2]       = if intersectionCertainlyEmpty b1 b2 then V.empty else nonEmptyIntersection b1 b2
intersectList (b1:b2:bs)    = if intersectionCertainlyEmpty b1 b2 then V.empty else intersectList $ nonEmptyIntersection b1 b2 : bs