module AERN2.Linear.Matrix.Inverse 
(inverse) 
where

import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN

import AERN2.Linear.Matrix.Type
import qualified AERN2.Linear.Vector.Type as V
import Data.Maybe
import AERN2.MP.Float
import AERN2.MP.Dyadic
import AERN2.MP

inverse :: Matrix (CN MPBall) -> Maybe (Matrix (CN MPBall))
inverse m = 
    if  CN.hasError m
    ||  (isNothing maybeY)
    ||  (not $ nerr !<! 1) then
        Nothing
    else
        Just $ aux x0
    where
    cm     = Matrix (width m) (V.map (\x -> mpFloat $ centre $ unCN x) $ entries m)
    idM    = identity (width m) (width m)                    :: Matrix (CN MPBall)
    maybeY = inverseGauss cm
    y      = fmap (cn . mpBall . dyadic) $ fromJust maybeY
    yid    = y * idM
    nye    = V.map V.inftyNorm (columns yid)
    err    = idM - y * m
    nerr   = inftyNorm err
    ui     = cn $ fromEndpointsAsIntervals (mpBall $ -1) (mpBall 1)
    x0     = create (width m) (width m) (\_ j -> (ui *  (nye V.! j)) / (1 - nerr))
    aux x  = let x' = it x in if getAccuracy x' <= getAccuracy x then x' else aux x'
    it x   = intersect x $ yid + err * x

inverseGauss :: Matrix MPFloat -> Maybe (Matrix MPFloat)
inverseGauss m = 
    aux 0 m inv0
    where
    inv0        = identity (width m) (width m) :: Matrix MPFloat
    aux j n inv = 
        if j == width m then
            Just inv
        else
            let
                n'    = pivot n j n
                inv'  = pivot n j inv
                a     = get n' j j 
                ia    = ((mpFloat 1)/.a)
                n''   = multiplyRow n'   j ia 
                inv'' = multiplyRow inv' j ia 
                elim i l = 
                    if i /= j then
                        let
                            b = get l i j
                        in
                            addRows l (-b) j i
                    else 
                        l
                n'''   = 
                    foldr elim n'' [0 .. (height m) - 1]
                inv''' = 
                    foldr elim inv'' [0 .. (height m) - 1]
            in 
            if a /= mpFloat 0 then
                aux (j + 1) n''' inv'''
            else 
                Nothing


largestRowIndex :: Matrix MPFloat -> Integer -> Integer
largestRowIndex m j =
    integer $ aux (j + 1) j (get m j j)
    where
    h = height m
    aux k i x =
        if k == h then
            i
        else 
            let
                x' = get m i j
            in 
            if x' > x then
                aux (k + 1) k x'
            else 
                aux (k + 1) i x
    
pivot :: Matrix MPFloat -> Integer -> Matrix MPFloat -> Matrix MPFloat
pivot m j n = 
    swapRows n j i
    where
    i = largestRowIndex m j

multiplyRow :: Matrix MPFloat -> Integer -> MPFloat -> Matrix MPFloat
multiplyRow m i a = 
    imap h m
    where
    h k _ x = 
        if k == i then 
            a *. x
        else 
            x

{- add a*(row i0) to row i1 -}
addRows :: Matrix MPFloat -> MPFloat -> Integer -> Integer -> Matrix MPFloat
addRows m a i0 i1 = 
    imap h m
    where
    h k l x = 
        if k == i1 then 
            x +. a *. get m i0 l
        else 
            x

swapRows :: Matrix MPFloat -> Integer -> Integer -> Matrix MPFloat
swapRows m i0 i1 =
    imap h m
    where
    h k l x = 
        if k == i0 then 
            get m i1 l
        else if k == i1 then
            get m i0 l
        else 
            x