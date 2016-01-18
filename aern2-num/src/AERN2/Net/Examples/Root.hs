{-# LANGUAGE FlexibleContexts #-}
module AERN2.Net.Examples.Root where

import AERN2.Num

import AERN2.RealFunction
import Control.Arrow

import AERN2.Net.Strategy.Direct

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

{-
    The following 3 tests were executed using fresh ghci sessions on 2015-12-29 using a 2012 i7 laptop:
    
*AERN2.Net AERN2.Net.Examples.Root> cauchyReal2ball rootTestDirectFnMPBall (bits 1000)
[1.414213562373095 ± 4.429356626880511e-302]
(0.33 secs, 175505200 bytes)
    
*AERN2.Net AERN2.Net.Examples.Root> cauchyReal2ball rootTestDirectFnCR  (bits 1000)
[1.414213562373095 ± 4.429356626880511e-302]
(0.55 secs, 331385640 bytes)

*AERN2.Net AERN2.Net.Examples.Root> cauchyReal2ball rootTestNoNet   (bits 1000)
[1.414213562373095 ± 4.429356626880511e-302]
(0.59 secs, 329167792 bytes)

-}

rootTestNoNet :: CauchyReal
rootTestNoNet =
    rootByTrisectionNoNet (\ x -> x * x - 2) (Interval 1.0 2.0)

rootTestDirectFnCR :: CauchyReal
rootTestDirectFnCR =
    rootByTrisection (sqr, dom)
    where
    sqr :: UnaryFnCR
    sqr = (dom, fn)
        where
        fn x 
            | shouldTrace = mapCauchyRealUnsafe tr $ x * x - 2
            | otherwise = x * x - 2
            where
            tr ac b =
                trace
                (
                    "sqr: ac = " ++ show ac 
                    ++ "; x = " ++ show (cauchyReal2ball x ac) 
                    ++ "; result = " ++ show b
                )
                b
    dom = Interval 1.0 2.0 

rootTestDirectFnMPBall :: CauchyReal
rootTestDirectFnMPBall =
    rootByTrisection (sqr, dom)
    where
    sqr :: UnaryFnMPBall
    sqr = (dom, fn)
        where
        fn x 
            | shouldTrace = tr $ x * x - 2
            | otherwise = x * x - 2
            where
            tr b =
                trace
                (
                    "sqr: x = " ++ show x ++ "; result = " ++ show b
                )
                b
    dom = Interval 1.0 2.0 

{-|
    Precondition: @fn@ is a function with a unique root @x@ on the domain @initX@
    and @x@ is in the interior of @initX@ and the function changes sign at @x@.
    
    Compute @x@ using trisection.
-}
rootByTrisection ::
    (RealUnaryFnA to fn, 
     CanLimitA to (Interval (UFnIn fn), UFnOut fn),
     LimitTypeA to (Interval (UFnIn fn), UFnOut fn) ~ UFnOut fn,
     Bool ~ OrderCompareTypeA to (UFnOut fn) (UFnOut fn),
     CanEmbedFnA to Rational (UFnIn fn), 
     HasParallelComparisonsA to (UFnOut fn))
    =>
    (fn, Interval (UFnIn fn)) `to` (UFnOut fn)
rootByTrisection =
    proc (fn, xInit@(Interval l _)) ->
        do
        z <- convertNamedA "0" -< 0
        fn_l <- evalAtPointUFnA -< (fn,l)
        isPositiveAtL <- lessThanA -< (z, fn_l)
        iterateLimWithA aux -< ((xInit, z), (z, isPositiveAtL, fn))
--        result <- limitIntervalsToRealA -< sq
--        returnA -< result
    where
    aux =
        proc (((Interval l r), _), (z, isPositiveAtL, fn)) ->
            do
            (m, fn_m) <- splitAwayFromRoot -< (fn,l,r)
            isPositiveAtM <- lessThanA -< (z, fn_m)
            let _ = [z, fn_m]
            xNew <- if isPositiveAtM == isPositiveAtL
                then do
                    returnA -< Interval m r
                else do
                    returnA -< Interval l m
            aux -< ((xNew, z), (z, isPositiveAtL, fn))
    splitAwayFromRoot =
        proc (fn,l,r) ->
            do
            m1 <- embedFnNamedA "m1" getM1 -< [l,r] 
            m2 <- embedFnNamedA "m2" getM2 -< [l,r] 
            fn_m1 <- evalAtPointUFnA -< (fn, m1)
            fn_m2 <- evalAtPointUFnA -< (fn, m2)
            Just (fn_m, m) <- pickNonZeroA -< [(fn_m1, m1), (fn_m2, m2)]
            let _ = [l,r,m1,m2]
            returnA -< (m, fn_m)
        where
        getM1 :: [Rational] -> Rational
        getM1 [l, r] = (9*l + 7*r)/16
        getM1 _ = error "getM1"
        getM2 :: [Rational] -> Rational
        getM2 [l, r] = (7*l + 9*r)/16
        getM2 _ = error "getM2"

{-|
    Precondition: @fn@ is a function with a unique root @x@ on the domain @initX@
    and @x@ is in the interior of @initX@ and the function changes sign at @x@.
    
    Compute @x@ using trisection.
    
    This version does not use networks. 
-}
rootByTrisectionNoNet ::
    (CauchyReal -> CauchyReal) {-^ @fn@ -} -> 
    Interval Rational {-^ @initX@ -} -> 
    CauchyReal {-^ @x@ -}
rootByTrisectionNoNet fn initX@(Interval initL _) =
    convergent2CauchyReal Nothing $ aux initX
    where
    isDecreasing = fn (cauchyReal initL) > 0
    aux xPrev@(Interval l r) =
        (rati2MPBall xPrev) : (aux xNew)
        where
        m1 = (9*l + 7*r)/16
        m2 = (7*l + 9*r)/16
        Just (fn_m, m) = pickNonZeroA [(fn (cauchyReal m1), m1), (fn (cauchyReal m2), m2)]
        isPositiveAtM = fn_m > 0
        xNew 
            | (isDecreasing == isPositiveAtM) = 
                Interval m r
            | otherwise = 
                Interval l m
        
    
    
