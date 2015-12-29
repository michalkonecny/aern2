{-# LANGUAGE Arrows, TypeOperators #-}
module AERN2.Net.Examples.Root where

import AERN2.Num
import Data.String (fromString)

import AERN2.Net.Spec.Arrow
import Control.Arrow

import AERN2.Net.Execution.Direct

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

*AERN2.Net AERN2.Net.Examples.Root> cauchyReal2ball rootTestDirectNoNet   (bits 1000)
[1.414213562373095 ± 4.429356626880511e-302]
(0.59 secs, 329167792 bytes)

-}

rootTestDirectNoNet :: CauchyReal
rootTestDirectNoNet =
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
    dom :: UFnDom UnaryFnCR
    dom = Interval (integer 1) (integer 2) 

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
    dom :: UFnDom UnaryFnMPBall
    dom = Interval (integer 1) (integer 2) 

{-|
    Precondition: @fn@ is a function with a unique root @x@ on the domain @initX@
    and @x@ is in the interior of @initX@ and the function changes sign at @x@.
    
    Compute @x@ using trisection.
-}
rootByTrisection ::
    (ArrowRealUnaryFn to fn, UFnDomR fn ~ UFnR fn)
    =>
    (fn, UFnDom fn) `to` (UFnR fn)
rootByTrisection =
    proc (fn, xInit) ->
        do
        z <- realConstA "0" (integer 0) -< ()
        (l,_) <- getEndpointsA -< xInit
        fn_l <- evalAtUFnDomEA -< (fn,l)
        isDecreasing <- lessA -< (z, fn_l)
        sq <- aux -< (z, isDecreasing, fn, xInit)
        result <- limitIntervalsToRealA -< sq 
        returnA -< result
    where
    aux =
        proc (z, isDecreasing, fn, xPrev) ->
            do
            (l,r) <- getEndpointsA -< xPrev
            (m, fn_m) <- splitAwayFromRoot -< (fn,l,r)
            isPositiveAtM <- lessA -< (z, fn_m)
            xNew <- if isPositiveAtM == isDecreasing
                then do
                    res <- fromEndpointsA -< (m,r)
                    returnA -< res
                else do
                    res <- fromEndpointsA -< (l,m)
                    returnA -< res
            restX <- aux -< (z, isDecreasing, fn, xNew)
            returnA -< (xPrev : restX)
    splitAwayFromRoot =
        proc (fn,l,r) ->
            do
            m1 <- rationalOpA "m1" (\[l,r] -> (9*l + 7*r)/16) -< [l,r] 
            m2 <- rationalOpA "m2" (\[l,r] -> (7*l + 9*r)/16) -< [l,r] 
            fn_m1 <- evalAtUFnDomEA -< (fn, m1)
            fn_m2 <- evalAtUFnDomEA -< (fn, m2)
            (fn_m, m) <- pickNonZeroA -< [(fn_m1, m1), (fn_m2, m2)]
            returnA -< (m, fn_m)

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
    convergent2CauchyReal $ aux initX
    where
    isDecreasing = fn (rational initL) > 0
    aux xPrev@(Interval l r) =
        (rati2MPBall xPrev) : (aux xNew)
        where
        m1 = (9*l + 7*r)/16
        m2 = (7*l + 9*r)/16
        (fn_m, m) = pickNonZeroReal [(fn (rational m1), m1), (fn (rational m2), m2)]
        isPositiveAtM = fn_m > 0
        xNew 
            | (isDecreasing == isPositiveAtM) = 
                Interval m r
            | otherwise = 
                Interval l m
        
    
    
