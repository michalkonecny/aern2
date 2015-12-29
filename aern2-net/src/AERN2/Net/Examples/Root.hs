{-# LANGUAGE Arrows, TypeOperators #-}
module AERN2.Net.Examples.Root where

import AERN2.Real hiding ((.), id)
import Data.String (fromString)

import AERN2.Net.Spec.Arrow
import Control.Arrow

import AERN2.Net.Execution.Direct

-- FIXME: this is terribly slow compared to rootByTrisectionNoNet
rootTestDirect :: CauchyReal
rootTestDirect =
    rootByTrisection (sqr, dom)
    where
    sqr :: UnaryFnCR
    sqr = (dom, \x -> x * x - 2)
    dom :: Interval CauchyReal
    dom = Interval (integer 1) (integer 2) 

{-|
    Precondition: @fn@ is a function with a unique root @x@ on the domain @initX@
    and @x@ is in the interior of @initX@ and the function changes sign at @x@.
    
    Compute @x@ using trisection.
-}
rootByTrisection ::
    (ArrowRealUnaryFn to r ri fn)
    =>
    (fn, ri) `to` r
rootByTrisection =
    proc (fn, xInit) ->
        do
        z <- realA (integer 0) "0" -< ()
        (l,_) <- getEndpointsA -< xInit
        fn_l <- evalAtPointUFnA -< (fn,l)
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
            -- TODO: simplify this arrow using AERN2.Net.Spec.Symbolic
            l9 <- mulConstA (integer 9) "*9" -< l
            r7 <- mulConstA (integer 7) "*7" -< r
            l9r7 <- addA -< (l9, r7)
            m1 <- mulConstA (rational $ 1/16) "/16" -< l9r7
            l7 <- mulConstA (integer 7) "*7" -< l
            r9 <- mulConstA (integer 9) "*9" -< r
            l7r9 <- addA -< (l7, r9)
            m2 <- mulConstA (rational $ 1/16) "/16" -< l7r9
            fn_m1 <- evalAtPointUFnA -< (fn, m1)
            fn_m2 <- evalAtPointUFnA -< (fn, m2)
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
        
    
    
