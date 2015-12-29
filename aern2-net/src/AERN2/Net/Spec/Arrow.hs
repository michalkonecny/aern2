{-# LANGUAGE Arrows, TypeOperators, GeneralizedNewtypeDeriving, FunctionalDependencies, DataKinds, FlexibleContexts #-}

module AERN2.Net.Spec.Arrow 
(
    _anet0, _anet1, _anet2, _anet3,
    ArrowRational(..),
    ArrowReal(..),
    piA,
    ArrowComplex(..),
    Complex(..),
    ArrowRationalInterval(..),
    ArrowRealUnaryFn(..),
    UFnDomE, UFnDomR,
    ArrowRealFn(..),
    FnDomE, FnDomR,
    VarName, VarMap,
    ArrowRealUnaryFnFromArrow(..),
    ArrowRealFnFromArrow(..),
    mapA, zipWithA
)
where

import AERN2.Real hiding (id, (.))
import Data.String (IsString(..),fromString)

--import Control.Category
import Control.Arrow
import qualified Data.Map as Map


{- mini examples -}

-- | sqrt(pi) + pi
_anet0 :: (ArrowReal to r) => () `to` r
_anet0 =
    proc _ -> do
        p <- piA -< ()
        sp <- sqrtA -< p
        psp <- addA -< (p,sp)
        returnA -< psp

-- | pi * sqrt(x) * x
_anet1 :: (ArrowReal to r) => r `to` r
_anet1 =
    proc x -> do
        p <- piA -< ()
        sx <- sqrtA -< x
        psx <- mulA -< (p,sx)
        psxx <- mulA -< (psx,x)
        returnA -< psxx

-- | sqrt(x^2+y^2+z^2)    
_anet2 :: (ArrowReal to r) => (r,r,r) `to` r
_anet2 =
    proc (x,y,z) -> do
        x2 <- mulA -< (x,x)
        y2 <- mulA -< (y,y)
        z2 <- mulA -< (z,z)
        x2y2 <- addA -< (x2,y2)
        x2y2z2 <- addA -< (x2y2, z2)
        r <- sqrtA -< x2y2z2
        returnA -< r


-- | sqrt(x^2+y^2+z^2)
_anet3 :: (ArrowReal to r) => (Map.Map String r) `to` r
_anet3 =
    proc valMap -> do
        let (Just x) = Map.lookup "x" valMap
        let (Just y) = Map.lookup "y" valMap
        let (Just z) = Map.lookup "z" valMap
        x2 <- mulA -< (x,x)
        y2 <- mulA -< (y,y)
        z2 <- mulA -< (z,z)
        x2y2 <- addA -< (x2,y2)
        x2y2z2 <- addA -< (x2y2, z2)
        r <- sqrtA -< x2y2z2
        returnA -< r

{-| An arrow enriched with real arithmetic operations. -}
class (ArrowChoice to) => ArrowRational to r where
    lessA :: (r,r) `to` Bool
    leqA :: (r,r) `to` Bool
    rationalConstA :: String -> Rational -> (() `to` r)
    rationalOpA ::  String -> ([Rational] -> Rational) -> ([r] `to` r) -- use a Rational computation, bypassing the arrow 
    addA :: (r,r) `to` r
    mulA :: (r,r) `to` r

class (ArrowRational to r) => ArrowReal to r where
    pickNonZeroA :: [(r,a)] `to` (r,a)
    realConstA :: String -> CauchyReal -> (() `to` r) -- TODO: change () to (SizeLimits r)
    realOpA ::  String -> ([CauchyReal] -> CauchyReal) -> ([r] `to` r) -- use a CauchyReal computation, bypassing the arrow 
    addRealA :: CauchyReal -> String -> r `to` r
    mulRealA :: CauchyReal -> String -> r `to` r
    sqrtA :: r `to` r
    expA :: r `to` r
-- TODO: add more operations

piA :: (ArrowReal to r) => () `to` r
piA = realConstA "pi" pi

class (ArrowReal to c) => ArrowComplex to c where
    complexConstA :: String -> Complex -> (() `to` c) -- TODO: change () to (SizeLimits c)
    complexOpA ::  String -> ([Complex] -> Complex) -> ([c] `to` c) -- use a Complex computation, bypassing the arrow 
    addComplexA :: Complex -> String -> c `to` c
    mulComplexA :: Complex -> String -> c `to` c

-- TODO: move Complex to aern2-real
data Complex = CauchyReal :+ CauchyReal 

class (ArrowRational to (IntervalE ri)) => ArrowRationalInterval to ri where
    type IntervalE ri
    type IntervalR ri
    getEndpointsA :: ri `to` (IntervalE ri,IntervalE ri)
    fromEndpointsA :: (IntervalE ri,IntervalE ri) `to` ri
    limitIntervalsToRealA :: [ri] `to` IntervalR ri 
--    splitIntervalA :: ri `to` (ri, ri)
--    subEqIntervalA :: (ri, ri) `to` Bool

type UFnDomE f = IntervalE (UFnDom f)
type UFnDomR f = IntervalR (UFnDom f)

class (ArrowRationalInterval to (UFnDom f), ArrowReal to (UFnR f)) => ArrowRealUnaryFn to f where
    type UFnDom f
    type UFnR f
    constUFnA :: (UFnDom f, UFnR f) `to` f
    projUFnA :: (UFnDom f) `to` f
    getDomainUFnA :: f `to` (UFnDom f)
    evalAtPointUFnA :: (f, UFnR f) `to` (UFnR f)
    evalAtUFnDomEA :: (f, UFnDomE f) `to` (UFnR f)
    evalOnIntervalUFnA :: (f, UFnDom f) `to` (UFnDom f)

newtype VarName = VarName String
    deriving (IsString, Eq, Ord, Show)

type VarMap = Map.Map VarName

type FnDomE f = IntervalE (FnDom f)
type FnDomR f = IntervalR (FnDom f)

class (ArrowRationalInterval to (FnDom f), ArrowReal to (FnR f)) => ArrowRealFn to f where
    type FnDom f
    type FnR f
    constFnA :: (VarMap (FnDom f), FnR f) `to` f
    projFnA :: (VarMap (FnDom f), VarName) `to` f
    getDomainA :: f `to` (VarMap (FnDom f))
    evalAtPointFnA :: (f,VarMap (FnR f)) `to` (FnR f)
    evalAtFnDomEA :: (f,VarMap (FnDomE f)) `to` (FnR f)
    evalOnIntervalA :: (f,VarMap (FnDom f)) `to` ri
    fixSomeVariablesA :: (f, VarMap (FnR f)) `to` f

class (ArrowRealUnaryFn to f) => ArrowRealUnaryFnFromArrow to f where
    encloseUFn :: (ArrowReal to2 r2) => (r2 `to2` r2, UFnDom f) -> () `to` f

class (ArrowRealFn to f) => ArrowRealFnFromArrow to f where
    encloseFn :: (ArrowReal to2 r2) => (VarMap r2 `to2` r2, VarMap (FnDom f)) -> () `to` f

{- Utilities for arrow programming -}

mapA :: (ArrowChoice to) => (a `to` b) -> ([a] `to` [b])
mapA processOne =
    proc xs ->
        case xs of
            [] -> returnA -< []
            (x : xrest) -> 
                do
                y <- processOne -< x
                yrest <- mapA processOne -< xrest
                returnA -< y : yrest

zipWithA :: (ArrowChoice to) => ((a,b) `to` c) -> (([a],[b]) `to` [c])
zipWithA processOne =
    proc (xs, ys) ->
        case (xs, ys) of
            ([], _) -> returnA -< []
            (_, []) -> returnA -< []
            (x : xrest, y : yrest) -> 
                do
                z <- processOne -< (x,y)
                zrest <- zipWithA processOne -< (xrest, yrest)
                returnA -< z : zrest
