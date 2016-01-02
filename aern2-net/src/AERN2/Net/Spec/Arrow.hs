{-# LANGUAGE Arrows, TypeOperators, GeneralizedNewtypeDeriving, FunctionalDependencies, DataKinds, FlexibleContexts, ConstraintKinds #-}

module AERN2.Net.Spec.Arrow 
(
    _anet0, _anet1, _anet2, _anet3,
    CanEmbedFnA, embedFnNamedA,
    RealA,
    piA,
    ComplexA,
    RationalIntervalA(..),
    RealUnaryFnA(..),
    UFnDomE, UFnDomR,
    RealFnA(..),
    FnDomE, FnDomR,
    VarName, VarMap,
    RealUnaryFnFromArrowA(..),
    RealFnFromArrowA(..)
)
where

import AERN2.Num
import Data.String (IsString(..),fromString)

import Control.Arrow
import qualified Data.Map as Map


{- mini examples -}

-- | sqrt(pi) + pi
_anet0 :: (RealA to r) => () `to` r
_anet0 =
    proc _ -> do
        p <- piA -< ()
        sp <- sqrtA -< p
        psp <- addA -< (p,sp)
        let _ = [p,psp] -- ensure the type of p is the same as the type of the result
        returnA -< psp

-- | pi * sqrt(x) * x
_anet1 :: (RealA to r) => r `to` r
_anet1 =
    proc x -> do
        p <- piA -< ()
        sx <- sqrtA -< x
        psx <- mulA -< (p,sx)
        psxx <- mulA -< (psx,x)
        let _ = [p,psxx] -- ensure the type of p is the same as the type of the result
        returnA -< psxx

-- | sqrt(x^2+y^2+z^2)    
_anet2 :: (RealA to r) => (r,r,r) `to` r
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
_anet3 :: (RealA to r) => (Map.Map String r) `to` r
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

type CanEmbedFnA to r1 r2 = ArrowConvert [r1] (->) r1 [r2] to r2

{-| use a normal computation, bypassing the arrow -}
embedFnNamedA ::
    (CanEmbedFnA to r1 r2) => String -> ([r1] -> r1) -> [r2] `to` r2
embedFnNamedA = fn2arrowNamed


class
    (FieldA to r, HasCauchyRealsA to r, CanSqrtSameTypeA to r, CanExpSameTypeA to r, CanSineCosineSameTypeA to r) 
    => 
    RealA to r

piA :: (HasCauchyRealsA to r) => () `to` r
piA = proc () -> convertA -< pi -- TODO: use convertNamedA instead and add the name "pi"

class 
    (RealA to c, HasComplexA r to c) => 
    ComplexA r to c

class (FieldA to (IntervalE ri)) => RationalIntervalA to ri where
    type IntervalE ri
    type IntervalR ri
    getEndpointsA :: ri `to` (IntervalE ri,IntervalE ri)
    fromEndpointsA :: (IntervalE ri,IntervalE ri) `to` ri
    limitIntervalsToRealA :: [ri] `to` IntervalR ri 
--    splitIntervalA :: ri `to` (ri, ri)
--    subEqIntervalA :: (ri, ri) `to` Bool

type UFnDomE f = IntervalE (UFnDom f)
type UFnDomR f = IntervalR (UFnDom f)

class (RationalIntervalA to (UFnDom f), RealA to (UFnR f)) => RealUnaryFnA to f where
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

class (RationalIntervalA to (FnDom f), RealA to (FnR f)) => RealFnA to f where
    type FnDom f
    type FnR f
    constFnA :: (VarMap (FnDom f), FnR f) `to` f
    projFnA :: (VarMap (FnDom f), VarName) `to` f
    getDomainA :: f `to` (VarMap (FnDom f))
    evalAtPointFnA :: (f,VarMap (FnR f)) `to` (FnR f)
    evalAtFnDomEA :: (f,VarMap (FnDomE f)) `to` (FnR f)
    evalOnIntervalA :: (f,VarMap (FnDom f)) `to` ri
    fixSomeVariablesA :: (f, VarMap (FnR f)) `to` f

class (RealUnaryFnA to f) => RealUnaryFnFromArrowA to f where
    encloseUFn :: (RealA to2 r2) => (r2 `to2` r2, UFnDom f) -> () `to` f

class (RealFnA to f) => RealFnFromArrowA to f where
    encloseFn :: (RealA to2 r2) => (VarMap r2 `to2` r2, VarMap (FnDom f)) -> () `to` f

