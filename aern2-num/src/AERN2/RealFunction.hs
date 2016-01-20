{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

module AERN2.RealFunction 
(
--    RationalIntervalA(..),
    RealUnaryFnA(..),
--    UFnDomE, UFnDomR,
--    RealFnA(..),
--    FnDomE, FnDomR,
--    VarName, VarMap,
--    RealUnaryFnFromArrowA(..),
--    RealFnFromArrowA(..),
    CanEmbedFnA, embedFnNamedA
)
where

import AERN2.Num

--import Control.Arrow
--import qualified Data.Map as Map



--class (FieldA to (IntervalE ri)) => RationalIntervalA to ri where
--    type IntervalE ri
--    type IntervalR ri
--    getEndpointsA :: ri `to` (IntervalE ri,IntervalE ri)
--    fromEndpointsA :: (IntervalE ri,IntervalE ri) `to` ri
--    limitIntervalsToRealA :: [ri] `to` IntervalR ri 
----    splitIntervalA :: ri `to` (ri, ri)
----    subEqIntervalA :: (ri, ri) `to` Bool
--
--type UFnDomE f = IntervalE (UFnDom f)
--type UFnDomR f = IntervalR (UFnDom f)

class (RealExprA to (UFnOut f), UFnOut f ~ LimitTypeA to (Interval (UFnIn f))) => RealUnaryFnA to f where
    type UFnIn f
    type UFnOut f
    constUFnA :: (Interval (UFnIn f), UFnOut f) `to` f
    projUFnA :: Interval (UFnIn f) `to` f
    getDomainUFnA :: f `to` (Interval (UFnIn f))
    evalAtOutPointUFnA :: (f, UFnOut f) `to` (UFnOut f)
    evalAtInPointUFnA :: (f, UFnIn f) `to` (UFnOut f)
    evalOnIntervalUFnA :: (f, Interval (UFnIn f)) `to` Interval (UFnIn f)

--type FnDomE f = IntervalE (FnDom f)
--type FnDomR f = IntervalR (FnDom f)
--
--class (RationalIntervalA to (FnDom f), RealExprA to (FnR f)) => RealFnA to f where
--    type FnDom f
--    type FnR f
--    constFnA :: (VarMap (FnDom f), FnR f) `to` f
--    projFnA :: (VarMap (FnDom f), VarName) `to` f
--    getDomainA :: f `to` (VarMap (FnDom f))
--    evalAtPointFnA :: (f,VarMap (FnR f)) `to` (FnR f)
--    evalAtFnDomEA :: (f,VarMap (FnDomE f)) `to` (FnR f)
--    evalOnIntervalA :: (f,VarMap (FnDom f)) `to` ri
--    fixSomeVariablesA :: (f, VarMap (FnR f)) `to` f

--class (RealUnaryFnA to f) => RealUnaryFnFromArrowA to f where
--    encloseUFn :: (RealExprA to2 r2) => (r2 `to2` r2, UFnDom f) -> () `to` f
--
--class (RealFnA to f) => RealFnFromArrowA to f where
--    encloseFn :: (RealExprA to2 r2) => (VarMap r2 `to2` r2, VarMap (FnDom f)) -> () `to` f


type CanEmbedFnA to r1 r2 = ArrowConvert [r1] (->) r1 [r2] to r2

{-| use a normal computation, bypassing the arrow -}
embedFnNamedA ::
    (CanEmbedFnA to r1 r2) => String -> ([r1] -> r1) -> [r2] `to` r2
embedFnNamedA = fn2arrowNamed
