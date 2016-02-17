{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

module AERN2.RealFunction 
(
--    RationalIntervalA(..),
    RealUnaryFnA(..),
--    UnaryFnDomE, UnaryFnDomR,
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
--type UnaryFnDomE f = IntervalE (UnaryFnDom f)
--type UnaryFnDomR f = IntervalR (UnaryFnDom f)

class 
    (ArrowReal to (UnaryFnOut f), 
     UnaryFnOut f ~ LimitTypeA to (Interval (UnaryFnIn f))) 
    => 
    RealUnaryFnA to f where
    type UnaryFnIn f
    type UnaryFnOut f
    constUnaryFnA :: (Interval (UnaryFnIn f), UnaryFnOut f) `to` f
    projUnaryFnA :: Interval (UnaryFnIn f) `to` f
    getDomainUnaryFnA :: f `to` (Interval (UnaryFnIn f))
    evalAtOutPointUnaryFnA :: (f, UnaryFnOut f) `to` (UnaryFnOut f)
    evalAtInPointUnaryFnA :: (f, UnaryFnIn f) `to` (UnaryFnOut f)
    evalOnIntervalUnaryFnA :: (f, Interval (UnaryFnIn f)) `to` Interval (UnaryFnIn f)

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
--    encloseUnaryFn :: (RealExprA to2 r2) => (r2 `to2` r2, UnaryFnDom f) -> () `to` f
--
--class (RealFnA to f) => RealFnFromArrowA to f where
--    encloseFn :: (RealExprA to2 r2) => (VarMap r2 `to2` r2, VarMap (FnDom f)) -> () `to` f


type CanEmbedFnA to r1 r2 = ArrowConvert [r1] (->) r1 [r2] to r2

{-| use a normal computation, bypassing the arrow -}
embedFnNamedA ::
    (CanEmbedFnA to r1 r2) => String -> ([r1] -> r1) -> [r2] `to` r2
embedFnNamedA = fn2arrowNamed
