{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

module AERN2.RealFunction 
(
    RealUnaryFnA(..)
)
where

import AERN2.Num

--import Control.Arrow
--import qualified Data.Map as Map

class 
    (ArrowReal to (UnaryFnOut f)
--    , UnaryFnOut f ~ LimitTypeA to (Interval (UnaryFnIn f))
    ) 
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


