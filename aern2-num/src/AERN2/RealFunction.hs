{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleContexts, ConstraintKinds #-}

module AERN2.RealFunction 
(
    RealUnaryFnA(..),
    CanIntegrateUnaryFnA(..)
)
where

import AERN2.Num

--import Control.Arrow
--import qualified Data.Map as Map

class 
    (ArrowReal to (UnaryFnPoint f)
--    , UnaryFnPoint f ~ LimitTypeA to (Interval (UnaryFnDomPoint f))
    ) 
    => 
    RealUnaryFnA to f where
    type UnaryFnDomPoint f
    type UnaryFnPoint f
    constUnaryFnA :: (Interval (UnaryFnDomPoint f), UnaryFnPoint f) `to` f
    projUnaryFnA :: Interval (UnaryFnDomPoint f) `to` f
    getDomainUnaryFnA :: f `to` (Interval (UnaryFnDomPoint f))
    evalAtPointUnaryFnA :: (f, UnaryFnPoint f) `to` (UnaryFnPoint f)
    evalAtDomPointUnaryFnA :: (f, UnaryFnDomPoint f) `to` (UnaryFnPoint f)
    rangeOnIntervalUnaryFnA :: (f, Interval (UnaryFnDomPoint f)) `to` Interval (UnaryFnPoint f)

class (RealUnaryFnA to f) => CanIntegrateUnaryFnA to f
    where
    integrateUnaryFnA :: (f, UnaryFnPoint f, UnaryFnPoint f) `to` UnaryFnPoint f 
